#include "Basic.h"

#include "IrGenerator.h"
#include "Array.h"
#include "Ast.h"
#include "Lexer.h"
#include "Infer.h"
#include "BucketedArenaAllocator.h"
#include "CoffWriter.h"

WorkQueue<ExprFunction *> irGeneratorQueue;


u64 generateIr(IrState *state, Expr *expr, u64 dest);
u64 generateIr(IrState *state, Expr *expr, u64 dest, bool forceDest);

void pushLoop(IrState *state, ExprLoop *loop) {
	if (state->loopCount >= state->loopStack.count) {
		state->loopStack.add();
	}

	state->loopStack[state->loopCount].start = state->ir.count;
	state->loopStack[state->loopCount].loop = loop;
	state->loopStack[state->loopCount].endPatches.clear();

	++state->loopCount;
}

void popLoop(IrState *state) {
	--state->loopCount;

	Loop loop = state->loopStack[state->loopCount];

	for (auto patch : loop.endPatches) {
		state->ir[patch].b = state->ir.count;
	}
}

bool binaryOpHasSideEffects(TokenT op) {
	switch (op) {
		case TOKEN('='):
		case TokenT::PLUS_EQUALS:
		case TokenT::MINUS_EQUALS:
		case TokenT::LOGIC_AND:
		case TokenT::LOGIC_OR:
		case TokenT::AND_EQUALS:
		case TokenT::OR_EQUALS:
		case TokenT::XOR_EQUALS:
		case TokenT::SHIFT_LEFT_EQUALS:
		case TokenT::SHIFT_RIGHT_EQUALS:
		case TokenT::TIMES_EQUALS:
		case TokenT::DIVIDE_EQUALS:
		case TokenT::MOD_EQUALS:
			return true;
		default:
			return false;
	}
}

void convertType(IrState *state, Type *destType, u64 dest, Type *srcType, u64 src) {
	Ir &extend = state->ir.add();


	extend.a = src;
	extend.opSize = srcType->size;
	extend.destSize = destType->size;
	extend.dest = dest;

	if (destType->flavor == TypeFlavor::FLOAT && srcType->flavor != TypeFlavor::FLOAT) {
		extend.op = IrOp::INT_TO_FLOAT;

		if (srcType->flags & TYPE_INTEGER_IS_SIGNED) {
			extend.flags |= IR_SIGNED_OP;
		}
	}
	else if (destType->flavor != TypeFlavor::FLOAT && srcType->flavor == TypeFlavor::FLOAT) {
		extend.op = IrOp::FLOAT_TO_INT;

		if (destType->flags & TYPE_INTEGER_IS_SIGNED) {
			extend.flags |= IR_SIGNED_OP;
		}
	}
	else {
		extend.op = IrOp::EXTEND;

		if (destType->flags & srcType->flags & TYPE_INTEGER_IS_SIGNED) {
			extend.flags |= IR_SIGNED_OP;
		}
		else if (destType->flavor == TypeFlavor::FLOAT) {
			extend.flags |= IR_FLOAT_OP;
		}
	}

}

IrOp getIrOpForCompare(TokenT op) {
	switch (op) {
		case TokenT::EQUAL:
			return IrOp::EQUAL;
		case TokenT::NOT_EQUAL:
			return IrOp::NOT_EQUAL;
		case TokenT::GREATER_EQUAL:
			return IrOp::GREATER;
		case TokenT::LESS_EQUAL:
			return IrOp::LESS_EQUAL;
		case TOKEN('>'):
			return IrOp::GREATER;
		case TOKEN('<'):
			return IrOp::LESS;
		default:
			assert(false);
			return IrOp::NOOP;
	}
}

IrOp getIrOpForBitwise(TokenT op) {
	switch (op) {
		case TokenT::SHIFT_LEFT:
		case TokenT::SHIFT_LEFT_EQUALS:
			return IrOp::SHIFT_LEFT;
		case TokenT::SHIFT_RIGHT:
		case TokenT::SHIFT_RIGHT_EQUALS:
			return IrOp::SHIFT_RIGHT;
		case TOKEN('&'):
		case TokenT::AND_EQUALS:
			return IrOp::AND;
		case TOKEN('|'):
		case TokenT::OR_EQUALS:
			return IrOp::OR;
		case TOKEN('^'):
		case TokenT::XOR_EQUALS:
			return IrOp::XOR;
		default:
			assert(false);
			return IrOp::NOOP;
	}
}

IrOp getIrOpForMultiply(TokenT op) {
	switch (op) {
		case TOKEN('*'):
		case TokenT::TIMES_EQUALS:
			return IrOp::MUL;
		case TOKEN('/'):
		case TokenT::DIVIDE_EQUALS:
			return IrOp::DIV;
		case TOKEN('%'):
		case TokenT::MOD_EQUALS:
			return IrOp::MOD;
		default:
			assert(false);
			return IrOp::NOOP;
	}
}

struct IrModifyWrite {
	u64 addressReg;
	u64 leftReg;
	u64 rightReg;
};

IrModifyWrite readForModifyWrite(IrState *state, Expr *left, Expr *right) {
	IrModifyWrite info;

	u64 dest;

	if (left->flavor == ExprFlavor::BINARY_OPERATOR) {
		auto binary = static_cast<ExprBinaryOperator *>(left);
		dest = state->nextRegister++;

		assert(binary->op == TOKEN('['));

		auto pointer = static_cast<TypePointer *>(binary->left->type);

		u64 leftReg = generateIr(state, binary->left, dest);

		u64 temp = state->nextRegister++;
		u64 rightReg = generateIr(state, binary->right, temp, true);

		assert(temp == rightReg);

		if (binary->right->type->size != 8) {
			convertType(state, (binary->right->type->flags & TYPE_INTEGER_IS_SIGNED) ? &TYPE_S64 : &TYPE_U64, temp, binary->right->type, temp);
		}

		if (pointer->pointerTo->size != 1) {
			Ir &mul = state->ir.add();

			mul.op = IrOp::MUL_BY_CONSTANT;
			mul.dest = temp;
			mul.a = temp;
			mul.b = pointer->pointerTo->size;
			mul.opSize = 8;

			rightReg = temp;
		}

		Ir &add = state->ir.add();
		add.op = IrOp::ADD;
		add.dest = dest;
		add.a = leftReg;
		add.b = temp;
		add.opSize = 8;

		Ir &read = state->ir.add();
		read.op = IrOp::READ;
		read.dest = temp;
		read.a = dest;
		add.opSize = 8;
		add.destSize = left->type->size;

		info.addressReg = dest;
		info.leftReg = temp;
	}
	else if (left->flavor == ExprFlavor::UNARY_OPERATOR) {
		auto unary = static_cast<ExprUnaryOperator *>(left);
		dest = state->nextRegister++;

		assert(unary->op == TokenT::SHIFT_LEFT);

		u64 leftReg = generateIr(state, unary->value, dest);

		Ir &read = state->ir.add();
		read.op = IrOp::READ;
		read.dest = state->nextRegister++;
		read.a = dest;
		read.opSize = 8;
		read.destSize = left->type->size;

		info.addressReg = dest;
		info.leftReg = read.dest;
	}
	else if (left->flavor == ExprFlavor::STRUCT_ACCESS) {
		assert(false); // @Incomplete
		return {};
	}
	else if (left->flavor == ExprFlavor::IDENTIFIER) {
		auto identifier = static_cast<ExprIdentifier *>(left);

		if (identifier->declaration->enclosingScope == &globalBlock) {
			dest = state->nextRegister++;

			Ir &address = state->ir.add();
			address.op = IrOp::ADDRESS_OF_GLOBAL;
			address.dest = dest;
			address.declaration = identifier->declaration;
			address.opSize = 8;
			address.destSize = 8;

			Ir &read = state->ir.add();
			read.op = IrOp::READ;
			read.dest = state->nextRegister++;
			read.a = dest;
			read.opSize = 8;
			read.destSize = left->type->size;

			info.addressReg = dest;
			info.leftReg = read.dest;
		}
		else {
			info.addressReg = 0;
			info.leftReg = identifier->declaration->irRegister;
		}
	}
	else {
		assert(false);
		return {};
	}

	info.rightReg = generateIr(state, right, state->nextRegister++);

	return info;
}

void writeForModifyWrite(IrState *state, IrModifyWrite info, Expr *left) {
	if (left->flavor != ExprFlavor::IDENTIFIER || static_cast<ExprIdentifier *>(left)->declaration->enclosingScope != &globalBlock) {
		Ir &write = state->ir.add();
		write.op = IrOp::WRITE;
		write.opSize = left->type->size;
		write.a = info.addressReg;
		write.b = info.leftReg;
	}
}

static void generateIncrement(IrState *state, ExprLoop *loop) {
	auto it = loop->iteratorBlock.declarations[0];
	auto it_index = loop->iteratorBlock.declarations[1];

	Ir &one = state->ir.add();

	Ir &index = state->ir.add();
	index.op = IrOp::ADD_CONSTANT;
	index.opSize = 8;
	index.a = it_index->irRegister;
	index.b = 1;
	index.dest = it_index->irRegister;

	Ir &increment = state->ir.add();
	increment.op = IrOp::ADD_CONSTANT;
	increment.opSize = loop->forBegin->type->size;
	increment.a = loop->irPointer;
	increment.dest = loop->irPointer;

	if (loop->forBegin->type->flavor == TypeFlavor::POINTER) {
		auto pointer = static_cast<TypePointer *>(loop->forBegin->type);

		increment.b = pointer->pointerTo->size;

		if (loop->flags & EXPR_FOR_BY_POINTER) {
			Ir &read = state->ir.add();
			read.dest = it->irRegister;
			read.opSize = 8;
			read.destSize = pointer->pointerTo->size;
			read.a = loop->irPointer;
			read.op = IrOp::READ;
		}
	}
	else {
		increment.b = 1;
	}
}

u64 generateIr(IrState *state, Expr *expr, u64 dest) {
	switch (expr->flavor) {
		case ExprFlavor::BINARY_OPERATOR: {

			auto binary = static_cast<ExprBinaryOperator *>(expr);
			if (dest == DEST_NONE) {
				if (binaryOpHasSideEffects(binary->op)) {
					// We have to do the op anyway
				}
				else {
					if (binary->op != TokenT::CAST) {
						generateIr(state, binary->left, dest);
						generateIr(state, binary->right, dest);
					}

					return DEST_NONE;
				}
			}

			auto left = binary->left;
			auto right = binary->right;

			switch (binary->op) {
				case TokenT::CAST: {
					assert(binary->left->flavor == ExprFlavor::TYPE_LITERAL);

					u64 rightReg = generateIr(state, right, dest);

					Type *castTo = binary->type;

					if (typesAreSame(castTo, right->type))
						return rightReg;

					switch(castTo->flavor) {
						case TypeFlavor::BOOL: {
							Ir &ir = state->ir.add();

							ir.op = IrOp::NOT_EQUAL;
							ir.dest = dest;
							ir.a = rightReg;
							ir.b = 0;
							ir.opSize = right->type->size;
							
							if (right->type->flavor == TypeFlavor::FLOAT)
								ir.flags |= IR_FLOAT_OP;

							break;
						}
						case TypeFlavor::FLOAT: {
							convertType(state, castTo, dest, right->type, rightReg);

							return dest;
						}
						case TypeFlavor::INTEGER: {
							if (right->type->flavor == TypeFlavor::POINTER) {
								return rightReg;
							}
							else if (right->type->flavor == TypeFlavor::FUNCTION) {
								return rightReg;
							}

							convertType(state, castTo, dest, right->type, rightReg);

							return dest;
						}
						case TypeFlavor::POINTER:
						case TypeFlavor::FUNCTION:
							return rightReg; // These casts should be a nop
						case TypeFlavor::TYPE:
						case TypeFlavor::VOID:
							assert(false);
							break;
					}
					
					assert(false);
				}
				case TOKEN('['): {
					assert(left->type->flavor == TypeFlavor::POINTER);

					auto pointer = static_cast<TypePointer *>(left->type);

					u64 leftReg = generateIr(state, left, dest);

					u64 temp = state->nextRegister++;

					u64 rightReg = generateIr(state, right, temp);

					if (right->type->size != 8) {
						convertType(state, (right->type->flags & TYPE_INTEGER_IS_SIGNED) ? &TYPE_S64 : &TYPE_U64, temp, right->type, rightReg);
						rightReg = temp;
					}

					if (pointer->pointerTo->size != 1) {
						Ir &mul = state->ir.add();

						mul.op = IrOp::MUL_BY_CONSTANT;
						mul.dest = temp;
						mul.a = rightReg;
						mul.b = pointer->pointerTo->size;
						mul.opSize = 8;

						rightReg = temp;
					}

					Ir &add = state->ir.add();
					add.op = IrOp::ADD;
					add.dest = dest;
					add.a = leftReg;
					add.b = rightReg;
					add.opSize = 8;
					
					Ir &read = state->ir.add();
					read.op = IrOp::READ;
					read.a = dest;
					read.dest = dest;
					read.opSize = 8;
					read.destSize = pointer->pointerTo->size;

					return dest;
				}
				case TokenT::EQUAL:
				case TokenT::NOT_EQUAL:
				case TokenT::GREATER_EQUAL:
				case TokenT::LESS_EQUAL:
				case TOKEN('>'):
				case TOKEN('<'): {
					u64 leftReg = generateIr(state, left, dest);

					u64 temp = state->nextRegister++;

					u64 rightReg = generateIr(state, right, temp);

					Ir &ir = state->ir.add();

					ir.op = getIrOpForCompare(binary->op);
					ir.dest = dest;
					ir.a = leftReg;
					ir.b = rightReg;
					ir.opSize = right->type->size;

					if (left->type->flags & TYPE_INTEGER_IS_SIGNED) {
						assert(right->type->flags & TYPE_INTEGER_IS_SIGNED);

						ir.flags |= IR_SIGNED_OP;
					}
					else if (right->type->flavor == TypeFlavor::FLOAT)
						ir.flags |= IR_FLOAT_OP;

					return dest;
				}
				case TOKEN('+'):
				case TOKEN('-'): {
					if (left->type->flavor == TypeFlavor::POINTER) {
						auto pointer = static_cast<TypePointer *>(left->type);

						u64 leftReg = generateIr(state, left, dest);

						u64 temp = state->nextRegister++;

						u64 rightReg = generateIr(state, right, temp);

						if (right->type->size != 8) {
							convertType(state, (right->type->flags & TYPE_INTEGER_IS_SIGNED) ? &TYPE_S64 : &TYPE_U64, temp, right->type, rightReg);
							rightReg = temp;
						}

						if (pointer->pointerTo->size != 1) {
							Ir &mul = state->ir.add();

							mul.op = IrOp::MUL_BY_CONSTANT;
							mul.dest = temp;
							mul.a = rightReg;
							mul.b = pointer->pointerTo->size;
							mul.opSize = 8;

							rightReg = temp;
						}

						Ir &add = state->ir.add();
						add.op = binary->op == TOKEN('+') ? IrOp::ADD : IrOp::SUB;
						add.dest = dest;
						add.a = leftReg;
						add.b = rightReg;
						add.opSize = 8;
					}
					else {
						u64 leftReg = generateIr(state, left, dest);

						u64 temp = state->nextRegister++;

						u64 rightReg = generateIr(state, right, temp);

						Ir &ir = state->ir.add();

						ir.op = binary->op == TOKEN('+') ? IrOp::ADD : IrOp::SUB;
						ir.dest = dest;
						ir.a = leftReg;
						ir.b = rightReg;
						ir.opSize = right->type->size;

						if (right->type->flavor == TypeFlavor::FLOAT)
							ir.flags |= IR_FLOAT_OP;
					}

					return dest;
				}
				case TOKEN('&'):
				case TOKEN('|'):
				case TOKEN('^'):
				case TokenT::SHIFT_LEFT:
				case TokenT::SHIFT_RIGHT: {
					u64 leftReg = generateIr(state, left, dest);

					u64 temp = state->nextRegister++;

					u64 rightReg = generateIr(state, right, temp);

					Ir &ir = state->ir.add();

					ir.op = getIrOpForBitwise(binary->op);
					ir.dest = dest;
					ir.a = leftReg;
					ir.b = rightReg;
					ir.opSize = right->type->size;

					if (binary->op == TokenT::SHIFT_RIGHT && left->type->flags & TYPE_INTEGER_IS_SIGNED) {
						assert(right->type->flags & TYPE_INTEGER_IS_SIGNED);

						ir.flags |= IR_SIGNED_OP;
					}

					return dest;
				}
				case TOKEN('*'):
				case TOKEN('/'):
				case TOKEN('%'): {
					u64 leftReg = generateIr(state, left, dest);

					u64 temp = state->nextRegister++;

					u64 rightReg = generateIr(state, right, temp);

					Ir &ir = state->ir.add();

					ir.op = getIrOpForMultiply(binary->op);
					ir.dest = dest;
					ir.a = leftReg;
					ir.b = rightReg;
					ir.opSize = right->type->size;

					if (binary->op != TOKEN('*') && left->type->flags & TYPE_INTEGER_IS_SIGNED) {
						assert(right->type->flags & TYPE_INTEGER_IS_SIGNED);

						ir.flags |= IR_SIGNED_OP;
					}
					
					return dest;
				}
				case TOKEN('='): {
					assert(dest == DEST_NONE);

					if (left->flavor == ExprFlavor::BINARY_OPERATOR) {
						auto binary = static_cast<ExprBinaryOperator *>(left);
						dest = state->nextRegister++;
						
						assert(binary->op == TOKEN('['));

						auto pointer = static_cast<TypePointer *>(binary->left->type);

						u64 leftReg = generateIr(state, binary->left, dest);

						u64 temp = state->nextRegister++;
						u64 rightReg = generateIr(state, binary->right, temp);

						if (binary->right->type->size != 8) {
							convertType(state, (binary->right->type->flags & TYPE_INTEGER_IS_SIGNED) ? &TYPE_S64 : &TYPE_U64, temp, binary->right->type, rightReg);
							rightReg = temp;
						}

						if (pointer->pointerTo->size != 1) {
							Ir &mul = state->ir.add();

							mul.op = IrOp::MUL_BY_CONSTANT;
							mul.dest = temp;
							mul.a = rightReg;
							mul.b = pointer->pointerTo->size;
							mul.opSize = 8;

							rightReg = temp;
						}

						Ir &add = state->ir.add();
						add.op = IrOp::ADD;
						add.dest = dest;
						add.a = leftReg;
						add.b = rightReg;
						add.opSize = 8;

						rightReg = generateIr(state, right, rightReg);

						Ir &write = state->ir.add();
						write.op = IrOp::WRITE;
						write.a = dest;
						write.b = rightReg;
						write.opSize = 8;

						return rightReg;
					}
					else if (left->flavor == ExprFlavor::UNARY_OPERATOR) {
						auto unary = static_cast<ExprUnaryOperator *>(left);
						dest = state->nextRegister++;

						assert(unary->op == TokenT::SHIFT_LEFT);

						u64 leftReg = generateIr(state, unary->value, dest);

						u64 rightReg = generateIr(state, right, state->nextRegister++);

						Ir &write = state->ir.add();
						write.op = IrOp::WRITE;
						write.a = leftReg;
						write.b = rightReg;
						write.opSize = right->type->size;

						return rightReg;
					}
					else if (left->flavor == ExprFlavor::STRUCT_ACCESS) {
						assert(false); // @Incomplete
					}
					else if (left->flavor == ExprFlavor::IDENTIFIER) {
						auto identifier = static_cast<ExprIdentifier *>(left);

						if (identifier->declaration->enclosingScope == &globalBlock) {
							dest = state->nextRegister++;

							Ir &address = state->ir.add();
							address.op = IrOp::ADDRESS_OF_GLOBAL;
							address.declaration = identifier->declaration;
							address.dest = dest;
							address.opSize = 8;

							u64 rightReg = generateIr(state, right, state->nextRegister++);
							
							Ir &write = state->ir.add();
							write.op = IrOp::WRITE;
							write.a = dest;
							write.b = rightReg;
							write.opSize = right->type->size;

							return rightReg;
						}
						else {
							u64 stored = generateIr(state, right, identifier->declaration->irRegister, true);

							assert(stored == identifier->declaration->irRegister);

							return stored;
						}
					}
					else {
						assert(false);
					}

					
					return dest;
				}
				case TokenT::PLUS_EQUALS:
				case TokenT::MINUS_EQUALS: {
					assert(dest == DEST_NONE);

					auto info = readForModifyWrite(state, left, right);

					if (left->type->flavor == TypeFlavor::POINTER) {
						auto pointer = static_cast<TypePointer *>(left->type);

						u64 temp = state->nextRegister++;

						if (right->type->size != 8) {
							convertType(state, (right->type->flags & TYPE_INTEGER_IS_SIGNED) ? &TYPE_S64 : &TYPE_U64, temp, right->type, info.rightReg);
							info.rightReg = temp;
						}

						if (pointer->pointerTo->size != 1) {
							Ir &mul = state->ir.add();

							mul.op = IrOp::MUL_BY_CONSTANT;
							mul.dest = temp;
							mul.a = info.rightReg;
							mul.b = pointer->pointerTo->size;
							mul.opSize = 8;

							info.rightReg = temp;
						}

						Ir &add = state->ir.add();
						add.op = binary->op == TOKEN('+') ? IrOp::ADD : IrOp::SUB;
						add.dest = info.leftReg;
						add.a = info.leftReg;
						add.b = info.rightReg;
						add.opSize = 8;
					}
					else {
						Ir &ir = state->ir.add();

						ir.op = binary->op == TOKEN('+') ? IrOp::ADD : IrOp::SUB;
						ir.dest = info.leftReg;
						ir.a = info.leftReg;
						ir.b = info.rightReg;
						ir.opSize = right->type->size;

						if (right->type->flavor == TypeFlavor::FLOAT)
							ir.flags |= IR_FLOAT_OP;
					}

					writeForModifyWrite(state, info, left);
					
					return info.leftReg;
				}
				case TokenT::LOGIC_AND:
				case TokenT::LOGIC_OR: {
					if (dest == DEST_NONE) dest = state->nextRegister++;

					generateIr(state, left, dest, true);
					
					u64 patch = state->ir.count;

					Ir &branch = state->ir.add();
					branch.op = binary->op == TokenT::LOGIC_AND ? IrOp::IF_Z_GOTO : IrOp::IF_NZ_GOTO;
					branch.a = dest;
					branch.opSize = 1;

					generateIr(state, right, dest, true);

					state->ir[patch].b = state->ir.count;

					return dest;
				}
				case TokenT::AND_EQUALS:
				case TokenT::OR_EQUALS:
				case TokenT::XOR_EQUALS:
				case TokenT::SHIFT_LEFT_EQUALS:
				case TokenT::SHIFT_RIGHT_EQUALS: {
					auto info = readForModifyWrite(state, left, right);

					Ir &ir = state->ir.add();

					ir.op = getIrOpForBitwise(binary->op);
					ir.dest = info.leftReg;
					ir.a = info.leftReg;
					ir.b = info.rightReg;
					ir.opSize = right->type->size;

					if (binary->op == TokenT::SHIFT_RIGHT_EQUALS && left->type->flags & TYPE_INTEGER_IS_SIGNED) {
						assert(right->type->flags & TYPE_INTEGER_IS_SIGNED);

						ir.flags |= IR_SIGNED_OP;
					}

					writeForModifyWrite(state, info, left);

					return info.leftReg;
				}
				case TokenT::TIMES_EQUALS:
				case TokenT::DIVIDE_EQUALS:
				case TokenT::MOD_EQUALS: {
					auto info = readForModifyWrite(state, left, right);

					Ir &ir = state->ir.add();

					ir.op = getIrOpForMultiply(binary->op);
					ir.dest = info.leftReg;
					ir.a = info.leftReg;
					ir.b = info.rightReg;
					ir.opSize = right->type->size;

					if (binary->op != TokenT::TIMES_EQUALS && left->type->flags & TYPE_INTEGER_IS_SIGNED) {
						assert(right->type->flags & TYPE_INTEGER_IS_SIGNED);

						ir.flags |= IR_SIGNED_OP;
					}

					writeForModifyWrite(state, info, left);

					return info.leftReg;
				}
				default:
					assert(false);
			}

			break;
		}
		case ExprFlavor::BLOCK: {
			auto block = static_cast<ExprBlock *>(expr);

			for (auto declaration : block->declarations.declarations) {
				if (!(declaration->flags & DECLARATION_IS_CONSTANT)) {
					declaration->irRegister = state->nextRegister++;
				}
			}

			for (auto subExpr : block->exprs) {
				generateIr(state, subExpr, DEST_NONE);
			}

			return DEST_NONE;
		}
		case ExprFlavor::BREAK: {
			auto break_ = static_cast<ExprBreakOrContinue *>(expr);


			for (u64 i = state->loopCount; i-- != 0;) {

				if (state->loopStack[i].loop == break_->refersTo) {
					state->loopStack[i].endPatches.add(state->ir.count);
					break;
				}
			}

			Ir &jump = state->ir.add();
			jump.op = IrOp::GOTO;

			return DEST_NONE;
		}
		case ExprFlavor::CONTINUE: {
			auto continue_ = static_cast<ExprBreakOrContinue *>(expr);

			u64 begin;

			for (u64 i = state->loopCount; i-- != 0;) {
				if (state->loopStack[i].loop->flavor == ExprFlavor::FOR) {
					generateIncrement(state, state->loopStack[i].loop);
				}

				if (state->loopStack[i].loop == continue_->refersTo) {
					begin = state->loopStack[i].start;
					break;
				}
			}

			Ir &jump = state->ir.add();
			jump.op = IrOp::GOTO;
			jump.b = begin;

			return DEST_NONE;
		}
		case ExprFlavor::INT_LITERAL: {
			if (dest == DEST_NONE) return DEST_NONE;

			auto literal = static_cast<ExprLiteral *>(expr);

			if (literal->unsignedValue == 0) {
				return 0;
			}
			else {
				Ir &load = state->ir.add();
				load.op = IrOp::IMMEDIATE;
				load.dest = dest;
				load.a = literal->unsignedValue;
				load.opSize = literal->type->size;

				if (literal->type->flags & TYPE_INTEGER_IS_SIGNED)
					load.flags |= IR_SIGNED_OP;

				return dest;
			}
		}
		case ExprFlavor::FLOAT_LITERAL: {
			if (dest == DEST_NONE) return DEST_NONE;

			auto literal = static_cast<ExprLiteral *>(expr);

			if (literal->floatValue == 0.0) {
				return 0;
			}
			else {
				Ir &load = state->ir.add();
				load.op = IrOp::IMMEDIATE;
				load.dest = dest;
				load.a = literal->unsignedValue;
				load.opSize = literal->type->size;

				load.flags |= IR_FLOAT_OP;

				return dest;
			}
		}
		case ExprFlavor::FOR: {
			auto loop = static_cast<ExprLoop *>(expr);

			auto it = loop->iteratorBlock.declarations[0];
			auto it_index = loop->iteratorBlock.declarations[1];

			it->irRegister = state->nextRegister++;
			it_index->irRegister = state->nextRegister++;

			u64 itReg = it->irRegister;
			u64 it_indexReg = it_index->irRegister;

			if (loop->forBegin->type->flavor == TypeFlavor::POINTER && !(loop->flags & EXPR_FOR_BY_POINTER)) {
				loop->irPointer = state->nextRegister++;
			}
			else {
				loop->irPointer = itReg;
			}


			generateIr(state, loop->forBegin, loop->irPointer, true);

			u64 irEnd = generateIr(state, loop->forEnd, state->nextRegister++);

			pushLoop(state, loop);

			u64 compareDest = state->nextRegister++;

			Ir &compare = state->ir.add();
			compare.op = IrOp::LESS;
			compare.a = loop->irPointer;
			compare.b = irEnd;
			compare.dest = compareDest;
			compare.opSize = loop->forBegin->type->size;

			if (loop->forBegin->type->flags & TYPE_INTEGER_IS_SIGNED)
				compare.flags |= IR_SIGNED_OP;

			state->loopStack[state->loopCount - 1].endPatches.add(state->ir.count);

			Ir &branch = state->ir.add();
			branch.op = IrOp::IF_Z_GOTO;
			branch.a = compareDest;
			branch.opSize = 1;
		
			if (loop->body)
				generateIr(state, loop->body, DEST_NONE);

			generateIncrement(state, loop);

			popLoop(state);
			
			return DEST_NONE;
		}
		case ExprFlavor::FUNCTION: {
			if (dest == DEST_NONE) return dest;

			Ir &address = state->ir.add();
			address.dest = dest;
			address.opSize = 8;
			address.op = IrOp::ADDRESS_OF_GLOBAL;
			address.function = static_cast<ExprFunction *>(expr);
			
			return dest;
		}
		case ExprFlavor::FUNCTION_CALL: {
			if (dest == DEST_NONE) dest = 0;

			auto call = static_cast<ExprFunctionCall *>(expr);

			if (call->function->flavor == ExprFlavor::FUNCTION) {
			u64 function = generateIr(state, call->function, state->nextRegister++);

			FunctionCall *argumentInfo = nullptr;

			if (call->argumentCount) {
				argumentInfo = static_cast<FunctionCall *>(state->allocator.allocate(sizeof(FunctionCall) + sizeof(u64) * call->argumentCount));
				argumentInfo->argCount = call->argumentCount;
			}

			for (u64 i = 0; i < call->argumentCount; i++) {
				argumentInfo->args[i] = generateIr(state, call->arguments[i], state->nextRegister++);
			}

			Ir &ir = state->ir.add();
			ir.op = IrOp::CALL;
			ir.a = function;
			ir.arguments = argumentInfo;
			ir.dest = dest;
			ir.opSize = call->type->size;

			return dest;
		}
		case ExprFlavor::IDENTIFIER: {
			auto identifier = static_cast<ExprIdentifier *>(expr);

			if (identifier->declaration->enclosingScope == &globalBlock) {
				if (dest == DEST_NONE) return DEST_NONE;

				Ir &address = state->ir.add();
				address.dest = dest;
				address.opSize = 8;
				address.op = IrOp::ADDRESS_OF_GLOBAL;
				address.declaration = identifier->declaration;

				Ir &read = state->ir.add();
				read.op = IrOp::READ;
				read.dest = dest;
				read.a = dest;
				read.opSize = 8;
				read.destSize = identifier->type->size;

				return dest;
			}
			else {
				return identifier->declaration->irRegister;
			}
		}
		case ExprFlavor::IF: {
			auto ifElse = static_cast<ExprIf *>(expr);

			u64 conditionReg = generateIr(state, ifElse->condition, state->nextRegister++);

			if (ifElse->ifBody && ifElse->elseBody) {
				u64 patchIfZ = state->ir.count;
				Ir &ifZ = state->ir.add();
				ifZ.op = IrOp::IF_Z_GOTO;
				ifZ.a = conditionReg;
				ifZ.opSize = 1;

				generateIr(state, ifElse->ifBody, DEST_NONE);
				
				u64 patchJump = state->ir.count;
				Ir &jump = state->ir.add();
				jump.op = IrOp::GOTO;
				
				state->ir[patchIfZ].b = state->ir.count;

				generateIr(state, ifElse->elseBody, DEST_NONE);

				state->ir[patchJump].b = state->ir.count;
			}
			else if (ifElse->ifBody) {
				u64 patchIfZ = state->ir.count;
				Ir &ifZ = state->ir.add();
				ifZ.op = IrOp::IF_Z_GOTO;
				ifZ.a = conditionReg;
				ifZ.opSize = 1;

				generateIr(state, ifElse->ifBody, DEST_NONE);

				state->ir[patchIfZ].b = state->ir.count;
			}
			else if (ifElse->elseBody) {
				u64 patchIfNZ = state->ir.count;
				Ir &ifNZ = state->ir.add();
				ifNZ.op = IrOp::IF_NZ_GOTO;
				ifNZ.a = conditionReg;
				ifNZ.opSize = 1;

				generateIr(state, ifElse->elseBody, DEST_NONE);

				state->ir[patchIfNZ].b = state->ir.count;
			}

			return DEST_NONE;
		}
		case ExprFlavor::RETURN: {
			auto return_ = static_cast<ExprReturn *>(expr);

			u64 result = 0;

			result = return_->value ? generateIr(state, return_->value, state->nextRegister++) : 0;

			Ir &ir = state->ir.add();
			ir.op = IrOp::RETURN;
			ir.a = result;
			ir.opSize = return_->value ? return_->value->type->size : 0;

			if (return_->value && return_->value->type->flavor == TypeFlavor::FLOAT) {
				ir.flags |= IR_FLOAT_OP;
			}

			return DEST_NONE;
		}
		case ExprFlavor::STRING_LITERAL: {
			// @Incomplete
			assert(false);
			return DEST_NONE;
		}
		case ExprFlavor::STRUCT_ACCESS: {
			// @Incomplete
			assert(false);
			return DEST_NONE;
		}
		case ExprFlavor::TYPE_LITERAL: {
			assert(false); // @ErrorMessage
			return DEST_NONE;
		}
		case ExprFlavor::UNARY_OPERATOR: {
			ExprUnaryOperator *unary = static_cast<ExprUnaryOperator *>(expr);

			if (dest == DEST_NONE) {
				generateIr(state, unary->value, DEST_NONE);
				return DEST_NONE;
			}

			switch (unary->op) {
				case TOKEN('*'): {
					if (unary->value->flavor == ExprFlavor::BINARY_OPERATOR) {
						auto binary = static_cast<ExprBinaryOperator *>(unary->value);

						assert(binary->op == TOKEN('['));

						auto pointer = static_cast<TypePointer *>(binary->left->type);

						u64 leftReg = generateIr(state, binary->left, dest);

						u64 temp = state->nextRegister++;
						u64 rightReg = generateIr(state, binary->right, temp, true);

						assert(temp == rightReg);

						if (binary->right->type->size != 8) {
							convertType(state, (binary->right->type->flags & TYPE_INTEGER_IS_SIGNED) ? &TYPE_S64 : &TYPE_U64, temp, binary->right->type, temp);
						}

						if (pointer->pointerTo->size != 1) {
							Ir &mul = state->ir.add();

							mul.op = IrOp::MUL_BY_CONSTANT;
							mul.dest = temp;
							mul.a = temp;
							mul.b = pointer->pointerTo->size;
							mul.opSize = 8;
						}

						Ir &add = state->ir.add();
						add.op = IrOp::ADD;
						add.dest = dest;
						add.a = leftReg;
						add.b = temp;
						add.opSize = 8;

						return dest;
					}
					else if (unary->value->flavor == ExprFlavor::STRUCT_ACCESS) {
						assert(false); // @Incomplete
						return DEST_NONE;
					}
					else if (unary->value->flavor == ExprFlavor::IDENTIFIER) {
						auto identifier = static_cast<ExprIdentifier *>(unary->value);

						Ir &address = state->ir.add();
						address.dest = dest;
						address.opSize = 8;

						if (identifier->declaration->enclosingScope == &globalBlock) {
							address.op = IrOp::ADDRESS_OF_GLOBAL;
							address.declaration = identifier->declaration;
						}
						else {
							address.op = IrOp::ADDRESS_OF_LOCAL;
							address.a = identifier->declaration->irRegister;
						}

						return dest;
					}
				}
				case TOKEN('-'): {
					u64 toNegate = generateIr(state, unary->value, dest);

					Ir &negate = state->ir.add();
					negate.op = IrOp::NEG;
					negate.a = toNegate;
					negate.opSize = unary->value->type->size;
					negate.destSize = unary->type->size;
					negate.dest = dest;

					if (unary->type->flavor == TypeFlavor::FLOAT) {
						negate.flags |= IR_FLOAT_OP;
					}

					return dest;
				}
				case TOKEN('~'): {
					u64 toInvert = generateIr(state, unary->value, dest);

					Ir &invert = state->ir.add();
					invert.op = IrOp::NOT;
					invert.a = toInvert;
					invert.opSize = unary->value->type->size;
					invert.destSize = unary->type->size;
					invert.dest = dest;

					return dest;
				}
				case TOKEN('!'): {
					u64 toInvert = generateIr(state, unary->value, dest);

					Ir &invert = state->ir.add();
					invert.op = IrOp::EQUAL;
					invert.a = toInvert;
					invert.b = 0;
					invert.opSize = unary->value->type->size;
					invert.dest = dest;

					return dest;
				}
				case TokenT::SHIFT_LEFT: {
					u64 addressReg = generateIr(state, unary->value, dest);

					Ir &read = state->ir.add();
					read.op = IrOp::READ;
					read.opSize = 8;
					read.destSize = unary->type->size;
					read.a = addressReg;
					read.dest = dest;

					return dest;
				}
				default:
					assert(false);
					return DEST_NONE;
			}
		}
		case ExprFlavor::WHILE: {
			ExprLoop *loop = static_cast<ExprLoop *>(expr);

			pushLoop(state, loop);

			u64 conditionReg = generateIr(state, loop->whileCondition, state->nextRegister++);
			
			state->loopStack[state->loopCount - 1].endPatches.add(state->ir.count);

			Ir &ifZ = state->ir.add();
			ifZ.op = IrOp::IF_Z_GOTO;
			ifZ.a = conditionReg;
			ifZ.opSize = 1;

			if (loop->body)
				generateIr(state, loop->body, DEST_NONE);

			Ir &jump = state->ir.add();
			jump.op = IrOp::GOTO;
			jump.b = state->loopStack[state->loopCount - 1].start;

			return DEST_NONE;
		}
		default:
			assert(false);
	}
}

u64 generateIr(IrState *state, Expr *expr, u64 dest, bool forceDest) {
	if (forceDest) {
		assert(dest != DEST_NONE);

		u64 stored = generateIr(state, expr, dest);

		if (stored != dest) {
			Ir &set = state->ir.add();
			set.op = IrOp::SET;
			set.destSize = expr->type->size;
			set.a = stored;
			set.dest = dest;
		}

		return dest;
	}
	else {
		return generateIr(state, expr, dest);
	}
}

void runIrGenerator() {

	while (true) {

		ExprFunction *function = irGeneratorQueue.take();

		if (!function)
			break;

		for (u64 i = 0; i < function->arguments.declarations.count; i++) {
			function->arguments.declarations[i]->irRegister = function->state.nextRegister++;
		}

		generateIr(&function->state, function->body, DEST_NONE);

		// @Incomplete check wether the function actually returns instead of just adding a void return
		Ir &ret = function->state.ir.add();
		ret.op = IrOp::RETURN;
		ret.a = 0;
		ret.opSize = 0;


		coffWriterQueue.add(function);



		// @Incomplete submit for platform code gen/interpreting
//
//		for (u64 i = 0; i < state.ir.count; i++) {
//			std::cout << i << ": ";
//
//			Ir &ir = state.ir[i];
//
//#define REG(reg) (reg ? "R" : "") << reg
//
//			switch (ir.op) {
//			case IrOp::ADD: {
//				std::cout << "add " << ir.opSize << REG(ir.dest) << " <- " << REG(ir.a) << ' ' << REG(ir.b) << '\n';
//				break;
//			}
//			case IrOp::ADDRESS_OF_GLOBAL: {
//				std::cout << "address_of_global " << REG(ir.dest) << " <- " << ir.declaration->name << " " << ir.declaration << '\n';
//				break;
//			}
//
//			case IrOp::ADDRESS_OF_LOCAL: {
//				std::cout << "address_of_local " << REG(ir.dest) << " <- " << REG(ir.a) << '\n';
//				break;
//			}
//			case IrOp::ADD_CONSTANT: {
//				std::cout << "add " << ir.opSize << REG(ir.dest) << " <- " << REG(ir.a) << ' ' << ir.b << '\n';
//				break;
//			}
//			case IrOp::AND: {
//				std::cout << "and " << ir.opSize << REG(ir.dest) << " <- " << REG(ir.a) << ' ' << ir.b << '\n';
//				break;
//			}
//
//			case IrOp::CALL: {
//
//
//				std::cout << "call " << ir.opSize ;
//				break;
//			}
//			}
//		}
	}

	coffWriterQueue.add(nullptr);
}