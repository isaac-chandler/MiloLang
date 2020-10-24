#include "Basic.h"

#include "IrGenerator.h"
#include "Array.h"
#include "Ast.h"
#include "Lexer.h"
#include "BucketedArenaAllocator.h"
#include "CoffWriter.h"
#include "TypeTable.h"

struct Loop {
	struct ExprLoop *loop;
	u64 start;

	Array<u64> endPatches;
};

Array<Expr *> deferStack;
static Block *currentBlock;

Array<Loop> loopStack;
u64 loopCount;

u64 generateIr(IrState *state, Expr *expr, u64 dest, bool destWasForced = false);
u64 generateIrForceDest(IrState *state, Expr *expr, u64 dest);

static void pushLoop(IrState *state, ExprLoop *loop) {
	if (loopCount >= loopStack.count) {
		loopStack.add();
	}

	loopStack[loopCount].start = state->ir.count;
	loopStack[loopCount].loop = loop;
	loopStack[loopCount].endPatches.clear();

	++loopCount;

	
}

static void popLoop(IrState *state) {
	--loopCount;

	Loop loop = loopStack[loopCount];

	for (auto patch : loop.endPatches) {
		state->ir[patch].b = state->ir.count;
	}
}



static void generateIncrement(IrState *state, ExprLoop *loop) {
	auto it = loop->iteratorBlock.declarations[0];
	auto it_index = loop->iteratorBlock.declarations[1];

	Ir &index = state->ir.add();
	index.op = IrOp::ADD_CONSTANT;
	index.opSize = 8;
	index.a = it_index->physicalStorage;
	index.b = 1;
	index.dest = it_index->physicalStorage;

	Ir &increment = state->ir.add();
	increment.op = IrOp::ADD_CONSTANT;
	increment.opSize = loop->forBegin->type->size;
	increment.a = loop->irPointer;
	increment.dest = loop->irPointer;

	if (loop->forBegin->type->flavor == TypeFlavor::POINTER) {
		auto pointer = static_cast<TypePointer *>(loop->forBegin->type);

		increment.b = pointer->pointerTo->size;
	}
	else if (loop->forBegin->type->flavor == TypeFlavor::ARRAY) {
		auto array = static_cast<TypeArray *>(loop->forBegin->type);

		increment.b = array->arrayOf->size;
	}
	else {
		increment.b = 1;
	}
}

void addLineMarker(IrState *state, Expr *expr) {
	Ir &ir = state->ir.add();

	ir.op = IrOp::LINE_MARKER;
	ir.location.start = expr->start;
	ir.location.end = expr->end;
}


static void exitBlock(IrState *state, Block *block, bool isBreak) {
	for (s64 i = deferStack.count - 1; i >= 0; --i) {
		auto expr = deferStack[i];

		if (expr->flavor == ExprFlavor::FOR) {
			auto loop = static_cast<ExprLoop *>(expr);

			auto current = &loop->iteratorBlock;
			bool found = false;

			while (current) {
				if (current == block) {
					found = true;
					break;
				}

				current = current->parentBlock;
			}

			if (!found)
				break;

			if (!isBreak) // Break statements shouldn't execute the increment
				generateIncrement(state, loop);
		}
		else {
			assert(expr->flavor == ExprFlavor::DEFER);

			auto defer = static_cast<ExprDefer *>(expr);

			if (block) {
				auto current = defer->enclosingScope;
				bool found = false;

				while (current) {
					if (current == block) {
						found = true;
						break;
					}

					current = current->parentBlock;
				}

				if (!found)
					break;
			}

			addLineMarker(state, defer);
			generateIr(state, defer->expr, DEST_NONE);
		}
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

void convertNumericType(IrState *state, Type *destType, u64 dest, Type *srcType, u64 src) {
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
		extend.op = IrOp::SET;

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
			return IrOp::GREATER_EQUAL;
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

u64 loadAddressForArrayDereference(IrState *state, Expr *deref, u64 dest);

u64 loadAddressOf(IrState *state, Expr *expr, u64 dest) {
	if (expr->flavor == ExprFlavor::BINARY_OPERATOR) {
		dest = loadAddressForArrayDereference(state, expr, dest);
	}
	else if (expr->flavor == ExprFlavor::UNARY_OPERATOR) {
		auto unary = static_cast<ExprUnaryOperator *>(expr);

		assert(unary->op == TokenT::SHIFT_LEFT);

		dest = generateIr(state, unary->value, dest);
	}
	else if (expr->flavor == ExprFlavor::IDENTIFIER) {
		auto identifier = static_cast<ExprIdentifier *>(expr);

		if (identifier->structAccess) {
			auto type = identifier->structAccess->type;


			u64 store;

			if (identifier->structAccess->type->flavor == TypeFlavor::POINTER) {
				store = generateIr(state, identifier->structAccess, dest);
			}
			else {
				store = loadAddressOf(state, identifier->structAccess, dest);
			}

			if (type->flags & TYPE_ARRAY_IS_FIXED) { // The only struct access we will generate for fixed arrays are .data, which is just the address of the array
				assert(identifier->name == "data");
				return store;
			}

			if (type->flavor == TypeFlavor::POINTER) {
				type = static_cast<TypePointer *>(type)->pointerTo;
			}

			u64 offset = identifier->declaration->physicalStorage; // In the case of struct members, physicalStorage is the offset within the struct

			if (offset == 0) {
				return store;
			}
			else {
				Ir &add = state->ir.add();
				add.op = IrOp::ADD_CONSTANT;
				add.dest = dest;
				add.a = store;
				add.b = offset;
				add.opSize = 8;

				return dest;
			}
		}
		else {
			if (identifier->declaration->enclosingScope == &globalBlock) {
				Ir &address = state->ir.add();
				address.op = IrOp::ADDRESS_OF_GLOBAL;
				address.declaration = identifier->declaration;
				address.dest = dest;
				address.opSize = 8;
			}
			else {
				Ir &address = state->ir.add();
				address.op = IrOp::ADDRESS_OF_LOCAL;
				address.a = identifier->declaration->physicalStorage;
				address.b = 0;
				address.dest = dest;
				address.opSize = 8;
			}
		}
	}
	else {
		u64 in = generateIr(state, expr, dest);

		Ir &address = state->ir.add();
		address.op = IrOp::ADDRESS_OF_LOCAL;
		address.a = in;
		address.b = 0;
		address.dest = dest;
		address.opSize = 8;
	}

	return dest;
}

u64 loadAddressForArrayDereference(IrState *state, Expr *deref, u64 dest) {
	auto binary = static_cast<ExprBinaryOperator *>(deref);

	assert(binary->op == TOKEN('['));

	u64 leftReg;

	if (binary->left->type->flags & TYPE_ARRAY_IS_FIXED) {
		leftReg = loadAddressOf(state, binary->left, dest);
	}
	else {
		leftReg = generateIr(state, binary->left, dest);
	}

	u64 temp = state->nextRegister++;
	u64 rightReg = generateIrForceDest(state, binary->right, temp);

	assert(temp == rightReg);

	if (binary->type->size != 1) {
		Ir &mul = state->ir.add();

		mul.op = IrOp::MUL_BY_CONSTANT;
		mul.dest = temp;
		mul.a = temp;
		mul.b = binary->type->size;
		mul.opSize = 8;

		rightReg = temp;
	}

	Ir &add = state->ir.add();
	add.op = IrOp::ADD;
	add.dest = dest;
	add.a = leftReg;
	add.b = temp;
	add.opSize = 8;

	return dest;
}

u64 allocateSpaceForType(IrState *state, Type *type) {
	u64 reg = state->nextRegister;

	state->nextRegister += (type->size + 7) / 8;

	return reg;
}

IrModifyWrite readForModifyWrite(IrState *state, Expr *left, Expr *right) {
	IrModifyWrite info;

	u64 dest;

	if (left->flavor == ExprFlavor::IDENTIFIER && !static_cast<ExprIdentifier *>(left)->structAccess) {
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
			info.leftReg = identifier->declaration->physicalStorage;
		}
	}
	else {
		dest = loadAddressOf(state, left, state->nextRegister++);

		Ir &read = state->ir.add();
		read.op = IrOp::READ;
		read.dest = allocateSpaceForType(state, left->type);
		read.a = dest;
		read.opSize = 8;
		read.destSize = left->type->size;

		info.addressReg = dest;
		info.leftReg = read.dest;
	}

	info.rightReg = generateIr(state, right, state->nextRegister++);

	return info;
}

void writeForModifyWrite(IrState *state, IrModifyWrite info, Expr *left) {
	if (left->flavor != ExprFlavor::IDENTIFIER || static_cast<ExprIdentifier *>(left)->structAccess || static_cast<ExprIdentifier *>(left)->declaration->enclosingScope == &globalBlock) {
		Ir &write = state->ir.add();
		write.op = IrOp::WRITE;
		write.opSize = left->type->size;
		write.a = info.addressReg;
		write.b = info.leftReg;
	}
}
void generateCall(IrState *state, ExprFunctionCall *call, u64 dest, ExprCommaAssignment *comma) {
	auto type = static_cast<TypeFunction *>(call->function->type);

	FunctionCall *argumentInfo = static_cast<FunctionCall *>(state->allocator.allocate(sizeof(FunctionCall) + sizeof(argumentInfo->args[0]) * (call->arguments.count + type->returnCount - 1)));
	argumentInfo->argCount = call->arguments.count + type->returnCount - 1;

	for (u64 i = 1; i < type->returnCount; i++) {
		if (comma && i < comma->exprCount) {
			u64 address = loadAddressOf(state, comma->left[i], state->nextRegister++);

			argumentInfo->args[call->arguments.count + i - 1].number = address;
			argumentInfo->args[call->arguments.count + i - 1].type = TYPE_VOID_POINTER;
		}
		else {
			argumentInfo->args[call->arguments.count + i - 1].number = static_cast<u64>(-1LL);
			argumentInfo->args[call->arguments.count + i - 1].type = TYPE_VOID_POINTER;
		}
	}

	u64 function = generateIr(state, call->function, state->nextRegister++);

	
	u64 paramOffset;

	if (!isStandardSize(type->returnTypes[0]->size)) {
		paramOffset = 1;
	}
	else {
		paramOffset = 0;
	}

	u64 callAuxStorage = my_max(4, type->argumentCount + type->returnCount - 1 + paramOffset);

	for (u64 i = 0; i < call->arguments.count; i++) {
		argumentInfo->args[i].number = generateIr(state, call->arguments.values[i], state->nextRegister++);
		argumentInfo->args[i].type = call->arguments.values[i]->type;

		if (!isStandardSize(call->arguments.values[i]->type->size)) {

			if (callAuxStorage & 1) {
				++callAuxStorage; // Make sure it is aligned by 16 bytes 
			}

			callAuxStorage += (call->arguments.values[i]->type->size + 7) / 8;
		}
	}

	argumentInfo->returnType = call->type;

	u64 returnSize = 0;

	if (dest == 0) {
		returnSize = (call->type->size + 7) / 8;
	}
	else if (!isStandardSize(call->type->size)) {
		callAuxStorage += (call->type->size + 7) / 8;
	}

	for (u64 i = comma ? comma->exprCount : 1; i < type->returnCount; i++) {
		returnSize = my_max(returnSize, (type->returnTypes[i]->size + 7) / 8);
	}

	callAuxStorage += returnSize;

	if (callAuxStorage > state->callAuxStorage) {
		state->callAuxStorage = callAuxStorage;
	}

	Ir &ir = state->ir.add();
	ir.op = IrOp::CALL;
	ir.a = function;
	ir.arguments = argumentInfo;
	ir.dest = dest;
	ir.opSize = call->type->size;
}

u64 generateEquals(IrState *state, u64 leftReg, Expr *right, u64 dest, bool equals) {
	u64 rightReg = generateIr(state, right, state->nextRegister++);

	if (right->type->flavor == TypeFlavor::STRING) {
		if (!stringsEqualFunction) {
			reportError("Internal Compiler Error: Comparing strings before __strings_equal is declared");
			assert(false);
			exit(1); // @Cleanup Forceful exit since we don't have good error handling here and its an internal compiler error
		}

		u64 function = generateIr(state, stringsEqualFunction, state->nextRegister++);

		FunctionCall *argumentInfo = static_cast<FunctionCall *>(state->allocator.allocate(sizeof(FunctionCall) + sizeof(argumentInfo->args[0]) * 2));
		argumentInfo->argCount = 2;

		u64 callAuxStorage = 4;

		argumentInfo->args[0].number = leftReg;
		argumentInfo->args[0].type = TYPE_VOID_POINTER;
		argumentInfo->args[1].number = leftReg;
		argumentInfo->args[1].type = TYPE_VOID_POINTER;

		argumentInfo->returnType = &TYPE_BOOL;

		if (4 > state->callAuxStorage) {
			state->callAuxStorage = 4;
		}

		Ir &ir = state->ir.add();
		ir.op = IrOp::CALL;
		ir.a = function;
		ir.arguments = argumentInfo;
		ir.dest = dest;
		ir.opSize = TYPE_BOOL.size;


		if (!equals) {
			Ir &invert = state->ir.add();

			invert.op = IrOp::EQUAL;
			invert.dest = dest;
			invert.a = dest;
			invert.b = 0;
			invert.opSize = 1;
		}
	}
	else {
		Ir &ir = state->ir.add();

		ir.op = equals ? IrOp::EQUAL : IrOp::NOT_EQUAL;
		ir.dest = dest;
		ir.a = leftReg;
		ir.b = rightReg;
		ir.opSize = right->type->size;

		if (right->type->flavor == TypeFlavor::FLOAT)
			ir.flags |= IR_FLOAT_OP;
	}

	return dest;
}

u64 generateIr(IrState *state, Expr *expr, u64 dest, bool destWasForced) {
	PROFILE_FUNC();
	if (dest != DEST_NONE && expr->type->size > 8 && !destWasForced) {
		// @Hack when ir generation was originally written it wasn't designed for large types, the dest register is often used for intermediates which may be larger than the final value
		// so if we are going to write a large value we should write it somewhere other than where they requested to be safe

		// @StringFormat this assumption is violated if strings are large since for loops force the dest for strings into a pointer size register
		// If we forced the dest we know it was safe

		dest = allocateSpaceForType(state, expr->type);
	}

	switch (expr->flavor) {
		case ExprFlavor::BINARY_OPERATOR: {

			auto binary = static_cast<ExprBinaryOperator *>(expr);

			assert(dest != DEST_NONE || binaryOpHasSideEffects(binary->op));

			auto left = binary->left;
			auto right = binary->right;

			switch (binary->op) {
				case TokenT::CAST: {
					assert(binary->left->flavor == ExprFlavor::TYPE_LITERAL);

					u64 rightReg = generateIr(state, right, state->nextRegister++);

					Type *castTo = binary->type;

					if (castTo == right->type || (binary->flags & EXPR_CAST_IS_BITWISE))
						return rightReg;

					switch(castTo->flavor) {
						case TypeFlavor::STRUCT: {
							assert(castTo == TYPE_ANY);

							u64 storage = allocateSpaceForType(state, right->type);

							Ir &copy = state->ir.add();
							copy.op = IrOp::SET;
							copy.dest = storage;
							copy.a = rightReg;
							copy.opSize = right->type->size;
							copy.destSize = copy.opSize;
							
							Ir &address = state->ir.add();
							address.op = IrOp::ADDRESS_OF_LOCAL;
							address.dest = dest;
							address.a = storage;
							address.b = 0;
							address.opSize = 8;

							Ir &type = state->ir.add();
							type.op = IrOp::TYPE;
							type.dest = dest + 1;
							type.type = right->type;
							type.opSize = 8;

							return dest;
						}
						case TypeFlavor::BOOL: {
							u64 src;
							u64 size;

							u64 patch;

							if (right->type->flavor == TypeFlavor::STRING) {
								Ir &set = state->ir.add();

								set.op = IrOp::SET;
								set.dest = dest;
								set.a = rightReg;
								set.b = 0;
								set.opSize = right->type->size;
								set.destSize = set.opSize;

								patch = state->ir.count;

								Ir &branch = state->ir.add();
								branch.op = IrOp::IF_Z_GOTO;
								branch.a = dest;
								branch.opSize = right->type->size;

								Ir &read = state->ir.add();
								read.op = IrOp::READ;
								read.dest = rightReg;
								read.a = rightReg;
								read.destSize = 1;
								read.opSize = 8;

								src = dest;
								size = 1;
							}
							else if (right->type->flavor == TypeFlavor::ARRAY) {
								if (right->type->flags & TYPE_ARRAY_IS_FIXED) {
									Ir &one = state->ir.add();

									one.op = IrOp::IMMEDIATE;
									one.dest = dest;
									one.a = 1;
									one.opSize = 1;

									return dest;
								}
								else {
									src = rightReg + 1;
									size = 8;
								}
							}
							else {
								src = rightReg;
								size = right->type->size;
							}

							Ir &ir = state->ir.add();

							ir.op = IrOp::NOT_EQUAL;
							ir.dest = dest;
							ir.a = src;
							ir.b = 0;
							ir.opSize = size;

							if (right->type->flavor == TypeFlavor::FLOAT)
								ir.flags |= IR_FLOAT_OP;

							if (right->type->flavor == TypeFlavor::STRING) {
								state->ir[patch].b = state->ir.count;
							}
							
							return dest;
						}
						case TypeFlavor::FLOAT: {
							convertNumericType(state, castTo, dest, right->type, rightReg);

							return dest;
						}
						case TypeFlavor::ENUM:
						case TypeFlavor::INTEGER: {
							if (right->type->flavor == TypeFlavor::POINTER) {
								return rightReg;
							}
							else if (right->type->flavor == TypeFlavor::FUNCTION) {
								return rightReg;
							}

							convertNumericType(state, castTo, dest, right->type, rightReg);

							return dest;
						}
						case TypeFlavor::ARRAY: {
							if (right->type->flags & TYPE_ARRAY_IS_DYNAMIC) {
								assert(!(left->type->flags & TYPE_ARRAY_IS_FIXED));
								return rightReg;
							}
							else {
								assert(right->type->flags & TYPE_ARRAY_IS_FIXED);
								assert(!(left->type->flags & TYPE_ARRAY_IS_DYNAMIC));

								Ir &buffer = state->ir.add();
								buffer.op = IrOp::ADDRESS_OF_LOCAL;
								buffer.dest = dest;
								buffer.a = rightReg;
								buffer.b = 0;

								Ir &count = state->ir.add();
								count.op = IrOp::IMMEDIATE;
								count.dest = dest + 1;
								count.a = static_cast<TypeArray *>(right->type)->count;
								count.opSize = 8;

								return dest;
							}
						}
						case TypeFlavor::POINTER: {
							if (right->type->flavor == TypeFlavor::ARRAY && (right->type->flags & TYPE_ARRAY_IS_FIXED)) {
								Ir &buffer = state->ir.add();
								buffer.op = IrOp::ADDRESS_OF_LOCAL;
								buffer.dest = dest;
								buffer.a = rightReg;
								buffer.b = 0;

								return dest;
							}
							else {
								return rightReg;
							}
						}
						case TypeFlavor::FUNCTION:
						case TypeFlavor::STRING:
							return rightReg; // These casts should be a nop
						case TypeFlavor::TYPE:
						case TypeFlavor::VOID:
							assert(false);
							break;
					}
					
					assert(false);
				}
				case TOKEN('['): {
					u64 address = loadAddressForArrayDereference(state, binary, dest);

					Ir &read = state->ir.add();
					read.op = IrOp::READ;
					read.a = address;
					read.dest = dest;
					read.opSize = 8;
					read.destSize = binary->type->size;

					return dest;
				}
				case TokenT::EQUAL:
				case TokenT::NOT_EQUAL: {
					assert(left->type == right->type);

					return generateEquals(state, generateIr(state, left, state->nextRegister++), right, dest, binary->op == TokenT::EQUAL);
				}
				case TokenT::GREATER_EQUAL:
				case TokenT::LESS_EQUAL:
				case TOKEN('>'):
				case TOKEN('<'): {
					u64 leftReg = generateIr(state, left, state->nextRegister++);

					u64 rightReg = generateIr(state, right, state->nextRegister++);

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

						u64 leftReg = generateIr(state, left, state->nextRegister++);

						u64 temp = state->nextRegister++;

						u64 rightReg = generateIr(state, right, temp);

						if (right->type->flavor == TypeFlavor::POINTER) {
							assert(binary->op == TOKEN('-'));
							Ir &sub = state->ir.add();
							sub.op = IrOp::SUB;
							sub.dest = dest;
							sub.a = leftReg;
							sub.b = rightReg;
							sub.opSize = 8;

							Ir &div = state->ir.add();
							div.op = IrOp::DIVIDE_BY_CONSTANT;
							div.dest = dest;
							div.a = dest;
							div.b = pointer->pointerTo->size;
							div.opSize = 8;
							div.flags |= IR_SIGNED_OP;
						}
						else {
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
					}
					else {
						u64 leftReg = generateIr(state, left, state->nextRegister++);

						u64 rightReg = generateIr(state, right, state->nextRegister++);

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
					u64 leftReg = generateIr(state, left, state->nextRegister++);

					u64 rightReg = generateIr(state, right, state->nextRegister++);

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
					u64 leftReg = generateIr(state, left, state->nextRegister++);

					u64 rightReg = generateIr(state, right, state->nextRegister++);

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

					if (left->type->flavor == TypeFlavor::FLOAT)
						ir.flags |= IR_FLOAT_OP;
					
					return dest;
				}
				case TOKEN('='): {
					assert(dest == DEST_NONE);

					if (left->flavor == ExprFlavor::IDENTIFIER && !static_cast<ExprIdentifier *>(left)->structAccess) {
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
							write.opSize = left->type->size;

							return rightReg;
						}
						else {
							u64 stored = generateIrForceDest(state, right, identifier->declaration->physicalStorage);

							assert(stored == identifier->declaration->physicalStorage);

							return stored;
						}
					}
					else {
						dest = loadAddressOf(state, left, state->nextRegister++);


						u64 rightReg = generateIr(state, right, state->nextRegister++);

						Ir &write = state->ir.add();
						write.op = IrOp::WRITE;
						write.a = dest;
						write.b = rightReg;
						write.opSize = left->type->size;

						return rightReg;
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
						add.op = binary->op == TokenT::PLUS_EQUALS ? IrOp::ADD : IrOp::SUB;
						add.dest = info.leftReg;
						add.a = info.leftReg;
						add.b = info.rightReg;
						add.opSize = 8;
					}
					else {
						Ir &ir = state->ir.add();

						ir.op = binary->op == TokenT::PLUS_EQUALS ? IrOp::ADD : IrOp::SUB;
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
					assert(dest != DEST_NONE);
					
					u64 temp = state->nextRegister++;


					generateIrForceDest(state, left, temp);
					
					u64 patch = state->ir.count;

					Ir &branch = state->ir.add();
					branch.op = binary->op == TokenT::LOGIC_AND ? IrOp::IF_Z_GOTO : IrOp::IF_NZ_GOTO;
					branch.a = temp;
					branch.opSize = 1;

					addLineMarker(state, right);
					generateIrForceDest(state, right, temp);

					state->ir[patch].b = state->ir.count;

					Ir &set = state->ir.add();
					set.op = IrOp::SET;
					set.dest = dest;
					set.a = temp;
					set.destSize = 1;
					set.opSize = 1;

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


					if (left->type->flavor == TypeFlavor::FLOAT)
						ir.flags |= IR_FLOAT_OP;

					writeForModifyWrite(state, info, left);

					return info.leftReg;
				}
				default:
					return DEST_NONE;
			}

			break;
		}
		case ExprFlavor::BLOCK: {
			auto block = static_cast<ExprBlock *>(expr);

			for (auto declaration : block->declarations.declarations) {
				if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_IMPLICIT_IMPORT))) {
					declaration->physicalStorage = allocateSpaceForType(state, static_cast<ExprLiteral *>(declaration->type)->typeValue);
				}
			}

			for (auto subExpr : block->exprs) {
				addLineMarker(state, subExpr);
				generateIr(state, subExpr, DEST_NONE);
			}

			exitBlock(state, &block->declarations, false);
			
			while (deferStack.count) {
				auto expr = deferStack[deferStack.count - 1];
				
				if (expr->flavor != ExprFlavor::DEFER)
					break;

				auto defer = static_cast<ExprDefer *>(expr);

				if (defer->enclosingScope != &block->declarations)
					break;

				deferStack.pop();
			}

			return DEST_NONE;
		}
		case ExprFlavor::DEFER: {
			assert(dest == DEST_NONE);

			deferStack.add(expr);
			
			return DEST_NONE;
		}
		case ExprFlavor::BREAK: {
			auto break_ = static_cast<ExprBreakOrContinue *>(expr);


			for (u64 i = loopCount; i-- != 0;) {

				if (loopStack[i].loop == break_->refersTo) {
					exitBlock(state, &loopStack[i].loop->iteratorBlock, true);
					loopStack[i].endPatches.add(state->ir.count);
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


			for (u64 i = loopCount; i-- != 0;) {
				if (loopStack[i].loop == continue_->refersTo) {
					exitBlock(state, &loopStack[i].loop->iteratorBlock, false);
					begin = loopStack[i].start;
					break;
				}
			}

			Ir &jump = state->ir.add();
			jump.op = IrOp::GOTO;
			jump.b = begin;

			return DEST_NONE;
		}
		case ExprFlavor::REMOVE: {
			auto remove = static_cast<ExprBreakOrContinue *>(expr);

			if (!removeFunction) {
				reportError(expr, "Internal Compiler Error: Removing something before __remove is declared");
				assert(false);
				exit(1); // @Cleanup Forceful exit since we don't have good error handling here and its an internal compiler error
			}

			assert(remove->refersTo->forBegin->type->flavor == TypeFlavor::ARRAY);
			assert(!(remove->refersTo->forBegin->type->flags & TYPE_ARRAY_IS_FIXED));

			u64 function = generateIr(state, removeFunction, state->nextRegister++);

			FunctionCall *argumentInfo = static_cast<FunctionCall *>(state->allocator.allocate(sizeof(FunctionCall) + sizeof(argumentInfo->args[0]) * 3));
			argumentInfo->argCount = 3;

			u64 callAuxStorage = 4;

			u64 sizeReg = state->nextRegister++;

			Ir &size = state->ir.add();
			size.op = IrOp::IMMEDIATE;
			size.dest = sizeReg;
			size.a = static_cast<TypeArray *>(remove->refersTo->forBegin->type)->arrayOf->size;
			size.opSize = 8;


			argumentInfo->args[0].number = remove->refersTo->arrayPointer;
			argumentInfo->args[0].type = TYPE_VOID_POINTER;
			argumentInfo->args[1].number = remove->refersTo->irPointer;
			argumentInfo->args[1].type = TYPE_VOID_POINTER;
			argumentInfo->args[2].number = sizeReg;
			argumentInfo->args[2].type = &TYPE_U64;

			argumentInfo->returnType = TYPE_VOID_POINTER;

			if (4 > state->callAuxStorage) {
				state->callAuxStorage = 4;
			}

			Ir &ir = state->ir.add();
			ir.op = IrOp::CALL;
			ir.a = function;
			ir.arguments = argumentInfo;
			ir.dest = remove->refersTo->irPointer;
			ir.opSize = TYPE_VOID_POINTER->size;

			Ir &sub = state->ir.add();
			sub.op = IrOp::ADD_CONSTANT;
			sub.dest = remove->refersTo->iteratorBlock.declarations[1]->physicalStorage;
			sub.a = remove->refersTo->iteratorBlock.declarations[1]->physicalStorage;
			sub.b = static_cast<u64>(-1LL);
			sub.opSize = 8;

			return DEST_NONE;
		}
		case ExprFlavor::INT_LITERAL: {
			if (dest == DEST_NONE) return DEST_NONE;

			auto literal = static_cast<ExprLiteral *>(expr);

			assert(literal->type->size);

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

			assert(literal->type->size);

			if (literal->floatValue == 0.0) {
				return 0;
			}
			else {
				u64 value = literal->unsignedValue;

				if (literal->type->size == 4) {
					*reinterpret_cast<float *>(&value) = static_cast<float>(literal->floatValue);

					value &= 0xFFFF'FFFFULL;
				}

				Ir &load = state->ir.add();
				load.op = IrOp::IMMEDIATE;
				load.dest = dest;
				load.a = value;
				load.opSize = literal->type->size;

				load.flags |= IR_FLOAT_OP;

				return dest;
			}
		}
		case ExprFlavor::FOR: {
			auto loop = static_cast<ExprLoop *>(expr);

			auto it = loop->iteratorBlock.declarations[0];
			auto it_index = loop->iteratorBlock.declarations[1];

			it->physicalStorage = allocateSpaceForType(state, static_cast<ExprLiteral *>(it->type)->typeValue);

			assert(static_cast<ExprLiteral *>(it_index->type)->typeValue == &TYPE_U64);
			it_index->physicalStorage = state->nextRegister++;

			u64 itReg = it->physicalStorage;
			u64 it_indexReg = it_index->physicalStorage;

			if ((loop->forBegin->type->flavor != TypeFlavor::INTEGER) && !(loop->flags & EXPR_FOR_BY_POINTER)) {
				loop->irPointer = state->nextRegister++;
			}
			else {
				loop->irPointer = itReg;
			}

			if (loop->forBegin->type->flavor == TypeFlavor::ARRAY) {
				auto begin = loop->forBegin;

				loop->arrayPointer = loadAddressOf(state, begin, state->nextRegister++);

				if (loop->forBegin->type->flags & TYPE_ARRAY_IS_FIXED) {
					Ir &set = state->ir.add();
					set.op = IrOp::SET;
					set.dest = loop->irPointer;
					set.a = loop->arrayPointer;
					set.opSize = 8;
					set.destSize = 8;
				}
				else {
					Ir &read = state->ir.add();
					read.op = IrOp::READ;
					read.dest = loop->irPointer;
					read.a = loop->arrayPointer;
					read.opSize = 8;
					read.destSize = 8;
				}
			}
			else {
				generateIrForceDest(state, loop->forBegin, loop->irPointer);
			}

			Ir &initItIndex = state->ir.add();
			initItIndex.op = IrOp::SET;
			initItIndex.dest = it_index->physicalStorage;
			initItIndex.a = 0;
			initItIndex.opSize = 8;
			initItIndex.destSize = 8;

			u64 irEnd;

			if (loop->forEnd) {
				irEnd = generateIr(state, loop->forEnd, state->nextRegister++);
			}

			pushLoop(state, loop);

			addLineMarker(state, expr);



			u64 compareDest;

			if (loop->forBegin->type->flavor == TypeFlavor::STRING && !(loop->flags & EXPR_FOR_BY_POINTER)) {
				compareDest = it->physicalStorage;
			}
			else {
				compareDest = state->nextRegister++;
			}
			
			if (loop->forBegin->type->flavor == TypeFlavor::STRING) {
				// @StringFormat
				Ir &read = state->ir.add();

				read.op = IrOp::READ;
				read.a = loop->irPointer;
				read.opSize = 8;
				read.dest = compareDest;
				read.destSize = 1;
			}
			else if (loop->forBegin->type->flavor == TypeFlavor::ARRAY) {
				if (loop->forBegin->type->flags & TYPE_ARRAY_IS_FIXED) {
					Ir &immediate = state->ir.add();
					immediate.op = IrOp::IMMEDIATE;
					immediate.dest = compareDest;
					immediate.a = static_cast<TypeArray *>(loop->forBegin->type)->count;
					immediate.opSize = 8;
				}
				else {
					Ir &add = state->ir.add();
					add.op = IrOp::ADD_CONSTANT;
					add.dest = compareDest;
					add.a = loop->arrayPointer;
					add.b = 8;
					add.opSize = 8;

					Ir &read = state->ir.add();
					read.op = IrOp::READ;
					read.dest = compareDest;
					read.a = compareDest;
					read.opSize = 8;
					read.destSize = 8;
				}

				Ir &compare = state->ir.add();
				compare.op = IrOp::LESS;
				compare.a = it_index->physicalStorage;
				compare.b = compareDest;
				compare.dest = compareDest;
				compare.opSize = 8;
			}
			else {
				Ir &compare = state->ir.add();
				compare.op = IrOp::LESS;
				compare.a = loop->irPointer;
				compare.b = irEnd;
				compare.dest = compareDest;
				compare.opSize = loop->forBegin->type->size;

				if (loop->forBegin->type->flags & TYPE_INTEGER_IS_SIGNED)
					compare.flags |= IR_SIGNED_OP;
			}

			u64 patch = state->ir.count; // Patch this manually so it doesn't skip the completed block if it exists

			Ir &branch = state->ir.add();
			branch.op = IrOp::IF_Z_GOTO;
			branch.a = compareDest;
			branch.opSize = 1;

			if (!(loop->flags & EXPR_FOR_BY_POINTER)) {
				if (loop->forBegin->type->flavor == TypeFlavor::ARRAY || loop->forBegin->type->flavor == TypeFlavor::POINTER) {
					Ir &read = state->ir.add();
					read.op = IrOp::READ;
					read.dest = it->physicalStorage;
					read.opSize = 8;
					read.destSize = static_cast<ExprLiteral *>(it->type)->typeValue->size;
					read.a = loop->irPointer;
				}
			}

			deferStack.add(loop);
		
			if (loop->body) {
				addLineMarker(state, loop->body);
				generateIr(state, loop->body, DEST_NONE);
			}

			exitBlock(state, &loop->iteratorBlock, false);

			Expr *inc = deferStack.pop();
			assert(inc == loop);

			Ir &jump = state->ir.add();
			jump.op = IrOp::GOTO;
			jump.b = loopStack[loopCount - 1].start;

			state->ir[patch].b = state->ir.count;

			if (loop->completedBody) {
				addLineMarker(state, loop->completedBody);
				generateIr(state, loop->completedBody, DEST_NONE);
			}

			popLoop(state);
			
			return DEST_NONE;
		}
		case ExprFlavor::FUNCTION: {
			if (dest == DEST_NONE) return dest;

			Ir &address = state->ir.add();
			address.dest = dest;
			address.opSize = 8;
			address.op = IrOp::FUNCTION;
			address.function = static_cast<ExprFunction *>(expr);
			
			return dest;
		}
		case ExprFlavor::STRING_LITERAL: {
			if (dest == DEST_NONE) return dest;

			Ir &address = state->ir.add();
			address.dest = dest;
			address.opSize = 8;
			address.op = IrOp::STRING;
			address.string = static_cast<ExprStringLiteral *>(expr);

			return dest;
		}
		case ExprFlavor::FUNCTION_CALL: {
			if (dest == DEST_NONE) dest = 0;

			auto call = static_cast<ExprFunctionCall *>(expr);

			generateCall(state, call, dest, nullptr);

			return dest;
		}
		case ExprFlavor::IDENTIFIER: {
			auto identifier = static_cast<ExprIdentifier *>(expr);

			if (identifier->structAccess) {
				u64 stored = loadAddressOf(state, expr, dest);

				Ir &read = state->ir.add();
				read.op = IrOp::READ;
				read.dest = dest;
				read.a = stored;
				read.opSize = 8;
				read.destSize = expr->type->size;

				return dest;
			}
			else {
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
					return identifier->declaration->physicalStorage;
				}
			}
		}
		case ExprFlavor::SWITCH: {
			assert(dest == DEST_NONE);

			auto switch_ = static_cast<ExprSwitch *>(expr);

			u64 value = generateIr(state, switch_->condition, state->nextRegister++);

			u64 compareResult = state->nextRegister++;
			
			ExprSwitch::Case *else_ = nullptr;

			for (auto &case_ : switch_->cases) {
				if (case_.condition) {
					auto condition = static_cast<ExprBinaryOperator *>(case_.condition);

					assert(condition->flavor == ExprFlavor::BINARY_OPERATOR);

					assert(condition->right->type == switch_->condition->type);

					u64 result = generateEquals(state, value, condition->right, compareResult, true);

					case_.irBranch = state->ir.count;

					Ir &branch = state->ir.add();

					branch.op = IrOp::IF_NZ_GOTO;
					branch.a = result;
					branch.opSize = 1;
				}
				else {
					else_ = &case_;
				}
			}

			u64 finalPatch = state->ir.count;

			Ir &final = state->ir.add();
			final.op = IrOp::GOTO;

			if (else_) {
				else_->irBranch = finalPatch;
			}

			for (auto &case_ : switch_->cases) {
				state->ir[case_.irBranch].b = state->ir.count;

				addLineMarker(state, case_.block);
				generateIr(state, case_.block, DEST_NONE);

				if (!case_.fallsThrough && &case_ + 1 != switch_->cases.end()) {
					case_.irSkip = state->ir.count;

					Ir &skip = state->ir.add();
					skip.op = IrOp::GOTO;
				}
			}

			if (!else_) {
				state->ir[finalPatch].b = state->ir.count;
			}

			for (u64 i = 0; i + 1 < switch_->cases.count; i++) {
				auto &case_ = switch_->cases[i];

				if (!case_.fallsThrough)
					state->ir[case_.irSkip].b = state->ir.count;
			}

			return DEST_NONE;
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

				addLineMarker(state, ifElse->ifBody);
				generateIr(state, ifElse->ifBody, DEST_NONE);
				
				u64 patchJump = state->ir.count;
				Ir &jump = state->ir.add();
				jump.op = IrOp::GOTO;
				
				state->ir[patchIfZ].b = state->ir.count;

				addLineMarker(state, ifElse->elseBody);
				generateIr(state, ifElse->elseBody, DEST_NONE);

				state->ir[patchJump].b = state->ir.count;
			}
			else if (ifElse->ifBody) {
				u64 patchIfZ = state->ir.count;
				Ir &ifZ = state->ir.add();
				ifZ.op = IrOp::IF_Z_GOTO;
				ifZ.a = conditionReg;
				ifZ.opSize = 1;

				addLineMarker(state, ifElse->ifBody);
				generateIr(state, ifElse->ifBody, DEST_NONE);

				state->ir[patchIfZ].b = state->ir.count;
			}
			else if (ifElse->elseBody) {
				u64 patchIfNZ = state->ir.count;
				Ir &ifNZ = state->ir.add();
				ifNZ.op = IrOp::IF_NZ_GOTO;
				ifNZ.a = conditionReg;
				ifNZ.opSize = 1;

				addLineMarker(state, ifElse->elseBody);
				generateIr(state, ifElse->elseBody, DEST_NONE);

				state->ir[patchIfNZ].b = state->ir.count;
			}

			return DEST_NONE;
		}
		case ExprFlavor::COMMA_ASSIGNMENT: {
			assert(dest == DEST_NONE);
			
			auto comma = static_cast<ExprCommaAssignment *>(expr);

			u64 address = loadAddressOf(state, comma->left[0], state->nextRegister++);

			dest = allocateSpaceForType(state, comma->left[0]->type);

			generateCall(state, static_cast<ExprFunctionCall *>(comma->call), dest, comma);

			Ir &write = state->ir.add();
			write.op = IrOp::WRITE;
			write.opSize = comma->left[0]->type->size;
			write.a = address;
			write.b = dest;
			

			return DEST_NONE;
		}
		case ExprFlavor::RETURN: {
			auto return_ = static_cast<ExprReturn *>(expr);

			u64 result = 0;

			if (return_->returns.count) {
				u64 result = generateIr(state, return_->returns.values[0], state->nextRegister++);

				for (u64 i = 1; i < return_->returns.count; i++) {
					u64 store = generateIr(state, return_->returns.values[i], state->nextRegister++);

					Ir &write = state->ir.add();
					write.op = IrOp::WRITE;
					write.a = return_->returnsFrom->returns.declarations[i]->physicalStorage;
					write.b = store;
					write.opSize = return_->returns.values[i]->type->size;
				}


				exitBlock(state, nullptr, true);

				Ir &ir = state->ir.add();
				ir.op = IrOp::RETURN;
				ir.a = result;
				ir.opSize = return_->returns.values[0]->type->size;

				if (return_->returns.values[0]->type->flavor == TypeFlavor::FLOAT) {
					ir.flags |= IR_FLOAT_OP;
				}
			}
			else {
				exitBlock(state, nullptr, true);

				Ir &ir = state->ir.add();
				ir.op = IrOp::RETURN;
				ir.a = 0;
				ir.opSize = 0;
			}


			return DEST_NONE;
		}
		case ExprFlavor::STRUCT_DEFAULT: {
			if (dest == DEST_NONE) return DEST_NONE;

			u64 memberSize = 0;

			auto struct_ = static_cast<TypeStruct *>(expr->type);

			for (auto decl : struct_->members.declarations) {
				if (decl->flags & (DECLARATION_IS_UNINITIALIZED | DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_IMPLICIT_IMPORT)) continue;

				if (decl->physicalStorage & 7) {
					memberSize = my_max(static_cast<ExprLiteral *>(decl->type)->typeValue->size, memberSize);
				}
			}

			u64 addressReg = state->nextRegister++;
			u64 memberTemp = state->nextRegister;
			state->nextRegister += (memberSize + 7) / 8;

			for (auto decl : struct_->members.declarations) {
				if (decl->flags & (DECLARATION_IS_UNINITIALIZED | DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_IMPLICIT_IMPORT)) continue;

				if (decl->physicalStorage & 7) {
					generateIrForceDest(state, decl->initialValue, memberTemp);

					Ir &address = state->ir.add();
					address.op = IrOp::ADDRESS_OF_LOCAL;
					address.a = dest;
					address.b = decl->physicalStorage;
					address.dest = addressReg;
					address.opSize = 8;

					Ir &write = state->ir.add();
					write.op = IrOp::WRITE;
					write.a = addressReg;
					write.b = memberTemp;
					write.opSize = static_cast<ExprLiteral *>(decl->type)->typeValue->size;
				}
				else {
					generateIrForceDest(state, decl->initialValue, dest + decl->physicalStorage / 8);
				}
			}

			return dest;
		}
		case ExprFlavor::TYPE_LITERAL: {
			if (dest == DEST_NONE) return DEST_NONE;

			auto type = static_cast<ExprLiteral *>(expr)->typeValue;

			if (type->flavor == TypeFlavor::NAMESPACE) {
				reportError(expr, "Error: Cannot operate on a namespace");
				return dest;
			}

			Ir &ir = state->ir.add();
			ir.op = IrOp::TYPE;
			ir.dest = dest;
			ir.type = type;
			ir.opSize = 8;

			return dest;
		}
		case ExprFlavor::UNARY_OPERATOR: {
			ExprUnaryOperator *unary = static_cast<ExprUnaryOperator *>(expr);

			if (dest == DEST_NONE) {
				generateIr(state, unary->value, DEST_NONE);
				return DEST_NONE;
			}

			switch (unary->op) {
				case TOKEN('*'): {
					return loadAddressOf(state, unary->value, dest);
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
				case TokenT::TYPE_INFO: {
					generateIr(state, unary->value, dest);

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
			
			u64 patch = state->ir.count; // Patch this manually so it doesn't skip the completed block if it exists

			Ir &ifZ = state->ir.add();
			ifZ.op = IrOp::IF_Z_GOTO;
			ifZ.a = conditionReg;
			ifZ.opSize = 1;

			if (loop->body) {
				addLineMarker(state, loop->body);
				generateIr(state, loop->body, DEST_NONE);
			}

			Ir &jump = state->ir.add();
			jump.op = IrOp::GOTO;
			jump.b = loopStack[loopCount - 1].start;

			state->ir[patch].b = state->ir.count;

			if (loop->completedBody) {
				addLineMarker(state, loop->completedBody);
				generateIr(state, loop->completedBody, DEST_NONE);
			}

			popLoop(state);

			return DEST_NONE;
		}
		case ExprFlavor::ARRAY: {
			if (dest == DEST_NONE) return DEST_NONE;

			auto array = static_cast<ExprArray *>(expr);

			if (array->type->flags & TYPE_ARRAY_IS_FIXED) {
				auto elementType = static_cast<TypeArray *>(array->type)->arrayOf;

				u64 addressReg = state->nextRegister++;
				u64 valueReg = allocateSpaceForType(state, elementType);
				for (u64 i = 0; i < array->count; i++) {
					if (i + 1 < array->count && array->storage[i + 1] == nullptr) {
						u64 countReg = state->nextRegister++;
						Ir &count = state->ir.add();
						count.op = IrOp::IMMEDIATE;
						count.dest = countReg;
						count.a = array->count - i;
						count.opSize = 8;
						
						Ir &address = state->ir.add();
						address.op = IrOp::ADDRESS_OF_LOCAL;
						address.dest = addressReg;
						address.a = dest;
						address.b = dest + i * elementType->size;
						address.opSize = 8;

						u64 patch = state->ir.count;

						generateIrForceDest(state, array->storage[i], valueReg);

						Ir &write = state->ir.add();
						write.op = IrOp::WRITE;
						write.a = addressReg;
						write.b = valueReg;
						write.opSize = elementType->size;

						Ir &add = state->ir.add();
						add.op = IrOp::ADD_CONSTANT;
						add.dest = addressReg;
						add.a = addressReg;
						add.b = elementType->size;
						add.opSize = 8;

						Ir &dec = state->ir.add();
						dec.op = IrOp::ADD_CONSTANT;
						dec.dest = countReg;
						dec.a = countReg;
						dec.b = static_cast<u64>(-1LL);
						dec.opSize = 8;

						Ir &branch = state->ir.add();
						branch.op = IrOp::IF_NZ_GOTO;
						branch.a = countReg;
						branch.b = patch;
						branch.opSize = 8;

						break;
					}
					else {
						Ir &address = state->ir.add();
						address.op = IrOp::ADDRESS_OF_LOCAL;
						address.dest = addressReg;
						address.a = dest;
						address.b = i * elementType->size;
						address.opSize = 8;

						generateIrForceDest(state, array->storage[i], valueReg);

						Ir &write = state->ir.add();
						write.op = IrOp::WRITE;
						write.a = addressReg;
						write.b = valueReg;
						write.opSize = elementType->size;
					}
				}
			}
			else {
				// The only time an array literal should have a type other than fixed is the compiler generated default empty array value
				assert(array->count == 0);

				Ir &set = state->ir.add();
				set.op = IrOp::SET;
				set.dest = dest;
				set.a = 0;
				set.opSize = array->type->size;
				set.destSize = set.opSize;
			}

			return dest;
		}
		case ExprFlavor::STATIC_IF: {
			return DEST_NONE; // In the event that the static if returns false and there is no else block, we just leave the static if expression in the tree, 
				   // so when we see a static if here we should just generate no code
		}
		default:
			assert(false);
			return DEST_NONE;
	}
}

u64 generateIrForceDest(IrState *state, Expr *expr, u64 dest) {
	assert(dest != DEST_NONE);

	u64 stored = generateIr(state, expr, dest, true);

	if (stored != dest) {
		Ir &set = state->ir.add();
		set.op = IrOp::SET;
		set.opSize = expr->type->size;
		set.destSize = expr->type->size;
		set.a = stored;
		set.dest = dest;
	}

	return dest;
}

void runIrGenerator() {
	PROFILE_FUNC();
	while (true) {

		ExprFunction *function = irGeneratorQueue.take();

		if (!function)
			break;

		u64 paramOffset;

		if (!isStandardSize(static_cast<ExprLiteral *>(function->returns.declarations[0]->type)->typeValue->size)) {
			paramOffset = 1;
		}
		else {
			paramOffset = 0;
		}


		function->state.parameterSpace = my_max(4, function->arguments.declarations.count + paramOffset + function->returns.declarations.count - 1);
		function->state.nextRegister += function->state.parameterSpace;

		for (u64 i = 0; i < function->arguments.declarations.count; i++) {
			auto declaration = function->arguments.declarations[i];
			auto type = static_cast<ExprLiteral *>(declaration->type)->typeValue;

			if (isStandardSize(type->size)) {
				declaration->physicalStorage = i + 1 + paramOffset;
			}
			else {
				declaration->physicalStorage = allocateSpaceForType(&function->state, type);
			}
		}

		for (u64 i = 1; i < function->returns.declarations.count; i++) {
			auto declaration = function->returns.declarations[i];

			declaration->physicalStorage = function->arguments.declarations.count + paramOffset + i;
		}


		generateIr(&function->state, function->body, DEST_NONE);

		if (hadError) {
			goto error;
		}

		//addLineMarker(&function->state, function->body->start.fileUid, function->body->end);

		// @Incomplete @ErrorMessage check wether the function actually returns

		CoffJob job;
		job.function = function;
		job.flavor = CoffJobFlavor::FUNCTION;

		coffWriterQueue.add(job);
	}

	error:
	coffWriterQueue.add({ nullptr, CoffJobFlavor::FUNCTION });
}