#include "Basic.h"

#include "IrGenerator.h"
#include "Array.h"
#include "Ast.h"
#include "Lexer.h"
#include "BucketedArenaAllocator.h"
#include "CoffWriter.h"
#include "TypeTable.h"
#include "Infer.h"
#include "Error.h"

u32 allocateRegister(IrState *state) {
	return state->nextRegister++;
}

u32 allocateStackSpace(IrState *state, u32 size, u32 alignment) {
	state->stackSpace = AlignPO2(state->stackSpace, alignment);
	u32 offset = state->stackSpace;
	state->stackSpace += size;
	return offset;
}

u32 allocateStackSpace(IrState *state, Type *type) {
	return allocateStackSpace(state, type->size, type->alignment);
}

u32 loadStackAddress(IrState *state, u32 offset) {
	Ir &address = state->ir.add();
	address.op = IrOp::STACK_ADDRESS;
	address.dest = allocateRegister(state);
	address.immediate = offset;

	return address.dest;
}

u32 allocateStackSpaceAndLoadAddress(IrState *state, u32 size, u32 alignment) {
	return loadStackAddress(state, allocateStackSpace(state, size, alignment));
}

u32 allocateStackSpaceAndLoadAddress(IrState *state, Type *type) {
	return allocateStackSpaceAndLoadAddress(state, type->size, type->alignment);
}

u32 memop(IrState *state, IrOp op, u32 dest, u32 src, u32 size, u32 offset = 0) {
	assert(op != IrOp::COPY || offset == 0);

	Ir &memop = state->ir.add();
	memop.op = op;
	memop.dest = dest;
	memop.a = src;
	memop.opSize = size;
	memop.immediate = offset;

	return dest;
}

u32 copyOrWrite(IrState *state, u32 dest, u32 src, Type *type, u32 offset = 0) {
	if (isStoredByPointer(type)) {
		if (offset) {
			Ir &add = state->ir.add();
			add.op = IrOp::ADD_CONSTANT;
			add.dest = allocateRegister(state);
			add.a = dest;
			add.immediate = offset;
			add.opSize = 8;

			dest = add.dest;
		}
		

		return memop(state, IrOp::COPY, dest, src, type->size);
	}
	else {
		return memop(state, IrOp::WRITE, dest, src, type->size, offset);
	}
}


u32 copyOrRead(IrState *state, u32 dest, u32 src, Type *type, u32 offset = 0) {
	if (isStoredByPointer(type)) {
		if (offset) {
			Ir &add = state->ir.add();
			add.op = IrOp::ADD_CONSTANT;
			add.dest = allocateRegister(state);
			add.a = src;
			add.immediate = offset;
			add.opSize = 8;

			src = add.dest;
		}

		return memop(state, IrOp::COPY, dest, src, type->size);
	}
	else {
		return memop(state, IrOp::READ, dest, src, type->size, offset);
	}
}

u32 set(IrState *state, u32 dest, u32 src, u32 size) {
	Ir &set = state->ir.add();
	set.op = IrOp::SET;
	set.dest = dest;
	set.a = src;
	set.opSize = size;

	return dest;
}

u32 constant(IrState *state, u32 dest, u32 size, u64 value) {
	Ir &constant = state->ir.add();
	constant.op = IrOp::IMMEDIATE;
	constant.dest = dest;
	constant.opSize = size;
	constant.immediate = value;

	return dest;
}

u32 generateIr(IrState *state, Expr *expr);

struct Loop {
	struct ExprLoop *loop;
	u32 start;

	Array<u32> endPatches;
};

static Array<Expr *> deferStack;
static Block *currentBlock;

static Array<Loop> loopStack;
static u32 loopCount;

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
	index.a = it_index->registerOfStorage;
	index.immediate = 1;
	index.dest = it_index->registerOfStorage;

	Ir &increment = state->ir.add();
	increment.op = IrOp::ADD_CONSTANT;
	increment.opSize = loop->forBegin->type->size;
	increment.a = loop->irPointer;
	increment.dest = loop->irPointer;

	if (loop->forBegin->type->flavor == TypeFlavor::POINTER) {
		auto pointer = static_cast<TypePointer *>(loop->forBegin->type);

		increment.immediate = pointer->pointerTo->size;
	}
	else if (loop->forBegin->type->flavor == TypeFlavor::ARRAY) {
		auto array = static_cast<TypeArray *>(loop->forBegin->type);

		increment.immediate = array->arrayOf->size;
		increment.opSize = 8;
	}
	else if (loop->forBegin->type->flavor == TypeFlavor::STRING) {
		increment.immediate = 1;
		increment.opSize = 8;
	}
	else {
		increment.immediate = 1;
	}
}

void addLineMarker(IrState *state, Expr *expr) {
	if (expr->flavor == ExprFlavor::BLOCK)
		return;

	Ir &ir = state->ir.add();

	ir.op = IrOp::LINE_MARKER;
	ir.location.start = expr->start;
	ir.location.end = expr->end;
}


static void exitBlock(IrState *state, Block *block, bool isBreak) {
	for (s64 i = static_cast<s64>(deferStack.count) - 1; i >= 0; --i) {
		auto expr = deferStack[static_cast<u32>(i)];

		if (expr->flavor == ExprFlavor::FOR) {
			auto loop = static_cast<ExprLoop *>(expr);

			auto current = &loop->iteratorBlock;
			bool found = false;

			if (block) {
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
			generateIr(state, defer->expr);
		}
	}
}

u32 generateSlice(IrState *state, ExprSlice *slice) {
	u32 array = generateIr(state, slice->array);
	u32 pointer = array;

	if (slice->array->type->flavor == TypeFlavor::ARRAY || slice->array->type->flavor == TypeFlavor::STRING) {
		if (!(slice->array->type->flags & TYPE_ARRAY_IS_FIXED)) {
			pointer = memop(state, IrOp::READ, allocateRegister(state), array, 8);
		}
	}


	u32 countReg = slice->sliceStart ? allocateRegister(state) : 0;

	if (!slice->sliceEnd) {
		if (slice->array->type->flags & TYPE_ARRAY_IS_FIXED) {
			constant(state, countReg, 8, static_cast<TypeArray *>(slice->array->type)->count);
		}
		else {
			memop(state, IrOp::READ, countReg, array, 8, 8);
		}
	}

	u32 elementSize;

	if (slice->array->type->flavor == TypeFlavor::POINTER) {
		elementSize = static_cast<TypePointer *>(slice->array->type)->pointerTo->size;
	}
	else if (slice->type->flavor == TypeFlavor::STRING) {
		elementSize = 1;
	}
	else if (slice->type->flavor == TypeFlavor::ARRAY) {
		elementSize = static_cast<TypeArray *>(slice->array->type)->arrayOf->size;
	}


	u32 result = allocateStackSpaceAndLoadAddress(state, slice->type);

	if (slice->sliceEnd && slice->sliceStart) {
		auto offset = generateIr(state, slice->sliceStart);
		auto end = generateIr(state, slice->sliceEnd);

		auto &count = state->ir.add();
		count.op = IrOp::SUB;
		count.dest = countReg;
		count.a = end;
		count.b = offset;
		count.opSize = 8;

		memop(state, IrOp::WRITE, result, countReg, 8, 8);

		if (elementSize != 1) {
			u32 mulReg = allocateRegister(state);

			auto &mul = state->ir.add();
			mul.op = IrOp::MUL_BY_CONSTANT;
			mul.dest = mulReg;
			mul.a = offset;
			mul.immediate = elementSize;
			mul.opSize = 8;

			offset = mulReg;
		}

		u32 dataReg = countReg;

		auto &add = state->ir.add();
		add.op = IrOp::ADD;
		add.dest = dataReg;
		add.a = pointer;
		add.b = offset;
		add.opSize = 8;

		memop(state, IrOp::WRITE, result, dataReg, 8);
	}
	else if (slice->sliceStart) {
		auto offset = generateIr(state, slice->sliceStart);

		auto &count = state->ir.add();
		count.op = IrOp::SUB;
		count.dest = countReg;
		count.a = countReg;
		count.b = offset;
		count.opSize = 8;

		memop(state, IrOp::WRITE, result, countReg, 8, 8);

		if (elementSize != 1) {
			u32 mulReg = allocateRegister(state);

			auto &mul = state->ir.add();
			mul.op = IrOp::MUL_BY_CONSTANT;
			mul.dest = mulReg;
			mul.a = offset;
			mul.immediate = elementSize;
			mul.opSize = 8;

			offset = mulReg;
		}

		u32 dataReg = countReg;

		auto &add = state->ir.add();
		add.op = IrOp::ADD;
		add.dest = dataReg;
		add.a = pointer;
		add.b = offset;
		add.opSize = 8;

		memop(state, IrOp::WRITE, result, dataReg, 8);
	}
	else {
		assert(slice->sliceEnd);
		auto end = generateIr(state, slice->sliceEnd);

		memop(state, IrOp::WRITE, result, end, 8, 8);
		memop(state, IrOp::WRITE, result, pointer, 8);
	}

	return result;
}

IrOp getOpForBinary(TokenT op) {
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
	case TOKEN('+'):
	case TokenT::PLUS_EQUALS:
		return IrOp::ADD;
	case TOKEN('-'):
	case TokenT::MINUS_EQUALS:
		return IrOp::SUB;
	case TOKEN('&'):
	case TokenT::AND_EQUALS:
		return IrOp::AND;
	case TOKEN('|'):
	case TokenT::OR_EQUALS:
		return IrOp::OR;
	case TOKEN('^'):
	case TokenT::XOR_EQUALS:
		return IrOp::XOR;
	case TokenT::SHIFT_LEFT:
	case TokenT::SHIFT_LEFT_EQUALS:
		return IrOp::SHIFT_LEFT;
	case TokenT::SHIFT_RIGHT:
	case TokenT::SHIFT_RIGHT_EQUALS:
		return IrOp::SHIFT_RIGHT;
	case TokenT::TIMES_EQUALS:
	case TOKEN('*'):
		return IrOp::MUL;
	case TOKEN('/'):
	case TokenT::DIVIDE_EQUALS:
		return IrOp::DIV;
	case TOKEN('%'):
	case TokenT::MOD_EQUALS:
		return IrOp::MOD;
	case TokenT::CAST:
	case TOKEN('['):
	case TOKEN('='):
	case TokenT::LOGIC_AND:
	case TokenT::LOGIC_OR:
	default:
		assert(false);
		return IrOp::NOOP;
	}
}

u32 loadAddressOf(IrState *state, Expr *expr, u32 offset = 0) {
	if (expr->flavor == ExprFlavor::BINARY_OPERATOR && static_cast<ExprBinaryOperator *>(expr)->op == TOKEN('[')) {
		auto binary = static_cast<ExprBinaryOperator *>(expr);

		u32 array = generateIr(state, binary->left);
		u32 pointer = array;

		if (binary->left->type->flavor == TypeFlavor::ARRAY || binary->left->type->flavor == TypeFlavor::STRING) {
			if (!(binary->left->type->flags & TYPE_ARRAY_IS_FIXED)) {
				pointer = memop(state, IrOp::READ, allocateRegister(state), array, 8);
			}
		}

		u32 elementSize;

		if (binary->left->type->flavor == TypeFlavor::POINTER) {
			elementSize = static_cast<TypePointer *>(binary->left->type)->pointerTo->size;
		}
		else if (binary->left->type->flavor == TypeFlavor::STRING) {
			elementSize = 1;
		}
		else if (binary->left->type->flavor == TypeFlavor::ARRAY) {
			elementSize = static_cast<TypeArray *>(binary->left->type)->arrayOf->size;
		}

		auto index = generateIr(state, binary->right);

		if (elementSize != 1) {
			u32 mulReg = allocateRegister(state);

			auto &mul = state->ir.add();
			mul.op = IrOp::MUL_BY_CONSTANT;
			mul.dest = mulReg;
			mul.a = index;
			mul.immediate = elementSize;
			mul.opSize = 8;

			index = mulReg;
		}

		u32 result = allocateRegister(state);

		auto &add = state->ir.add();
		add.op = IrOp::ADD;
		add.dest = result;
		add.a = pointer;
		add.b = index;
		add.opSize = 8;

		if (offset) {
			auto &add = state->ir.add();
			add.op = IrOp::ADD_CONSTANT;
			add.dest = result;
			add.a = result;
			add.b = offset;
			add.opSize = 8;
		}
		
		return result;
	}
	else if (expr->flavor == ExprFlavor::UNARY_OPERATOR && static_cast<ExprUnaryOperator *>(expr)->op == TokenT::SHIFT_LEFT) {
		auto unary = static_cast<ExprUnaryOperator *>(expr);

		u32 address = generateIr(state, unary->value);

		if (offset) {
			u32 result = allocateRegister(state);

			Ir &add = state->ir.add();
			add.op = IrOp::ADD_CONSTANT;
			add.dest = result;
			add.a = address;
			add.immediate = offset;
			add.opSize = 8;

			address = result;
		}

		return address;
	}
	else if (expr->flavor == ExprFlavor::IDENTIFIER) {
		auto identifier = static_cast<ExprIdentifier *>(expr);

		if (identifier->structAccess) {
			offset += identifier->declaration->physicalStorage;

			if (identifier->structAccess->type->flavor == TypeFlavor::POINTER) {
				u32 address = generateIr(state, identifier->structAccess);

				if (offset) {
					u32 result = allocateRegister(state);

					Ir &add = state->ir.add();
					add.op = IrOp::ADD_CONSTANT;
					add.dest = result;
					add.a = address;
					add.immediate = offset;
					add.opSize = 8;

					address = result;
				}

				return address;
			}
			else {
				return loadAddressOf(state, identifier->structAccess, offset);
			}
		}
		else {
			if (identifier->declaration->enclosingScope->flavor == BlockFlavor::GLOBAL) {
				u32 result = allocateRegister(state);

				Ir &address = state->ir.add();
				address.op = IrOp::ADDRESS_OF_GLOBAL;
				address.dest = result;
				address.a = offset;
				address.data = identifier->declaration;

				return result;
			}
			else {
				u32 address = identifier->declaration->registerOfStorage;

				assert(declarationIsStoredByPointer(identifier->declaration));

				if (offset) {
					u32 result = allocateRegister(state);
					Ir &add = state->ir.add();
					add.op = IrOp::ADD_CONSTANT;
					add.dest = result;
					add.a = address;
					add.immediate = offset;
					add.opSize = 8;

					address = result;
				}

				return address;
			}
		}
	}
	else {
		assert(isStoredByPointer(expr->type));
		u32 address = generateIr(state, expr);

		if (offset) {
			u32 result = allocateRegister(state);

			Ir &add = state->ir.add();
			add.op = IrOp::ADD_CONSTANT;
			add.opSize = 8;
			add.dest = result;
			add.a = address;
			add.immediate = offset;

			return result;
		}
		else {
			return address;
		}
	}
}

u32 generateCast(IrState *state, ExprBinaryOperator *binary) {
	auto left = binary->left;
	auto right = binary->right;

	assert(left->flavor == ExprFlavor::TYPE_LITERAL);

	u32 rightReg = generateIr(state, right);

	Type *castTo = binary->type;

	if (castTo == right->type || (binary->flags & EXPR_CAST_IS_BITWISE)) {
		if (isStoredByPointer(castTo) && !isStoredByPointer(right->type)) {
			assert(castTo->size == right->type->size);

			return memop(state, IrOp::WRITE, allocateStackSpaceAndLoadAddress(state, castTo), rightReg, castTo->size);
		}
		else if (!isStoredByPointer(castTo) && isStoredByPointer(right->type)) {
			assert(castTo->size == right->type->size);

			return memop(state, IrOp::READ, allocateRegister(state), rightReg, castTo->size);
		}

		return rightReg;
	}

	if (right->type == TYPE_ANY) {
		u32 pointerReg = memop(state, IrOp::READ, allocateRegister(state), rightReg, 8);

		if (isStoredByPointer(castTo)) {
			return memop(state, IrOp::COPY, allocateStackSpaceAndLoadAddress(state, castTo), pointerReg, castTo->size);
		}
		else {
			return memop(state, IrOp::READ, pointerReg, pointerReg, castTo->size);
		}
	}

	switch (castTo->flavor) {
	case TypeFlavor::STRUCT: {
		assert(castTo == TYPE_ANY);

		u32 result = allocateStackSpaceAndLoadAddress(state, TYPE_ANY);

		u32 storage = allocateStackSpaceAndLoadAddress(state, right->type);
		memop(state, IrOp::WRITE, result, storage, 8);
		
		copyOrWrite(state, storage, rightReg, right->type);

		u32 typeReg = allocateRegister(state);
		Ir &type = state->ir.add();
		type.op = IrOp::TYPE;
		type.dest = typeReg;
		type.data = right->type;


		Ir &ir = state->ir.add();
		ir.op = IrOp::TYPE_INFO;
		ir.a = typeReg;
		ir.dest = typeReg;

		memop(state, IrOp::WRITE, result, typeReg, 8, 8);
		return result;
	}
	case TypeFlavor::BOOL: {
		u32 src;
		u32 size;

		if (right->type->flavor == TypeFlavor::ARRAY || right->type == &TYPE_STRING) {
			if (right->type->flags & TYPE_ARRAY_IS_FIXED) {
				return constant(state, allocateRegister(state), 1, 1);
			}
			else {
				src = memop(state, IrOp::READ, allocateRegister(state), rightReg, 8, 8);
				size = 8;
			}
		}
		else {
			src = rightReg;
			size = right->type->size;
		}

		u32 zero = constant(state, allocateRegister(state), size, 0);

		u32 result = allocateRegister(state);

		Ir &ir = state->ir.add();
		ir.op = IrOp::NOT_EQUAL;
		ir.dest = result;
		ir.a = src;
		ir.b = zero;
		ir.opSize = size;

		if (right->type->flavor == TypeFlavor::FLOAT)
			ir.flags |= IR_FLOAT_OP;

		return result;
	}
	case TypeFlavor::FLOAT: {
		u32 result = allocateRegister(state);

		Ir &conversion = state->ir.add();
		conversion.dest = result;
		conversion.a = rightReg;
		conversion.b = castTo->size;
		conversion.opSize = right->type->size;

		if (right->type->flavor == TypeFlavor::FLOAT) {
			conversion.op = IrOp::FLOAT_CAST;
		}
		else if (right->type->flavor == TypeFlavor::INTEGER) {
			conversion.op = IrOp::INT_TO_FLOAT;

			if (right->type->flags & TYPE_INTEGER_IS_SIGNED) {
				conversion.flags |= IR_SIGNED_OP;
			}
		}

		return result;
	}
	case TypeFlavor::ENUM:
	case TypeFlavor::INTEGER: {
		if (right->type->flavor == TypeFlavor::POINTER) {
			return rightReg;
		}
		else if (right->type->flavor == TypeFlavor::FUNCTION) {
			return rightReg;
		}

		if (right->type->flavor == TypeFlavor::FLOAT) {
			u32 result = allocateRegister(state);

			Ir &conversion = state->ir.add();
			conversion.op = IrOp::FLOAT_TO_INT;
			conversion.dest = result;
			conversion.a = rightReg;
			conversion.b = castTo->size;
			conversion.opSize = right->type->size;

			if (right->type->flags & TYPE_INTEGER_IS_SIGNED) {
				conversion.flags |= IR_SIGNED_OP;
			}

			return result;
		}
		else {
			if (castTo->size <= right->type->size) {
				return rightReg;
			}

			u32 result = allocateRegister(state);

			Ir &conversion = state->ir.add();
			conversion.op = IrOp::EXTEND_INT;
			conversion.dest = result;
			conversion.a = rightReg;
			conversion.b = castTo->size;
			conversion.opSize = right->type->size;

			if ((right->type->flags & TYPE_INTEGER_IS_SIGNED) && (castTo->flags & TYPE_INTEGER_IS_SIGNED)) {
				conversion.flags |= IR_SIGNED_OP;
			}

			return result;
		}
	}
	case TypeFlavor::ARRAY: {
		if (right->type == &TYPE_STRING || (right->type->flags & TYPE_ARRAY_IS_DYNAMIC)) {
			assert(!(left->type->flags & TYPE_ARRAY_IS_FIXED));
			return rightReg;
		}
		else {
			assert(right->type->flags & TYPE_ARRAY_IS_FIXED);
			assert(!(left->type->flags & TYPE_ARRAY_IS_DYNAMIC));

			u32 result = allocateStackSpaceAndLoadAddress(state, castTo);

			memop(state, IrOp::WRITE, result, rightReg, 8);
			u32 count = constant(state, allocateRegister(state), 8, static_cast<TypeArray *>(right->type)->count);
			memop(state, IrOp::WRITE, result, count, 8, 8);

			return result;
		}
	}
	case TypeFlavor::POINTER:
	case TypeFlavor::FUNCTION:
	case TypeFlavor::STRING: // []u8 -> string
		return rightReg; // These casts should be a nop
	case TypeFlavor::TYPE:
	case TypeFlavor::VOID:
		assert(false);
		return 0;
	}

	assert(false);
	return 0;
}

u32 generateMathBinaryOp(IrState *state, ExprBinaryOperator *binary) {
	u32 leftReg = generateIr(state, binary->left);
	u32 rightReg = generateIr(state, binary->right);

	u32 result = allocateRegister(state);

	Ir &ir = state->ir.add();
	ir.op = getOpForBinary(binary->op);
	ir.dest = result;
	ir.a = leftReg;
	ir.b = rightReg;
	ir.opSize = binary->right->type->size;

	if (binary->left->type->flavor == TypeFlavor::FLOAT) {
		ir.flags |= IR_FLOAT_OP;
	}

	if (binary->left->type->flags & TYPE_INTEGER_IS_SIGNED) {
		assert(binary->right->type->flags & TYPE_INTEGER_IS_SIGNED);

		ir.flags |= IR_SIGNED_OP;
	}

	return result;
}

struct RMWInfo {
	u32 address;
	u32 value;
};

RMWInfo readForRMW(IrState *state, Expr *expr) {
	RMWInfo result;

	if (expr->flavor == ExprFlavor::IDENTIFIER) {
		auto identifier = static_cast<ExprIdentifier *>(expr);

		if (!identifier->structAccess && !declarationIsStoredByPointer(identifier->declaration)) {
			result.value = identifier->declaration->registerOfStorage;
			return result;
		}
	}

	assert(!isStoredByPointer(expr->type));
	result.address = loadAddressOf(state, expr);
	result.value = memop(state, IrOp::READ, allocateRegister(state), result.address, expr->type->size);

	return result;
}

void writeForRMW(IrState *state, Expr *expr, RMWInfo registers) {
	if (expr->flavor == ExprFlavor::IDENTIFIER) {
		auto identifier = static_cast<ExprIdentifier *>(expr);

		if (!identifier->structAccess && !declarationIsStoredByPointer(identifier->declaration)) {
			// No op, result has already been written here
			return;
		}
	}

	assert(!isStoredByPointer(expr->type));
	memop(state, IrOp::WRITE, registers.address, registers.value, expr->type->size);
}

void generateAssignBinaryOp(IrState *state, ExprBinaryOperator *binary) {
	RMWInfo registers = readForRMW(state, binary->left);

	u32 rightReg = generateIr(state, binary->right);

	Ir &ir = state->ir.add();
	ir.op = getOpForBinary(binary->op);
	ir.dest = registers.value;
	ir.a = registers.value;
	ir.b = rightReg;
	ir.opSize = binary->right->type->size;

	if (binary->left->type->flavor == TypeFlavor::FLOAT) {
		ir.flags |= IR_FLOAT_OP;
	}

	if (binary->left->type->flags & TYPE_INTEGER_IS_SIGNED) {
		assert(binary->right->type->flags & TYPE_INTEGER_IS_SIGNED);

		ir.flags |= IR_SIGNED_OP;
	}

	writeForRMW(state, binary->left, registers);
}

u32 generateEquals(IrState *state, u32 leftReg, Expr *right, bool equals) {
	u32 result = allocateRegister(state);

	u32 rightReg = generateIr(state, right);

	if (right->type == &TYPE_STRING) {
		if (!stringsEqualFunction) {
			reportError("Internal Compiler Error: Comparing strings before __strings_equal is declared");
			assert(false);
			exit(1); // @Cleanup Forceful exit since we don't have good error handling here and its an internal compiler error
		}

		u32 function = generateIr(state, stringsEqualFunction);

		FunctionCall *argumentInfo = static_cast<FunctionCall *>(state->allocator.allocate(sizeof(FunctionCall) + sizeof(argumentInfo->args[0]) * 2));
		argumentInfo->argCount = 2;

		argumentInfo->args[0].number = leftReg;
		argumentInfo->args[0].type = TYPE_VOID_POINTER;
		argumentInfo->args[1].number = rightReg;
		argumentInfo->args[1].type = TYPE_VOID_POINTER;

		argumentInfo->returnType = &TYPE_BOOL;

		state->maxCallArguments = my_max(state->maxCallArguments, 4);

		Ir &ir = state->ir.add();
		ir.op = IrOp::CALL;
		ir.a = function;
		ir.data = argumentInfo;
		ir.dest = result;
		ir.opSize = 1;
		ir.flags |= IR_C_CALL;


		if (!equals) {
			Ir &invert = state->ir.add();

			invert.op = IrOp::EQUAL;
			invert.dest = result;
			invert.a = result;
			invert.b = 0;
			invert.opSize = 1;
		}
	}
	else {
		Ir &ir = state->ir.add();

		ir.op = equals ? IrOp::EQUAL : IrOp::NOT_EQUAL;
		ir.dest = result;
		ir.a = leftReg;
		ir.b = rightReg;
		ir.opSize = right->type->size;

		if (right->type->flavor == TypeFlavor::FLOAT)
			ir.flags |= IR_FLOAT_OP;
	}

	return result;
}

u32 generateBinary(IrState *state, ExprBinaryOperator *binary) {
	auto left = binary->left;
	auto right = binary->right;

	switch (binary->op) {
	case TokenT::CAST: {
		return generateCast(state, binary);
	}
	case TOKEN('['): {
		u32 address = loadAddressOf(state, binary);

		if (isStoredByPointer(binary->type)) {
			return address;
		}
		else {
			return memop(state, IrOp::READ, allocateRegister(state), address, binary->type->size);
		}
	}
	case TokenT::EQUAL:
	case TokenT::NOT_EQUAL: {
		assert(left->type == right->type);

		return generateEquals(state, generateIr(state, binary->left), binary->right, binary->op == TokenT::EQUAL);
	}
	case TokenT::GREATER_EQUAL:
	case TokenT::LESS_EQUAL:
	case TOKEN('>'):
	case TOKEN('<'):
	case TOKEN('&'):
	case TOKEN('|'):
	case TOKEN('^'):
	case TokenT::SHIFT_LEFT:
	case TokenT::SHIFT_RIGHT:
	case TOKEN('*'):
	case TOKEN('/'):
	case TOKEN('%'): {
		assert(right->type == left->type);

		return generateMathBinaryOp(state, binary);
	}
	case TOKEN('+'):
	case TOKEN('-'): {
		if (left->type->flavor == TypeFlavor::POINTER) {
			auto pointer = static_cast<TypePointer *>(left->type);

			u32 result = allocateRegister(state);

			u32 leftReg = generateIr(state, left);


			u32 rightReg = generateIr(state, right);

			if (right->type->flavor == TypeFlavor::POINTER) {
				assert(binary->op == TOKEN('-'));
				Ir &sub = state->ir.add();
				sub.op = IrOp::SUB;
				sub.dest = result;
				sub.a = leftReg;
				sub.b = rightReg;
				sub.opSize = 8;

				if (pointer->pointerTo->size) {
					Ir &div = state->ir.add();
					div.op = IrOp::DIVIDE_BY_CONSTANT;
					div.dest = result;
					div.a = result;
					div.immediate = pointer->pointerTo->size;
					div.opSize = 8;
					div.flags |= IR_SIGNED_OP;
				}
			}
			else {
				u32 offset = rightReg;

				if (pointer->pointerTo->size != 1) {
					u32 mulReg = allocateRegister(state);

					Ir &mul = state->ir.add();

					mul.op = IrOp::MUL_BY_CONSTANT;
					mul.dest = mulReg;
					mul.a = rightReg;
					mul.immediate = pointer->pointerTo->size;
					mul.opSize = 8;

					offset = mulReg;
				}

				Ir &add = state->ir.add();
				add.op = binary->op == TOKEN('+') ? IrOp::ADD : IrOp::SUB;
				add.dest = result;
				add.a = leftReg;
				add.b = offset;
				add.opSize = 8;
			}

			return result;
		}
		else {
			return generateMathBinaryOp(state, binary);
		}
	}
	case TOKEN('='): {
		if (left->flavor == ExprFlavor::IDENTIFIER) {
			auto identifier = static_cast<ExprIdentifier *>(left);

			if (!identifier->structAccess && !declarationIsStoredByPointer(identifier->declaration)) {
				u32 value = generateIr(state, right);
				set(state, identifier->declaration->registerOfStorage, value, identifier->type->size);
				
				return 0;
			}
		}

		u32 address = loadAddressOf(state, left);
		copyOrWrite(state, address, generateIr(state, right), left->type);
		return 0;
	}
	case TokenT::PLUS_EQUALS: {
		if (left->type->flavor == TypeFlavor::POINTER) {
			auto pointer = static_cast<TypePointer *>(left->type);

			RMWInfo registers = readForRMW(state, left);
			u32 offset = generateIr(state, right);

			if (pointer->pointerTo->size != 1) {
				u32 mulReg = allocateRegister(state);

				Ir &mul = state->ir.add();

				mul.op = IrOp::MUL_BY_CONSTANT;
				mul.dest = mulReg;
				mul.a = offset;
				mul.immediate = pointer->pointerTo->size;
				mul.opSize = 8;

				offset = mulReg;
			}

			Ir &add = state->ir.add();
			add.op = IrOp::ADD;
			add.dest = registers.value;
			add.a = registers.value;
			add.b = offset;
			add.opSize = 8;

			writeForRMW(state, left, registers);
			return 0;
		}
		else {
			generateAssignBinaryOp(state, binary);
			return 0;
		}
	}
	case TokenT::LOGIC_AND:
	case TokenT::LOGIC_OR: {
		u32 result = allocateRegister(state);


		u32 leftReg = generateIr(state, left);
		set(state, result, leftReg, 1);

		u32 patch = state->ir.count;

		Ir &branch = state->ir.add();
		branch.op = binary->op == TokenT::LOGIC_AND ? IrOp::IF_Z_GOTO : IrOp::IF_NZ_GOTO;
		branch.a = result;
		branch.opSize = 1;

		//addLineMarker(state, right);
		u32 rightReg = generateIr(state, right);
		set(state, result, rightReg, 1);

		state->ir[patch].b = state->ir.count;

		return result;
	}
	case TokenT::AND_EQUALS:
	case TokenT::OR_EQUALS:
	case TokenT::XOR_EQUALS:
	case TokenT::SHIFT_LEFT_EQUALS:
	case TokenT::SHIFT_RIGHT_EQUALS:
	case TokenT::MINUS_EQUALS:
	case TokenT::TIMES_EQUALS:
	case TokenT::DIVIDE_EQUALS:
	case TokenT::MOD_EQUALS: {
		generateAssignBinaryOp(state, binary);
		return 0;
	}
	default:
		assert(false);
		return 0;
	}
}

void generateBlock(IrState *state, ExprBlock *block) {
	auto &enter = state->ir.add();
	enter.op = IrOp::BLOCK;
	enter.data = &block->declarations;

	for (auto declaration : block->declarations.declarations) {
		if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING))) {
			if (declarationIsStoredByPointer(declaration)) {
				declaration->physicalStorage = allocateStackSpace(state, getDeclarationType(declaration));
				declaration->registerOfStorage = loadStackAddress(state, declaration->physicalStorage);
			}
			else {
				declaration->registerOfStorage = allocateRegister(state);
			}
		}
	}

	for (auto subExpr : block->exprs) {
		addLineMarker(state, subExpr);
		generateIr(state, subExpr);
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

	auto &exit = state->ir.add();
	exit.op = IrOp::BLOCK;
	exit.data = nullptr;
}

void generateBreak(IrState *state, ExprBreakOrContinue *break_) {
	for (u32 i = loopCount; i-- != 0;) {

		if (loopStack[i].loop == break_->refersTo) {
			exitBlock(state, &loopStack[i].loop->iteratorBlock, true);
			loopStack[i].endPatches.add(state->ir.count);
			break;
		}
	}

	Ir &jump = state->ir.add();
	jump.op = IrOp::GOTO;
}

void generateContinue(IrState *state, ExprBreakOrContinue *continue_) {
	u32 begin;

	for (u32 i = loopCount; i-- != 0;) {
		if (loopStack[i].loop == continue_->refersTo) {
			exitBlock(state, &loopStack[i].loop->iteratorBlock, false);
			begin = loopStack[i].start;
			break;
		}
	}

	Ir &jump = state->ir.add();
	jump.op = IrOp::GOTO;
	jump.b = begin;
}

void generateRemove(IrState *state, ExprBreakOrContinue *remove) {
	if (!removeFunction) {
		reportError(remove, "Internal Compiler Error: Removing something before __remove is declared");
		assert(false);
		exit(1); // @Cleanup Forceful exit since we don't have good error handling here and its an internal compiler error
	}

	assert(remove->refersTo->forBegin->type->flavor == TypeFlavor::ARRAY);
	assert(!(remove->refersTo->forBegin->type->flags & TYPE_ARRAY_IS_FIXED));

	u32 function = generateIr(state, removeFunction);

	FunctionCall *argumentInfo = static_cast<FunctionCall *>(state->allocator.allocate(sizeof(FunctionCall) + sizeof(argumentInfo->args[0]) * 3));
	argumentInfo->argCount = 3;

	u32 sizeReg = constant(state, allocateRegister(state), 8, static_cast<TypeArray *>(remove->refersTo->forBegin->type)->arrayOf->size);

	argumentInfo->args[0].number = remove->refersTo->arrayPointer;
	argumentInfo->args[0].type = TYPE_VOID_POINTER;
	argumentInfo->args[1].number = remove->refersTo->irPointer;
	argumentInfo->args[1].type = TYPE_VOID_POINTER;
	argumentInfo->args[2].number = sizeReg;
	argumentInfo->args[2].type = &TYPE_U64;

	argumentInfo->returnType = TYPE_VOID_POINTER;

	state->maxCallArguments = my_max(state->maxCallArguments, 4);

	Ir &ir = state->ir.add();
	ir.op = IrOp::CALL;
	ir.a = function;
	ir.data = argumentInfo;
	ir.dest = remove->refersTo->irPointer;
	ir.opSize = TYPE_VOID_POINTER->size;
	ir.flags |= IR_C_CALL;

	Ir &sub = state->ir.add();
	sub.op = IrOp::ADD_CONSTANT;
	sub.dest = remove->refersTo->iteratorBlock.declarations[1]->registerOfStorage;
	sub.a = remove->refersTo->iteratorBlock.declarations[1]->registerOfStorage;
	sub.immediate = static_cast<u64>(-1LL);
	sub.opSize = 8;
}

void generateFor(IrState *state, ExprLoop *loop) {
	auto it = loop->iteratorBlock.declarations[0];
	auto it_index = loop->iteratorBlock.declarations[1];

	auto &enter = state->ir.add();
	enter.op = IrOp::BLOCK;
	enter.data = &loop->iteratorBlock;

	if (isStoredByPointer(getDeclarationType(it))) {
		it->physicalStorage = allocateStackSpace(state, getDeclarationType(it));
		it->registerOfStorage = loadStackAddress(state, it->physicalStorage);
	}
	else {
		it->registerOfStorage = allocateRegister(state);
	}

	assert(getDeclarationType(it_index) == &TYPE_U64);
	it_index->registerOfStorage = allocateRegister(state);

	u32 itReg = it->registerOfStorage;
	u32 it_indexReg = it_index->registerOfStorage;


	if ((loop->forBegin->type->flavor != TypeFlavor::INTEGER) && !(loop->flags & EXPR_FOR_BY_POINTER)) {
		loop->irPointer = allocateRegister(state);
	}
	else {
		loop->irPointer = itReg;
	}

	if (loop->forBegin->type->flavor == TypeFlavor::ARRAY || loop->forBegin->type == &TYPE_STRING) {
		auto begin = loop->forBegin;

		loop->arrayPointer = loadAddressOf(state, begin);

		if (loop->forBegin->type->flags & TYPE_ARRAY_IS_FIXED) {
			set(state, loop->irPointer, loop->arrayPointer, 8);
		}
		else {
			memop(state, IrOp::READ, loop->irPointer, loop->arrayPointer, 8);
		}
	}
	else {
		u32 reg = generateIr(state, loop->forBegin);
		set(state, loop->irPointer, reg, 8);
	}

	constant(state, it_indexReg, 8, 0);

	u32 irEnd;

	if (loop->forEnd) {
		irEnd = generateIr(state, loop->forEnd);
	}

	pushLoop(state, loop);
	addLineMarker(state, loop);

	u32 compareDest;

	compareDest = allocateRegister(state);

	if (loop->forBegin->type->flavor == TypeFlavor::ARRAY || loop->forBegin->type->flavor == TypeFlavor::STRING) {
		if (loop->forBegin->type->flags & TYPE_ARRAY_IS_FIXED) {
			constant(state, compareDest, 8, static_cast<TypeArray *>(loop->forBegin->type)->count);
		}
		else {
			memop(state, IrOp::READ, compareDest, loop->arrayPointer, 8, 8);
		}

		Ir &compare = state->ir.add();
		compare.op = IrOp::LESS;
		compare.a = it_index->registerOfStorage;
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

	u32 patch = state->ir.count; // Patch this manually so it doesn't skip the completed block if it exists

	Ir &branch = state->ir.add();
	branch.op = IrOp::IF_Z_GOTO;
	branch.a = compareDest;
	branch.opSize = 1;

	if (!(loop->flags & EXPR_FOR_BY_POINTER)) {
		if (loop->forBegin->type->flavor == TypeFlavor::ARRAY || loop->forBegin->type->flavor == TypeFlavor::POINTER || loop->forBegin->type == &TYPE_STRING) {
			copyOrRead(state, it->registerOfStorage, loop->irPointer, getDeclarationType(it));
		}
	}

	deferStack.add(loop);

	if (loop->body) {
		addLineMarker(state, loop->body);
		generateIr(state, loop->body);
	}

	exitBlock(state, &loop->iteratorBlock, false);

	Expr *inc = deferStack.pop();
	assert(inc == loop);

	Ir &jump = state->ir.add();
	jump.op = IrOp::GOTO;
	jump.b = loopStack[loopCount - 1].start;

	state->ir[patch].b = state->ir.count;


	auto &exit = state->ir.add();
	exit.op = IrOp::BLOCK;
	exit.data = nullptr;

	if (loop->completedBody) {
		addLineMarker(state, loop->completedBody);
		generateIr(state, loop->completedBody);
	}

	popLoop(state);
}

u32 generateCall(IrState *state, ExprFunctionCall *call, ExprCommaAssignment *comma = nullptr) {
	if ((call->flags & EXPR_FUNCTION_CALL_IS_STATEMENT_LEVEL) || comma) {
		addLineMarker(state, call);
	}

	auto type = static_cast<TypeFunction *>(call->function->type);

	if (call->function->flags & EXPR_FUNCTION_IS_INSTRINSIC) {
		if (call->function->valueOfDeclaration->name == "pop_count") {
			auto argumentType = type->argumentTypes[0];

			if (argumentType->flavor != TypeFlavor::INTEGER && argumentType->flavor != TypeFlavor::ENUM) {
				reportError(call->arguments.values[0], "Error: pop_count can only operate on integers or enums");
				return 0;
			}

			u32 argument = generateIr(state, call->arguments.values[0]);

			u32 result = allocateRegister(state);

			auto &ir = state->ir.add();
			ir.op = IrOp::POP_COUNT;
			ir.opSize = argumentType->size;
			ir.dest = result;
			ir.a = argument;
			
			return result;
		}
		else if (call->function->valueOfDeclaration->name == "bit_scan_forward" || call->function->valueOfDeclaration->name == "bit_scan_reverse") {
			auto argumentType = type->argumentTypes[0];

			if (argumentType->flavor != TypeFlavor::INTEGER && argumentType->flavor != TypeFlavor::ENUM) {
				reportError(call->arguments.values[0], "Error: bit_scan_forward can only operate on integers or enums");
				return 0;
			}

			u32 argument = generateIr(state, call->arguments.values[0]);

			u32 resultIndex = allocateRegister(state);
			u32 resultZero = allocateRegister(state);

			auto &ir = state->ir.add();
			if (call->function->valueOfDeclaration->name == "bit_scan_forward") {
				ir.op = IrOp::BIT_SCAN_FORWARD;
			}
			else {
				ir.op = IrOp::BIT_SCAN_REVERSE;
			}
			ir.opSize = argumentType->size;
			ir.dest = resultIndex;
			ir.b = resultZero;
			ir.a = argument;

			if (comma && comma->exprCount >= 2) {
				u32 zeroAddress = loadAddressOf(state, comma->left[1]);

				memop(state, IrOp::WRITE, zeroAddress, resultZero, TYPE_BOOL.size);
			}

			return resultIndex;
		}
		else {
			reportError(call, "Error: Call to unknown intrinsic function: %.*s", STRING_PRINTF(call->function->valueOfDeclaration->name));
			return 0;
		}
	}

	u32 extraParams = 0;
	bool bigReturn = !isStandardSize(type->returnTypes[0]->size);

	if (bigReturn) {
		extraParams++;
	}

	if (!(type->flags & TYPE_FUNCTION_IS_C_CALL)) {
		extraParams++;
	}

	u32 argCount = extraParams + call->arguments.count + type->returnCount - 1;
	FunctionCall *argumentInfo = static_cast<FunctionCall *>(state->allocator.allocate(sizeof(FunctionCall) + 
		sizeof(argumentInfo->args[0]) * argCount));
	argumentInfo->argCount = argCount;

	u32 contextArgumentIndex = bigReturn ? 1 : 0;

	argumentInfo->args[contextArgumentIndex].number = state->contextRegister;
	argumentInfo->args[contextArgumentIndex].type   = TYPE_VOID_POINTER;

	u32 unusedReturnSize = 0;
	u32 unusedReturnAlignment = 0;


	for (u32 i = comma ? comma->exprCount : 1; i < type->returnCount; i++) {
		unusedReturnSize = my_max(unusedReturnSize, type->returnTypes[i]->size);
		unusedReturnAlignment = my_max(unusedReturnAlignment, type->returnTypes[i]->alignment);
	}

	u32 unusedReturnReg;

	if (unusedReturnSize) {
		unusedReturnReg = allocateStackSpaceAndLoadAddress(state, unusedReturnSize, unusedReturnAlignment);
	}

	for (u32 i = 1; i < type->returnCount; i++) {
		if (comma && i < comma->exprCount) {
			if (comma->left[i]->flavor == ExprFlavor::IDENTIFIER) {
				auto identifier = static_cast<ExprIdentifier *>(comma->left[i]);

				if (!identifier->structAccess && !declarationIsStoredByPointer(identifier->declaration)) {
					u32 address = allocateStackSpaceAndLoadAddress(state, comma->left[i]->type);

					argumentInfo->args[call->arguments.count + extraParams + i - 1].number = address;
					argumentInfo->args[call->arguments.count + extraParams + i - 1].type = TYPE_VOID_POINTER;
					continue;
				}
			}
			u32 address = loadAddressOf(state, comma->left[i]);

			argumentInfo->args[call->arguments.count + extraParams + i - 1].number = address;
			argumentInfo->args[call->arguments.count + extraParams + i - 1].type = TYPE_VOID_POINTER;
		}
		else {
			argumentInfo->args[call->arguments.count + extraParams + i - 1].number = unusedReturnReg;
			argumentInfo->args[call->arguments.count + extraParams + i - 1].type = TYPE_VOID_POINTER;
		}
	}

	u32 function = generateIr(state, call->function);

	u32 result;

	if (bigReturn) {
		result = allocateStackSpaceAndLoadAddress(state, type->returnTypes[0]);
		argumentInfo->args[0].number = result;
		argumentInfo->args[0].type = TYPE_VOID_POINTER;
	}
	else {
		result = allocateRegister(state);
	}
	
	for (u32 i = 0; i < call->arguments.count; i++) {
		auto arg = call->arguments.values[i];
		u32 argument = generateIr(state, arg);

		
		if (!isStandardSize(arg->type->size)) {
			argumentInfo->args[i + extraParams].number = memop(state, IrOp::COPY, allocateStackSpaceAndLoadAddress(state, arg->type->size, 16), argument, arg->type->size);
			argumentInfo->args[i + extraParams].type = TYPE_VOID_POINTER;
		}
		else if (isStoredByPointer(call->arguments.values[i]->type)) {
			argumentInfo->args[i + extraParams].number = memop(state, IrOp::READ, allocateRegister(state), argument, arg->type->size);
			argumentInfo->args[i + extraParams].type = arg->type;
		}
		else {
			argumentInfo->args[i + extraParams].number = argument;
			argumentInfo->args[i + extraParams].type = arg->type;
		}
	}

	argumentInfo->returnType = bigReturn ? TYPE_VOID_POINTER : call->type;

	state->maxCallArguments = my_max(state->maxCallArguments, 4);
	state->maxCallArguments = my_max(state->maxCallArguments, argCount);

	
	Ir &ir = state->ir.add();
	ir.op = IrOp::CALL;
	ir.a = function;
	ir.data = argumentInfo;
	ir.dest = bigReturn ? 0 : result;
	ir.opSize = argumentInfo->returnType == &TYPE_VOID || bigReturn ? 0 : argumentInfo->returnType->size;

	if (call->function->type->flags & TYPE_FUNCTION_IS_C_CALL) {
		ir.flags |= IR_C_CALL;
	}

	if (!bigReturn && isStoredByPointer(type->returnTypes[0])) {
		result = memop(state, IrOp::WRITE, allocateStackSpaceAndLoadAddress(state, type->returnTypes[0]), result, type->returnTypes[0]->size);
	}

	if (comma) {
		for (u32 i = 1; i < comma->exprCount; i++) {
			if (comma->left[i]->flavor == ExprFlavor::IDENTIFIER) {
				auto identifier = static_cast<ExprIdentifier *>(comma->left[i]);

				if (!identifier->structAccess && !declarationIsStoredByPointer(identifier->declaration)) {
					u32 address = argumentInfo->args[call->arguments.count + extraParams + i - 1].number;

					memop(state, IrOp::READ, identifier->declaration->registerOfStorage, address, identifier->type->size);
				}
			}
		}
	}

	return result;
}

u32 generateArrayLiteral(IrState *state, ExprArrayLiteral *array) {
	auto arrayType = static_cast<TypeArray *>(array->type);
	u32 arrayCount = (arrayType->flags & TYPE_ARRAY_IS_FIXED) ? arrayType->count : array->count;

	u32 addressReg = allocateStackSpaceAndLoadAddress(state, arrayType->arrayOf->size * arrayCount, arrayType->arrayOf->alignment);

	for (u32 i = 0; i < arrayCount; i++) {
		if (i + 1 == array->count && arrayCount > array->count) {
			u32 countReg = constant(state, allocateRegister(state), 8, arrayCount - i);

			Ir &address = state->ir.add();
			address.op = IrOp::ADD_CONSTANT;
			address.dest = addressReg;
			address.a = addressReg;
			address.immediate = i * arrayType->arrayOf->size;
			address.opSize = 8;

			u32 value = generateIr(state, array->values[i]);

			u32 patch = state->ir.count;

			copyOrWrite(state, addressReg, value, arrayType->arrayOf);

			Ir &add = state->ir.add();
			add.op = IrOp::ADD_CONSTANT;
			add.dest = addressReg;
			add.a = addressReg;
			add.immediate = arrayType->arrayOf->size;
			add.opSize = 8;

			Ir &dec = state->ir.add();
			dec.op = IrOp::ADD_CONSTANT;
			dec.dest = countReg;
			dec.a = countReg;
			dec.immediate = static_cast<u64>(-1LL);
			dec.opSize = 8;

			Ir &branch = state->ir.add();
			branch.op = IrOp::IF_NZ_GOTO;
			branch.a = countReg;
			branch.b = patch;
			branch.opSize = 8;

			break;
		}
		else {
			u32 value = generateIr(state, array->values[i]);
			copyOrWrite(state, addressReg, value, arrayType->arrayOf, i * arrayType->arrayOf->size);
		}
	}

	return addressReg;
}

u32 generateIr(IrState *state, Expr *expr) {
	PROFILE_FUNC();

	switch (expr->flavor) {
	case ExprFlavor::SLICE: {
		return generateSlice(state, static_cast<ExprSlice *>(expr));
	}
	case ExprFlavor::BINARY_OPERATOR: {
		return generateBinary(state, static_cast<ExprBinaryOperator *>(expr));
	}
	case ExprFlavor::BLOCK: {
		generateBlock(state, static_cast<ExprBlock *>(expr));

		return 0;
	}
	case ExprFlavor::DEFER: {
		deferStack.add(expr);

		return 0;
	}
	case ExprFlavor::BREAK: {
		generateBreak(state, static_cast<ExprBreakOrContinue *>(expr));

		return 0;
	}
	case ExprFlavor::CONTINUE: {
		generateContinue(state, static_cast<ExprBreakOrContinue *>(expr));

		return 0;
	}
	case ExprFlavor::REMOVE: {
		generateRemove(state, static_cast<ExprBreakOrContinue *>(expr));		

		return 0;
	}
	case ExprFlavor::INT_LITERAL: {
		auto literal = static_cast<ExprLiteral *>(expr);

		if (literal->unsignedValue == 0 && isStoredByPointer(literal->type)) {
			return memop(state, IrOp::ZERO_MEMORY, allocateStackSpaceAndLoadAddress(state, literal->type), 0, literal->type->size);
		}

		assert(literal->type->size);

		return constant(state, allocateRegister(state), literal->type->size, literal->unsignedValue);
	}
	case ExprFlavor::FLOAT_LITERAL: {
		auto literal = static_cast<ExprLiteral *>(expr);

		assert(literal->type->size);

		u64 value = literal->unsignedValue;

		if (literal->type->size == 4) {
			*reinterpret_cast<float *>(&value) = static_cast<float>(literal->floatValue);

			value &= 0xFFFF'FFFFULL;
		}

		return constant(state, allocateRegister(state), literal->type->size, value);
	}
	case ExprFlavor::FOR: {
		generateFor(state, static_cast<ExprLoop *>(expr));

		return 0;
	}
	case ExprFlavor::FUNCTION: {
		u32 result = allocateRegister(state);

		Ir &address = state->ir.add();
		address.dest = result;
		address.opSize = 8;
		address.op = IrOp::FUNCTION;
		address.function = static_cast<ExprFunction *>(expr);

		return result;
	}
	case ExprFlavor::STRING_LITERAL: {
		u32 result = allocateStackSpaceAndLoadAddress(state, &TYPE_STRING);

		u32 temp = allocateRegister(state);

		Ir &address = state->ir.add();
		address.op = IrOp::STRING;
		address.dest = temp;
		address.data = static_cast<ExprStringLiteral *>(expr);
		
		memop(state, IrOp::WRITE, result, temp, 8);

		constant(state, temp, 8, static_cast<ExprStringLiteral *>(expr)->string.length);

		memop(state, IrOp::WRITE, result, temp, 8, 8);

		return result;
	}
	case ExprFlavor::FUNCTION_CALL: {
		auto call = static_cast<ExprFunctionCall *>(expr);

		return generateCall(state, call);
	}
	case ExprFlavor::IDENTIFIER: {
		auto identifier = static_cast<ExprIdentifier *>(expr);


		if (identifier->structAccess) {
			u32 address = loadAddressOf(state, expr);

			if (isStoredByPointer(identifier->type)) {
				return address;
			}
			else {
				return memop(state, IrOp::READ, allocateRegister(state), address, identifier->type->size);
			}
		}
		else {
			if (declarationIsStoredByPointer(identifier->declaration)) {
				u32 address = loadAddressOf(state, expr);

				if (isStoredByPointer(identifier->type)) {
					return address;
				}
				else {
					return memop(state, IrOp::READ, allocateRegister(state), address, identifier->type->size);
				}
			}
			else {
				return identifier->declaration->registerOfStorage;
			}
		}

		return 0;
	}
	case ExprFlavor::SWITCH: {
		auto switch_ = static_cast<ExprSwitch *>(expr);

		u32 value = generateIr(state, switch_->condition);

		ExprSwitch::Case *else_ = nullptr;

		for (auto &case_ : switch_->cases) {
			if (case_.condition) {
				auto condition = case_.condition;

				assert(condition->type == switch_->condition->type);

				u32 result = generateEquals(state, value, condition, true);

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

		u32 finalPatch = state->ir.count;

		Ir & final = state->ir.add();
		final.op = IrOp::GOTO;

		if (else_) {
			else_->irBranch = finalPatch;
		}

		for (auto &case_ : switch_->cases) {
			state->ir[case_.irBranch].b = state->ir.count;

			generateIr(state, case_.block);

			if (!case_.fallsThrough && &case_ + 1 != switch_->cases.end()) {
				case_.irSkip = state->ir.count;

				Ir &skip = state->ir.add();
				skip.op = IrOp::GOTO;
			}
		}

		if (!else_) {
			state->ir[finalPatch].b = state->ir.count;
		}

		for (u32 i = 0; i + 1 < switch_->cases.count; i++) {
			auto &case_ = switch_->cases[i];

			if (!case_.fallsThrough)
				state->ir[case_.irSkip].b = state->ir.count;
		}

		return 0;
	}
	case ExprFlavor::IF: {
		auto ifElse = static_cast<ExprIf *>(expr);

		u32 conditionReg = generateIr(state, ifElse->condition);

		if (ifElse->ifBody && ifElse->elseBody) {
			u32 patchIfZ = state->ir.count;
			Ir &ifZ = state->ir.add();
			ifZ.op = IrOp::IF_Z_GOTO;
			ifZ.a = conditionReg;
			ifZ.opSize = 1;

			addLineMarker(state, ifElse->ifBody);
			generateIr(state, ifElse->ifBody);

			u32 patchJump = state->ir.count;
			Ir &jump = state->ir.add();
			jump.op = IrOp::GOTO;

			state->ir[patchIfZ].b = state->ir.count;

			addLineMarker(state, ifElse->elseBody);
			generateIr(state, ifElse->elseBody);

			state->ir[patchJump].b = state->ir.count;
		}
		else if (ifElse->ifBody) {
			u32 patchIfZ = state->ir.count;
			Ir &ifZ = state->ir.add();
			ifZ.op = IrOp::IF_Z_GOTO;
			ifZ.a = conditionReg;
			ifZ.opSize = 1;

			addLineMarker(state, ifElse->ifBody);
			generateIr(state, ifElse->ifBody);

			state->ir[patchIfZ].b = state->ir.count;
		}
		else if (ifElse->elseBody) {
			u32 patchIfNZ = state->ir.count;
			Ir &ifNZ = state->ir.add();
			ifNZ.op = IrOp::IF_NZ_GOTO;
			ifNZ.a = conditionReg;
			ifNZ.opSize = 1;

			addLineMarker(state, ifElse->elseBody);
			generateIr(state, ifElse->elseBody);

			state->ir[patchIfNZ].b = state->ir.count;
		}

		return 0;
	}
	case ExprFlavor::COMMA_ASSIGNMENT: {
		auto comma = static_cast<ExprCommaAssignment *>(expr);

		if (comma->left[0]->flavor == ExprFlavor::IDENTIFIER) {
			auto identifier = static_cast<ExprIdentifier *>(comma->left[0]);

			if (!identifier->structAccess && !declarationIsStoredByPointer(identifier->declaration)) {
				u32 result = generateCall(state, static_cast<ExprFunctionCall *>(comma->call), comma);

				set(state, identifier->declaration->registerOfStorage, result, identifier->type->size);

				return 0;
			}
		}
		u32 address = loadAddressOf(state, comma->left[0]);

		u32 result = generateCall(state, static_cast<ExprFunctionCall *>(comma->call), comma);

		copyOrWrite(state, address, result, comma->left[0]->type);

		return 0;
	}
	case ExprFlavor::RETURN: {
		auto return_ = static_cast<ExprReturn *>(expr);
		
		u32 result = 0;

		if (return_->returns.count) {
			u32 result = generateIr(state, return_->returns.values[0]);

			u32 bigReturn = !isStandardSize(return_->returns.values[0]->type->size);

			if (bigReturn) {
				memop(state, IrOp::COPY, return_->returnsFrom->returns.declarations[0]->registerOfStorage, result, return_->returns.values[0]->type->size);
			}
			else if (isStoredByPointer(return_->returns.values[0]->type)) {
				result = memop(state, IrOp::READ, allocateRegister(state), result, return_->returns.values[0]->type->size);
			}

			// @Incomplete: Make the writes happen after exitBlock in case the return pointers alias
			for (u32 i = 1; i < return_->returns.count; i++) {
				u32 store = generateIr(state, return_->returns.values[i]);

				copyOrWrite(state, return_->returnsFrom->returns.declarations[i]->registerOfStorage, store, return_->returns.values[i]->type);
			}


			exitBlock(state, nullptr, true);

			Ir &ir = state->ir.add();
			ir.op = IrOp::RETURN;
			ir.a = bigReturn ? return_->returnsFrom->returns.declarations[0]->registerOfStorage : result;
			ir.opSize = bigReturn ? 8 : return_->returns.values[0]->type->size;

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


		return 0;
	}
	case ExprFlavor::STRUCT_LITERAL: {
		auto literal = static_cast<ExprStructLiteral *>(expr);
		if (structIsLiteral(literal)) {

			u32 result = allocateRegister(state);

			Ir &address = state->ir.add();
			address.op = IrOp::STRUCT_LITERAL;
			address.dest = result;
			address.data = literal;

			return result;
		}
		else {

			u32 address = allocateStackSpaceAndLoadAddress(state, literal->type);

			for (u32 i = 0; i < literal->initializers.count; i++) {
				auto offset = literal->initializers.declarations[i]->physicalStorage;
				auto value = literal->initializers.values[i];

				copyOrWrite(state, address, generateIr(state, value), value->type, offset);
			}

			return address;
		}
	}
	case ExprFlavor::IMPORT: {
		reportError(expr, "Error: Cannot operate on a module");

		return 0;
	}
	case ExprFlavor::TYPE_LITERAL: {
		auto type = static_cast<ExprLiteral *>(expr)->typeValue;

		if (type->flavor == TypeFlavor::MODULE) {
			reportError(expr, "Error: Cannot operate on a module");
			return 0;
		}

		u32 result = allocateRegister(state);

		Ir &ir = state->ir.add();
		ir.op = IrOp::TYPE;
		ir.dest = result;
		ir.data = type;

		return result;
	}
	case ExprFlavor::UNARY_OPERATOR: {
		ExprUnaryOperator *unary = static_cast<ExprUnaryOperator *>(expr);

		switch (unary->op) {
		case TOKEN('*'): {
			return loadAddressOf(state, unary->value);
		}
		case TOKEN('-'): {
			u32 result = allocateRegister(state);

			u32 toNegate = generateIr(state, unary->value);

			Ir &negate = state->ir.add();
			negate.op = IrOp::NEG;
			negate.a = toNegate;
			negate.opSize = unary->value->type->size;
			negate.dest = result;

			if (unary->type->flavor == TypeFlavor::FLOAT) {
				negate.flags |= IR_FLOAT_OP;
			}

			return result;
		}
		case TOKEN('~'): {
			u32 result = allocateRegister(state);

			u32 toInvert = generateIr(state, unary->value);

			Ir &invert = state->ir.add();
			invert.op = IrOp::NOT;
			invert.a = toInvert;
			invert.opSize = unary->value->type->size;
			invert.dest = result;

			return result;
		}
		case TOKEN('!'): {
			u32 result = allocateRegister(state);

			u32 zero = constant(state, allocateRegister(state), unary->value->type->size, 0);

			u32 toInvert = generateIr(state, unary->value);

			Ir &invert = state->ir.add();
			invert.op = IrOp::EQUAL;
			invert.a = toInvert;
			invert.b = zero;
			invert.opSize = unary->value->type->size;
			invert.dest = result;

			return result;
		}
		case TokenT::SHIFT_LEFT: {
			u32 addressReg = generateIr(state, unary->value);

			if (isStoredByPointer(unary->type)) {
				return addressReg;
			}
			else {
				return memop(state, IrOp::READ, allocateRegister(state), addressReg, unary->type->size);
			}
		}
		case TokenT::TYPE_INFO: {
			u32 value = generateIr(state, unary->value);

			u32 result = allocateRegister(state);

			Ir &ir = state->ir.add();
			ir.op = IrOp::TYPE_INFO;
			ir.a = value;
			ir.dest = result;

			return result;
		}
		default:
			assert(false);
			return 0;
		}
	}
	case ExprFlavor::WHILE: {
		ExprLoop *loop = static_cast<ExprLoop *>(expr);

		pushLoop(state, loop);

		u32 conditionReg = generateIr(state, loop->whileCondition);

		u32 patch = state->ir.count; // Patch this manually so it doesn't skip the completed block if it exists

		Ir &ifZ = state->ir.add();
		ifZ.op = IrOp::IF_Z_GOTO;
		ifZ.a = conditionReg;
		ifZ.opSize = 1;

		if (loop->body) {
			addLineMarker(state, loop->body);
			generateIr(state, loop->body);
		}

		Ir &jump = state->ir.add();
		jump.op = IrOp::GOTO;
		jump.b = loopStack[loopCount - 1].start;

		state->ir[patch].b = state->ir.count;

		if (loop->completedBody) {
			addLineMarker(state, loop->completedBody);
			generateIr(state, loop->completedBody);
		}

		popLoop(state);

		return 0;
	}
	case ExprFlavor::ARRAY_LITERAL: {
		auto array = static_cast<ExprArrayLiteral *>(expr);

		if (arrayIsLiteral(array)) {

			u32 result = allocateRegister(state);

			Ir &address = state->ir.add();
			address.op = IrOp::ARRAY_LITERAL;
			address.dest = result;
			address.data = array;

			return result;
		}
		else {
			u32 data = generateArrayLiteral(state, array);

			if (array->type->flags & TYPE_ARRAY_IS_FIXED) {
				return data;
			}
			else {				
				u32 value = allocateStackSpaceAndLoadAddress(state, array->type);
				memop(state, IrOp::WRITE, value, data, 8);

				u32 count = constant(state, allocateRegister(state), 8, array->count);
				memop(state, IrOp::WRITE, value, count, 8, 8);

				return value;
			}
		}
	}
	case ExprFlavor::CONTEXT: {
		return state->contextRegister;
	}
	case ExprFlavor::PUSH_CONTEXT: {
		auto pushContext = static_cast<ExprBinaryOperator *>(expr);

		auto oldContext = state->contextRegister;

		state->contextRegister = generateIr(state, pushContext->left);

		addLineMarker(state, pushContext->right);
		generateIr(state, pushContext->right);

		state->contextRegister = oldContext;
		return 0;
	}
	case ExprFlavor::RUN: // Statement level runs are not removed from the ast but shouldn't generate code
	case ExprFlavor::STATIC_IF: {
		return 0; // In the event that the static if returns false and there is no else block, we just leave the static if expression in the tree, 
			   // so when we see a static if here we should just generate no code
	}
	default:
		assert(false);
		return 0;
	}
}

void generateCCallPreamble(ExprFunction *function) {
	for (auto argument : function->arguments.declarations) {
		auto type = getDeclarationType(argument);
		if (isStandardSize(type->size) && isStoredByPointer(type)) {
			argument->physicalStorage = allocateStackSpace(&function->state, type);

			argument->registerOfStorage = memop(&function->state, IrOp::WRITE, loadStackAddress(&function->state, argument->physicalStorage), argument->registerOfStorage, type->size);
		}
	}
}

bool generateIrForFunction(ExprFunction *function) {
	function->state.nextRegister = 0;

	if (!isStandardSize(getDeclarationType(function->returns.declarations[0])->size)) {
		function->returns.declarations[0]->registerOfStorage = function->state.nextRegister++;
	}

	if (!(function->flags & EXPR_FUNCTION_IS_C_CALL)) {
		function->state.contextRegister = function->state.nextRegister++;
	}


	for (u32 i = 0; i < function->arguments.declarations.count; i++) {
		auto declaration = function->arguments.declarations[i];
		auto type = getDeclarationType(declaration);

		declaration->registerOfStorage = function->state.nextRegister++;
	}

	for (u32 i = 1; i < function->returns.declarations.count; i++) {
		auto declaration = function->returns.declarations[i];

		declaration->registerOfStorage = function->state.nextRegister++;
	}

	function->state.parameters = function->state.nextRegister;

	generateCCallPreamble(function);
	generateIr(&function->state, function->body);

	if (hadError) {
		return false;
	}

	irInstructions += function->state.ir.count;

	function->flags |= EXPR_FUNCTION_RUN_READY;
	_ReadWriteBarrier();
	inferQueue.add(InferQueueJob(function, nullptr));

	CoffJob job;
	job.function = function;
	job.flavor = CoffJobFlavor::FUNCTION;

	if (!(function->returns.declarations[0]->flags & DECLARATION_IS_RUN_RETURN)) {
		coffWriterQueue.add(job);
	}

	return true;
}

void runIrGenerator() {
	PROFILE_FUNC();
	while (true) {

		ExprFunction *function = irGeneratorQueue.take();

		if (!function)
			break;

		if (!generateIrForFunction(function))
			break;
	}

	coffWriterQueue.add({ nullptr, CoffJobFlavor::FUNCTION });
}