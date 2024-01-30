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

#define INVALID_REGISTER UINT32_MAX

u32 allocateRegister(ExprFunction *function) {
	return function->state.nextRegister++;
}

u32 allocateStackSpace(ExprFunction *function, u32 size, u32 alignment) {
	function->state.stackSpace = AlignPO2(function->state.stackSpace, alignment);
	u32 offset = function->state.stackSpace;
	function->state.stackSpace += size;
	function->state.stackSpace = AlignPO2(function->state.stackSpace, 8);
	return offset;
}

u32 allocateStackSpace(ExprFunction *function, Type *type) {
	return allocateStackSpace(function, type->size, type->alignment);
}

u32 loadStackAddress(ExprFunction *function, u32 offset) {
	Ir &address = function->state.ir.add();
	address.op = IrOp::STACK_ADDRESS;
	address.dest = allocateRegister(function);
	address.immediate = offset;

	return address.dest;
}

u32 allocateStackSpaceAndLoadAddress(ExprFunction *function, u32 size, u32 alignment) {
	return loadStackAddress(function, allocateStackSpace(function, size, alignment));
}

u32 allocateStackSpaceAndLoadAddress(ExprFunction *function, Type *type) {
	return allocateStackSpaceAndLoadAddress(function, type->size, type->alignment);
}

u32 memop(ExprFunction *function, IrOp op, u32 dest, u32 src, u32 size, u32 offset = 0) {
	Ir &memop = function->state.ir.add();
	memop.op = op;
	memop.dest = dest;
	memop.a = src;
	memop.opSize = size;
	memop.immediate = offset;

	return dest;
}

u32 copyOrWrite(ExprFunction *function, u32 dest, u32 src, Type *type, u32 offset = 0) {
	if (isStoredByPointer(type)) {
		if (offset) {
			Ir &add = function->state.ir.add();
			add.op = IrOp::ADD_CONSTANT;
			add.dest = allocateRegister(function);
			add.a = dest;
			add.immediate = offset;
			add.opSize = 8;

			dest = add.dest;
		}
		

		return memop(function, IrOp::COPY_SRC_OFFSET, dest, src, type->size);
	}
	else {
		return memop(function, IrOp::WRITE, dest, src, type->size, offset);
	}
}


u32 copyOrRead(ExprFunction *function, u32 dest, u32 src, Type *type, u32 offset = 0) {
	if (isStoredByPointer(type)) {
		if (offset) {
			Ir &add = function->state.ir.add();
			add.op = IrOp::ADD_CONSTANT;
			add.dest = allocateRegister(function);
			add.a = src;
			add.immediate = offset;
			add.opSize = 8;

			src = add.dest;
		}

		return memop(function, IrOp::COPY_SRC_OFFSET, dest, src, type->size);
	}
	else {
		return memop(function, IrOp::READ, dest, src, type->size, offset);
	}
}

u32 set(ExprFunction *function, u32 dest, u32 src, u32 size) {
	Ir &set = function->state.ir.add();
	set.op = IrOp::SET;
	set.dest = dest;
	set.a = src;
	set.opSize = size;

	return dest;
}

u32 constant(ExprFunction *function, u32 dest, u32 size, u64 value) {
	Ir &constant = function->state.ir.add();
	constant.op = IrOp::IMMEDIATE;
	constant.dest = dest;
	constant.opSize = size;
	constant.immediate = value;

	return dest;
}

u32 generateIr(ExprFunction *function, Expr *expr);

struct Loop {
	struct ExprLoop *loop;
	u32 start;

	Array<u32> endPatches;
};

static Array<Expr *> deferStack;

static Array<Loop> loopStack;
static u32 loopCount;

static void pushLoop(ExprFunction *function, ExprLoop *loop) {
	if (loopCount >= loopStack.count) {
		loopStack.add();
	}

	loopStack[loopCount].start = function->state.ir.count;
	loopStack[loopCount].loop = loop;
	loopStack[loopCount].endPatches.clear();

	++loopCount;


}

static void popLoop(ExprFunction *function) {
	--loopCount;

	Loop loop = loopStack[loopCount];

	for (auto patch : loop.endPatches) {
		function->state.ir[patch].b = function->state.ir.count;
	}
}



static void generateIncrement(ExprFunction *function, ExprLoop *loop) {
	auto it_index = loop->iteratorBlock.declarations[1];

	Ir &index = function->state.ir.add();
	index.op = IrOp::ADD_CONSTANT;
	index.opSize = 8;
	index.a = it_index->registerOfStorage;
	index.immediate = 1;
	index.dest = it_index->registerOfStorage;

	Ir &increment = function->state.ir.add();
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

void addLineMarker(ExprFunction *function, Expr *expr) {
	if (expr->flavor == ExprFlavor::BLOCK)
		return;

	Ir &ir = function->state.ir.add();

	ir.op = IrOp::LINE_MARKER;
	ir.location.start = expr->start;
	ir.location.end = expr->end;
}


static void exitBlock(ExprFunction *function, Block *block, bool isBreak) {
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
				generateIncrement(function, loop);
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

			addLineMarker(function, defer);
			generateIr(function, defer->expr);
		}
	}
}

u32 generateSlice(ExprFunction *function, ExprSlice *slice) {
	u32 array = generateIr(function, slice->array);
	u32 pointer = array;

	if (slice->array->type->flavor == TypeFlavor::ARRAY || slice->array->type->flavor == TypeFlavor::STRING) {
		if (!(slice->array->type->flags & TYPE_ARRAY_IS_FIXED)) {
			pointer = memop(function, IrOp::READ, allocateRegister(function), array, 8);
		}
	}


	u32 countReg = slice->sliceStart ? allocateRegister(function) : 0;

	if (!slice->sliceEnd) {
		if (slice->array->type->flags & TYPE_ARRAY_IS_FIXED) {
			constant(function, countReg, 8, static_cast<TypeArray *>(slice->array->type)->count);
		}
		else {
			memop(function, IrOp::READ, countReg, array, 8, 8);
		}
	}

	u32 elementSize = 0;

	if (slice->array->type->flavor == TypeFlavor::POINTER) {
		elementSize = static_cast<TypePointer *>(slice->array->type)->pointerTo->size;
	}
	else if (slice->type->flavor == TypeFlavor::STRING) {
		elementSize = 1;
	}
	else if (slice->type->flavor == TypeFlavor::ARRAY) {
		elementSize = static_cast<TypeArray *>(slice->array->type)->arrayOf->size;
	}


	u32 result = allocateStackSpaceAndLoadAddress(function, slice->type);

	if (slice->sliceEnd && slice->sliceStart) {
		auto offset = generateIr(function, slice->sliceStart);
		auto end = generateIr(function, slice->sliceEnd);

		auto &count = function->state.ir.add();
		count.op = IrOp::SUB;
		count.dest = countReg;
		count.a = end;
		count.b = offset;
		count.opSize = 8;

		memop(function, IrOp::WRITE, result, countReg, 8, 8);

		if (elementSize != 1) {
			u32 mulReg = allocateRegister(function);

			auto &mul = function->state.ir.add();
			mul.op = IrOp::MUL_BY_CONSTANT;
			mul.dest = mulReg;
			mul.a = offset;
			mul.immediate = elementSize;
			mul.opSize = 8;

			offset = mulReg;
		}

		u32 dataReg = countReg;

		auto &add = function->state.ir.add();
		add.op = IrOp::ADD;
		add.dest = dataReg;
		add.a = pointer;
		add.b = offset;
		add.opSize = 8;

		memop(function, IrOp::WRITE, result, dataReg, 8);
	}
	else if (slice->sliceStart) {
		auto offset = generateIr(function, slice->sliceStart);

		auto &count = function->state.ir.add();
		count.op = IrOp::SUB;
		count.dest = countReg;
		count.a = countReg;
		count.b = offset;
		count.opSize = 8;

		memop(function, IrOp::WRITE, result, countReg, 8, 8);

		if (elementSize != 1) {
			u32 mulReg = allocateRegister(function);

			auto &mul = function->state.ir.add();
			mul.op = IrOp::MUL_BY_CONSTANT;
			mul.dest = mulReg;
			mul.a = offset;
			mul.immediate = elementSize;
			mul.opSize = 8;

			offset = mulReg;
		}

		u32 dataReg = countReg;

		auto &add = function->state.ir.add();
		add.op = IrOp::ADD;
		add.dest = dataReg;
		add.a = pointer;
		add.b = offset;
		add.opSize = 8;

		memop(function, IrOp::WRITE, result, dataReg, 8);
	}
	else {
		assert(slice->sliceEnd);
		auto end = generateIr(function, slice->sliceEnd);

		memop(function, IrOp::WRITE, result, end, 8, 8);
		memop(function, IrOp::WRITE, result, pointer, 8);
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

u32 loadAddressOf(ExprFunction *function, Expr *expr, u32 offset = 0) {
	if (expr->flavor == ExprFlavor::BINARY_OPERATOR && static_cast<ExprBinaryOperator *>(expr)->op == TOKEN('[')) {
		auto binary = static_cast<ExprBinaryOperator *>(expr);

		u32 array = generateIr(function, binary->left);
		u32 pointer = array;

		if (binary->left->type->flavor == TypeFlavor::ARRAY || binary->left->type->flavor == TypeFlavor::STRING) {
			if (!(binary->left->type->flags & TYPE_ARRAY_IS_FIXED)) {
				pointer = memop(function, IrOp::READ, allocateRegister(function), array, 8);
			}
		}

		u32 elementSize = 0;

		if (binary->left->type->flavor == TypeFlavor::POINTER) {
			elementSize = static_cast<TypePointer *>(binary->left->type)->pointerTo->size;
		}
		else if (binary->left->type->flavor == TypeFlavor::STRING) {
			elementSize = 1;
		}
		else if (binary->left->type->flavor == TypeFlavor::ARRAY) {
			elementSize = static_cast<TypeArray *>(binary->left->type)->arrayOf->size;
		}

		auto index = generateIr(function, binary->right);

		if (elementSize != 1) {
			u32 mulReg = allocateRegister(function);

			auto &mul = function->state.ir.add();
			mul.op = IrOp::MUL_BY_CONSTANT;
			mul.dest = mulReg;
			mul.a = index;
			mul.immediate = elementSize;
			mul.opSize = 8;

			index = mulReg;
		}

		u32 result = allocateRegister(function);

		auto &add = function->state.ir.add();
		add.op = IrOp::ADD;
		add.dest = result;
		add.a = pointer;
		add.b = index;
		add.opSize = 8;

		if (offset) {
			auto &add = function->state.ir.add();
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

		u32 address = generateIr(function, unary->value);

		if (offset) {
			u32 result = allocateRegister(function);

			Ir &add = function->state.ir.add();
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
				u32 address = generateIr(function, identifier->structAccess);

				if (offset) {
					u32 result = allocateRegister(function);

					Ir &add = function->state.ir.add();
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
				return loadAddressOf(function, identifier->structAccess, offset);
			}
		}
		else {
			if (identifier->declaration->enclosingScope->flavor == BlockFlavor::GLOBAL) {
				u32 result = allocateRegister(function);

				Ir &address = function->state.ir.add();
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
					u32 result = allocateRegister(function);
					Ir &add = function->state.ir.add();
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
		u32 address = generateIr(function, expr);

		if (offset) {
			u32 result = allocateRegister(function);

			Ir &add = function->state.ir.add();
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

u32 generateCast(ExprFunction *function, ExprBinaryOperator *binary) {
	auto left = binary->left;
	auto right = binary->right;

	(void)left;
	assert(left->flavor == ExprFlavor::TYPE_LITERAL);

	u32 rightReg = generateIr(function, right);

	Type *castTo = binary->type;

	if (castTo == right->type || (binary->flags & EXPR_CAST_IS_BITWISE)) {
		if (isStoredByPointer(castTo) && !isStoredByPointer(right->type)) {
			assert(castTo->size == right->type->size);

			return memop(function, IrOp::WRITE, allocateStackSpaceAndLoadAddress(function, castTo), rightReg, castTo->size);
		}
		else if (!isStoredByPointer(castTo) && isStoredByPointer(right->type)) {
			assert(castTo->size == right->type->size);

			return memop(function, IrOp::READ, allocateRegister(function), rightReg, castTo->size);
		}

		return rightReg;
	}

	if (right->type == TYPE_ANY) {
		u32 pointerReg = memop(function, IrOp::READ, allocateRegister(function), rightReg, 8);

		if (isStoredByPointer(castTo)) {
			return memop(function, IrOp::COPY_SRC_OFFSET, allocateStackSpaceAndLoadAddress(function, castTo), pointerReg, castTo->size);
		}
		else {
			return memop(function, IrOp::READ, pointerReg, pointerReg, castTo->size);
		}
	}

	switch (castTo->flavor) {
	case TypeFlavor::STRUCT: {
		assert(castTo == TYPE_ANY);

		u32 result = allocateStackSpaceAndLoadAddress(function, TYPE_ANY);

		u32 storage = allocateStackSpaceAndLoadAddress(function, right->type);
		memop(function, IrOp::WRITE, result, storage, 8);
		
		copyOrWrite(function, storage, rightReg, right->type);

		u32 typeReg = allocateRegister(function);
		Ir &type = function->state.ir.add();
		type.op = IrOp::TYPE;
		type.dest = typeReg;
		type.data = right->type;


		Ir &ir = function->state.ir.add();
		ir.op = IrOp::TYPE_INFO;
		ir.a = typeReg;
		ir.dest = typeReg;

		memop(function, IrOp::WRITE, result, typeReg, 8, 8);
		return result;
	}
	case TypeFlavor::BOOL: {
		u32 src;
		u32 size;

		if (right->type->flavor == TypeFlavor::ARRAY || right->type == &TYPE_STRING) {
			if (right->type->flags & TYPE_ARRAY_IS_FIXED) {
				return constant(function, allocateRegister(function), 1, 1);
			}
			else {
				src = memop(function, IrOp::READ, allocateRegister(function), rightReg, 8, 8);
				size = 8;
			}
		}
		else {
			src = rightReg;
			size = right->type->size;
		}

		u32 zero = constant(function, allocateRegister(function), size, 0);

		u32 result = allocateRegister(function);

		Ir &ir = function->state.ir.add();
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
		u32 result = allocateRegister(function);

		Ir &conversion = function->state.ir.add();
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
			u32 result = allocateRegister(function);

			Ir &conversion = function->state.ir.add();
			conversion.op = IrOp::FLOAT_TO_INT;
			conversion.dest = result;
			conversion.a = rightReg;
			conversion.b = castTo->size;
			conversion.opSize = right->type->size;

			if (castTo->flags & TYPE_INTEGER_IS_SIGNED) {
				conversion.flags |= IR_SIGNED_OP;
			}

			return result;
		}
		else {
			if (castTo->size <= right->type->size) {
				return rightReg;
			}

			u32 result = allocateRegister(function);

			Ir &conversion = function->state.ir.add();
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

			u32 result = allocateStackSpaceAndLoadAddress(function, castTo);

			memop(function, IrOp::WRITE, result, rightReg, 8);
			u32 count = constant(function, allocateRegister(function), 8, static_cast<TypeArray *>(right->type)->count);
			memop(function, IrOp::WRITE, result, count, 8, 8);

			return result;
		}
	}
	case TypeFlavor::POINTER:
	case TypeFlavor::FUNCTION:
		return rightReg;
	case TypeFlavor::STRING: // []u8 -> string
		assert(right->type->flavor == TypeFlavor::ARRAY);
		assert(static_cast<TypeArray *>(right->type)->arrayOf == &TYPE_U8);
		if (right->type->flags & TYPE_ARRAY_IS_FIXED) {
			u32 result = allocateStackSpaceAndLoadAddress(function, castTo);

			memop(function, IrOp::WRITE, result, rightReg, 8);
			u32 count = constant(function, allocateRegister(function), 8, static_cast<TypeArray *>(right->type)->count);
			memop(function, IrOp::WRITE, result, count, 8, 8);

			return result;
		}
		else {
			return rightReg;
		}
	case TypeFlavor::TYPE:
	case TypeFlavor::VOID:
		assert(false);
		return 0;
	}

	assert(false);
	return 0;
}

u32 generateMathBinaryOp(ExprFunction *function, ExprBinaryOperator *binary) {
	u32 leftReg = generateIr(function, binary->left);
	u32 rightReg = generateIr(function, binary->right);

	u32 result = allocateRegister(function);

	Ir &ir = function->state.ir.add();
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

RMWInfo readForRMW(ExprFunction *function, Expr *expr) {
	RMWInfo result;

	if (expr->flavor == ExprFlavor::IDENTIFIER) {
		auto identifier = static_cast<ExprIdentifier *>(expr);

		if (!identifier->structAccess && !declarationIsStoredByPointer(identifier->declaration)) {
			result.value = identifier->declaration->registerOfStorage;
			return result;
		}
	}

	assert(!isStoredByPointer(expr->type));
	result.address = loadAddressOf(function, expr);
	result.value = memop(function, IrOp::READ, allocateRegister(function), result.address, expr->type->size);

	return result;
}

void writeForRMW(ExprFunction *function, Expr *expr, RMWInfo registers) {
	if (expr->flavor == ExprFlavor::IDENTIFIER) {
		auto identifier = static_cast<ExprIdentifier *>(expr);

		if (!identifier->structAccess && !declarationIsStoredByPointer(identifier->declaration)) {
			// No op, result has already been written here
			return;
		}
	}

	assert(!isStoredByPointer(expr->type));
	memop(function, IrOp::WRITE, registers.address, registers.value, expr->type->size);
}

void generateAssignBinaryOp(ExprFunction *function, ExprBinaryOperator *binary) {
	RMWInfo registers = readForRMW(function, binary->left);

	u32 rightReg = generateIr(function, binary->right);

	Ir &ir = function->state.ir.add();
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

	writeForRMW(function, binary->left, registers);
}

u32 generateEquals(ExprFunction *function, u32 leftReg, Expr *right, bool equals) {
	u32 result = allocateRegister(function);

	u32 rightReg = generateIr(function, right);

	if (right->type == &TYPE_STRING) {
		if (!stringsEqualFunction || !stringsEqualFunction->type) {
			reportError("Internal Compiler Error: Comparing strings before __strings_equal is declared");
			assert(false);
			exit(1); // @Cleanup Forceful exit since we don't have good error handling here and its an internal compiler error
		}

		u32 functionRegister = generateIr(function, stringsEqualFunction);

		u32 returnValue = allocateStackSpaceAndLoadAddress(function, 1, 1);
		FunctionCall *arguments = static_cast<FunctionCall *>(function->state.allocator.allocate(sizeof(FunctionCall) + 2 * sizeof(u32)));
		arguments->function = static_cast<TypeFunction *>(stringsEqualFunction->type);
		arguments->arguments[0] = leftReg;
		arguments->arguments[1] = rightReg;

		Ir &call = function->state.ir.add();
		call.op = IrOp::CALL;
		call.dest = returnValue;
		call.a = functionRegister;
		call.data = arguments;
		call.opSize = 2;

		memop(function, IrOp::READ, result, returnValue, 1);

		if (!equals) {
			Ir &invert = function->state.ir.add();

			invert.op = IrOp::EQUAL;
			invert.dest = result;
			invert.a = result;
			invert.b = 0;
			invert.opSize = 1;
		}
	}
	else {
		Ir &ir = function->state.ir.add();

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

u32 generateBinary(ExprFunction *function, ExprBinaryOperator *binary) {
	auto left = binary->left;
	auto right = binary->right;

	switch (binary->op) {
	case TokenT::CAST: {
		return generateCast(function, binary);
	}
	case TOKEN('['): {
		u32 address = loadAddressOf(function, binary);

		if (isStoredByPointer(binary->type)) {
			return address;
		}
		else {
			return memop(function, IrOp::READ, allocateRegister(function), address, binary->type->size);
		}
	}
	case TokenT::EQUAL:
	case TokenT::NOT_EQUAL: {
		assert(left->type == right->type);

		return generateEquals(function, generateIr(function, binary->left), binary->right, binary->op == TokenT::EQUAL);
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

		return generateMathBinaryOp(function, binary);
	}
	case TOKEN('+'):
	case TOKEN('-'): {
		if (left->type->flavor == TypeFlavor::POINTER) {
			auto pointer = static_cast<TypePointer *>(left->type);

			u32 result = allocateRegister(function);

			u32 leftReg = generateIr(function, left);


			u32 rightReg = generateIr(function, right);

			if (right->type->flavor == TypeFlavor::POINTER) {
				assert(binary->op == TOKEN('-'));
				Ir &sub = function->state.ir.add();
				sub.op = IrOp::SUB;
				sub.dest = result;
				sub.a = leftReg;
				sub.b = rightReg;
				sub.opSize = 8;

				if (pointer->pointerTo->size) {
					Ir &div = function->state.ir.add();
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
					u32 mulReg = allocateRegister(function);

					Ir &mul = function->state.ir.add();

					mul.op = IrOp::MUL_BY_CONSTANT;
					mul.dest = mulReg;
					mul.a = rightReg;
					mul.immediate = pointer->pointerTo->size;
					mul.opSize = 8;

					offset = mulReg;
				}

				Ir &add = function->state.ir.add();
				add.op = binary->op == TOKEN('+') ? IrOp::ADD : IrOp::SUB;
				add.dest = result;
				add.a = leftReg;
				add.b = offset;
				add.opSize = 8;
			}

			return result;
		}
		else {
			return generateMathBinaryOp(function, binary);
		}
	}
	case TOKEN('='): {
		if (left->flavor == ExprFlavor::IDENTIFIER) {
			auto identifier = static_cast<ExprIdentifier *>(left);

			if (!identifier->structAccess && !declarationIsStoredByPointer(identifier->declaration)) {
				u32 value = generateIr(function, right);
				set(function, identifier->declaration->registerOfStorage, value, identifier->type->size);
				
				return 0;
			}
		}

		u32 address = loadAddressOf(function, left);
		copyOrWrite(function, address, generateIr(function, right), left->type);
		return 0;
	}
	case TokenT::PLUS_EQUALS: {
		if (left->type->flavor == TypeFlavor::POINTER) {
			auto pointer = static_cast<TypePointer *>(left->type);

			RMWInfo registers = readForRMW(function, left);
			u32 offset = generateIr(function, right);

			if (pointer->pointerTo->size != 1) {
				u32 mulReg = allocateRegister(function);

				Ir &mul = function->state.ir.add();

				mul.op = IrOp::MUL_BY_CONSTANT;
				mul.dest = mulReg;
				mul.a = offset;
				mul.immediate = pointer->pointerTo->size;
				mul.opSize = 8;

				offset = mulReg;
			}

			Ir &add = function->state.ir.add();
			add.op = IrOp::ADD;
			add.dest = registers.value;
			add.a = registers.value;
			add.b = offset;
			add.opSize = 8;

			writeForRMW(function, left, registers);
			return 0;
		}
		else {
			generateAssignBinaryOp(function, binary);
			return 0;
		}
	}
	case TokenT::LOGIC_AND:
	case TokenT::LOGIC_OR: {
		u32 result = allocateRegister(function);


		u32 leftReg = generateIr(function, left);
		set(function, result, leftReg, 1);

		u32 patch = function->state.ir.count;

		Ir &branch = function->state.ir.add();
		branch.op = binary->op == TokenT::LOGIC_AND ? IrOp::IF_Z_GOTO : IrOp::IF_NZ_GOTO;
		branch.a = result;
		branch.opSize = 1;

		//addLineMarker(function, right);
		u32 rightReg = generateIr(function, right);
		set(function, result, rightReg, 1);

		function->state.ir[patch].b = function->state.ir.count;

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
		generateAssignBinaryOp(function, binary);
		return 0;
	}
	default:
		assert(false);
		return 0;
	}
}

void generateBlock(ExprFunction *function, ExprBlock *block) {
	auto &enter = function->state.ir.add();
	enter.op = IrOp::BLOCK;
	enter.data = &block->declarations;

	for (auto declaration : block->declarations.declarations) {
		if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING))) {
			if (declarationIsStoredByPointer(declaration)) {
				declaration->physicalStorage = allocateStackSpace(function, getDeclarationType(declaration));
				declaration->registerOfStorage = loadStackAddress(function, declaration->physicalStorage);
			}
			else {
				declaration->registerOfStorage = allocateRegister(function);
			}
		}
	}

	for (auto subExpr : block->exprs) {
		addLineMarker(function, subExpr);
		generateIr(function, subExpr);
	}

	exitBlock(function, &block->declarations, false);

	while (deferStack.count) {
		auto expr = deferStack[deferStack.count - 1];

		if (expr->flavor != ExprFlavor::DEFER)
			break;

		auto defer = static_cast<ExprDefer *>(expr);

		if (defer->enclosingScope != &block->declarations)
			break;

		deferStack.pop();
	}

	auto &exit = function->state.ir.add();
	exit.op = IrOp::BLOCK;
	exit.data = nullptr;
}

void generateBreak(ExprFunction *function, ExprBreakOrContinue *break_) {
	for (u32 i = loopCount; i-- != 0;) {

		if (loopStack[i].loop == break_->refersTo) {
			exitBlock(function, &loopStack[i].loop->iteratorBlock, true);
			loopStack[i].endPatches.add(function->state.ir.count);
			break;
		}
	}

	Ir &jump = function->state.ir.add();
	jump.op = IrOp::GOTO;
}

void generateContinue(ExprFunction *function, ExprBreakOrContinue *continue_) {
	u32 begin = 0;

	for (u32 i = loopCount; i-- != 0;) {
		if (loopStack[i].loop == continue_->refersTo) {
			exitBlock(function, &loopStack[i].loop->iteratorBlock, false);
			begin = loopStack[i].start;
			break;
		}
	}

	Ir &jump = function->state.ir.add();
	jump.op = IrOp::GOTO;
	jump.b = begin;
}

void generateRemove(ExprFunction *function, ExprBreakOrContinue *remove) {
	if (!removeFunction || !removeFunction->type) {
		reportError(remove, "Internal Compiler Error: Removing something before __remove is declared");
		assert(false);
		exit(1); // @Cleanup Forceful exit since we don't have good error handling here and its an internal compiler error
	}

	assert(remove->refersTo->forBegin->type->flavor == TypeFlavor::ARRAY);
	assert(!(remove->refersTo->forBegin->type->flags & TYPE_ARRAY_IS_FIXED));

	u32 returnValue = allocateStackSpaceAndLoadAddress(function, 8, 8);
	u32 functionRegister = generateIr(function, removeFunction);

	FunctionCall *argumentInfo = static_cast<FunctionCall *>(function->state.allocator.allocate(sizeof(FunctionCall) + sizeof(u32) * 3));

	u32 sizeReg = constant(function, allocateRegister(function), 8, static_cast<TypeArray *>(remove->refersTo->forBegin->type)->arrayOf->size);

	argumentInfo->arguments[0] = remove->refersTo->arrayPointer;
	argumentInfo->arguments[1] = remove->refersTo->irPointer;
	argumentInfo->arguments[2] = sizeReg;
	argumentInfo->function = static_cast<TypeFunction *>(removeFunction->type);

	Ir &ir = function->state.ir.add();
	ir.op = IrOp::CALL;
	ir.a = functionRegister;
	ir.data = argumentInfo;
	ir.dest = returnValue;
	ir.opSize = 3;

	memop(function, IrOp::READ, remove->refersTo->irPointer, returnValue, 8);

	Ir &sub = function->state.ir.add();
	sub.op = IrOp::ADD_CONSTANT;
	sub.dest = remove->refersTo->iteratorBlock.declarations[1]->registerOfStorage;
	sub.a = remove->refersTo->iteratorBlock.declarations[1]->registerOfStorage;
	sub.immediate = static_cast<u64>(-1LL);
	sub.opSize = 8;
}

void generateFor(ExprFunction *function, ExprLoop *loop) {
	auto it = loop->iteratorBlock.declarations[0];
	auto it_index = loop->iteratorBlock.declarations[1];

	auto &enter = function->state.ir.add();
	enter.op = IrOp::BLOCK;
	enter.data = &loop->iteratorBlock;

	if (isStoredByPointer(getDeclarationType(it))) {
		it->physicalStorage = allocateStackSpace(function, getDeclarationType(it));
		it->registerOfStorage = loadStackAddress(function, it->physicalStorage);
	}
	else {
		it->registerOfStorage = allocateRegister(function);
	}

	assert(getDeclarationType(it_index) == &TYPE_U64);
	it_index->registerOfStorage = allocateRegister(function);

	u32 itReg = it->registerOfStorage;
	u32 it_indexReg = it_index->registerOfStorage;


	if ((loop->forBegin->type->flavor != TypeFlavor::INTEGER) && !(loop->flags & EXPR_FOR_BY_POINTER)) {
		loop->irPointer = allocateRegister(function);
	}
	else {
		loop->irPointer = itReg;
	}

	if (loop->forBegin->type->flavor == TypeFlavor::ARRAY || loop->forBegin->type == &TYPE_STRING) {
		auto begin = loop->forBegin;

		loop->arrayPointer = loadAddressOf(function, begin);

		if (loop->forBegin->type->flags & TYPE_ARRAY_IS_FIXED) {
			set(function, loop->irPointer, loop->arrayPointer, 8);
		}
		else {
			memop(function, IrOp::READ, loop->irPointer, loop->arrayPointer, 8);
		}
	}
	else {
		u32 reg = generateIr(function, loop->forBegin);
		set(function, loop->irPointer, reg, 8);
	}

	constant(function, it_indexReg, 8, 0);

	u32 irEnd = 0;

	if (loop->forEnd) {
		irEnd = generateIr(function, loop->forEnd);
	}

	pushLoop(function, loop);
	addLineMarker(function, loop);

	u32 compareDest;

	compareDest = allocateRegister(function);

	if (loop->forBegin->type->flavor == TypeFlavor::ARRAY || loop->forBegin->type->flavor == TypeFlavor::STRING) {
		if (loop->forBegin->type->flags & TYPE_ARRAY_IS_FIXED) {
			constant(function, compareDest, 8, static_cast<TypeArray *>(loop->forBegin->type)->count);
		}
		else {
			memop(function, IrOp::READ, compareDest, loop->arrayPointer, 8, 8);
		}

		Ir &compare = function->state.ir.add();
		compare.op = IrOp::LESS;
		compare.a = it_index->registerOfStorage;
		compare.b = compareDest;
		compare.dest = compareDest;
		compare.opSize = 8;
	}
	else {
		Ir &compare = function->state.ir.add();
		compare.op = IrOp::LESS;
		compare.a = loop->irPointer;
		compare.b = irEnd;
		compare.dest = compareDest;
		compare.opSize = loop->forBegin->type->size;

		if (loop->forBegin->type->flags & TYPE_INTEGER_IS_SIGNED)
			compare.flags |= IR_SIGNED_OP;
	}

	u32 patch = function->state.ir.count; // Patch this manually so it doesn't skip the completed block if it exists

	Ir &branch = function->state.ir.add();
	branch.op = IrOp::IF_Z_GOTO;
	branch.a = compareDest;
	branch.opSize = 1;

	if (!(loop->flags & EXPR_FOR_BY_POINTER)) {
		if (loop->forBegin->type->flavor == TypeFlavor::ARRAY || loop->forBegin->type->flavor == TypeFlavor::POINTER || loop->forBegin->type == &TYPE_STRING) {
			copyOrRead(function, it->registerOfStorage, loop->irPointer, getDeclarationType(it));
		}
	}

	deferStack.add(loop);

	if (loop->body) {
		addLineMarker(function, loop->body);
		generateIr(function, loop->body);
	}

	exitBlock(function, &loop->iteratorBlock, false);

	Expr *inc = deferStack.pop();
	(void)inc;
	assert(inc == loop);

	Ir &jump = function->state.ir.add();
	jump.op = IrOp::GOTO;
	jump.b = loopStack[loopCount - 1].start;

	function->state.ir[patch].b = function->state.ir.count;


	auto &exit = function->state.ir.add();
	exit.op = IrOp::BLOCK;
	exit.data = nullptr;

	if (loop->completedBody) {
		addLineMarker(function, loop->completedBody);
		generateIr(function, loop->completedBody);
	}

	popLoop(function);
}

bool returnsViaPointer(TypeFunction *function) {
	if (function->returnCount > 1)
		return true;

	#if BUILD_WINDOWS
	return !isStandardSize(function->returnTypes[0]->size);
	#else
	return getSystemVCallingType(function->returnTypes[0]) == SystemVCallingType::MEMORY;
	#endif
}

SystemVCallingState initSystemVCallingState(TypeFunction *function) {
	SystemVCallingState state;
	state.intRegsRemaining = 6;
	state.floatRegsRemaining = 8;

	if (function->returnCount > 1) {
		state.intRegsRemaining -= 1;
	}
	else if (getSystemVCallingType(function->returnTypes[0]) == SystemVCallingType::MEMORY) {
		state.intRegsRemaining -= 1;
	}

	if (!(function->flags & TYPE_FUNCTION_IS_C_CALL)) {
		state.intRegsRemaining -= 1;
	}

	return state;
}

SystemVCallingType passSystemVParameter(SystemVCallingState *state, Type *type) {
	switch (getSystemVCallingType(type)) {
		case SystemVCallingType::EMPTY:
			return SystemVCallingType::EMPTY;
		case SystemVCallingType::FLOAT:
			if (state->floatRegsRemaining) {
				state->floatRegsRemaining--;
				return SystemVCallingType::FLOAT;
			}
			else {
				return SystemVCallingType::MEMORY;
			}
			break;
		case SystemVCallingType::INT:
			if (state->intRegsRemaining) {
				state->intRegsRemaining--;
				return SystemVCallingType::INT;
			}
			else {
				return SystemVCallingType::MEMORY;
			}
			break;
		case SystemVCallingType::FLOAT_FLOAT:
			if (state->floatRegsRemaining >= 2) {
				state->floatRegsRemaining -= 2;
				return SystemVCallingType::FLOAT_FLOAT;
			}
			else {
				return SystemVCallingType::MEMORY;
			} 
		case SystemVCallingType::INT_INT:
			if (state->intRegsRemaining >= 2) {
				state->intRegsRemaining -= 2;
				return SystemVCallingType::INT_INT;
			} 
			else {
				return SystemVCallingType::MEMORY;
			}
		case SystemVCallingType::FLOAT_INT:
			if (state->intRegsRemaining && state->floatRegsRemaining) {
				state->intRegsRemaining--;
				state->floatRegsRemaining--;
				return SystemVCallingType::FLOAT_INT;
			}
			else {
				return SystemVCallingType::MEMORY;
			}
			break;
		case SystemVCallingType::INT_FLOAT:
			if (state->intRegsRemaining && state->floatRegsRemaining) {
				state->intRegsRemaining--;
				state->floatRegsRemaining--;
				return SystemVCallingType::FLOAT_INT;
			}
			else {
				return SystemVCallingType::MEMORY;
			}
			break;
		case SystemVCallingType::MEMORY:
			return SystemVCallingType::MEMORY;
		default:
			assert(false);
			return SystemVCallingType::UNKNOWN;
	}
}

u32 requiredStackSpaceForCallingConvention(TypeFunction *function) {
#if BUILD_WINDOWS
	bool hasContext = !(function->flags & TYPE_FUNCTION_IS_C_CALL);
	bool bigReturn = !isStandardSize(function->returnTypes[0]->size) || function->returnCount > 1;
	
	u32 argumentCount = my_max(bigReturn + hasContext + function->argumentCount, 4);

	u32 space = AlignPO2(argumentCount * 8, 16);

	// External functions may write to the space we pass large arguments in, 
	// (at least according to what MSVC outputs, actual documentation doesn't specify)
	// For our own functions we know the arguments are immutable so we effectively pass by
	// const reference
	if (function->flags & TYPE_FUNCTION_IS_C_CALL) {
		for (u32 i = 0; i < function->argumentCount; i++) {
			if (!isStandardSize(function->argumentTypes[i]->size)) {
				space = AlignPO2(space + function->argumentTypes[i]->size, 16);
			}
		}
	}

	return space;
#else
	auto callingState = initSystemVCallingState(function);

	u32 space = 0;
	for (u32 i = 0; i < function->argumentCount; i++) {
		if (passSystemVParameter(&callingState, function->argumentTypes[i]) == SystemVCallingType::MEMORY) {
			space = AlignPO2(space + function->argumentTypes[i]->size, 8);
		}
	}

	return space;
#endif
}

u32 generateCall(ExprFunction *function, ExprFunctionCall *call, ExprCommaAssignment *comma = nullptr) {
	if ((call->flags & EXPR_FUNCTION_CALL_IS_STATEMENT_LEVEL) || comma) {
		addLineMarker(function, call);
	}

	auto type = static_cast<TypeFunction *>(call->function->type);

	if (call->function->flags & EXPR_FUNCTION_IS_INSTRINSIC) {
		if (call->function->valueOfDeclaration->name->name == "pop_count") {
			auto argumentType = type->argumentTypes[0];

			if (argumentType->flavor != TypeFlavor::INTEGER && argumentType->flavor != TypeFlavor::ENUM) {
				reportError(call->arguments.values[0], "Error: pop_count can only operate on integers or enums");
				return 0;
			}

			u32 argument = generateIr(function, call->arguments.values[0]);

			u32 result = allocateRegister(function);

			auto &ir = function->state.ir.add();
			ir.op = IrOp::POP_COUNT;
			ir.opSize = argumentType->size;
			ir.dest = result;
			ir.a = argument;
			
			return result;
		}
		else if (call->function->valueOfDeclaration->name->name == "bit_scan_forward" || call->function->valueOfDeclaration->name->name == "bit_scan_reverse") {
			auto argumentType = type->argumentTypes[0];

			if (argumentType->flavor != TypeFlavor::INTEGER && argumentType->flavor != TypeFlavor::ENUM) {
				reportError(call->arguments.values[0], "Error: bit_scan_forward can only operate on integers or enums");
				return 0;
			}

			u32 argument = generateIr(function, call->arguments.values[0]);

			u32 resultIndex = allocateRegister(function);
			u32 resultZero = allocateRegister(function);

			auto &ir = function->state.ir.add();
			if (call->function->valueOfDeclaration->name->name == "bit_scan_forward") {
				ir.op = IrOp::BIT_SCAN_FORWARD;
			}
			else {
				ir.op = IrOp::BIT_SCAN_REVERSE;
			}
			ir.opSize = argumentType->size;
			ir.dest = resultIndex;
			ir.b = resultZero;
			ir.a = argument;

			if (comma) {
				auto left = comma->left[0];

				if (left->flavor == ExprFlavor::IDENTIFIER) {
					auto identifier = static_cast<ExprIdentifier *>(left);
					if (!declarationIsStoredByPointer(static_cast<ExprIdentifier *>(left)->declaration)) {
						set(function, identifier->declaration->registerOfStorage, resultIndex, argumentType->size);
					}
					else {
						memop(function, IrOp::WRITE, loadAddressOf(function, left), resultIndex, argumentType->size);
					}
				}
				else {
					memop(function, IrOp::WRITE, loadAddressOf(function, left), resultIndex, argumentType->size);
				}

				assert(comma->exprCount >= 2); 
				
				left = comma->left[1];

				if (left->flavor == ExprFlavor::IDENTIFIER) {
					auto identifier = static_cast<ExprIdentifier *>(left);
					if (!declarationIsStoredByPointer(static_cast<ExprIdentifier *>(left)->declaration)) {
						set(function, identifier->declaration->registerOfStorage, resultZero, 1);
					}
					else {
						memop(function, IrOp::WRITE, loadAddressOf(function, left), resultZero, 1);
					}
				}
				else {
					memop(function, IrOp::WRITE, loadAddressOf(function, left), resultZero, 1);
				}
			}

			return resultIndex;
		}
		else {
			reportError(call, "Error: Call to unknown intrinsic function: %.*s", STRING_PRINTF(call->function->valueOfDeclaration->name->name));
			return 0;
		}
	}

	function->state.stackSpaceForCallingConvention = my_max(function->state.stackSpaceForCallingConvention, requiredStackSpaceForCallingConvention(type));

	bool hasReturns = type->returnTypes[0] != &TYPE_VOID;
	bool hasContext = !(type->flags & TYPE_FUNCTION_IS_C_CALL);
	u32 returnStorage = 0;

	if (hasReturns) {
		u32 stackSpaceForReturn = 0;
		u32 alignmentForReturn = 1;

		for (u32 i = 0; i < type->returnCount; i++) {
			auto returnType = type->returnTypes[i];

			alignmentForReturn = my_max(alignmentForReturn, returnType->alignment);
			stackSpaceForReturn = AlignPO2(stackSpaceForReturn, returnType->alignment);
			stackSpaceForReturn += returnType->size;
		}

		if (stackSpaceForReturn)
			returnStorage = allocateStackSpaceAndLoadAddress(function, stackSpaceForReturn, alignmentForReturn);
	}

	u32 functionReg = generateIr(function, call->function);

	u32 argumentCount = hasContext + type->argumentCount;
	FunctionCall *callArguments = static_cast<FunctionCall *>(function->state.allocator.allocate(sizeof(FunctionCall) + argumentCount * sizeof(u32)));
	callArguments->function = type;

	if (hasContext) {
		callArguments->arguments[0] = function->state.contextRegister;
	}

	for (u32 i = 0; i < call->arguments.count; i++) {
		callArguments->arguments[hasContext + i] = generateIr(function, call->arguments.values[i]);
	}

	if (function->valueOfDeclaration) {
		String name = function->valueOfDeclaration->name->name;
		callArguments->stackTrace.function.data = reinterpret_cast<u8 *>(name.characters);
		callArguments->stackTrace.function.count = name.length;
	}
	else {
		callArguments->stackTrace.function.data = nullptr;
		callArguments->stackTrace.function.count = 0;
	}

	auto filepath = getFileInfoByUid(call->start.fileUid)->path;

	callArguments->stackTrace.filename.data = reinterpret_cast<u8 *>(filepath.characters);
	callArguments->stackTrace.filename.count = filepath.length;
	callArguments->stackTrace.line = call->start.line;

	Ir &ir = function->state.ir.add();
	ir.op = IrOp::CALL;
	ir.dest = returnStorage;
	ir.a = functionReg;
	ir.data = callArguments;
	ir.opSize = argumentCount;

	if (comma) {
		u32 returnOffset = 0;

		for (u32 i = 0; i < comma->exprCount; i++) {
			auto left = comma->left[i];

			returnOffset = AlignPO2(returnOffset, type->returnTypes[i]->alignment);
			if (left->flavor == ExprFlavor::IDENTIFIER) {
				auto identifier = static_cast<ExprIdentifier *>(left);
				if (!declarationIsStoredByPointer(static_cast<ExprIdentifier *>(left)->declaration)) {
					memop(function, IrOp::READ, identifier->declaration->registerOfStorage, returnStorage, type->returnTypes[i]->size, returnOffset);
				}
				else {
					memop(function, IrOp::COPY_SRC_OFFSET, loadAddressOf(function, left), returnStorage, type->returnTypes[i]->size, returnOffset);
				}
			}
			else {
				memop(function, IrOp::COPY_SRC_OFFSET, loadAddressOf(function, left), returnStorage, type->returnTypes[i]->size, returnOffset);
			}

			returnOffset += type->returnTypes[i]->size;
		}

		return 0;
	}
	else if ((call->flags & EXPR_FUNCTION_CALL_IS_STATEMENT_LEVEL) || !hasReturns) {
		return 0;
	}
	else if (isStoredByPointer(type->returnTypes[0])) {
		return returnStorage;
	}
	else {
		return memop(function, IrOp::READ, allocateRegister(function), returnStorage, type->returnTypes[0]->size);
	}
}

u32 generateArrayLiteral(ExprFunction *function, ExprArrayLiteral *array) {
	auto arrayType = static_cast<TypeArray *>(array->type);
	u32 arrayCount = (arrayType->flags & TYPE_ARRAY_IS_FIXED) ? arrayType->count : array->count;

	u32 addressReg = allocateStackSpaceAndLoadAddress(function, arrayType->arrayOf->size * arrayCount, arrayType->arrayOf->alignment);

	for (u32 i = 0; i < arrayCount; i++) {
		if (i + 1 == array->count && arrayCount > array->count) {
			u32 countReg = constant(function, allocateRegister(function), 8, arrayCount - i);

			Ir &address = function->state.ir.add();
			address.op = IrOp::ADD_CONSTANT;
			address.dest = addressReg;
			address.a = addressReg;
			address.immediate = i * arrayType->arrayOf->size;
			address.opSize = 8;

			u32 value = generateIr(function, array->values[i]);

			u32 patch = function->state.ir.count;

			copyOrWrite(function, addressReg, value, arrayType->arrayOf);

			Ir &add = function->state.ir.add();
			add.op = IrOp::ADD_CONSTANT;
			add.dest = addressReg;
			add.a = addressReg;
			add.immediate = arrayType->arrayOf->size;
			add.opSize = 8;

			Ir &dec = function->state.ir.add();
			dec.op = IrOp::ADD_CONSTANT;
			dec.dest = countReg;
			dec.a = countReg;
			dec.immediate = static_cast<u64>(-1LL);
			dec.opSize = 8;

			Ir &branch = function->state.ir.add();
			branch.op = IrOp::IF_NZ_GOTO;
			branch.a = countReg;
			branch.b = patch;
			branch.opSize = 8;

			break;
		}
		else {
			u32 value = generateIr(function, array->values[i]);
			copyOrWrite(function, addressReg, value, arrayType->arrayOf, i * arrayType->arrayOf->size);
		}
	}

	return addressReg;
}

u32 generateIr(ExprFunction *function, Expr *expr) {
	PROFILE_FUNC();

	switch (expr->flavor) {
	case ExprFlavor::SLICE: {
		return generateSlice(function, static_cast<ExprSlice *>(expr));
	}
	case ExprFlavor::BINARY_OPERATOR: {
		return generateBinary(function, static_cast<ExprBinaryOperator *>(expr));
	}
	case ExprFlavor::INIT_IMPERATIVE_DECLARATION: {
		Declaration *declaration = expr->valueOfDeclaration;
		if (declaration->flags & DECLARATION_IS_UNINITIALIZED) {
			return 0;
		}

		u32 value = generateIr(function, declaration->initialValue);
		
		if (declarationIsStoredByPointer(declaration)) {
			copyOrWrite(function, declaration->registerOfStorage, value, declaration->type_);
		} 
		else {
			set(function, declaration->registerOfStorage, value, declaration->type_->size);
		}

		return 0;
	}
	case ExprFlavor::BLOCK: {
		generateBlock(function, static_cast<ExprBlock *>(expr));

		return 0;
	}
	case ExprFlavor::DEFER: {
		deferStack.add(expr);

		return 0;
	}
	case ExprFlavor::BREAK: {
		generateBreak(function, static_cast<ExprBreakOrContinue *>(expr));

		return 0;
	}
	case ExprFlavor::CONTINUE: {
		generateContinue(function, static_cast<ExprBreakOrContinue *>(expr));

		return 0;
	}
	case ExprFlavor::REMOVE: {
		generateRemove(function, static_cast<ExprBreakOrContinue *>(expr));		

		return 0;
	}
	case ExprFlavor::INT_LITERAL: {
		auto literal = static_cast<ExprLiteral *>(expr);

		if (literal->unsignedValue == 0 && isStoredByPointer(literal->type)) {
			return memop(function, IrOp::ZERO_MEMORY, allocateStackSpaceAndLoadAddress(function, literal->type), 0, literal->type->size);
		}

		assert(literal->type->size);

		return constant(function, allocateRegister(function), literal->type->size, literal->unsignedValue);
	}
	case ExprFlavor::FLOAT_LITERAL: {
		auto literal = static_cast<ExprLiteral *>(expr);

		assert(literal->type->size);

		u64 value = literal->unsignedValue;

		if (literal->type->size == 4) {
			*reinterpret_cast<float *>(&value) = static_cast<float>(literal->floatValue);

			value &= 0xFFFF'FFFFULL;
		}

		return constant(function, allocateRegister(function), literal->type->size, value);
	}
	case ExprFlavor::FOR: {
		generateFor(function, static_cast<ExprLoop *>(expr));

		return 0;
	}
	case ExprFlavor::FUNCTION: {
		u32 result = allocateRegister(function);

		Ir &address = function->state.ir.add();
		address.dest = result;
		address.opSize = 8;
		address.op = IrOp::FUNCTION;
		address.data = expr;

		return result;
	}
	case ExprFlavor::STRING_LITERAL: {
		u32 result = allocateStackSpaceAndLoadAddress(function, &TYPE_STRING);

		u32 temp = allocateRegister(function);

		Ir &address = function->state.ir.add();
		address.op = IrOp::STRING;
		address.dest = temp;
		address.data = static_cast<ExprStringLiteral *>(expr);
		
		memop(function, IrOp::WRITE, result, temp, 8);

		constant(function, temp, 8, static_cast<ExprStringLiteral *>(expr)->string.length);

		memop(function, IrOp::WRITE, result, temp, 8, 8);

		return result;
	}
	case ExprFlavor::FUNCTION_CALL: {
		auto call = static_cast<ExprFunctionCall *>(expr);

		return generateCall(function, call);
	}
	case ExprFlavor::IDENTIFIER: {
		auto identifier = static_cast<ExprIdentifier *>(expr);


		if (identifier->structAccess) {
			u32 address = loadAddressOf(function, expr);

			if (isStoredByPointer(identifier->type)) {
				return address;
			}
			else {
				return memop(function, IrOp::READ, allocateRegister(function), address, identifier->type->size);
			}
		}
		else {
			if (declarationIsStoredByPointer(identifier->declaration)) {
				u32 address = loadAddressOf(function, expr);

				if (isStoredByPointer(identifier->type)) {
					return address;
				}
				else {
					return memop(function, IrOp::READ, allocateRegister(function), address, identifier->type->size);
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

		u32 value = generateIr(function, switch_->condition);

		ExprSwitch::Case *else_ = nullptr;

		for (auto &case_ : switch_->cases) {
			if (case_.condition) {
				auto condition = case_.condition;

				assert(condition->type == switch_->condition->type);

				u32 result = generateEquals(function, value, condition, true);

				case_.irBranch = function->state.ir.count;

				Ir &branch = function->state.ir.add();

				branch.op = IrOp::IF_NZ_GOTO;
				branch.a = result;
				branch.opSize = 1;
			}
			else {
				else_ = &case_;
			}
		}

		u32 finalPatch = function->state.ir.count;

		Ir & final = function->state.ir.add();
		final.op = IrOp::GOTO;

		if (else_) {
			else_->irBranch = finalPatch;
		}

		for (auto &case_ : switch_->cases) {
			function->state.ir[case_.irBranch].b = function->state.ir.count;

			generateIr(function, case_.block);

			if (!case_.fallsThrough && &case_ + 1 != switch_->cases.end()) {
				case_.irSkip = function->state.ir.count;

				Ir &skip = function->state.ir.add();
				skip.op = IrOp::GOTO;
			}
		}

		if (!else_) {
			function->state.ir[finalPatch].b = function->state.ir.count;
		}

		for (u32 i = 0; i + 1 < switch_->cases.count; i++) {
			auto &case_ = switch_->cases[i];

			if (!case_.fallsThrough)
				function->state.ir[case_.irSkip].b = function->state.ir.count;
		}

		return 0;
	}
	case ExprFlavor::IF: {
		auto ifElse = static_cast<ExprIf *>(expr);

		u32 conditionReg = generateIr(function, ifElse->condition);

		if (ifElse->ifBody && ifElse->elseBody) {
			u32 patchIfZ = function->state.ir.count;
			Ir &ifZ = function->state.ir.add();
			ifZ.op = IrOp::IF_Z_GOTO;
			ifZ.a = conditionReg;
			ifZ.opSize = 1;

			addLineMarker(function, ifElse->ifBody);
			generateIr(function, ifElse->ifBody);

			u32 patchJump = function->state.ir.count;
			Ir &jump = function->state.ir.add();
			jump.op = IrOp::GOTO;

			function->state.ir[patchIfZ].b = function->state.ir.count;

			addLineMarker(function, ifElse->elseBody);
			generateIr(function, ifElse->elseBody);

			function->state.ir[patchJump].b = function->state.ir.count;
		}
		else if (ifElse->ifBody) {
			u32 patchIfZ = function->state.ir.count;
			Ir &ifZ = function->state.ir.add();
			ifZ.op = IrOp::IF_Z_GOTO;
			ifZ.a = conditionReg;
			ifZ.opSize = 1;

			addLineMarker(function, ifElse->ifBody);
			generateIr(function, ifElse->ifBody);

			function->state.ir[patchIfZ].b = function->state.ir.count;
		}
		else if (ifElse->elseBody) {
			u32 patchIfNZ = function->state.ir.count;
			Ir &ifNZ = function->state.ir.add();
			ifNZ.op = IrOp::IF_NZ_GOTO;
			ifNZ.a = conditionReg;
			ifNZ.opSize = 1;

			addLineMarker(function, ifElse->elseBody);
			generateIr(function, ifElse->elseBody);

			function->state.ir[patchIfNZ].b = function->state.ir.count;
		}

		return 0;
	}
	case ExprFlavor::COMMA_ASSIGNMENT: {
		auto comma = static_cast<ExprCommaAssignment *>(expr);
		generateCall(function, static_cast<ExprFunctionCall *>(comma->call), comma);
		return 0;
	}
	case ExprFlavor::RETURN: {
		auto return_ = static_cast<ExprReturn *>(expr);
		
		if (return_->returns.count) {
			auto functionType = static_cast<TypeFunction *>(return_->returnsFrom->type);
			if (returnsViaPointer(functionType)) {
				u32 offset = 0;
				for (u32 i = 0; i < return_->returns.count; i++) {
					auto value = return_->returns.values[i];
					offset = AlignPO2(offset, value->type->alignment);
					copyOrWrite(function, function->state.returnPointerRegister, generateIr(function, value), value->type, offset);
					offset += value->type->size;
				}

				exitBlock(function, nullptr, true);
				

				Ir &ir = function->state.ir.add();
				ir.op = IrOp::RETURN;
				ir.opSize = static_cast<u32>(SystemVCallingType::MEMORY);
				ir.a = function->state.contextRegister;
				ir.data = TYPE_VOID_POINTER;
			}
			else {
				assert(return_->returns.count == 1);

				auto returnType = functionType->returnTypes[0];

				auto value = generateIr(function, return_->returns.values[0]);

				exitBlock(function, nullptr, true);

				Ir &ir = function->state.ir.add();
				ir.op = IrOp::RETURN;
				ir.opSize = static_cast<u32>(getSystemVCallingType(returnType));
				ir.a = value;
				ir.data = returnType;
			}
		}
		else {
			exitBlock(function, nullptr, true);

			Ir &ir = function->state.ir.add();
			ir.op = IrOp::RETURN;
			ir.opSize = static_cast<u32>(SystemVCallingType::EMPTY);
			ir.data = &TYPE_VOID;
		}


		return 0;
	}
	case ExprFlavor::STRUCT_LITERAL: {
		auto literal = static_cast<ExprStructLiteral *>(expr);
		if (structIsLiteral(literal)) {

			u32 result = allocateRegister(function);

			Ir &address = function->state.ir.add();
			address.op = IrOp::STRUCT_LITERAL;
			address.dest = result;
			address.data = literal;

			return result;
		}
		else {

			u32 address = allocateStackSpaceAndLoadAddress(function, literal->type);

			for (u32 i = 0; i < literal->initializers.count; i++) {
				auto offset = literal->initializers.declarations[i]->physicalStorage;
				auto value = literal->initializers.values[i];

				copyOrWrite(function, address, generateIr(function, value), value->type, offset);
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

		u32 result = allocateRegister(function);

		Ir &ir = function->state.ir.add();
		ir.op = IrOp::TYPE;
		ir.dest = result;
		ir.data = type;

		return result;
	}
	case ExprFlavor::UNARY_OPERATOR: {
		ExprUnaryOperator *unary = static_cast<ExprUnaryOperator *>(expr);

		switch (unary->op) {
		case TOKEN('*'): {
			return loadAddressOf(function, unary->value);
		}
		case TOKEN('-'): {
			u32 result = allocateRegister(function);

			u32 toNegate = generateIr(function, unary->value);

			Ir &negate = function->state.ir.add();
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
			u32 result = allocateRegister(function);

			u32 toInvert = generateIr(function, unary->value);

			Ir &invert = function->state.ir.add();
			invert.op = IrOp::NOT;
			invert.a = toInvert;
			invert.opSize = unary->value->type->size;
			invert.dest = result;

			return result;
		}
		case TOKEN('!'): {
			u32 result = allocateRegister(function);

			u32 zero = constant(function, allocateRegister(function), unary->value->type->size, 0);

			u32 toInvert = generateIr(function, unary->value);

			Ir &invert = function->state.ir.add();
			invert.op = IrOp::EQUAL;
			invert.a = toInvert;
			invert.b = zero;
			invert.opSize = unary->value->type->size;
			invert.dest = result;

			return result;
		}
		case TokenT::SHIFT_LEFT: {
			u32 addressReg = generateIr(function, unary->value);

			if (isStoredByPointer(unary->type)) {
				return addressReg;
			}
			else {
				return memop(function, IrOp::READ, allocateRegister(function), addressReg, unary->type->size);
			}
		}
		case TokenT::TYPE_INFO: {
			u32 value = generateIr(function, unary->value);

			u32 result = allocateRegister(function);

			Ir &ir = function->state.ir.add();
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

		pushLoop(function, loop);

		u32 conditionReg = generateIr(function, loop->whileCondition);

		u32 patch = function->state.ir.count; // Patch this manually so it doesn't skip the completed block if it exists

		Ir &ifZ = function->state.ir.add();
		ifZ.op = IrOp::IF_Z_GOTO;
		ifZ.a = conditionReg;
		ifZ.opSize = 1;

		if (loop->body) {
			addLineMarker(function, loop->body);
			generateIr(function, loop->body);
		}

		Ir &jump = function->state.ir.add();
		jump.op = IrOp::GOTO;
		jump.b = loopStack[loopCount - 1].start;

		function->state.ir[patch].b = function->state.ir.count;

		if (loop->completedBody) {
			addLineMarker(function, loop->completedBody);
			generateIr(function, loop->completedBody);
		}

		popLoop(function);

		return 0;
	}
	case ExprFlavor::ARRAY_LITERAL: {
		auto array = static_cast<ExprArrayLiteral *>(expr);

		if (arrayIsLiteral(array)) {

			u32 result = allocateRegister(function);

			Ir &address = function->state.ir.add();
			address.op = IrOp::ARRAY_LITERAL;
			address.dest = result;
			address.data = array;

			return result;
		}
		else {
			u32 data = generateArrayLiteral(function, array);

			if (array->type->flags & TYPE_ARRAY_IS_FIXED) {
				return data;
			}
			else {				
				u32 value = allocateStackSpaceAndLoadAddress(function, array->type);
				memop(function, IrOp::WRITE, value, data, 8);

				u32 count = constant(function, allocateRegister(function), 8, array->count);
				memop(function, IrOp::WRITE, value, count, 8, 8);

				return value;
			}
		}
	}
	case ExprFlavor::CONTEXT: {
		return function->state.contextRegister;
	}
	case ExprFlavor::PUSH_CONTEXT: {
		auto pushContext = static_cast<ExprBinaryOperator *>(expr);

		auto oldContext = function->state.contextRegister;

		function->state.contextRegister = generateIr(function, pushContext->left);

		addLineMarker(function, pushContext->right);
		generateIr(function, pushContext->right);

		function->state.contextRegister = oldContext;
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

bool generateIrForFunction(ExprFunction *function) {
	function->state.nextRegister = 0;

	auto functionType = static_cast<TypeFunction *>(function->type);
	if (returnsViaPointer(functionType)) {
		function->state.returnPointerRegister = allocateRegister(function);
	}
	
	if (!(function->flags & EXPR_FUNCTION_IS_C_CALL)) {
		function->state.contextRegister = allocateRegister(function);
	}
	else {
		function->state.contextRegister = INVALID_REGISTER;
	}

	SystemVCallingState callingState = initSystemVCallingState(functionType);

	for (u32 i = 0; i < function->arguments.declarations.count; i++) {
		auto declaration = function->arguments.declarations[i];
		auto type = getDeclarationType(declaration);

		declaration->registerOfStorage = allocateRegister(function);

#if BUILD_WINDOWS
		auto passedInMemory = !isStandardSize(type->size) || declaration->registerOfStorage >= 4;
#else
		auto passedInMemory = passSystemVParameter(&callingState, type) == SystemVCallingType::MEMORY;
#endif
		if (isStoredByPointer(type) && !passedInMemory) {
			declaration->physicalStorage = allocateStackSpace(function, getDeclarationType(declaration));
		}
	}

	for (u32 i = 1; i < function->returns.declarations.count; i++) {
		auto declaration = function->returns.declarations[i];

		declaration->registerOfStorage = function->state.nextRegister++;
	}

	generateIr(function, function->body);

	if (hadError) {
		return false;
	}

	irInstructions += function->state.ir.count;

	function->flags |= EXPR_FUNCTION_RUN_READY;
	read_write_barrier();
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
	profiler_register_this_thread();
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