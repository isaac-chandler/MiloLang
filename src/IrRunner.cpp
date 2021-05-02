#include "Basic.h"
#include "IrRunner.h"
#include <dyncall.h>
#include <dyncall_signature.h>
#include <dyncall_callback.h>
#include "Infer.h"
#include "CompilerMain.h"
#include "Error.h"

struct StackNode {
	StackNode *next;
	StackNode *previous;

	u32 count;
	u32 offset;
	u64 data[];
};

StackNode *allocateStackBlock(u32 size) {
	StackNode *block = static_cast<StackNode *>(malloc(sizeof(StackNode) + sizeof(u64) * size));

	block->next = nullptr;
	block->previous = nullptr;
	block->count = size;
	block->offset = 0;

	return block;
}

u64 *allocateStack(VMState *state, u32 count) {
	StackNode *previous = nullptr;

	while (state->stackAllocator && count > state->stackAllocator->count - state->stackAllocator->offset) {
		previous = state->stackAllocator;
		state->stackAllocator = state->stackAllocator->next;
	}

	if (!state->stackAllocator) {
		state->stackAllocator = allocateStackBlock(my_max(4096, count));
		state->stackAllocator->previous = previous;
		
		if (previous)
			previous->next = state->stackAllocator;
	}

	auto result = state->stackAllocator->data + state->stackAllocator->offset;

	state->stackAllocator->offset += count;

	return result;
}

void freeStack(VMState *state, u32 count) {
	assert(state->stackAllocator);
	assert(state->stackAllocator->offset >= count);

	state->stackAllocator->offset -= count;

	while (state->stackAllocator->previous && state->stackAllocator->offset == 0) {
		state->stackAllocator = state->stackAllocator->previous;
	}
}



char getSigChar(Type *type) {
	if (type == &TYPE_S8) {
		return DC_SIGCHAR_CHAR;
	}
	else if (type == &TYPE_S16) {
		return DC_SIGCHAR_SHORT;
	}
	else if (type == &TYPE_S32) {
		return DC_SIGCHAR_INT;
	}
	else if (type == &TYPE_S64) {
		return DC_SIGCHAR_LONGLONG;
	}
	else if (type == &TYPE_U8) {
		return DC_SIGCHAR_UCHAR;
	}
	else if (type == &TYPE_U16) {
		return DC_SIGCHAR_USHORT;
	}
	else if (type == &TYPE_U32) {
		return DC_SIGCHAR_UINT;
	}
	else if (type == &TYPE_U64) {
		return DC_SIGCHAR_ULONGLONG;
	}
	else if (type == &TYPE_BOOL) {
		return DC_SIGCHAR_BOOL;
	}
	else if (type == &TYPE_VOID) {
		return DC_SIGCHAR_VOID;
	}
	else if (type == &TYPE_F32) {
		return DC_SIGCHAR_FLOAT;
	}
	else if (type == &TYPE_F64) {
		return DC_SIGCHAR_DOUBLE;
	}
	else if (type->flavor == TypeFlavor::ENUM) {
		return getSigChar(static_cast<TypeEnum *>(type)->integerType);
	}
	else if (type->flavor == TypeFlavor::POINTER || type->flavor == TypeFlavor::TYPE || type->flavor == TypeFlavor::FUNCTION) {
		return DC_SIGCHAR_POINTER;
	}
	else if (type->size == 1) {
		return DC_SIGCHAR_UCHAR;
	}
	else if (type->size == 2) {
		return DC_SIGCHAR_USHORT;
	}
	else if (type->size == 4) {
		return DC_SIGCHAR_UINT;
	}
	else if (type->size == 8) {
		return DC_SIGCHAR_ULONGLONG;
	}
	else {
		return DC_SIGCHAR_POINTER;
	}

}

void createRuntimeValue(Expr *value, void *dest) {
	PROFILE_FUNC();
	if (value->flavor == ExprFlavor::INT_LITERAL) {
		auto literal = static_cast<ExprLiteral *>(value);

		if (literal->type->size && literal->unsignedValue == 0) {
			memset(dest, 0, value->type->size);
		}
		else {
			memcpy(dest, &literal->unsignedValue, value->type->size == 0 ? 8 : value->type->size);
		}
	}
	else if (value->flavor == ExprFlavor::FLOAT_LITERAL) {
		auto literal = static_cast<ExprLiteral *>(value);

		if (literal->type == &TYPE_F32) {
			*reinterpret_cast<float *>(dest) = static_cast<float>(literal->floatValue);
		}
		else {
			*reinterpret_cast<double *>(dest) = literal->floatValue;
		}
	}
	else if (value->flavor == ExprFlavor::FUNCTION) {
		auto function = static_cast<ExprFunction *>(value);

		if (function->flags & EXPR_FUNCTION_IS_C_CALL) {
			memcpy(dest, &function->loadedFunctionPointer, function->type->size);
		}
		else {
			memcpy(dest, &function, function->type->size);
		}
	}
	else if (value->flavor == ExprFlavor::ARRAY_LITERAL) {
		auto array = static_cast<ExprArrayLiteral *>(value);

		auto arrayType = static_cast<TypeArray *>(value->type);
		auto store = static_cast<u8 *>(dest);

		for (u32 i = 0; i < arrayType->count; i++) {
			createRuntimeValue(array->values[i], store + i * arrayType->arrayOf->size);

			if (i + 1 == array->count && arrayType->count > array->count) {
				for (u32 j = i + 1; j < arrayType->count; j++) {
					memcpy(store + j * arrayType->arrayOf->size, store + i * arrayType->arrayOf->size, arrayType->arrayOf->size);
				}

				break;
			}
		}
	}
	else if (value->flavor == ExprFlavor::TYPE_LITERAL) {
		assert(static_cast<ExprLiteral *>(value)->typeValue->runtimeTypeInfo);
		memcpy(dest, &static_cast<ExprLiteral *>(value)->typeValue->runtimeTypeInfo, sizeof(Type_Info *));
	}
	else if (value->flavor == ExprFlavor::STRING_LITERAL) {
		memcpy(dest, &static_cast<ExprStringLiteral *>(value)->string, sizeof(String));
	}
	else {
		assert(false);
	}
}


void initVMState(VMState *state) {
	state->dc = dcNewCallVM(16384); // @Robustness
	dcMode(state->dc, DC_CALL_C_X64_WIN64);
	dcReset(state->dc);
	state->stackAllocator = nullptr;
}

void deinitVMState(VMState *state) {
	dcFree(state->dc);
	
	while (state->stackAllocator && state->stackAllocator->previous) {
		state->stackAllocator = state->stackAllocator->previous;
	}

	while (state->stackAllocator) {
		auto next = state->stackAllocator->next;

		free(state->stackAllocator);
		state->stackAllocator = next;
	}
}

static ExprRun *runningDirective;



void runFunction(VMState *state, ExprFunction *expr, Ir *caller, DCArgs *dcArgs, u64 *callerStack) {
	if (expr->flags & EXPR_FUNCTION_IS_COMPILER) {
		assert(expr->valueOfDeclaration);

		if (std::this_thread::get_id() != mainThread) {
			reportError(caller, "Error: Compiler functions can only be called from the initial thread", STRING_PRINTF(expr->valueOfDeclaration->name));
			return;
		}

		if (expr->valueOfDeclaration->name == "add_build_file") {
			
			auto name = new ExprStringLiteral;
			name->flavor = ExprFlavor::STRING_LITERAL;
			name->start = {};
			name->end = {};
			name->string = *reinterpret_cast<String *>(callerStack + caller->arguments->args[0].number);
			name->type = &TYPE_STRING;

			auto load = new ExprLoad;
			load->flavor = ExprFlavor::LOAD;
			load->start = {};
			load->end = {};
			load->file = name;
			load->module = runningDirective->module;

			auto import = new Importer;
			import->import = load;
			
			inferInput.add(InferQueueJob(import, runningDirective->module));
		}
		else if (expr->valueOfDeclaration->name == "set_build_options") {
			auto options = *reinterpret_cast<Build_Options *>(callerStack + caller->arguments->args[0].number);

			if (options.backend != Build_Options::Backend::X64 && options.backend != Build_Options::Backend::LLVM) {
				reportError(caller, "Error: Unrecognized backend %llu in set_build_options", options.backend);
				reportError(runningDirective, "   ..: From #run directive");
				return;
			}

			buildOptions = options;
		}
		else if (expr->valueOfDeclaration->name == "get_build_options") {
			if (caller->dest) {
				*reinterpret_cast<Build_Options *>(callerStack + caller->dest) = buildOptions;
			}
		}
		else if (expr->valueOfDeclaration->name == "get_build_arguments") {
			if (caller->dest) {
				*reinterpret_cast<MiloArray<MiloString> *>(callerStack + caller->dest) = buildArguments;
			}
		}
		else {
			reportError(caller,           "Error: Unknown compiler function: %.*s", STRING_PRINTF(expr->valueOfDeclaration->name));
			reportError(runningDirective, "   ..: From #run directive");
		}
		return;
	}

	u32 dummyStorage = 0;
	u32 outParameters = expr->returns.declarations.count - 1;

	for (u32 i = 0; i < outParameters; i++) {
		if (caller->arguments->args[expr->arguments.declarations.count + i].number == static_cast<u32>(-1)) {
			dummyStorage = my_max(dummyStorage,
				(getDeclarationType(expr->returns.declarations[i + 1])->size + 7) / 8);
		}
	}

	u64 *stack = allocateStack(state, expr->state.nextRegister + dummyStorage);
	stack[0] = 0; // The 0th register is the special constant 0 value

#if BUILD_DEBUG
	CodeLocation line;
#endif

	if (dcArgs) {
		for (u32 i = 0; i < expr->arguments.declarations.count; i++) {
			auto &argument = expr->arguments.declarations[i];
			auto argumentType = getDeclarationType(argument);

			if (argumentType == &TYPE_F32) {
				*reinterpret_cast<float *>(stack + expr->arguments.declarations[i]->physicalStorage) = dcbArgFloat(dcArgs);
			}
			else if (argumentType == &TYPE_F64) {
				*reinterpret_cast<double *>(stack + expr->arguments.declarations[i]->physicalStorage) = dcbArgDouble(dcArgs);
			}
			else if (argumentType->size == 1) {
				stack[expr->arguments.declarations[i]->physicalStorage] = dcbArgUChar(dcArgs);
			}
			else if (argumentType->size == 2) {
				stack[expr->arguments.declarations[i]->physicalStorage] = dcbArgUShort(dcArgs);
			}
			else if (argumentType->size == 4) {
				stack[expr->arguments.declarations[i]->physicalStorage] = dcbArgUInt(dcArgs);
			}
			else if (argumentType->size == 8) {
				stack[expr->arguments.declarations[i]->physicalStorage] = dcbArgULongLong(dcArgs);
			}
			else {
				memcpy(stack + expr->arguments.declarations[i]->physicalStorage, dcbArgPointer(dcArgs), argumentType->size);
			}
		}
	}
	else {

		assert(expr->arguments.declarations.count == caller->arguments->argCount - outParameters);

		for (u32 i = 0; i < expr->arguments.declarations.count; i++) {
			auto &argument = caller->arguments->args[i];
			for (u32 j = 0; j < (argument.type->size + 7) / 8; j++) {
				stack[expr->arguments.declarations[i]->physicalStorage + j] = callerStack[argument.number + (argument.number == 0 ? 0 : j)];
			}
		}

		for (u32 i = 0; i < outParameters; i++) {
			auto number = caller->arguments->args[expr->arguments.declarations.count + i].number;
			if (number == static_cast<u64>(-1))
				stack[expr->returns.declarations[i + 1]->physicalStorage] = reinterpret_cast<u64>(stack + expr->state.nextRegister);
			else
				stack[expr->returns.declarations[i + 1]->physicalStorage] = callerStack[number];
		}
	}

	for (u32 i = 0; i < expr->state.ir.count;) {
#define A (stack[op.a] & opMask)
#define B (stack[op.b] & opMask)
#define S_A	(static_cast<s64>(stack[op.a] & signBit ? stack[op.a] | ~opMask : A))
#define S_B	(static_cast<s64>(stack[op.b] & signBit ? stack[op.b] | ~opMask : B))

#define FLOAT_OP(x) \
if (op.flags & IR_FLOAT_OP) {\
	if (op.opSize == 8) {\
		*reinterpret_cast<double *>(stack + op.dest) = *reinterpret_cast<double *>(stack + op.a) x *reinterpret_cast<double *>(stack + op.b);\
	}\
	else {\
		*reinterpret_cast<float *>(stack + op.dest) = *reinterpret_cast<float *>(stack + op.a) x *reinterpret_cast<float *>(stack + op.b);\
	}\
	break;\
}

#define FLOAT_COMP(x) \
if (op.flags & IR_FLOAT_OP) {\
	if (op.opSize == 8) {\
		stack[op.dest] = *reinterpret_cast<double *>(stack + op.a) x *reinterpret_cast<double *>(stack + op.b);\
	}\
	else {\
		stack[op.dest] = *reinterpret_cast<float *>(stack + op.a) x *reinterpret_cast<float *>(stack + op.b);\
	}\
	break;\
}

		auto &op = expr->state.ir[i++];
		u64 opMask = 0;
		u64 destMask = 0;

		if (op.opSize == 8) {
			opMask = 0xFFFF'FFFF'FFFF'FFFFULL;
		}
		else {
			opMask = (1ULL << (op.opSize * 8ULL)) - 1ULL;
		}

		u64 signBit = 1ULL << (op.opSize * 8ULL - 1ULL);

		if (op.destSize == 8) {
			destMask = 0xFFFF'FFFF'FFFF'FFFFULL;
		}
		else {
			destMask = (1ULL << (op.destSize * 8ULL)) - 1ULL;
		}

		switch (op.op) {
		case IrOp::ADD: {
			FLOAT_OP(+);
			u64 result = A + B;
			stack[op.dest] = result & opMask;
			break;
		}
		case IrOp::ADD_CONSTANT: {
			u64 result = A + op.immediate;
			stack[op.dest] = result & opMask;
			break;
		}
		case IrOp::SUB: {
			FLOAT_OP(-);
			u64 result = A - B;
			stack[op.dest] = result & opMask;
			break;
		}
		case IrOp::MOD: {
			if (op.flags & IR_FLOAT_OP) {
				if (op.opSize == 8) {
					*reinterpret_cast<double *>(stack + op.dest) = fmod(*reinterpret_cast<double *>(stack + op.a), *reinterpret_cast<double *>(stack + op.b));
				}\
				else {
					*reinterpret_cast<float *>(stack + op.dest) = fmod(*reinterpret_cast<float *>(stack + op.a), *reinterpret_cast<float *>(stack + op.b));
				}
			}
			else if (op.flags & IR_SIGNED_OP) {
				s64 a = S_A;
				s64 b = S_B;

				stack[op.dest] = static_cast<u64>(a % b) & opMask;
				break;
			}
			else {
				u64 result = A % B;
				stack[op.dest] = result & opMask;
				break;
			}
		}
		case IrOp::MUL: {
			FLOAT_OP(*);
			u64 result = A * B;
			stack[op.dest] = result & opMask;
			break;
		}
		case IrOp::MUL_BY_CONSTANT: {
			u64 result = A * op.b;
			stack[op.dest] = result & opMask;
			break;
		}
		case IrOp::DIV: {
			FLOAT_OP(/ );
			if (op.flags & IR_SIGNED_OP) {
				s64 a = S_A;
				s64 b = S_B;

				stack[op.dest] = static_cast<u64>(a / b) & opMask;
				break;
			}
			else {
				u64 result = A / B;
				stack[op.dest] = result & opMask;
				break;
			}
		}
		case IrOp::DIVIDE_BY_CONSTANT: {
			stack[op.dest] = static_cast<u64>(S_A / static_cast<s64>(op.b)) & opMask;
			break;
		}
		case IrOp::AND: {
			stack[op.dest] = A & B;
			break;
		}
		case IrOp::OR: {
			stack[op.dest] = A | B;
			break;
		}
		case IrOp::XOR: {
			stack[op.dest] = A ^ B;
			break;
		}
		case IrOp::SHIFT_LEFT: {
			u64 result = A << B;
			stack[op.dest] = result & opMask;
			break;
		}
		case IrOp::SHIFT_RIGHT: {
			if (op.flags & IR_SIGNED_OP) {
				s64 a = S_A;
				s64 b = S_B;

				stack[op.dest] = static_cast<u64>(a >> b) & opMask;
				break;
			}
			else {
				u64 result = A >> B;
				stack[op.dest] = result & opMask;
				break;
			}
		}
		case IrOp::NEG: {
			if (op.flags & IR_FLOAT_OP) {
				if (op.opSize == 4) {
					*reinterpret_cast<float *>(stack + op.dest) = -*reinterpret_cast<float *>(stack + op.a);
				}
				else {
					*reinterpret_cast<double *>(stack + op.dest) = -*reinterpret_cast<double *>(stack + op.a);
				}
				break;
			}
			u64 result = -A;
			stack[op.dest] = result & opMask;
			break;
		}
		case IrOp::NOT: {
			u64 result = ~A;
			stack[op.dest] = result & opMask;
			break;
		}
		case IrOp::EQUAL: {
			FLOAT_COMP(== );
			stack[op.dest] = A == B;
			break;
		}
		case IrOp::NOT_EQUAL: {
			FLOAT_COMP(!= );
			stack[op.dest] = A != B;
			break;
		}
		case IrOp::GREATER: {
			FLOAT_COMP(> );
			if (op.flags & IR_SIGNED_OP) {
				stack[op.dest] = S_A > S_B;
			}
			else {
				stack[op.dest] = A > B;
			}
			break;
		}
		case IrOp::GREATER_EQUAL: {
			FLOAT_COMP(>= );
			if (op.flags & IR_SIGNED_OP) {
				stack[op.dest] = S_A >= S_B;
			}
			else {
				stack[op.dest] = A >= B;
			}
			break;
		}
		case IrOp::LESS: {
			FLOAT_COMP(< );
			if (op.flags & IR_SIGNED_OP) {
				stack[op.dest] = S_A < S_B;
			}
			else {
				stack[op.dest] = A < B;
			}
			break;
		}
		case IrOp::LESS_EQUAL: {
			FLOAT_COMP(<= );
			if (op.flags & IR_SIGNED_OP) {
				stack[op.dest] = S_A <= S_B;
			}
			else {
				stack[op.dest] = A <= B;
			}
			break;
		}
		case IrOp::IF_NZ_GOTO: {
			if (A) {
				i = op.b;
			}
			break;
		}
		case IrOp::IF_Z_GOTO: {
			if (!A) {
				i = op.b;
			}
			break;
		}
		case IrOp::GOTO: {
			i = op.b;
			break;
		}
		case IrOp::CALL: {
			if (op.flags & IR_C_CALL) {
				auto function = reinterpret_cast<void *>(stack[op.a]);

				dcReset(state->dc);

				for (u64 i = 0; i < op.arguments->argCount; i++) {
					auto type = op.arguments->args[i].type;
					auto number = op.arguments->args[i].number;

					if (type->flavor == TypeFlavor::ENUM) {
						type = static_cast<TypeEnum *>(type)->integerType;
					}

					if (type == &TYPE_BOOL) {
						dcArgBool(state->dc, stack[number] & 0xFF);
					}
					else if (type == &TYPE_S8 || type == &TYPE_U8) {
						dcArgChar(state->dc, stack[number] & 0xFF);
					}
					else if (type == &TYPE_S16 || type == &TYPE_U16) {
						dcArgShort(state->dc, stack[number] & 0xFFFF);
					}
					else if (type == &TYPE_S32 || type == &TYPE_U32) {
						dcArgInt(state->dc, stack[number] & 0xFFFF'FFFFULL);
					}
					else if (type == &TYPE_S64 || type == &TYPE_U64) {
						dcArgLongLong(state->dc, stack[number]);
					}
					else if (type == &TYPE_TYPE || type->flavor == TypeFlavor::POINTER || type->flavor == TypeFlavor::FUNCTION) {
						dcArgPointer(state->dc, reinterpret_cast<void *>(stack[number]));
					}
					else if (type == &TYPE_F32) {
						dcArgFloat(state->dc, *reinterpret_cast<float *>(stack + number));
					}
					else if (type == &TYPE_F64) {
						dcArgDouble(state->dc, *reinterpret_cast<double *>(stack + number));
					}
					else if (type->flavor == TypeFlavor::STRUCT || type->flavor == TypeFlavor::ARRAY || type == &TYPE_STRING) {
						auto struct_ = dcNewStruct(1, DEFAULT_ALIGNMENT); // @Speed: allocating a struct every time

						// @Hack: This may not work for some calling conventions if they separate structs into their individual values
						dcStructField(struct_, DC_SIGCHAR_CHAR, type->alignment, type->size);

						if (number == 0 && type->size > 8) {
							auto zeros = malloc(type->size); // @Speed
							memset(zeros, 0, type->size);

							dcArgStruct(state->dc, struct_, zeros);

							free(zeros);
						}
						else {
							dcArgStruct(state->dc, struct_, stack + number);
						}

						dcFreeStruct(struct_);
					}
					else {
						assert(false);
					}
				}

				auto type = op.arguments->returnType;


				if (type->flavor == TypeFlavor::ENUM) {
					type = static_cast<TypeEnum *>(type)->integerType;
				}

				if (type == &TYPE_VOID) {
					dcCallVoid(state->dc, function);
				}
				else if (type == &TYPE_BOOL) {
					bool val = dcCallBool(state->dc, function);

					if (op.dest)
						stack[op.dest] = val;
				}
				else if (type == &TYPE_S8 || type == &TYPE_U8) {
					char val = dcCallChar(state->dc, function);

					if (op.dest)
						stack[op.dest] = type->flags & TYPE_INTEGER_IS_SIGNED ? (u64) (s64) val : (u64) val;
				}
				else if (type == &TYPE_S16 || type == &TYPE_U16) {
					short val = dcCallShort(state->dc, function);

					if (op.dest)
						stack[op.dest] = type->flags & TYPE_INTEGER_IS_SIGNED ? (u64) (s64) val : (u64) val;
				}
				else if (type == &TYPE_S32 || type == &TYPE_U32) {
					int val = dcCallInt(state->dc, function);

					if (op.dest)
						stack[op.dest] = type->flags & TYPE_INTEGER_IS_SIGNED ? (u64) (s64) val : (u64) val;
				}
				else if (type == &TYPE_S64 || type == &TYPE_U64) {
					long long val = dcCallLongLong(state->dc, function);
					
					if (op.dest)
						stack[op.dest] = val;
				}
				else if (type == &TYPE_TYPE || type->flavor == TypeFlavor::POINTER || type->flavor == TypeFlavor::FUNCTION) {
					void *val = dcCallPointer(state->dc, function);

					if (op.dest)
						stack[op.dest] = reinterpret_cast<u64>(val);
				}
				else if (type == &TYPE_F32) {
					float val = dcCallFloat(state->dc, function);

					if (op.dest)
						*reinterpret_cast<float *>(stack + op.dest) = val;
				}
				else if (type == &TYPE_F64) {
					double val = dcCallDouble(state->dc, function);

					if (op.dest)
						*reinterpret_cast<double *>(stack + op.dest) = val;
				}
				else if (type->flavor == TypeFlavor::STRUCT || type->flavor == TypeFlavor::ARRAY || type == &TYPE_STRING) {
					auto struct_ = dcNewStruct(1, DEFAULT_ALIGNMENT); // @Speed

					// @Hack: This may not work for some calling conventions
					dcStructField(struct_, DC_SIGCHAR_CHAR, type->alignment, type->size);

					if (op.dest)
						dcCallStruct(state->dc, function, struct_, stack + op.dest);
					else {
						void *dummy = malloc(type->size); // @Speed
						dcCallStruct(state->dc, function, struct_, dummy);
						free(dummy);
					}

					dcFreeStruct(struct_);
				}
				else {
					assert(false);
				}
			}
			else {
				runFunction(state, reinterpret_cast<ExprFunction *>(stack[op.a]), &op, nullptr, stack);
			}
			break;
		}
		case IrOp::RETURN: {
			if (op.opSize != 0 && caller->dest) {
				if (op.a == 0) {
					memset(callerStack + caller->dest, 0, op.opSize);
				}
				else {
					memcpy(callerStack + caller->dest, stack + op.a, op.opSize);
				}
			}
			freeStack(state, expr->state.nextRegister);
			return;
		}
		case IrOp::FLOAT_TO_INT: {
			if (op.flags & IR_SIGNED_OP) {
				if (op.opSize == 8) {
					stack[op.dest] = static_cast<u64>(static_cast<s64>(*reinterpret_cast<double *>(stack + op.a))) & destMask;
				}
				else {
					stack[op.dest] = static_cast<u64>(static_cast<s64>(*reinterpret_cast<float *>(stack + op.a))) & destMask;
				}
			}
			else {
				if (op.opSize == 8) {
					stack[op.dest] = static_cast<u64>(*reinterpret_cast<double *>(stack + op.a)) & destMask;
				}
				else {
					stack[op.dest] = static_cast<u64>(*reinterpret_cast<float *>(stack + op.a)) & destMask;
				}
			}
			break;
		}
		case IrOp::INT_TO_FLOAT: {
			if (op.flags & IR_SIGNED_OP) {
				if (op.destSize == 8) {
					*reinterpret_cast<double *>(stack + op.dest) = static_cast<double>(S_A);
				}
				else {
					*reinterpret_cast<float *>(stack + op.dest) = static_cast<float>(S_A);
				}
			}
			else {
				if (op.destSize == 8) {
					*reinterpret_cast<double *>(stack + op.dest) = static_cast<double>(A);
				}
				else {
					*reinterpret_cast<float *>(stack + op.dest) = static_cast<float>(A);
				}
			}
			break;
		}
		case IrOp::IMMEDIATE: {
			stack[op.dest] = op.immediate & opMask;
			break;
		}
		case IrOp::SET: {
			if (op.opSize != op.destSize && isStandardSize(op.opSize)) {
				if (op.flags & IR_FLOAT_OP) {
					if (op.opSize == 4) {
						assert(op.destSize == 8);

						*reinterpret_cast<double *>(stack + op.dest) = *reinterpret_cast<float *>(stack + op.a);
					}
					else {
						assert(op.opSize == 8);
						assert(op.destSize == 4);

						*reinterpret_cast<float *>(stack + op.dest) = static_cast<float>(*reinterpret_cast<double *>(stack + op.a));
					}
				}
				else {
					auto result = A;

					if (op.flags & IR_SIGNED_OP) {
						result = result * signBit ? result | ~destMask : result;
					}

					stack[op.dest] = result & destMask;
				}
			}
			else {
				assert(op.opSize == op.destSize);

				if (op.a == 0) {
					memset(stack + op.dest, 0, op.opSize);
				}
				else {
					memcpy(stack + op.dest, stack + op.a, op.opSize);
				}
			}
			break;
		}
		case IrOp::READ: {
			memcpy(stack + op.dest, reinterpret_cast<void *>(stack[op.a]), op.destSize);
			break;
		}
		case IrOp::WRITE: {
			if (op.b == 0) {
				memset(reinterpret_cast<void *>(stack[op.a]), 0, op.opSize);
			}
			else {
				memcpy(reinterpret_cast<void *>(stack[op.a]), stack + op.b, op.opSize);
			}
			break;
		}
		case IrOp::ADDRESS_OF_GLOBAL: {
			assert(op.declaration->runtimeValue);
			stack[op.dest] = reinterpret_cast<u64>(op.declaration->runtimeValue) + op.a;
			break;
		}
		case IrOp::ADDRESS_OF_LOCAL: {
			stack[op.dest] = reinterpret_cast<u64>(reinterpret_cast<u8 *>(stack + op.a) + op.immediate);
			break;
		}
		case IrOp::FUNCTION: {
			auto function = static_cast<ExprFunction *>(op.function);

			if (function->flags & EXPR_FUNCTION_IS_C_CALL) {
				assert(function->loadedFunctionPointer);
				stack[op.dest] = reinterpret_cast<u64>(function->loadedFunctionPointer);
			}
			else {
				stack[op.dest] = reinterpret_cast<u64>(function);
			}
			break;
		}
		case IrOp::STRING: {
			stack[op.dest] = reinterpret_cast<u64>(op.string->string.characters);
			stack[op.dest + 1] = op.string->string.length;
			break;
		}
		case IrOp::TYPE_INFO: {
			auto type = reinterpret_cast<Type *>(stack[op.a]);
			assert(type->runtimeTypeInfo);
			stack[op.dest] = reinterpret_cast<u64>(type->runtimeTypeInfo);
			break;
		}
		case IrOp::TYPE: {
			stack[op.dest] = reinterpret_cast<u64>(op.type);
			break;
		}
		case IrOp::LINE_MARKER: {
#if BUILD_DEBUG
			line = op.location.start;
#endif
			break;
		}
		case IrOp::BLOCK:
		case IrOp::NOOP: {
			break;
		}
		default:
			assert(false);
			printf("Unhandled ir operation");
		}
	}

	assert(stack[0] == 0);

	// We shouldn't go off the end without returning
	assert(false);
}

Expr *getReturnValueFromBytes(CodeLocation start, EndLocation end, Type *type, void *bytes) {
	switch (type->flavor) {
	case TypeFlavor::ARRAY: {
		if (type->flags & TYPE_ARRAY_IS_FIXED) {
			auto arrayType = static_cast<TypeArray *>(type);

			auto array = new ExprArrayLiteral;
			array->flavor = ExprFlavor::ARRAY_LITERAL;
			array->start = start;
			array->end = end;
			array->values = new Expr * [arrayType->count];
			array->count = arrayType->count;
			array->type = type;

			u8 *storage = static_cast<u8 *>(bytes);

			for (u64 i = 0; i < arrayType->count; i++) {
				array->values[i] = getReturnValueFromBytes(start, end, arrayType->arrayOf, storage);

				storage += arrayType->arrayOf->size;
			}

			return array;
		}
		else {
			assert(false); // @Incomplete
			return nullptr;
		}
	}
	case TypeFlavor::BOOL: {
		auto value = *static_cast<u8 *>(bytes);

		auto literal = new ExprLiteral;
		literal->flavor = ExprFlavor::INT_LITERAL;
		literal->start = start;
		literal->end = end;
		literal->unsignedValue = value;
		literal->type = type;

		return literal;
	}
	case TypeFlavor::ENUM: {
		auto integer = static_cast<TypeEnum *>(type)->integerType;

		u64 value;

		if (integer == &TYPE_U8) {
			value = *static_cast<u8 *>(bytes);
		}
		else if (integer == &TYPE_U16) {
			value = *static_cast<u16 *>(bytes);
		}
		else if (integer == &TYPE_U32) {
			value = *static_cast<u32 *>(bytes);
		}
		else if (integer == &TYPE_U64) {
			value = *static_cast<u64 *>(bytes);
		}
		else if (integer == &TYPE_S8) {
			value = static_cast<s64>(*static_cast<s8 *>(bytes));
		}
		else if (integer == &TYPE_S16) {
			value = static_cast<s64>(*static_cast<s16 *>(bytes));
		}
		else if (integer == &TYPE_S32) {
			value = static_cast<s64>(*static_cast<s32 *>(bytes));
		}
		else if (integer == &TYPE_S64) {
			value = *static_cast<s64 *>(bytes);
		}

		auto literal = new ExprLiteral;
		literal->flavor = ExprFlavor::INT_LITERAL;
		literal->start = start;
		literal->end = end;
		literal->unsignedValue = value;
		literal->type = type;

		return literal;
	}
	case TypeFlavor::FLOAT: {
		double value;

		if (type == &TYPE_F32) {
			value = *static_cast<float *>(bytes);
		}
		else if (type == &TYPE_F64) {
			value = *static_cast<double *>(bytes);
		}

		auto literal = new ExprLiteral;
		literal->flavor = ExprFlavor::FLOAT_LITERAL;
		literal->start = start;
		literal->end = end;
		literal->floatValue = value;

		literal->type = &TYPE_FLOAT_LITERAL;

		return literal;
	}
	case TypeFlavor::FUNCTION: {
		if (type->flags & TYPE_FUNCTION_IS_C_CALL) {
			assert(false);
		}

		return *static_cast<ExprFunction **>(bytes);
	}
	case TypeFlavor::INTEGER: {
		u64 value;

		if (type == &TYPE_U8) {
			value = *static_cast<u8 *>(bytes);
		}
		else if (type == &TYPE_U16) {
			value = *static_cast<u16 *>(bytes);
		}
		else if (type == &TYPE_U32) {
			value = *static_cast<u32 *>(bytes);
		}
		else if (type == &TYPE_U64) {
			value = *static_cast<u64 *>(bytes);
		}
		else if (type == &TYPE_S8) {
			value = static_cast<s64>(*static_cast<s8 *>(bytes));
		}
		else if (type == &TYPE_S16) {
			value = static_cast<s64>(*static_cast<s16 *>(bytes));
		}
		else if (type == &TYPE_S32) {
			value = static_cast<s64>(*static_cast<s32 *>(bytes));
		}
		else if (type == &TYPE_S64) {
			value = *static_cast<s64 *>(bytes);
		}

		auto literal = new ExprLiteral;
		literal->flavor = ExprFlavor::INT_LITERAL;
		literal->start = start;
		literal->end = end;
		literal->unsignedValue = value;

		literal->type = type;

		return literal;
	}
	case TypeFlavor::POINTER: {
		auto value = *static_cast<u64 *>(bytes);

		auto literal = new ExprLiteral;
		literal->flavor = ExprFlavor::INT_LITERAL;
		literal->start = start;
		literal->end = end;
		literal->unsignedValue = value;
		literal->type = type;

		return literal;
	}
	case TypeFlavor::STRING: {
		auto value = *static_cast<String *>(bytes);

		auto literal = new ExprStringLiteral;
		literal->flavor = ExprFlavor::STRING_LITERAL;
		literal->start = start;
		literal->end = end;
		literal->string = value;
		literal->type = type;

		return literal;

	}
	case TypeFlavor::STRUCT: {
		assert(!(type->flags & TYPE_STRUCT_IS_UNION)); // @Incomplete

		auto struct_ = static_cast<TypeStruct *>(type);

		auto literal = new ExprStructLiteral;
		literal->flavor = ExprFlavor::STRUCT_LITERAL;
		literal->start = start;
		literal->end = end;
		literal->type = type;
		literal->typeValue = nullptr;
		literal->initializers.count = 0;
		literal->initializers.names = nullptr;

		Array<Expr *> values;
		Array<Declaration *> declarations;

		for (auto member : struct_->members.declarations) {
			if (member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING))
				continue;

			literal->initializers.count++;
			values.add(getReturnValueFromBytes(start, end, getDeclarationType(member), static_cast<char *>(bytes) + member->physicalStorage));
			declarations.add(member);
		}

		literal->initializers.values = values.storage;
		literal->initializers.declarations = declarations.storage;

		return literal;
	}
	case TypeFlavor::TYPE: {
		auto value = *static_cast<Type **>(bytes);

		auto literal = new ExprLiteral;
		literal->flavor = ExprFlavor::TYPE_LITERAL;
		literal->start = start;
		literal->end = end;
		literal->typeValue = value;
		literal->type = type;

		return literal;
	}
	case TypeFlavor::VOID: {
		return nullptr;
	}
	default:
		assert(false);
		return nullptr;
	}
}

Expr *runFunctionRoot(VMState *state, ExprRun *directive) {
	PROFILE_FUNC();
	u64 store[32];

	runningDirective = directive;

	auto function = static_cast<ExprFunction *>(directive->function);

	u64 *stackPointer = store;

	auto returnType = getDeclarationType(function->returns.declarations[0]);

	if (returnType->size > sizeof(store)) {
		stackPointer = static_cast<u64 *>(malloc(returnType->size));
	}

	Ir dummyOp;
	dummyOp.op = IrOp::CALL;
	FunctionCall arguments;
	arguments.argCount = 0;
	arguments.returnType = returnType;
	dummyOp.arguments = &arguments;
	dummyOp.dest = arguments.returnType == &TYPE_VOID ? 0 : 1;

	runFunction(state, function, &dummyOp, nullptr, stackPointer - 1);

	auto result = getReturnValueFromBytes(function->start, function->end, returnType, stackPointer);

	if (returnType->size > sizeof(store)) {
		free(stackPointer);
	}

	return result;
}


char cCallCallback(DCCallback *pcb, DCArgs *args, DCValue *result, void *userdata) {
	auto function = static_cast<ExprFunction *>(userdata);

	auto returnType = getDeclarationType(function->returns.declarations[0]);

	u64 *resultPointer = &result->L;

	if (!isStandardSize(returnType->size)) {
		resultPointer = static_cast<u64 *>(dcbArgPointer(args));
	}

	Ir dummyOp;
	dummyOp.op = IrOp::CALL;
	FunctionCall arguments;
	arguments.argCount = 0;
	arguments.returnType = returnType;
	dummyOp.arguments = &arguments;
	dummyOp.dest = arguments.returnType == &TYPE_VOID ? 0 : 1;

	VMState state;

	initVMState(&state);
	runFunction(&state, function, &dummyOp, args, resultPointer - 1);
	deinitVMState(&state);


	if (!isStandardSize(returnType->size)) {
		result->p = resultPointer;
	}

	return getSigChar(returnType);
}


void *createCCallFunction(ExprFunction *function) {
	assert(function->returns.declarations.count == 1);
	auto returnType = getDeclarationType(function->returns.declarations[0]);

	char *callback;

	if (!isStandardSize(returnType->size)) {
		callback = static_cast<char *>(malloc(function->arguments.declarations.count + 4));

		callback[0] = 'p';

		for (u32 i = 0; i < function->arguments.declarations.count; i++) {
			auto arg = getDeclarationType(function->arguments.declarations[i]);

			callback[i + 1] = getSigChar(arg);
		}

		callback[function->arguments.declarations.count + 1] = ')';
		callback[function->arguments.declarations.count + 2] = 'p';
		callback[function->arguments.declarations.count + 3] = 0;
	}
	else {
		callback = static_cast<char *>(malloc(function->arguments.declarations.count + 3));

		for (u32 i = 0; i < function->arguments.declarations.count; i++) {
			auto arg = getDeclarationType(function->arguments.declarations[i]);

			callback[i] = getSigChar(arg);
		}

		callback[function->arguments.declarations.count] = ')';
		callback[function->arguments.declarations.count + 1] = getSigChar(returnType);
		callback[function->arguments.declarations.count + 2] = 0;
	}

	return dcbNewCallback(callback, cCallCallback, function);
}