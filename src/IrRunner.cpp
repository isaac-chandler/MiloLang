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
	alignas(16) u64 data[];
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
	if (type == &TYPE_VOID) {
		return DC_SIGCHAR_VOID;
	}
	else if (type == &TYPE_F32) {
		return DC_SIGCHAR_FLOAT;
	}
	else if (type == &TYPE_F64) {
		return DC_SIGCHAR_DOUBLE;
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

void createRuntimeValue(Expr *value, void *dest);

void createRuntimeArrayLiteral(ExprArrayLiteral *array, void *dest) {
	auto arrayType = static_cast<TypeArray *>(array->type);
	u32 arrayCount = arrayType->flags & TYPE_ARRAY_IS_FIXED ? arrayType->count : array->count;
	auto store = static_cast<u8 *>(dest);

	for (u32 i = 0; i < arrayCount; i++) {
		createRuntimeValue(array->values[i], store + i * arrayType->arrayOf->size);

		if (i + 1 == array->count && arrayCount > array->count) {
			for (u32 j = i + 1; j < arrayCount; j++) {
				memcpy(store + j * arrayType->arrayOf->size, store + i * arrayType->arrayOf->size, arrayType->arrayOf->size);
			}

			break;
		}
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

		if (array->type->flags & TYPE_ARRAY_IS_FIXED) {
			createRuntimeArrayLiteral(array, dest);
		}
		else {
			auto arrayType = static_cast<TypeArray *>(array->type);
			auto data = malloc(arrayType->arrayOf->size * array->count);

			createRuntimeArrayLiteral(array, data);

			*reinterpret_cast<void **>(dest) = data;
			reinterpret_cast<u64 *>(dest)[1] = array->count;
		}
	}
	else if (value->flavor == ExprFlavor::STRUCT_LITERAL) {
		auto literal = static_cast<ExprStructLiteral *>(value);

		auto arrayType = static_cast<TypeArray *>(value->type);
		auto store = static_cast<u8 *>(dest);

		for (u32 i = 0; i < literal->initializers.count; i++) {
			createRuntimeValue(literal->initializers.values[i], store + literal->initializers.declarations[i]->physicalStorage);
		}
	}
	else if (value->flavor == ExprFlavor::TYPE_LITERAL) {
		assert(static_cast<ExprLiteral *>(value)->typeValue->runtimeTypeInfo);
		memcpy(dest, &static_cast<ExprLiteral *>(value)->typeValue->runtimeTypeInfo, sizeof(Type_Info *));
	}
	else if (value->flavor == ExprFlavor::STRING_LITERAL) {
		auto destString = static_cast<MiloString *>(dest);
		auto srcString = static_cast<ExprStringLiteral *>(value)->string;

		destString->data = (u8 *)srcString.characters;
		destString->count = srcString.length;
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

void getDcParameter(u64 *dest, Type *type, DCArgs *dcArgs) {
	if (type == &TYPE_F32) {
		*reinterpret_cast<float *>(dest) = dcbArgFloat(dcArgs);
	}
	else if (type == &TYPE_F64) {
		*reinterpret_cast<double *>(dest) = dcbArgDouble(dcArgs);
	}
	else if (type->size == 1) {
		*dest = dcbArgUChar(dcArgs);
	}
	else if (type->size == 2) {
		*dest = dcbArgUShort(dcArgs);
	}
	else if (type->size == 4) {
		*dest = dcbArgUInt(dcArgs);
	}
	else if (type->size == 8) {
		*dest = dcbArgULongLong(dcArgs);
	}
	else {
		*reinterpret_cast<void **>(dest) = dcbArgPointer(dcArgs);
	}
}

void runFunction(VMState *state, ExprFunction *expr, Ir *caller, DCArgs *dcArgs, u64 *callerStack) {
	auto callerArguments = static_cast<FunctionCall *>(caller->data);

	if (expr->flags & EXPR_FUNCTION_IS_COMPILER) {
		assert(!dcArgs);
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
			name->string = *reinterpret_cast<String *>(callerStack[callerArguments->args[1].number]);
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
			auto options = *reinterpret_cast<Build_Options *>(callerStack[callerArguments->args[1].number]);

			if (options.backend != Build_Options::Backend::X64 && options.backend != Build_Options::Backend::LLVM) {
				reportError(caller, "Error: Unrecognized backend %llu in set_build_options", options.backend);
				reportError(runningDirective, "   ..: From #run directive");
				return;
			}

			buildOptions = options;
		}
		else if (expr->valueOfDeclaration->name == "get_build_options") {
			*reinterpret_cast<Build_Options *>(callerStack[callerArguments->args[0].number]) = buildOptions;
		}
		else if (expr->valueOfDeclaration->name == "get_build_arguments") {
			*reinterpret_cast<MiloArray<MiloString> *>(callerStack[callerArguments->args[0].number]) = buildArguments;
		}
		else {
			reportError(caller,           "Error: Unknown compiler function: %.*s", STRING_PRINTF(expr->valueOfDeclaration->name));
			reportError(runningDirective, "   ..: From #run directive");
		}
		return;
	}

	u32 spaceToAllocate = AlignPO2(expr->state.nextRegister, 2) + AlignPO2(expr->state.stackSpace, 16) / 8;

	u64 *stack = allocateStack(state, spaceToAllocate);
	u64 stackSpace = reinterpret_cast<u64>(stack + AlignPO2(expr->state.nextRegister, 2));

#if BUILD_DEBUG
	CodeLocation line;
#endif

	if (dcArgs) {
		u32 argIndex = 0;

		if (!isStandardSize(getDeclarationType(expr->returns.declarations[0])->size)) {
			getDcParameter(stack + argIndex++, getDeclarationType(expr->returns.declarations[0]), dcArgs);
		}

		for (auto argument : expr->arguments.declarations) {
			getDcParameter(stack + argIndex++, getDeclarationType(argument), dcArgs);
		}

		for (u32 i = 0; i < expr->returns.declarations.count; i++) {
			getDcParameter(stack + argIndex++, getDeclarationType(expr->returns.declarations[i]), dcArgs);
		}
	}
	else {
		for (u32 i = 0; i < callerArguments->argCount; i++) {
			stack[i] = callerStack[callerArguments->args[i].number];
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

		if (op.b == 8) {
			destMask = 0xFFFF'FFFF'FFFF'FFFFULL;
		}
		else {
			destMask = (1ULL << (op.b * 8ULL)) - 1ULL;
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

				auto arguments = static_cast<FunctionCall *>(op.data);

				for (u64 i = 0; i < arguments->argCount; i++) {
					auto type = arguments->args[i].type;
					auto number = arguments->args[i].number;

					if (type == &TYPE_F32) {
						dcArgFloat(state->dc, *reinterpret_cast<float *>(stack + number));
					}
					else if (type == &TYPE_F64) {
						dcArgDouble(state->dc, *reinterpret_cast<double *>(stack + number));
					}
					if (type->size == 1) {
						dcArgChar(state->dc, stack[number] & 0xFF);
					}
					else if (type->size == 2) {
						dcArgShort(state->dc, stack[number] & 0xFF);
					}
					else if (type->size == 4) {
						dcArgInt(state->dc, stack[number] & 0xFFFF'FFFFULL);
					}
					else if (type->size == 8) {
						dcArgLongLong(state->dc, stack[number]);
					}
					else {
						assert(false);
					}
				}

				auto type = arguments->returnType;


				if (type == &TYPE_VOID || op.opSize == 0) {
					dcCallVoid(state->dc, function);
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
				else if (type->size == 1) {
					char val = dcCallChar(state->dc, function);

					if (op.dest)
						stack[op.dest] = val;
				}
				else if (type->size == 2) {
					short val = dcCallShort(state->dc, function);

					if (op.dest)
						stack[op.dest] = type->flags & TYPE_INTEGER_IS_SIGNED ? (u64) (s64) val : (u64) val;
				}
				else if (type->size == 4) {
					int val = dcCallInt(state->dc, function);

					if (op.dest)
						stack[op.dest] = type->flags & TYPE_INTEGER_IS_SIGNED ? (u64) (s64) val : (u64) val;
				}
				else if (type->size == 8) {
					long long val = dcCallLongLong(state->dc, function);
					
					if (op.dest)
						stack[op.dest] = val;
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
			if (op.opSize != 0 && caller->opSize != 0) {
				memcpy(callerStack + caller->dest, stack + op.a, op.opSize);
			}
			freeStack(state, spaceToAllocate);
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
				if (op.b == 8) {
					*reinterpret_cast<double *>(stack + op.dest) = static_cast<double>(S_A);
				}
				else {
					*reinterpret_cast<float *>(stack + op.dest) = static_cast<float>(S_A);
				}
			}
			else {
				if (op.b == 8) {
					*reinterpret_cast<double *>(stack + op.dest) = static_cast<double>(A);
				}
				else {
					*reinterpret_cast<float *>(stack + op.dest) = static_cast<float>(A);
				}
			}
			break;
		}
		case IrOp::EXTEND_INT: {
			auto result = A;

			if (op.flags & IR_SIGNED_OP) {
				result = result & signBit ? result | ~destMask : result;
			}

			stack[op.dest] = result & destMask;
			break;
		}
		case IrOp::FLOAT_CAST: {
			if (op.opSize == 4) {
				assert(op.b == 8);

				*reinterpret_cast<double *>(stack + op.dest) = *reinterpret_cast<float *>(stack + op.a);
			}
			else {
				assert(op.opSize == 8);
				assert(op.b == 4);

				*reinterpret_cast<float *>(stack + op.dest) = static_cast<float>(*reinterpret_cast<double *>(stack + op.a));
			}
			break;
		}
		case IrOp::IMMEDIATE: {
			stack[op.dest] = op.immediate & opMask;
			break;
		}
		case IrOp::SET: {
			stack[op.dest] = stack[op.a];
			break;
		}
		case IrOp::READ: {
			memcpy(stack + op.dest, reinterpret_cast<char *>(stack[op.a]) + op.immediate, op.opSize);
			break;
		}
		case IrOp::WRITE: {
			memcpy(reinterpret_cast<u8 *>(stack[op.dest]) + op.immediate, stack + op.a, op.opSize);
			break;
		}
		case IrOp::COPY: {
			memcpy(reinterpret_cast<u8 *>(stack[op.dest]) + op.immediate, reinterpret_cast<void *>(stack[op.a]), op.opSize);
			break;
		}
		case IrOp::ZERO_MEMORY: {
			memset(reinterpret_cast<char *>(stack[op.dest]) + op.immediate, 0, op.opSize);
			break;
		}
		case IrOp::ADDRESS_OF_GLOBAL: {
			stack[op.dest] = reinterpret_cast<u64>(static_cast<Declaration *>(op.data)->runtimeValue) + op.a;
			break;
		}
		case IrOp::STACK_ADDRESS: {
			stack[op.dest] = stackSpace + op.immediate;
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
			stack[op.dest] = reinterpret_cast<u64>(static_cast<ExprStringLiteral *>(op.data)->string.characters);
			break;
		}
		case IrOp::ARRAY_LITERAL: {
			auto literal = static_cast<ExprArrayLiteral *>(op.data);

			if (!literal->runtimeValue) {
				literal->runtimeValue = malloc(literal->type->size);
				createRuntimeValue(literal, literal->runtimeValue);
			}

			stack[op.dest] = reinterpret_cast<u64>(literal->runtimeValue);
			break;
		}
		case IrOp::STRUCT_LITERAL: {
			auto literal = static_cast<ExprStructLiteral *>(op.data);

			if (!literal->runtimeValue) {
				literal->runtimeValue = malloc(literal->type->size);
				createRuntimeValue(literal, literal->runtimeValue);
			}

			stack[op.dest] = reinterpret_cast<u64>(literal->runtimeValue);
			break;
		}
		case IrOp::TYPE: {
			stack[op.dest] = reinterpret_cast<u64>(op.data);
			break;
		}
		case IrOp::TYPE_INFO: {
			stack[op.dest] = reinterpret_cast<u64>(reinterpret_cast<Type *>(stack[op.a])->runtimeTypeInfo);
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
	char argumentsStore[sizeof(FunctionCall) + sizeof(Argument) * 2];
	u64 argumentData[3];

	runningDirective = directive;

	auto function = static_cast<ExprFunction *>(directive->function);

	auto returnType = getDeclarationType(function->returns.declarations[0]);

	
	auto contextPointer = malloc(TYPE_CONTEXT.size);

	assert(TYPE_CONTEXT.defaultValue);
	createRuntimeValue(TYPE_CONTEXT.defaultValue, contextPointer);

	u32 bigReturn = !isStandardSize(returnType->size);

	Ir dummyOp;
	dummyOp.op = IrOp::CALL;
	FunctionCall *arguments = reinterpret_cast<FunctionCall *>(argumentsStore);

	arguments->args[0].number = 1;
	arguments->args[0].type = TYPE_VOID_POINTER;

	void *returnPointer;
	if (bigReturn) {
		returnPointer = malloc(returnType->size);
		arguments->argCount = 2;
		arguments->args[1].number = 2;
		arguments->args[1].type = TYPE_VOID_POINTER;

		argumentData[1] = reinterpret_cast<u64>(returnPointer);
		argumentData[2] = reinterpret_cast<u64>(contextPointer);
	}
	else {
		returnPointer = static_cast<void *>(argumentData);
		arguments->argCount = 1;

		argumentData[1] = reinterpret_cast<u64>(contextPointer);
	}

	arguments->returnType = bigReturn ? &TYPE_VOID : returnType;
	dummyOp.data = arguments;
	dummyOp.opSize = bigReturn || returnType == &TYPE_VOID ? 0 : returnType->size;
	dummyOp.dest = 0;

	runFunction(state, function, &dummyOp, nullptr, argumentData);

	auto result = getReturnValueFromBytes(function->start, function->end, returnType, returnPointer);

	if (bigReturn)
		free(returnPointer);
	free(contextPointer);

	return result;
}


char cCallCallback(DCCallback *pcb, DCArgs *args, DCValue *result, void *userdata) {
	auto function = static_cast<ExprFunction *>(userdata);

	auto returnType = getDeclarationType(function->returns.declarations[0]);

	bool bigReturn = !isStandardSize(returnType->size);

	u64 *resultPointer = &result->L;

	Ir dummyOp;
	dummyOp.op = IrOp::CALL;
	FunctionCall arguments;
	arguments.argCount = 0;
	arguments.returnType = bigReturn ? TYPE_VOID_POINTER : returnType;
	dummyOp.data = &arguments;
	dummyOp.dest = 0;
	dummyOp.opSize = arguments.returnType == &TYPE_VOID ? 0 : arguments.returnType->size;

	VMState state;

	initVMState(&state);
	runFunction(&state, function, &dummyOp, args, resultPointer);
	deinitVMState(&state);

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