#include "Basic.h"
#include "IrGenerator.h"
#include "IrRunner.h"
#include <dyncall.h>
#include <dyncall_signature.h>
#include <dyncall_callback.h>
#include "Infer.h"
#include "CompilerMain.h"
#include "Error.h"
#include "TypeTable.h"
#include "dyncall_args.h"
#include <math.h>

BucketedArenaAllocator irRunnerArena(1024 * 1024);

#define IR_RUNNER_NEW(T) new (static_cast<T *>(irRunnerArena.allocate(sizeof(T)))) T
#define IR_RUNNER_NEW_ARRAY(T, C) new (static_cast<T *>(irRunnerArena.allocate((C) * sizeof(T)))) T[C]

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

struct MemoryDcAggr {
	DCaggr *aggr;
	u32 size;
};

DCaggr *emptyDcAggr;
DCaggr *intDcAggr[16];
DCaggr *float4DcAggr;
DCaggr *float8DcAggr;
DCaggr *float12DcAggr;
DCaggr *float16DcAggr;
DCaggr *floatIntDcAggr[8]; // Can have a packed struct with a weird number of int bytes after a double
DCaggr *intFloat12DcAggr;
DCaggr *intFloat16DcAggr;
Array<MemoryDcAggr> memoryDcAggr;

DCaggr *getDcAggr(Type *type) {
	switch (getSystemVCallingType(type)) {
		case SystemVCallingType::EMPTY:
			if (!emptyDcAggr) {
				emptyDcAggr = dcNewAggr(0, 1);
				dcCloseAggr(emptyDcAggr);
			}
			return emptyDcAggr;
		case SystemVCallingType::INT:
		case SystemVCallingType::INT_INT:
			if (!intDcAggr[type->size]) {
				auto aggr = intDcAggr[type->size] = dcNewAggr(1, type->size);
				
				if (type->size % 8 == 0) {
					dcAggrField(aggr, DC_SIGCHAR_ULONGLONG, 0, type->size / 8);
				}
				else if (type->size % 4 == 0) {
					dcAggrField(aggr, DC_SIGCHAR_UINT, 0, type->size / 4);
				}
				else if (type->size % 2 == 0) {
					dcAggrField(aggr, DC_SIGCHAR_USHORT, 0, type->size / 2);
				}
				else {
					dcAggrField(aggr, DC_SIGCHAR_UCHAR, 0, type->size);
				}
				dcCloseAggr(aggr);
			}
			return intDcAggr[type->size];
		case SystemVCallingType::FLOAT:
			if (type->size == 4) {
				if (!float4DcAggr) {
					float4DcAggr = dcNewAggr(1, 4);
					dcAggrField(float4DcAggr, DC_SIGCHAR_FLOAT, 0, 1);
					dcCloseAggr(float4DcAggr);
				}
				return float4DcAggr;
			} else {
				if (!float8DcAggr) {
					float8DcAggr = dcNewAggr(1, 8);
					dcAggrField(float8DcAggr, DC_SIGCHAR_DOUBLE, 0, 1);
					dcCloseAggr(float8DcAggr);
				}
				return float8DcAggr;
			}
		case SystemVCallingType::INT_FLOAT:
			if (type->size == 12) {
				if (!intFloat12DcAggr) {
					intFloat12DcAggr = dcNewAggr(2, 12);
					dcAggrField(intFloat12DcAggr, DC_SIGCHAR_UINT, 0, 2);
					dcAggrField(intFloat12DcAggr, DC_SIGCHAR_FLOAT, 8, 1);
					dcCloseAggr(intFloat12DcAggr);
				}
				return intFloat12DcAggr;
			} else {
				if (!intFloat16DcAggr) {
					intFloat16DcAggr = dcNewAggr(2, 16);
					dcAggrField(intFloat16DcAggr, DC_SIGCHAR_UINT, 0, 2);
					dcAggrField(intFloat16DcAggr, DC_SIGCHAR_DOUBLE, 8, 1);
					dcCloseAggr(intFloat16DcAggr);
				}
				return intFloat16DcAggr;
			}
		case SystemVCallingType::FLOAT_INT:
			if (!floatIntDcAggr[type->size - 8]) {
				auto aggr = floatIntDcAggr[type->size - 8] = dcNewAggr(2, type->size);
				dcAggrField(aggr, DC_SIGCHAR_DOUBLE, 0, 1);
				dcAggrField(aggr, DC_SIGCHAR_UCHAR, 8, type->size - 8);
				dcCloseAggr(aggr);
			}
			return floatIntDcAggr[type->size - 8];
		case SystemVCallingType::FLOAT_FLOAT:
			if (type->size == 12) {
				if (!float12DcAggr) {
					float12DcAggr = dcNewAggr(1, 12);
					dcAggrField(float12DcAggr, DC_SIGCHAR_FLOAT, 0, 3);
					dcCloseAggr(float12DcAggr);
				}
				return float12DcAggr;
			} else {
				if (!float16DcAggr) {
					float16DcAggr = dcNewAggr(1, 16);
					dcAggrField(float16DcAggr, DC_SIGCHAR_DOUBLE, 0, 2);
					dcCloseAggr(float16DcAggr);
				}
				return float16DcAggr;
			}
		case SystemVCallingType::MEMORY: {
			for (auto &aggr : memoryDcAggr) {
				if (aggr.size == type->size)
					return aggr.aggr;
			}
			DCaggr *aggr = dcNewAggr(1 + type->size >= 3, type->size);
			if (type->size % 8 == 0) {
				dcAggrField(aggr, DC_SIGCHAR_ULONGLONG, 0, type->size / 8);
			}
			else if (type->size % 4 == 0) {
				dcAggrField(aggr, DC_SIGCHAR_UINT, 0, type->size / 4);
			}
			else if (type->size % 2 == 0) {
				dcAggrField(aggr, DC_SIGCHAR_USHORT, 0, type->size / 2);
			}
			else {
				dcAggrField(aggr, DC_SIGCHAR_UCHAR, 0, type->size);
			}

			if (type->size >= 3) {
				// Enforce this aggr being passed in memory with an unaligned field
				dcAggrField(aggr, DC_SIGCHAR_USHORT, 1, 1);
			}

			dcCloseAggr(aggr);
			memoryDcAggr.add({aggr, type->size});
			return aggr;
		}
		case SystemVCallingType::UNKNOWN:
		default:
			assert(false);
			return nullptr;
	}
}

char getSigChar(Type *type) {
	if (type->flavor == TypeFlavor::STRUCT || type->flavor == TypeFlavor::ARRAY || type->flavor == TypeFlavor::STRING) {
		return DC_SIGCHAR_AGGREGATE;
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
		assert(false);
		return DC_SIGCHAR_CHAR;
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

void runFunction(VMState *state, ExprFunction *expr, Ir *caller, DCArgs *dcArgs, u64 *callerStack) {
	auto callerArguments = static_cast<FunctionCall *>(caller->data);

	if (expr->flags & EXPR_FUNCTION_IS_COMPILER) {
		assert(!dcArgs);
		assert(expr->valueOfDeclaration);

		if (std::this_thread::get_id() != mainThread) {
			reportError(caller, "Error: Compiler functions can only be called from the initial thread", STRING_PRINTF(expr->valueOfDeclaration->name->name));
			return;
		}

		if (expr->valueOfDeclaration->name->name == "add_build_file") {
			
			auto name = IR_RUNNER_NEW(ExprStringLiteral);
			name->flavor = ExprFlavor::STRING_LITERAL;
			name->start = {};
			name->end = {};
			name->string = *reinterpret_cast<String *>(callerStack[callerArguments->arguments[1]]);
			name->type = &TYPE_STRING;

			auto load = IR_RUNNER_NEW(ExprLoad);
			load->flavor = ExprFlavor::LOAD;
			load->start = {};
			load->end = {};
			load->file = name;
			load->module = runningDirective->module;

			auto import = IR_RUNNER_NEW(Importer);
			import->import = load;
			
			inferInput.add(InferQueueJob(import, runningDirective->module));
		}
		else if (expr->valueOfDeclaration->name->name == "set_build_options") {
			auto options = *reinterpret_cast<Build_Options *>(callerStack[callerArguments->arguments[1]]);

			if (options.backend != Build_Options::Backend::X64 && options.backend != Build_Options::Backend::LLVM) {
				reportError(caller, "Error: Unrecognized backend %llu in set_build_options", options.backend);
				reportError(runningDirective, "   ..: From #run directive");
				return;
			}

			if (options.llvm_options.count) {
				MiloArray<MiloString> llvmOptionsCopy;
				llvmOptionsCopy.count = options.llvm_options.count;
				llvmOptionsCopy.data = IR_RUNNER_NEW_ARRAY(MiloString, llvmOptionsCopy.count);

				for (u64 i = 0; i < llvmOptionsCopy.count; i++) {
					auto count = options.llvm_options.data[i].count;
					llvmOptionsCopy.data[i].count = count;

					if (count) {
						llvmOptionsCopy.data[i].data = IR_RUNNER_NEW_ARRAY(u8, count);
						memcpy(llvmOptionsCopy.data[i].data, options.llvm_options.data[i].data, count);
					}
					else {
						llvmOptionsCopy.data[i].data = nullptr;
					}
				}

				options.llvm_options = llvmOptionsCopy;
			}

			if (options.output_name.count) {
				MiloString outputNameCopy;
				outputNameCopy.count = options.output_name.count;
				outputNameCopy.data = IR_RUNNER_NEW_ARRAY(u8, outputNameCopy.count);
				memcpy(outputNameCopy.data, options.output_name.data, outputNameCopy.count);
				options.output_name = outputNameCopy;
			}

			if (options.icon_name.count) {
				MiloString iconNameCopy;
				iconNameCopy.count = options.icon_name.count;
				iconNameCopy.data = IR_RUNNER_NEW_ARRAY(u8, iconNameCopy.count);
				memcpy(iconNameCopy.data, options.icon_name.data, iconNameCopy.count);
				options.icon_name = iconNameCopy;
			}

			buildOptions = options;
		}
		else if (expr->valueOfDeclaration->name->name == "get_build_options") {
			*reinterpret_cast<Build_Options *>(callerStack[caller->dest]) = buildOptions;
		}
		else if (expr->valueOfDeclaration->name->name == "get_build_arguments") {
			*reinterpret_cast<MiloArray<MiloString> *>(callerStack[caller->dest]) = buildArguments;
		}
		else {
			reportError(caller,           "Error: Unknown compiler function: %.*s", STRING_PRINTF(expr->valueOfDeclaration->name->name));
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

	bool returnPointer = returnsViaPointer(static_cast<TypeFunction *>(expr->type));
	if (dcArgs) {
		// Pointer return is handled by the callback handler
		// If we are here this is a c call function so there is no context parameter

		for (auto argument : expr->arguments.declarations) {
			auto type = getDeclarationType(argument);

			if (isStoredByPointer(type)) {
				u32 allocate = (type->size + 7) / 8;
				spaceToAllocate += allocate;
				auto address = allocateStack(state, allocate);
				dcbArgAggr(dcArgs, address);
				stack[argument->registerOfStorage] = reinterpret_cast<u64>(address);
			}
			else if (type->flavor == TypeFlavor::FLOAT) {
				*reinterpret_cast<double *>(&stack[argument->registerOfStorage]) = dcbArgDouble(dcArgs);
			}
			else {
				stack[argument->registerOfStorage] = dcbArgULongLong(dcArgs);
			}
		}
	}
	else {

		// C call functions will take the other branch of the if so we always have a context register
		stack[expr->state.contextRegister] = callerStack[callerArguments->arguments[0]];

		for (u32 i = 0; i < expr->arguments.declarations.count; i++) {
			stack[expr->arguments.declarations[i]->registerOfStorage] = callerStack[callerArguments->arguments[i + 1]];
		}
	}

	if (returnPointer) {
		stack[expr->state.returnPointerRegister] = callerStack[caller->dest];
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
				break;
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
			auto arguments = static_cast<FunctionCall *>(op.data);
			if (arguments->function->flags & TYPE_FUNCTION_IS_C_CALL) {
				auto function = reinterpret_cast<void *>(stack[op.a]);

				bool pointerReturn = returnsViaPointer(arguments->function);
				DCaggr *aggrReturn = nullptr;

				dcReset(state->dc);
				
				if (pointerReturn) {
					dcArgPointer(state->dc, (DCpointer)stack[op.dest]);
				} else if (isStoredByPointer(arguments->function->returnTypes[0])) {
					aggrReturn = getDcAggr(arguments->function->returnTypes[0]);
					dcBeginCallAggr(state->dc, aggrReturn);
				}

				// C call function so no context

				for (u64 i = 0; i < op.opSize; i++) {
					auto type = arguments->function->argumentTypes[i];
					auto number = arguments->arguments[i];

					if (isStoredByPointer(type)) {
						dcArgAggr(state->dc, getDcAggr(type), (DCpointer) stack[number]);
					}
					else if (type->flavor == TypeFlavor::FLOAT) {
						dcArgDouble(state->dc, *(double *)&stack[number]);
					}
					else {
						dcArgLongLong(state->dc, stack[number]);
					}
				}

				auto type = arguments->function->returnTypes[0];
				if (pointerReturn) {
					dcCallPointer(state->dc, function);
				} else if (aggrReturn) {
					dcCallAggr(state->dc, function, aggrReturn, (DCpointer)stack[op.dest]);
				} else if (type == &TYPE_VOID) {
					dcCallVoid(state->dc, function);
				}
				else if (type == &TYPE_F32) {
					*(float *)stack[op.dest] = dcCallFloat(state->dc, function);
				}
				else if (type == &TYPE_F64) {
					*(double *)stack[op.dest] = dcCallDouble(state->dc, function);
				}
				else if (type->size == 1) {
					*(char *)stack[op.dest] = dcCallChar(state->dc, function);
				}
				else if (type->size == 2) {
					*(short *)stack[op.dest] = dcCallShort(state->dc, function);
				}
				else if (type->size == 4) {
					*(int *)stack[op.dest] = dcCallInt(state->dc, function);
				}
				else if (type->size == 8) {
					*(long long *)stack[op.dest] = dcCallLongLong(state->dc, function);
				}
				else {
					assert(false);
				}
			}
			else {
				void **context = reinterpret_cast<void **>(stack[arguments->arguments[0]]);

				void *node[2];
				node[0] = *context;
				node[1] = static_cast<void *>(&arguments->stackTrace);

				*context = node;

				runFunction(state, reinterpret_cast<ExprFunction *>(stack[op.a]), &op, nullptr, stack);

				*context = node[0];
			}
			break;
		}
		case IrOp::RETURN: {
			switch (static_cast<SystemVCallingType>(op.opSize)) {
				case SystemVCallingType::INT:
				case SystemVCallingType::FLOAT:
				case SystemVCallingType::INT_INT:
				case SystemVCallingType::INT_FLOAT:
				case SystemVCallingType::FLOAT_INT:
				case SystemVCallingType::FLOAT_FLOAT: {
					auto returnType = static_cast<Type *>(op.data);
					if (isStoredByPointer(returnType)) {
						memcpy(reinterpret_cast<void *>(callerStack[caller->dest]), reinterpret_cast<void *>(stack[op.a]), returnType->size);
					}
					else {
						memcpy(reinterpret_cast<void *>(callerStack[caller->dest]), &stack[op.a], returnType->size);
					}
					break;
				}
				case SystemVCallingType::MEMORY:
				case SystemVCallingType::EMPTY:
					break;
				case SystemVCallingType::UNKNOWN:
					assert(false);
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
		case IrOp::COPY_SRC_OFFSET: {
			memcpy(reinterpret_cast<u8 *>(stack[op.dest]), reinterpret_cast<void *>(stack[op.a] + op.immediate), op.opSize);
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
			auto function = static_cast<ExprFunction *>(op.data);

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
	PROFILE_FUNC();
	switch (type->flavor) {
	case TypeFlavor::ARRAY: {
		assert(!(type->flags & TYPE_ARRAY_IS_DYNAMIC));
		auto arrayType = static_cast<TypeArray *>(type);

		auto array = IR_RUNNER_NEW(ExprArrayLiteral);
		array->flavor = ExprFlavor::ARRAY_LITERAL;
		array->start = start;
		array->end = end;
		array->type = type;
		
		u8 *storage = static_cast<u8 *>(bytes);

		if (type->flags & TYPE_ARRAY_IS_FIXED) {
			array->count = arrayType->count;
		}
		else {
			auto arrayData = static_cast<MiloArray<u8> *>(bytes);

			storage = arrayData->data;
			array->count = arrayData->count;
		}

		array->values = IR_RUNNER_NEW_ARRAY(Expr * , array->count);

		for (u64 i = 0; i < array->count; i++) {
			array->values[i] = getReturnValueFromBytes(start, end, arrayType->arrayOf, storage);

			storage += arrayType->arrayOf->size;
		}

		return array;
	}
	case TypeFlavor::BOOL: {
		auto value = *static_cast<u8 *>(bytes);

		auto literal = IR_RUNNER_NEW(ExprLiteral);
		literal->flavor = ExprFlavor::INT_LITERAL;
		literal->start = start;
		literal->end = end;
		literal->unsignedValue = value;
		literal->type = type;

		return literal;
	}
	case TypeFlavor::ENUM: {
		auto integer = static_cast<TypeEnum *>(type)->integerType;

		u64 value = 0;

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

		auto literal = IR_RUNNER_NEW(ExprLiteral);
		literal->flavor = ExprFlavor::INT_LITERAL;
		literal->start = start;
		literal->end = end;
		literal->unsignedValue = value;
		literal->type = type;

		return literal;
	}
	case TypeFlavor::FLOAT: {
		double value = 0;

		if (type == &TYPE_F32) {
			value = *static_cast<float *>(bytes);
		}
		else if (type == &TYPE_F64) {
			value = *static_cast<double *>(bytes);
		}

		auto literal = IR_RUNNER_NEW(ExprLiteral);
		literal->flavor = ExprFlavor::FLOAT_LITERAL;
		literal->start = start;
		literal->end = end;
		literal->floatValue = value;

		literal->type = type;

		return literal;
	}
	case TypeFlavor::FUNCTION: {
		if (type->flags & TYPE_FUNCTION_IS_C_CALL) {
			assert(false);
		}

		return *static_cast<ExprFunction **>(bytes);
	}
	case TypeFlavor::INTEGER: {
		u64 value = 0;

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

		auto literal = IR_RUNNER_NEW(ExprLiteral);
		literal->flavor = ExprFlavor::INT_LITERAL;
		literal->start = start;
		literal->end = end;
		literal->unsignedValue = value;

		literal->type = type;

		return literal;
	}
	case TypeFlavor::POINTER: {
		auto value = *static_cast<u64 *>(bytes);

		auto literal = IR_RUNNER_NEW(ExprLiteral);
		literal->flavor = ExprFlavor::INT_LITERAL;
		literal->start = start;
		literal->end = end;
		literal->unsignedValue = value;
		literal->type = type;

		return literal;
	}
	case TypeFlavor::STRING: {
		auto value = *static_cast<String *>(bytes);

		auto literal = IR_RUNNER_NEW(ExprStringLiteral);
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

		auto literal = IR_RUNNER_NEW(ExprStructLiteral);
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

		auto literal = IR_RUNNER_NEW(ExprLiteral);
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
	char argumentsStore[sizeof(FunctionCall) + sizeof(u32)];
	u64 argumentData[2] = {};

	runningDirective = directive;

	auto function = static_cast<ExprFunction *>(directive->function);

	Ir dummyOp;
	dummyOp.op = IrOp::CALL;
	FunctionCall *arguments = reinterpret_cast<FunctionCall *>(argumentsStore);
	arguments->function = static_cast<TypeFunction *>(function->type);

	auto returnType = getDeclarationType(function->returns.declarations[0]);
	void *returnPointer = malloc(returnType->size);

	argumentData[0] = reinterpret_cast<u64>(returnPointer);

	auto contextPointer = malloc(TYPE_CONTEXT.size);
	assert(TYPE_CONTEXT.defaultValue);
	createRuntimeValue(TYPE_CONTEXT.defaultValue, contextPointer);

	arguments->arguments[0] = 1;
	argumentData[1] = reinterpret_cast<u64>(contextPointer);


	dummyOp.data = arguments;
	dummyOp.dest = 0;

	runFunction(state, function, &dummyOp, nullptr, argumentData);

	auto result = getReturnValueFromBytes(function->start, function->end, returnType, returnPointer);

	free(returnPointer);
	free(contextPointer);

	return result;
}
	

char cCallCallback(DCCallback *pcb, DCArgs *args, DCValue *result, void *userdata) {
	(void)pcb;
	auto function = static_cast<ExprFunction *>(userdata);
	auto functionType = static_cast<TypeFunction *>(function->type);

	u64 callerStack[2];
	char returnBuffer[16];
	
	bool pointerReturn = returnsViaPointer(functionType);
	bool aggrReturn = false;
	
	callerStack[1] = (u64)&returnBuffer[0];
	if (pointerReturn) {
		callerStack[1] = (u64)dcbArgPointer(args);
	} else {
		aggrReturn = isStoredByPointer(functionType->returnTypes[0]);
	}

	Ir dummyOp;
	dummyOp.op = IrOp::CALL;
	FunctionCall arguments;
	arguments.function = functionType;
	dummyOp.data = &arguments;
	dummyOp.dest = 1;
	dummyOp.opSize = 0;

	VMState state;

	initVMState(&state);
	runFunction(&state, function, &dummyOp, args, callerStack);
	deinitVMState(&state);

	if (pointerReturn) {
		result->L = callerStack[1];
	} 
	else if (aggrReturn) {
		dcbReturnAggr(args, result, returnBuffer);
		return 'A';
	}
	else {
		memcpy(result, returnBuffer, functionType->returnTypes[0]->size);
	}

	return getSigChar(functionType->returnTypes[0]);
}


void *createCCallFunction(ExprFunction *function) {
	assert(function->returns.declarations.count == 1);
	
	Array<char> signature;
	Array<DCaggr *> aggrs;

	auto functionType = static_cast<TypeFunction *>(function->type);
	bool pointerReturn = returnsViaPointer(functionType);
	
	if (pointerReturn) {
		signature.add(DC_SIGCHAR_POINTER);
	} 

	for (u32 i = 0; i < functionType->argumentCount; i++) {
		auto type = functionType->argumentTypes[i];
		if (isStoredByPointer(type)) {
			signature.add(DC_SIGCHAR_AGGREGATE);
			aggrs.add(getDcAggr(type));
		}
		else if (type->flavor == TypeFlavor::FLOAT) {
			signature.add(DC_SIGCHAR_DOUBLE);
		}
		else {
			signature.add(DC_SIGCHAR_LONGLONG);
		}
	}

	signature.add(')');

	auto returnType = functionType->returnTypes[0];
	if (pointerReturn) {
		signature.add(DC_SIGCHAR_POINTER);
	}
	else if (isStoredByPointer(functionType->returnTypes[0])) {
		signature.add(DC_SIGCHAR_AGGREGATE);
		aggrs.add(getDcAggr(functionType->returnTypes[0]));
	}
	else if (returnType == &TYPE_VOID) {
		signature.add(DC_SIGCHAR_VOID);
	}
	else if (returnType == &TYPE_F32) {
		signature.add(DC_SIGCHAR_FLOAT);
	} 
	else if (returnType == &TYPE_F64) {
		signature.add(DC_SIGCHAR_DOUBLE);
	}
	else if (returnType->size == 1) {
		signature.add(DC_SIGCHAR_CHAR);
	}
	else if (returnType->size == 2) {
		signature.add(DC_SIGCHAR_SHORT);
	}
	else if (returnType->size == 4) {
		signature.add(DC_SIGCHAR_INT);
	}
	else if (returnType->size == 8) {
		signature.add(DC_SIGCHAR_LONGLONG);
	}
	else {
		assert(false);
	}

	signature.add(0);

	return dcbNewCallback2(signature.storage, cCallCallback, function, aggrs.storage);
}