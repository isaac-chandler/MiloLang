#include "Ast.h"

typedef struct DCCallVM_    DCCallVM;

struct VMState {
	DCCallVM *dc;

	struct StackNode *stackAllocator;
};

void *createCCallFunction(ExprFunction *function);

void createRuntimeValue(Expr *value, void *dest);

void initVMState(VMState *state);

void deinitVMState(VMState *state);

Expr *runFunctionRoot(VMState *state, ExprFunction *expr, Module *module);