#pragma once

#include "WorkQueue.h"
#include "Ast.h"

inline u32 irInstructions;

inline ExprFunction *removeFunction = nullptr;
inline ExprFunction *stringsEqualFunction = nullptr;

inline MPMCWorkQueue<ExprFunction *> irGeneratorQueue;

struct SystemVCallingState {
	int intRegsRemaining;
	int floatRegsRemaining;
};

SystemVCallingState initSystemVCallingState(TypeFunction *function);
SystemVCallingType passSystemVParameter(SystemVCallingState *state, Type *type);
bool returnsViaPointer(TypeFunction *function);

inline bool isStoredByPointer(Type *type) {
	return type->flavor == TypeFlavor::ARRAY || type->flavor == TypeFlavor::STRUCT || type->flavor == TypeFlavor::STRING;
}

inline bool declarationIsStoredByPointer(Declaration *declaration) {
	return isStoredByPointer(getDeclarationType(declaration)) || (declaration->flags & DECLARATION_IS_POINTED_TO) || (declaration->enclosingScope->flavor == BlockFlavor::GLOBAL);
}

void runIrGenerator();

void runLlvm();