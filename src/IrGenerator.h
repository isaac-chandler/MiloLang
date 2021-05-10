#pragma once

#include "WorkQueue.h"
#include "Ast.h"

inline u32 irInstructions;

inline ExprFunction *removeFunction = nullptr;
inline ExprFunction *stringsEqualFunction = nullptr;

inline MPMCWorkQueue<ExprFunction *> irGeneratorQueue;


inline bool isStoredByPointer(Type *type) {
	return type->flavor == TypeFlavor::ARRAY || type->flavor == TypeFlavor::STRUCT || type->flavor == TypeFlavor::STRING;
}

void runIrGenerator();

void runLlvm();