#pragma once

#include "WorkQueue.h"
#include "Ast.h"

inline ExprFunction *removeFunction = nullptr;
inline ExprFunction *stringsEqualFunction = nullptr;

inline MPSCWorkQueue<ExprFunction *> irGeneratorQueue;

void runIrGenerator();

void runLlvm();