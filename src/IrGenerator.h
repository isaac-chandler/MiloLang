#pragma once

#include "WorkQueue.h"
#include "Ast.h"

inline ExprFunction *removeFunction = nullptr;

inline MPSCWorkQueue<ExprFunction *> irGeneratorQueue;

void runIrGenerator();

void runLlvm();