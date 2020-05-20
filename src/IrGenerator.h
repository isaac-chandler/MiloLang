#pragma once

#include "WorkQueue.h"
#include "Ast.h"

inline ExprFunction *removeFunction = nullptr;

extern MPSCWorkQueue<ExprFunction *> irGeneratorQueue;

void runIrGenerator();