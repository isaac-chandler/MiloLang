#pragma once

#include "WorkQueue.h"
#include "Ast.h"

extern WorkQueue<ExprFunction *> irGeneratorQueue;

void runIrGenerator();