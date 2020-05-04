#pragma once

#include "WorkQueue.h"
#include "Ast.h"

extern MPSCWorkQueue<ExprFunction *> irGeneratorQueue;

void runIrGenerator();