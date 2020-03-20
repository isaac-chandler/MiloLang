#pragma once

#include "Ast.h"
#include "WorkQueue.h"

extern WorkQueue<ExprFunction *> coffWriterQueue;

void runCoffWriter();