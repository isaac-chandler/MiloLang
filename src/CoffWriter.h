#pragma once

#include "Ast.h"
#include "WorkQueue.h"

struct CoffJob {
	union {
		Declaration *declaration;
		ExprFunction *function;
	};
	bool isFunction;
};

extern WorkQueue<CoffJob> coffWriterQueue;

void runCoffWriter();