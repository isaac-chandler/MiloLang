#pragma once

#include "Ast.h"
#include "WorkQueue.h"


inline MPMCWorkQueue<CoffJob> coffWriterQueue;

void runCoffWriter();