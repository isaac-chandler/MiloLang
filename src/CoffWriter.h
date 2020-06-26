#pragma once

#include "Ast.h"
#include "WorkQueue.h"


extern MPSCWorkQueue<CoffJob> coffWriterQueue;

void runCoffWriter();