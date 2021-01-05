#pragma once

#include "CompilerMain.h"
#include "WorkQueue.h"

inline MPMCWorkQueue<struct FileInfo *> parserQueue;

void runParser();