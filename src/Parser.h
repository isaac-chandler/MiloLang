#pragma once

#include "WorkQueue.h"

inline MPMCWorkQueue<struct FileInfo *> parserQueue;

void runParser();