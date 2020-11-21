#pragma once

#include "WorkQueue.h"
#include "Ast.h"

inline ExprFunction *removeFunction = nullptr;
inline ExprFunction *stringsEqualFunction = nullptr;

inline MPMCWorkQueue<ExprFunction *> irGeneratorQueue;

void runIrGenerator();

inline std::mutex startLlvmLock;
inline std::condition_variable startLlvm;

void runLlvm();