#pragma once

#include "Ast.h"
#include "WorkQueue.h"

inline u64 totalDeclarations;
inline u64 totalFunctions;
inline u64 totalTypesSized;
inline u64 totalInfers;
inline u64 totalSizes;

enum class DeclarationPackType : u8 {
	BLOCK, 
	GLOBAL_DECLARATION, 
	EXPRESSION
};


inline MPMCWorkQueue<Declaration *> inferQueue;

void runInfer();