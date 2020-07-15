#pragma once

#include "Ast.h"
#include "WorkQueue.h"

inline u64 totalImporters;
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

struct InferQueueJob {
	bool isImporter;

	union {
		Declaration *declaration;
		Importer *importer;
	};

	InferQueueJob(Importer *importer) : isImporter(true), importer(importer) {}
	InferQueueJob(Declaration *declaration) : isImporter(false), declaration(declaration) {}
};

inline MPMCWorkQueue<InferQueueJob> inferQueue;

void runInfer();