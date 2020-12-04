#pragma once

#include "Ast.h"
#include "WorkQueue.h"

inline u64 totalImporters;
inline u64 totalDeclarations;
inline u64 totalFunctions;
inline u64 totalTypesSized;
inline u64 totalInfers;
inline u64 totalSizes;

enum class InferJobType : u8 {
	GLOBAL_DECLARATION,
	IMPORTER, 
	FUNCTION_IR
};

struct InferQueueJob {
	InferJobType type;

	union {
		ExprFunction *function;
		Declaration *declaration;
		Importer *importer;
	};

	InferQueueJob(Importer *importer) : type(InferJobType::IMPORTER), importer(importer) {}
	InferQueueJob(Declaration *declaration) : type(InferJobType::GLOBAL_DECLARATION), declaration(declaration) {}
	InferQueueJob(ExprFunction *function) : type(InferJobType::FUNCTION_IR), function(function) {}
};

inline MPMCWorkQueue<InferQueueJob> inferQueue;

void runInfer();