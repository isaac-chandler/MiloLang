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
	FUNCTION_IR, 
	LOAD_COMPLETE
};

struct InferQueueJob {
	InferJobType type;

	union {
		Declaration *declaration;
		Importer *importer;
		ExprFunction *function;
		s64 fileUid;
	};

	InferQueueJob(Declaration *declaration) : type(InferJobType::GLOBAL_DECLARATION), declaration(declaration) {}
	InferQueueJob(Importer *importer) : type(InferJobType::IMPORTER), importer(importer) {}
	InferQueueJob(ExprFunction *function) : type(InferJobType::FUNCTION_IR), function(function) {}
	explicit InferQueueJob(s64 fileUid) : type(InferJobType::LOAD_COMPLETE), fileUid(fileUid) {}
};

inline MPMCWorkQueue<InferQueueJob> inferQueue;

void runInfer();