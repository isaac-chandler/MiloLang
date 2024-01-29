#pragma once

#include "Ast.h"
#include "WorkQueue.h"

inline u64 totalInferImporters;
inline u64 totalInferRuns;
inline u64 totalInitImperativeDeclarations;
inline u64 totalInferDeclarationValues;
inline u64 totalInferFunctionHeaders;
inline u64 totalInferFunctionBodies;
inline u64 totalInferStructSizes;
inline u64 totalInferEnumSizes;
inline u64 totalInferArraySizes;
inline u64 totalDeclarations;
inline u64 totalInferredDeclarations;
inline u64 totalFunctions;
inline u64 totalSizes;
inline u64 totalImporters;
inline u64 totalRuns;
inline u64 totalFlattenedInfers;
inline u64 totalInferIterations;

enum class InferJobType : u8 {
	GLOBAL_DECLARATION,
	IMPORTER, 
	EXPR, 
	LOAD_COMPLETE, 
	DONE
};

struct InferQueueJob {
	InferJobType type;

	union {
		Declaration *declaration;
		Importer *importer;
		Expr *expr;
		s64 fileUid;
	};

	struct Module *module;

	InferQueueJob(InferJobType type) : type(type) {}
	InferQueueJob(Declaration *declaration, struct Module *module) : type(InferJobType::GLOBAL_DECLARATION), declaration(declaration), module(module) {}
	InferQueueJob(Importer *importer, struct Module *module) : type(InferJobType::IMPORTER), importer(importer), module(module) {}
	InferQueueJob(Expr *expr, struct Module *module) : type(InferJobType::EXPR), expr(expr), module(module) {}
	InferQueueJob(s64 fileUid, struct Module *module) : type(InferJobType::LOAD_COMPLETE), fileUid(fileUid), module(module) {}
};

inline MPMCWorkQueue<InferQueueJob> inferQueue;
inline Array<InferQueueJob> inferInput;

void runInfer(String inputFile);