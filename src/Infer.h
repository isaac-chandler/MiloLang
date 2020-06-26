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

struct DeclarationPack {
	DeclarationPackType type;

	union {
		Block *block;
		Declaration *declaration;
		Expr *expr;
	} data;
};

inline DeclarationPack makeDeclarationPack(Declaration *declaration) {
	DeclarationPack result;
	result.type = DeclarationPackType::GLOBAL_DECLARATION;

	result.data.declaration = declaration;

	return result;
}


inline DeclarationPack makeDeclarationPack(Block *block) {
	DeclarationPack result;
	result.type = DeclarationPackType::BLOCK;

	result.data.block = block;

	return result;
}

inline DeclarationPack makeDeclarationPack(Expr *expr) {
	DeclarationPack result;
	result.type = DeclarationPackType::EXPRESSION;

	result.data.expr = expr;
	return result;
}

inline DeclarationPack makeStopSignal() {
	return { DeclarationPackType::EXPRESSION, nullptr };
}


inline MPMCWorkQueue<DeclarationPack> inferQueue;

void runInfer();