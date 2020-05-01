#pragma once

#include "Ast.h"
#include "WorkQueue.h"

extern Block globalBlock;


inline Declaration *findDeclaration(Block *block, String name, u64 *index, bool *yield, u64 usingYieldLimit = -1) {
	PROFILE_FUNC_DATA(block == &globalBlock ? "global" : "local");
	if (usingYieldLimit == -1) {
		usingYieldLimit = block->declarations.count;
	}

	*yield = false;

	for (*index = 0; *index < block->declarations.count; (*index)++) {
		auto declaration = block->declarations[*index];
		if ((declaration->flags & DECLARATION_IS_USING) && !(declaration->flags & DECLARATION_USING_IS_RESOLVED) && declaration->indexInBlock < usingYieldLimit) {
			if (block != &globalBlock) { // Optimization, don't yield if we encounter a using at global scope, since there are no outer scopes we could erroneously start resoving in
				*yield = true;
				return nullptr;
			}
		}
		else {
			if (declaration->name == name) {
				return declaration;
			}
		}
	}

	return nullptr;
}

struct DeclarationPack {
	u64 count; // If the count is zero we are submitting a function

	union {
		Declaration **declarations;
		Declaration *declaration;
		Expr *expr;
	} data;
};

inline DeclarationPack makeDeclarationPack(Declaration *declaration) {
	DeclarationPack result;
	result.count = 1;

	result.data.declaration = declaration;

	return result;
}


inline DeclarationPack makeDeclarationPack(u64 count, Declaration **declarations) {
	DeclarationPack result;
	result.count = count;

	assert(count); // If count is zero this should be a function pack

	if (count == 1) {
		result.data.declaration = *declarations;
	}
	else {
		result.data.declarations = declarations;
	}

	return result;
}

inline DeclarationPack makeDeclarationPack(Expr *expr) {
	DeclarationPack result;
	result.count = 0;

	result.data.expr = expr;
	return result;
}

inline DeclarationPack makeStopSignal() {
	return { 0, nullptr };
}


extern WorkQueue<DeclarationPack> inferQueue;

void runInfer();