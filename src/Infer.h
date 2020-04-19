#pragma once

#include "Ast.h"
#include "WorkQueue.h"

extern Block globalBlock;


inline Declaration *findDeclaration(Block *block, String name, u64 *index) {
	for (*index = 0; *index < block->declarations.count; (*index)++) {
		auto declaration = block->declarations[*index];
		if (declaration->name == name) {
			return declaration;
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