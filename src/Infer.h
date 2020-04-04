#pragma once

#include "Ast.h"
#include "WorkQueue.h"

extern Block globalBlock;


struct DeclarationPack {
	u64 count; // If the count is zero we are submitting a function

	union {
		Declaration **declarations;
		Declaration *declaration;
		ExprFunction *function;
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

inline DeclarationPack makeDeclarationPack(ExprFunction *function) {
	DeclarationPack result;
	result.count = 0;

	result.data.function = function;
	return result;
}

inline DeclarationPack makeStopSignal() {
	return { 0, nullptr };
}


extern WorkQueue<DeclarationPack> inferQueue;

void runInfer();

inline Expr *getDefaultValueForType(Type *type) {

}