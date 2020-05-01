#pragma once

#include "Ast.h"
#include "CompilerMain.h"

inline bool checkForRedeclaration(Block *block, Declaration *declaration, Expr *using_) {
	assert(block);
	assert(declaration);

	if (declaration->name.length) { // Multiple zero length names are used in the arguments block for function prototypes with unnamed arguments
		for (auto previous : block->declarations) {
			if (previous->name == declaration->name) {
				if (using_) {
					reportError(using_, "Error: Cannot import variable '%.*s' into scope, it already exists there", STRING_PRINTF(declaration->name));
					reportError(previous, "   ..: Here is the location it was declared");
					reportError(declaration, "   ..: Here is the location it was imported from");
				}
				else {
					reportError(declaration, "Error: Cannot redeclare variable '%.*s' within the same scope", STRING_PRINTF(declaration->name));
					reportError(previous, "   ..: Here is the location it was declared");
				}
				return false;
			}
		}
	}

	return true;
}

inline bool addDeclarationToBlock(Block *block, Declaration *declaration) {
	if (!checkForRedeclaration(block, declaration, nullptr)) {
		return false;
	}

	declaration->indexInBlock = block->declarations.count;
	block->declarations.add(declaration);
	declaration->enclosingScope = block;

	return true;
}

void parseFile(struct FileInfo *filename);