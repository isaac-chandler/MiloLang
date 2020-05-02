#pragma once

#include "Basic.h"
#include "Array.h"
#include "String.h"
#include "CompilerMain.h"

#define DECLARATION_IS_CONSTANT       0x1
#define DECLARATION_IS_UNINITIALIZED  0x2
#define DECLARATION_IS_ITERATOR       0x4
#define DECLARATION_IS_ITERATOR_INDEX 0x8
#define DECLARATION_VALUE_IS_READY    0x10
#define DECLARATION_TYPE_IS_READY     0x20
#define DECLARATION_IS_ARGUMENT       0x40
#define DECLARATION_HAS_STORAGE       0x80
#define DECLARATION_IS_STRUCT_MEMBER  0x100
#define DECLARATION_IS_USING          0x200
#define DECLARATION_IMPORTED_BY_USING 0x400
#define DECLARATION_USING_IS_RESOLVED 0x800
#define DECLARATION_MARKED_AS_USING   0x1000


struct Declaration {
	CodeLocation start;
	EndLocation end;
	String name;
	union {
		struct Expr *type;
		Declaration *import;
	};

	struct Expr *initialValue;

	struct Block *enclosingScope;
	u64 indexInBlock;

	struct InferJob *inferJob;

	union Symbol *symbol;
	u64 physicalStorage;

	u64 flags = 0;

};


#define BLOCK_IS_ARGUMENTS 0x1
#define BLOCK_IS_COMPLETE 0x2
#define BLOCK_IS_LOOP 0x4
#define BLOCK_IS_STRUCT 0x8

#define BLOCK_HASHTABLE_MIN_COUNT 32

struct Block {
	Array<Declaration *> declarations;
	Array<Declaration *> usings;

	struct BlockEntry *table = nullptr;
	u64 tableCapacity;

	Block *parentBlock = nullptr;
	u64 indexInParent;
	u64 flags = 0;
};

inline Block globalBlock;

Declaration *findInBlock(Block *block, String name);

inline Declaration *findDeclarationNoYield(Block *block, String name, u64 *index) {
	if (block->table) {
		return findInBlock(block, name);
	}
	else {
		for (*index = 0; *index < block->declarations.count; (*index)++) {
			auto declaration = block->declarations[*index];

			if (declaration->name == name) {
				return declaration;
			}
		}
	}

	return nullptr;
}

inline bool checkForRedeclaration(Block *block, Declaration *declaration, struct Expr *using_) {
	assert(block);
	assert(declaration);

	if (declaration->name.length) { // Multiple zero length names are used in the arguments block for function prototypes with unnamed arguments
		u64 index;
		auto previous = findDeclarationNoYield(block, declaration->name, &index);

		if (previous) {
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

	return true;
}

void addToTable(Block *block, Declaration *declaration);
void initTable(Block *block);

inline void addDeclarationToBlockUnchecked(Block *block, Declaration *declaration) {
	declaration->indexInBlock = block->declarations.count;
	block->declarations.add(declaration);
	declaration->enclosingScope = block;

	if (declaration->flags & DECLARATION_IS_USING) {
		block->usings.add(declaration);
	}

	if (block->table) {
		addToTable(block, declaration);
	}
	else if (block->declarations.count == BLOCK_HASHTABLE_MIN_COUNT) {
		initTable(block);
	}
}

inline bool addDeclarationToBlock(Block *block, Declaration *declaration) {
	if (!checkForRedeclaration(block, declaration, nullptr)) {
		return false;
	}

	addDeclarationToBlockUnchecked(block, declaration);

	return true;
}

inline Declaration *findDeclaration(Block *block, String name, u64 *index, bool *yield, u64 usingYieldLimit = -1) {
	if (usingYieldLimit == -1) {
		usingYieldLimit = block->declarations.count;
	}

	for (auto declaration : block->usings) {
		if (declaration->indexInBlock < usingYieldLimit) {
			assert((declaration->flags & DECLARATION_IS_USING) && !(declaration->flags & DECLARATION_USING_IS_RESOLVED));
			*yield = true;
			return nullptr;
		}
	}

	*yield = false;

	return findDeclarationNoYield(block, name, index);
}