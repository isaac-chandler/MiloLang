#pragma once

#include "Basic.h"
#include "Array.h"
#include "String.h"
#include "CompilerMain.h"

#define DECLARATION_IS_CONSTANT        0x0'0001
#define DECLARATION_IS_UNINITIALIZED   0x0'0002
#define DECLARATION_IS_ITERATOR        0x0'0004
#define DECLARATION_IS_ITERATOR_INDEX  0x0'0008
#define DECLARATION_VALUE_IS_READY     0x0'0010
#define DECLARATION_TYPE_IS_READY      0x0'0020
#define DECLARATION_IS_ARGUMENT        0x0'0040
#define DECLARATION_HAS_STORAGE        0x0'0080
#define DECLARATION_IS_STRUCT_MEMBER   0x0'0100
#define DECLARATION_IS_USING           0x0'0200
#define DECLARATION_IMPORTED_BY_USING  0x0'0400
#define DECLARATION_USING_IS_RESOLVED  0x0'0800
#define DECLARATION_MARKED_AS_USING    0x0'1000
#define DECLARATION_IS_IMPLICIT_IMPORT 0x0'2000
#define DECLARATION_IS_ENUM_VALUE      0x0'4000
#define DECLARATION_IS_RETURN          0x0'8000
#define DECLARATION_IS_MUST            0x1'0000
#define DECLARATION_IS_IN_COMPOUND     0x2'0000


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

	struct InferJob *inferJob = nullptr;

	union Symbol *symbol;
	u64 physicalStorage;

	u64 flags = 0;

};


#define BLOCK_IS_ARGUMENTS 0x1
#define BLOCK_IS_COMPLETE 0x2
#define BLOCK_IS_LOOP 0x4
#define BLOCK_IS_STRUCT 0x8
#define BLOCK_IS_RETURNS 0x10

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

inline Declaration *findDeclarationIncludeImplictImports(Block *block, String name, u64 *index) {
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

inline Declaration *findDeclarationNoYield(Block *block, String name, u64 *index) {
	auto declaration = findDeclarationIncludeImplictImports(block, name, index);

	if (!declaration)
		return nullptr;

	return declaration->flags & DECLARATION_IS_IMPLICIT_IMPORT ? nullptr : declaration;
}

inline bool checkForRedeclaration(Block *block, Declaration *declaration, struct Expr *using_) {
	assert(block);
	assert(declaration);

	if (declaration->name.length) { // Multiple zero length names are used in the arguments block for function prototypes with unnamed arguments
		u64 index;
		auto previous = findDeclarationIncludeImplictImports(block, declaration->name, &index);

		if (previous) {
			if (previous->flags & DECLARATION_IS_IMPLICIT_IMPORT) {
				if (using_) {
					reportError(using_, "Error: Attempt to import a variable that was used previously in the scope", STRING_PRINTF(declaration->name));
					reportError(previous, "   ..: Here is the usage");
					reportError(declaration, "   ..: Here is the location it was imported from");
				}
				else {
					reportError(declaration, "Error: Attempt to redeclare a variable that was used previously in the scope");
					reportError(previous, "   ..: Here is the usage");
				}
			} else if (using_) {
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

inline bool addImplicitImport(Block *block, String name, CodeLocation *start, EndLocation *end) {
	PROFILE_FUNC();
	if (block->flags & (BLOCK_IS_ARGUMENTS | BLOCK_IS_STRUCT | BLOCK_IS_RETURNS)) return true;

	u64 index;
	auto declaration = findDeclarationIncludeImplictImports(block, name, &index);

	if (declaration) {
		if (declaration->flags & DECLARATION_IS_IMPLICIT_IMPORT) {
			return true; 
		}
		else {
			// @Incomplete Keep track of the using that imported something so we can report if the redeclaration is from a using
			reportError(declaration, "Error: Attempt to redeclare a variable that was used previously in the scope");
			reportError(start, end,  "   ..: Here is the usage");
			return false;
		}
	}
	else {
		Declaration *import = new Declaration;
		import->name = name;
		import->start = *start;
		import->end = *end;
		import->flags |= DECLARATION_IS_IMPLICIT_IMPORT;
		addDeclarationToBlockUnchecked(block, import);

		return true;
	}
}