#pragma once

#include "Basic.h"
#include "Array.h"
#include "String.h"
#include "CompilerMain.h"

#define IMPORTER_IS_CONSTANT 0x1
#define IMPORTER_IS_COMPLETE 0x2
#define IMPORTER_IS_IMPORTED 0x4

#define DECLARATION_IS_CONSTANT         0x0'0001
#define DECLARATION_IS_UNINITIALIZED    0x0'0002

// These flags denote if the declaration is a for loop it or it_index
#define DECLARATION_IS_ITERATOR         0x0'0004 // This is also set for while loop labels
#define DECLARATION_IS_ITERATOR_INDEX   0x0'0008

// Set when the initial value of a declaration is completely inferred
#define DECLARATION_VALUE_IS_READY      0x0'0010

// Set when the type of a declaration is completely inferred
#define DECLARATION_TYPE_IS_READY       0x0'0020

#define DECLARATION_IS_ARGUMENT         0x0'0040

// Set when a declaration has been allocated space in the output executable
#define DECLARATION_HAS_STORAGE         0x0'0080

#define DECLARATION_IMPORTED_BY_USING   0x0'0100

// This is the actual variable that the using refers to
// This is the actual variable that the using refers to
#define DECLARATION_MARKED_AS_USING     0x0'0200

// Set on declarations that are silently inserted when a variable from some outer scopes is used so that later usings will cause an error if they try to override it
#define DECLARATION_IS_IMPLICIT_IMPORT  0x0'0400 // @Speed @Cleanup This is a very costly mechanism for this relatively uncommon check

#define DECLARATION_IS_ENUM_VALUE       0x0'0800

#define DECLARATION_IS_RETURN           0x0'1000

// Set if a return value declaration is marked as #must
#define DECLARATION_IS_MUST             0x0'2000

// Set if a declaration is on the left hand side of a := comma assignment
#define DECLARATION_IS_IN_COMPOUND      0x0'4000

#define DECLARATION_IS_VARARGS          0x0'8000

// Set if a declaration is initialized using the x := .. or x :: .. syntax
#define DECLARATION_IS_EXPLICIT_DEFAULT 0x1'0000

#define DECLARATION_IS_RUN_RETURN 0x2'0000

struct Declaration {
	CodeLocation start;
	EndLocation end;
	u32 flags = 0;
	String name;
	union {
		struct Expr *type;
		Declaration *import;
	};

	struct Expr *initialValue;

	struct Block *enclosingScope;
	u32 serial;

	struct DeclarationJob *inferJob = nullptr;

	Array<struct SubJob *> sleepingOnMyType;
	Array<struct SubJob *> sleepingOnMyValue;

	union {
		struct {
			union Symbol *symbol;
			u32 physicalStorage;
		};

		class llvm::Value *llvmStorage;
	};

	void *runtimeValue = nullptr;
};


#define BLOCK_IS_ARGUMENTS 0x1

// Set when a block is fully parsed so that we don't skip through it in identifier resolving until then
#define BLOCK_IS_QUEUED 0x2

#define BLOCK_IS_LOOP 0x4
#define BLOCK_IS_STRUCT 0x8
#define BLOCK_IS_RETURNS 0x10

#define BLOCK_HASHTABLE_MIN_COUNT 32

struct Importer {
	u32 serial;
	u8 flags = 0;

	Block *enclosingScope = nullptr;
	Expr *import;
	Expr *structAccess = nullptr;

};

struct Block {
	Array<Declaration *> declarations;
	Array<Importer *> importers;

	struct BlockEntry *table = nullptr;
	u32 tableCapacity;
	u32 serial;

	Block *parentBlock = nullptr;
	u8 flags = 0;

	Array<struct SubJob *> sleepingOnMe;
};

inline Block globalBlock;

Declaration *findInBlock(Block *block, String name);

inline Declaration *findDeclarationIncludeImplictImports(Block *block, String name) {
	if (block->table) {
		return findInBlock(block, name);
	}
	else {
		for (auto declaration : block->declarations) {
			if (declaration->name == name) {
				return declaration;
			}
		}
	}

	return nullptr;
}

inline Declaration *findDeclarationNoYield(Block *block, String name) {
	PROFILE_FUNC();
	auto declaration = findDeclarationIncludeImplictImports(block, name);

	if (!declaration)
		return nullptr;

	return declaration->flags & DECLARATION_IS_IMPLICIT_IMPORT ? nullptr : declaration;
}

bool replaceInTable(Block *block, Declaration *old, Declaration *declaration);

inline void replaceDeclaration(Block *block, Declaration *&old, Declaration *declaration) {
	old = declaration;

	if (block->table) {
		replaceInTable(block, old, declaration);
	}

	declaration->enclosingScope = block;
}

inline bool checkForRedeclaration(Block *block, Declaration *declaration, struct Expr *using_) {
	PROFILE_FUNC();
	assert(block);
	assert(declaration);

	if (declaration->name.length) { // Multiple zero length names are used in the arguments block for function prototypes with unnamed 
		auto previous = findDeclarationIncludeImplictImports(block, declaration->name);

		if (previous) {
			if (previous->flags & DECLARATION_IS_IMPLICIT_IMPORT) {
				if (using_) {
					reportError(using_, "Error: Attempt to import a variable that was used previously in the scope", STRING_PRINTF(declaration->name));
					reportError(previous, "   ..: Here is the usage");
					reportError(previous->import, "   ..: Here is the declaration of the previous usage");
					reportError(declaration, "   ..: Here is the location it was imported from");
				}
				else {
					reportError(declaration, "Error: Attempt to redeclare a variable that was used previously in the scope");
					reportError(previous, "   ..: Here is the usage");
					reportError(previous->import, "   ..: Here is the declaration of the previous usage");
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

inline u32 globalSerial = 0;

inline void addImporterToBlock(Block *block, Importer *importer, s64 serial = -1) {
	assert(!(block->flags & (BLOCK_IS_ARGUMENTS | BLOCK_IS_RETURNS)));

	importer->serial = serial == -1 ? globalSerial++ : static_cast<u32>(serial);
	importer->enclosingScope = block;

	block->importers.add(importer);
}

inline void putDeclarationInBlock(Block *block, Declaration *declaration) {
	PROFILE_FUNC();
	block->declarations.add(declaration);

	if (block->table) {
		addToTable(block, declaration);
	}
	else if (block->declarations.count == BLOCK_HASHTABLE_MIN_COUNT) {
		initTable(block);
	}
}

inline void addDeclarationToBlockUnchecked(Block *block, Declaration *declaration, s64 serial = -1) {
	if (serial == -1) {
		if (block->flags & BLOCK_IS_ARGUMENTS | BLOCK_IS_RETURNS) {
			declaration->serial = block->declarations.count;
		}
		else {
			declaration->serial = globalSerial++;
		}
	}
	else {
		declaration->serial = serial;
	}

	declaration->enclosingScope = block;
	
	putDeclarationInBlock(block, declaration);
}

inline bool addDeclarationToBlock(Block *block, Declaration *declaration, s64 index = -1) {
	if (!checkForRedeclaration(block, declaration, nullptr)) {
		return false;
	}

	addDeclarationToBlockUnchecked(block, declaration, index);

	return true;
}

inline Declaration *findDeclaration(Block *block, String name, bool *yield, u64 usingYieldLimit = UINT64_MAX) {
	PROFILE_FUNC();

	for (auto importer : block->importers) {
		if (importer->serial < usingYieldLimit && !(importer->flags & IMPORTER_IS_COMPLETE)) {
			*yield = true;
			return nullptr;
		}
	}

	*yield = false;

	return findDeclarationNoYield(block, name);
}

inline bool addImplicitImport(Block *block, Declaration *old, CodeLocation *start, EndLocation *end) {
	PROFILE_FUNC();
	if (block->flags & (BLOCK_IS_ARGUMENTS | BLOCK_IS_STRUCT | BLOCK_IS_RETURNS)) return true;

	auto declaration = findDeclarationIncludeImplictImports(block, old->name);

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
		import->name = old->name;
		import->start = *start;
		import->end = *end;
		import->import = old;
		import->flags |= DECLARATION_IS_IMPLICIT_IMPORT;
		addDeclarationToBlockUnchecked(block, import);

		return true;
	}
}