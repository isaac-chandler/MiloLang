#pragma once

#include "Basic.h"
#include "Array.h"
#include "String.h"

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

#define DECLARATION_IS_MODULE_SCOPE 0x4'0000

#define DECLARATION_OVERLOADS_LOCKED 0x8'0000

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
	Declaration *nextOverload = nullptr;

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

	Declaration() : llvmStorage(nullptr) {}
};

#define BLOCK_HASHTABLE_MIN_COUNT 32

struct Importer {
	u32 serial;
	bool moduleScope = false;

	Block *enclosingScope = nullptr;
	Expr *import;
	Expr *structAccess = nullptr;

};

enum class BlockFlavor : u8 {
	IMPERATIVE,
	ARGUMENTS,
	RETURNS,
	STRUCT,
	GLOBAL
};

struct Block {
	Array<Declaration *> declarations;
	Array<Importer *> importers;
	Array<struct ExprIdentifier *> implicitImports;

	struct BlockEntry *table = nullptr;
	u32 tableCapacity;
	u32 serial;

	Block *parentBlock = nullptr;

	Array<struct SubJob *> sleepingOnMe;

	BlockFlavor flavor;
	bool queued = false;
	bool loop = false;
	bool module = false;
};

struct Module {
	String name;
	Block members;
	Array<struct Importer *> imports;
};

Declaration *findInBlock(Block *block, String name);

inline Declaration *findDeclarationNoYield(Block *block, String name) {
	PROFILE_FUNC();
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

void addImplicitImport(Block *block, struct ExprIdentifier *identifier);

bool checkForRedeclaration(Block *block, Declaration *declaration, Declaration **potentialOverloadSet, struct Expr *using_);

void addToTable(Block *block, Declaration *declaration);
void initTable(Block *block);

inline void addImporterToBlock(Block *block, Importer *importer, u32 serial) {
	assert(block->flavor != BlockFlavor::ARGUMENTS && block->flavor != BlockFlavor::RETURNS);

	importer->serial = serial;
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

void addToOverloads(Block *block, Declaration *overload, Declaration *add);

inline void addDeclarationToBlockUnchecked(Block *block, Declaration *declaration, Declaration *overloadSet, u32 serial) {
	assert(checkForRedeclaration(block, declaration, &overloadSet, nullptr));

	declaration->serial = serial;

	declaration->enclosingScope = block;
	
	if (overloadSet) {
		addToOverloads(block, overloadSet, declaration);
	}
	else {
		putDeclarationInBlock(block, declaration);
	}
}

inline bool addDeclarationToBlock(Block *block, Declaration *declaration, u32 serial) {
	Declaration *potentialOverloadSet;
	if (!checkForRedeclaration(block, declaration, &potentialOverloadSet, nullptr)) {
		return false;
	}

	addDeclarationToBlockUnchecked(block, declaration, potentialOverloadSet, serial);

	return true;
}

inline Declaration *findDeclaration(Block *block, String name, bool *yield, u32 usingYieldLimit = UINT32_MAX) {
	PROFILE_FUNC();

	*yield = false;

	auto declaration = findDeclarationNoYield(block, name);

	if (!declaration) {
		for (auto importer : block->importers) {
			if (importer->serial < usingYieldLimit) {
				*yield = true;
				break;
			}
		}
	}


	return declaration;
}