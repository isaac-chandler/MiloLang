#pragma once

#include "Basic.h"
#include "Array.h"
#include "String.h"
#include "IdentTable.h"

#define DECLARATION_IS_CONSTANT         0x0'0001
#define DECLARATION_IS_UNINITIALIZED    0x0'0002

// These flags denote if the declaration is a for loop it or it_index
#define DECLARATION_IS_ITERATOR         0x0'0004 // This is also set for while loop labels
#define DECLARATION_IS_ITERATOR_INDEX   0x0'0008

// Set when the initial value of a declaration is completely inferred
#define DECLARATION_VALUE_IS_READY      0x0'0010

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

#define DECLARATION_TYPE_POLYMORPHIC  0x10'0000
#define DECLARATION_VALUE_POLYMORPHIC 0x20'0000

#define DECLARATION_IS_POINTED_TO 0x40'0000

#define DECLARATION_IS_ENTRY_POINT 0x80'0000

struct Declaration {
	CodeLocation start;
	EndLocation end;
	u32 flags = 0;
	Identifier *name;
	union {
		struct Expr *typeExpr;
		Declaration *import;
	};

	struct Expr *initialValue;
	struct Type *type_ = nullptr;

	struct Block *enclosingScope;
	u32 serial;

	struct DeclarationJob *inferJob = nullptr;
	Declaration *nextOverload = nullptr;

	Array<struct SubJob *> sleepingOnMyType;
	Array<struct SubJob *> sleepingOnMyValue;

	union {
		struct {
			void *symbol;
			u32 physicalStorage;
			u32 registerOfStorage;
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

};

enum class BlockFlavor : u8 {
	IMPERATIVE,
	CONSTANTS, 
	ARGUMENTS,
	RETURNS,
	STRUCT,
	ENUM, 
	GLOBAL
};

struct Block {
	Array<Declaration *> declarations;
	Array<Importer *> importers;
	Array<struct ExprIdentifier *> implicitImports;

	Declaration **table = nullptr;
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
	u32 moduleId;
	Array<struct Importer *> imports;
};

Declaration **findInBlock(Block *block, Identifier *name);

inline Declaration *findDeclarationNoYield(Block *block, Identifier *name) {
	PROFILE_FUNC();
	if (block->table) {
		PROFILE_ZONE("findDeclaration table");
		return *findInBlock(block, name);
	}
	else {
		PROFILE_ZONE("findDeclaration array");
		for (auto declaration : block->declarations) {
			if (declaration->name == name) {
				return declaration;

			}
		}
	}

	return nullptr;
}

void addImplicitImport(Block *block, struct ExprIdentifier *identifier);

bool checkForRedeclaration(Block *block, Declaration *declaration, Declaration **potentialOverloadSet, struct Expr *using_, Declaration ***availableSlot);

void addToTable(Block *block, Declaration *declaration, Declaration **availableSlot);
void initTable(Block *block);

inline void addImporterToBlock(Block *block, Importer *importer, u32 serial) {
	assert(block->flavor != BlockFlavor::ARGUMENTS && block->flavor != BlockFlavor::RETURNS);

	importer->serial = serial;
	importer->enclosingScope = block;

	block->importers.add(importer);
}

inline void putDeclarationInBlock(Block *block, Declaration *declaration, Declaration **availableSlot) {
	PROFILE_FUNC();
	block->declarations.add(declaration);

	if (block->table) {
		addToTable(block, declaration, availableSlot);
	}
	else if (block->declarations.count == BLOCK_HASHTABLE_MIN_COUNT) {
		initTable(block);
	}
}

void addToOverloads(Declaration *overload, Declaration *add);

inline void addDeclarationToBlockUnchecked(Block *block, Declaration *declaration, Declaration *overloadSet, u32 serial, Declaration **availableSlot) {
	Declaration **slot;
	assert(checkForRedeclaration(block, declaration, &overloadSet, nullptr, &slot));

	declaration->serial = serial;

	declaration->enclosingScope = block;
	
	if (overloadSet) {
		addToOverloads(overloadSet, declaration);
	}
	else {
		putDeclarationInBlock(block, declaration, availableSlot);
	}
}

inline bool addDeclarationToBlock(Block *block, Declaration *declaration, u32 serial) {
	Declaration *potentialOverloadSet;
	Declaration **availableSlot;
	if (!checkForRedeclaration(block, declaration, &potentialOverloadSet, nullptr, &availableSlot)) {
		return false;
	}

	addDeclarationToBlockUnchecked(block, declaration, potentialOverloadSet, serial, availableSlot);

	return true;
}

inline Declaration *findDeclaration(Block *block, Identifier *name, bool *yield, u32 usingYieldLimit = UINT32_MAX) {
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

inline u32 getDeclarationIndex(Block *block, Declaration *declaration, u32 flaggedToIgnore) {
	u32 index = 0;

	assert(!(declaration->flags & flaggedToIgnore));

	for (auto member : block->declarations) {
		if (member->flags & flaggedToIgnore)
			continue;

		if (member == declaration)
			return index;

		index++;
	}

	assert(false);
	return index;
}

inline Declaration *getDeclarationByIndex(Block *block, u32 index, u32 flaggedToIgnore) {
	for (auto member : block->declarations) {
		if (member->flags & flaggedToIgnore)
			continue;

		if (index == 0)
			return member;

		index--;
	}

	assert(false);
	return nullptr;
}