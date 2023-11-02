#include "Basic.h"

#include "Block.h"
#include "Ast.h"
#include "Error.h"


void insert(Block *block, Declaration *entry) {
	PROFILE_FUNC();
	u64 slot = entry->name->hash & (block->tableCapacity - 1);

	u64 dist = 1;

	while (block->table[slot]) {
		slot += dist++;
		slot &= block->tableCapacity - 1;
	}

	block->table[slot] = entry;
}

Declaration **findInBlock(Block *block, Identifier *name) {
	u64 hash = name->hash;
	u64 slot = hash & (block->tableCapacity - 1);

	u64 dist = 1;

	while (block->table[slot]) {
		if (block->table[slot]->name == name) {
			return &block->table[slot];
		}
		slot += dist++;
		slot &= block->tableCapacity - 1;
	}

	return &block->table[slot];
}

void rehash(Block *block) {
	PROFILE_FUNC();

	auto oldTable = block->table;
	auto oldCapacity = block->tableCapacity;

	block->tableCapacity *= 2;
	block->table = new Declaration * [block->tableCapacity];
	memset(block->table, 0, sizeof(Declaration *) * block->tableCapacity);

	for (u64 i = 0; i < oldCapacity; i++) {
		if (oldTable[i]) {
			insert(block, oldTable[i]);
		}
	}

	delete[] oldTable;
}

void addToTable(Block *block, Declaration *declaration, Declaration **availableSlot) {
	PROFILE_FUNC();
	if (declaration->name) {
		if (availableSlot) {
			assert(!*availableSlot);
			*availableSlot = declaration;
		}
		else {
			insert(block, declaration);
		}

		if (block->declarations.count * 10 > block->tableCapacity * 7) {
			rehash(block);
		}
	}
}

void initTable(Block *block) {
	PROFILE_FUNC();

	block->tableCapacity = BLOCK_HASHTABLE_MIN_COUNT * 2;
	block->table = new Declaration * [block->tableCapacity];
	memset(block->table, 0, sizeof(Declaration *) * block->tableCapacity);

	for (auto declaration : block->declarations) {
		if (declaration->name) {
			insert(block, declaration);
		}
	}
}


void addImplicitImport(Block *block, ExprIdentifier *identifier) {
	PROFILE_FUNC();
	for (auto import : block->implicitImports) {
		if (import->declaration->name == identifier->declaration->name) {
			return;
		}
	}

	block->implicitImports.add(identifier);
}

void addToOverloads(Declaration *overload, Declaration *add) {
	while (overload->nextOverload) {
		overload = overload->nextOverload;
	}

	overload->nextOverload = add;
}

bool checkForRedeclaration(Block *block, Declaration *declaration, Declaration **potentialOverloadSet, Expr *using_, Declaration ***availableSlot) {
	PROFILE_FUNC();
	assert(block);
	assert(declaration);

	*potentialOverloadSet = nullptr;

	if (!declaration->name) // Multiple zero length names are used in the arguments block for function prototypes with unnamed 
		return true;

	Declaration *previous = nullptr;

	if (block->table) {
		PROFILE_ZONE("checkForRedeclaration table");
		*availableSlot = findInBlock(block, declaration->name);
		previous = **availableSlot;
	}
	else {
		PROFILE_ZONE("checkForRedeclaration array");
		for (auto existingDeclaration : block->declarations) {
			if (declaration->name == existingDeclaration->name) {
				previous = existingDeclaration;
				break;
			}
		}
	}

	if (!previous) {
		for (auto import : block->implicitImports) {
			if (import->name == declaration->name) {
				if (using_) {
					reportError(using_, "Error: Cannot import variable '%.*s' into a scope, where a variable of the same name was previously used",
						STRING_PRINTF(declaration->name->name));
					reportError(import, "   ..: Here is the location it was used");
					reportError(import->declaration, "   ..: Here is the location it was declared");
					reportError(declaration, "   ..: Here is the location it was imported from");
				}
				else {
					reportError(declaration, "Error: Cannot declare variable '%.*s' in a scope where a variable of the same name was previously used",
						STRING_PRINTF(declaration->name->name));
					reportError(import, "   ..: Here is the location it was used");
					reportError(previous, "   ..: Here is the location it was declared");
				}
				return false;
			}
		}

		return true;
	}

	if (block->flavor != BlockFlavor::CONSTANTS &&                               // Polymorph variables cannot be overloaded
		(previous->flags & declaration->flags & DECLARATION_IS_CONSTANT) &&      // Declarations must be a constant
		!((previous->flags | declaration->flags) & DECLARATION_IS_ENUM_VALUE)) { // Enums use putDeclarationInBlock
		if (previous->flags & DECLARATION_OVERLOADS_LOCKED) {
			if (using_) {
				reportError(declaration, "Error: Cannot import an overload into an overload set that has already been used", STRING_PRINTF(declaration->name->name));
				reportError(using_, "   ..: Here is the import location");
			}
			else {
				reportError(declaration, "Error: Cannot add an overload to an overload set that has already been used", STRING_PRINTF(declaration->name->name));
			}

			if (previous->nextOverload) {
				reportError("   ..: Here are the other overloads");
			}
			else {
				reportError("   ..: Here is the other overload");
			}

			do {
				reportError(previous, "");
			} while (previous = previous->nextOverload);

			return false;
		}

		*potentialOverloadSet = previous;

		return true;
	}

	if (using_) {
		reportError(using_, "Error: Cannot import variable '%.*s' into scope, it already exists there", STRING_PRINTF(declaration->name->name));
		reportError(previous, "   ..: Here is the location it was declared");
		reportError(declaration, "   ..: Here is the location it was imported from");
	}
	else {
		reportError(declaration, "Error: Cannot redeclare variable '%.*s' within the same scope", STRING_PRINTF(declaration->name->name));
		reportError(previous, "   ..: Here is the location it was declared");
	}
	return false;
}