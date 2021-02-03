#include "Basic.h"

#include "Block.h"
#include "Ast.h"
#include "Error.h"

// Modified from original
__forceinline u64 MurmurHash64A(const void *key, u64 len, u64 seed) {
	static_assert(sizeof(u64) == 8);
	constexpr static u64 multiplier = 0xc6a4a7935bd1e995;
	constexpr static u64 shift = 47;

	u64 hash = seed ^ (len * multiplier);

	const u64 *data = (const u64 *) key;
	const u64 *end = data + (len / sizeof(u64));

	for (u64 block = *data; data != end; ++data) {

		block *= multiplier;
		block ^= block >> shift;
		block *= multiplier;

		hash ^= block;
		hash *= multiplier;
	}

	const u8 *data2 = reinterpret_cast<const u8 *>(data);

	switch (len & 7) {
		case 7: hash ^= static_cast<u64>(data2[6]) << 48;
		case 6: hash ^= static_cast<u64>(data2[5]) << 40;
		case 5: hash ^= static_cast<u64>(data2[4]) << 32;
		case 4: hash ^= static_cast<u64>(data2[3]) << 24;
		case 3: hash ^= static_cast<u64>(data2[2]) << 16;
		case 2: hash ^= static_cast<u64>(data2[1]) << 8;
		case 1: hash ^= static_cast<u64>(data2[0]);
		hash *= multiplier;
	};

	hash ^= hash >> shift;
	hash *= multiplier;
	hash ^= hash >> shift;

	return hash;
}

struct BlockEntry {
	Declaration *declaration = nullptr;
	u64 hash;
};

u64 doHash(String string) {
	u64 hash = 0;

	for (u32 i = 0; i < string.length; i++) {
		hash *= 251;
		hash += string.characters[i];
	}
	// @Incomplete: 
	//return MurmurHash64A(string.characters, string.length, 0);

	return hash;
}

void insert(Block *block, BlockEntry entry) {
	PROFILE_ZONE();
	u64 slot = entry.hash & (block->tableCapacity - 1);

	u64 dist = 1;

	while (block->table[slot].declaration) {
		slot += dist++;
		slot &= block->tableCapacity - 1;
	}

	block->table[slot] = entry;
}

Declaration *findInBlock(Block *block, String name) {
	u64 hash = doHash(name);
	u64 slot = hash & (block->tableCapacity - 1);

	u64 dist = 1;

	while (block->table[slot].declaration) {
		if (block->table[slot].hash == hash && block->table[slot].declaration->name == name) {
			return block->table[slot].declaration;
		}
		slot += dist++;
		slot &= block->tableCapacity - 1;
	}

	return nullptr;
}

void rehash(Block *block) {
	PROFILE_FUNC();

	auto oldTable = block->table;
	auto oldCapacity = block->tableCapacity;

	block->tableCapacity *= 2;
	block->table = new BlockEntry[block->tableCapacity];

	for (u64 i = 0; i < oldCapacity; i++) {
		if (oldTable[i].declaration) {
			insert(block, oldTable[i]);
		}
	}

	delete[] oldTable;
}

void addToTable(Block *block, Declaration *declaration) {
	if (declaration->name.length) {
		u64 hash = doHash(declaration->name);

		if (block->declarations.count * 10 > block->tableCapacity * 7) {
			rehash(block);
		}

		insert(block, { declaration, hash });
	}
}

void initTable(Block *block) {
	PROFILE_FUNC();

	block->tableCapacity = BLOCK_HASHTABLE_MIN_COUNT * 2;
	block->table = new BlockEntry[block->tableCapacity];

	for (auto declaration : block->declarations) {
		if (declaration->name.length) {
			insert(block, { declaration, doHash(declaration->name) });
		}
	}
}


void addImplicitImport(Block *block, ExprIdentifier *identifier) {
	for (auto import : block->implicitImports) {
		if (import->declaration->name == identifier->declaration->name) {
			return;
		}
	}

	block->implicitImports.add(identifier);
}

void addToOverloads(Block *block, Declaration *overload, Declaration *add) {	
	while (overload->nextOverload) {
		overload = overload->nextOverload;
	}

	overload->nextOverload = add;
}

bool checkForRedeclaration(Block *block, Declaration *declaration, Declaration **potentialOverloadSet, Expr *using_) {
	PROFILE_FUNC();
	assert(block);
	assert(declaration);

	*potentialOverloadSet = nullptr;

	if (declaration->name.length) { // Multiple zero length names are used in the arguments block for function prototypes with unnamed 
		auto previous = findDeclarationNoYield(block, declaration->name);

		if (previous) {
			if (block->flavor != BlockFlavor::CONSTANTS &&                               // Polymorph variables cannot be overloaded
				(previous->flags & declaration->flags & DECLARATION_IS_CONSTANT) &&      // Declarations must be a constant
				!((previous->flags | declaration->flags) & DECLARATION_IS_ENUM_VALUE)) { // Enums use putDeclarationInBlock
				if (previous->flags & DECLARATION_OVERLOADS_LOCKED) {
					if (using_) {
						reportError(declaration, "Error: Cannot import an overload into an overload set that has already been used", STRING_PRINTF(declaration->name));
					}
					else {
						reportError(declaration, "Error: Cannot add an overload to an overload set that has already been used", STRING_PRINTF(declaration->name));
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

		for (auto import : block->implicitImports) {
			if (import->name == declaration->name) {
				if (using_) {
					reportError(using_, "Error: Cannot import variable '%.*s' into a scope, where a variable of the same name was previously used", 
						STRING_PRINTF(declaration->name));
					reportError(import, "   ..: Here is the location it was used");
					reportError(import->declaration, "   ..: Here is the location it was declared");
					reportError(declaration, "   ..: Here is the location it was imported from");
				}
				else {
					reportError(declaration, "Error: Cannot declare variable '%.*s' in a scope where a variable of the same name was previously used", 
						STRING_PRINTF(declaration->name));
					reportError(import, "   ..: Here is the location it was used");
					reportError(previous, "   ..: Here is the location it was declared");
				}
				return false;
			}
		}
	}

	return true;
}