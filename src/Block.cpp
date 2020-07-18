#include "Basic.h"

#include "Block.h"

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
	// @Incomplete: 
	return MurmurHash64A(string.characters, string.length, 0);
}

void insert(Block *block, BlockEntry entry) {
	u64 slot = entry.hash & (block->tableCapacity - 1);

	while (block->table[slot].declaration) {
		++slot;
		slot &= block->tableCapacity - 1;
	}

	block->table[slot] = entry;
}

Declaration *findInBlock(Block *block, String name) {
	u64 hash = doHash(name);
	u64 slot = hash & (block->tableCapacity - 1);

	while (block->table[slot].declaration) {
		if (block->table[slot].hash == hash && block->table[slot].declaration->name == name) {
			return block->table[slot].declaration;
		}
		++slot;
		slot &= block->tableCapacity - 1;
	}

	return nullptr;
}

bool replaceInBlock(Block *block, Declaration *old, Declaration *declaration) {
	assert(old->name == declaration->name);

	u64 hash = doHash(old->name);
	u64 slot = hash & (block->tableCapacity - 1);

	while (block->table[slot].declaration) {
		if (block->table[slot].declaration == old) {
			block->table[slot].declaration = declaration;
			return true;
		}
		++slot;
		slot &= block->tableCapacity - 1;
	}

	return false;
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