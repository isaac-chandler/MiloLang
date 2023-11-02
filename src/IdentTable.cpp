#include "Basic.h"
#include "IdentTable.h"
#include "BucketedArenaAllocator.h"
#include "OS.h"


alignas(64) static volatile s64 identTableReadCount;
alignas(64) static volatile u32 identTableReadLock;
alignas(64) static volatile u32 identTableWriteLock;

#define lock(x) while (!CompareExchange(&(x), (u32) 1, (u32) 0))
#define unlock(x) (x) = 0

static BucketedArenaAllocator allocator(65536);

static Identifier **identTable;
static u64 identTableMask;
static u64 identTableCount;


// Modified from original
static u64 MurmurHash64A(const void *key, u64 len, u64 seed) {
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
	case 7: hash ^= static_cast<u64>(data2[6]) << 48; [[fallthrough]];
	case 6: hash ^= static_cast<u64>(data2[5]) << 40; [[fallthrough]];
	case 5: hash ^= static_cast<u64>(data2[4]) << 32; [[fallthrough]];
	case 4: hash ^= static_cast<u64>(data2[3]) << 24; [[fallthrough]];
	case 3: hash ^= static_cast<u64>(data2[2]) << 16; [[fallthrough]];
	case 2: hash ^= static_cast<u64>(data2[1]) << 8; [[fallthrough]];
	case 1: hash ^= static_cast<u64>(data2[0]);
		hash *= multiplier;
	};

	hash ^= hash >> shift;
	hash *= multiplier;
	hash ^= hash >> shift;

	return hash;
}

static u64 doHash(String string) {
	PROFILE_FUNC();
	/*
	u64 hash = 0;

	for (u32 i = 0; i < string.length; i++) {
		hash *= 251;
		hash += string.characters[i];
	}
	*/

	u64 hash = MurmurHash64A(string.characters, string.length, 0);

	return hash;
}

static void rehash() {
	PROFILE_ZONE("Ident Table Rehash");
	u64 newMask = identTableMask * 2 + 1;
	Identifier **newTable = static_cast<Identifier **>(calloc(newMask + 1, sizeof(Identifier *)));

	for (u64 i = 0; i <= identTableMask; i++) {
		Identifier *ident = identTable[i];
		if (!ident)
			continue;

		u64 slot = ident->hash & newMask;

		while (newTable[slot]) {
			++slot;
			slot &= newMask;
		}

		newTable[slot] = ident;
	}

	free(identTable);
	identTable = newTable;
	identTableMask = newMask;
}

Identifier *getIdentifier(String name) {
	PROFILE_FUNC();



	u64 hash = doHash(name);
	
	{
		lock(identTableReadLock);
		++identTableReadCount;
		if (identTableReadCount == 1) {
			lock(identTableWriteLock);
		}
		unlock(identTableReadLock);
		
		at_exit{
			lock(identTableReadLock);
			if (identTableReadCount == 1) {
				unlock(identTableWriteLock);
			}
			--identTableReadCount;
			unlock(identTableReadLock);
		};


		u64 slot = hash & identTableMask;

		while (identTable[slot]) {
			// Maybe duplicate hash and name inline in the hashtable to avoid extra indirection?
			if (identTable[slot]->hash == hash && identTable[slot]->name == name) {
				return identTable[slot];
			}
			++slot;
			slot &= identTableMask;
		}
	}

	lock(identTableWriteLock);
	Identifier *identifier = static_cast<Identifier *>(allocator.allocate(sizeof(Identifier)));
	identifier->hash = hash;
	identifier->name = name;

	if (identTableCount * 2 > identTableMask) {
		rehash();
	}

	// Another thread may have sniped the write lock from under us
	// so we cannot use the previously found slot as it may have
	// been stolen are we may have rehashed
	u64 slot = hash & identTableMask;
	
	while (identTable[slot]) {
		++slot;
		slot &= identTableMask;
	}

	identTable[slot] = identifier;
	unlock(identTableWriteLock);

	return identifier;
}

void initIdentTable() {
	identTableMask = 65535;
	identTable = static_cast<Identifier **>(calloc(identTableMask + 1, sizeof(Identifier *)));

	identIt = getIdentifier("it");
	identItIndex = getIdentifier("it_index");
	identData = getIdentifier("data");
	identCount = getIdentifier("count");
	identCapacity = getIdentifier("capacity");
	identInteger = getIdentifier("integer");
}