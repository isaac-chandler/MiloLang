#include "Basic.h"
#include "Identifier.h"

u64 doHash(String string);

static unsigned int readCount;
static volatile unsigned int readLock;
static volatile unsigned int writeLock;

static Identifier **table;
static u32 capacity;
static u32 count;

#define lock(x) while (InterlockedCompareExchange(&(x), 1, 0) != 0)
#define unlock(x) (x) = 0

void identifierRehash() {
	u32 newCapacity = capacity * 2;
	Identifier **newTable = (Identifier**)calloc(newCapacity, sizeof(Identifier *));

	for (u32 i = 0; i < capacity; i++) {
		if (auto identifier = table[i]) {
			u32 slot = identifier->hash & (newCapacity - 1);

			while (newTable[slot]) {
				slot++;
				slot &= newCapacity - 1;
			}

			newTable[slot] = identifier;
		}
	}

	free(table);
	table = newTable;
	capacity = newCapacity;
}

Identifier *getIdentifier(String name) {
	u32 hash = (u32)doHash(name);
	u32 slot;

	{
		lock(readLock);
		if (readCount++ == 0) {
			lock(writeLock);
		}
		unlock(readLock);

		at_exit{
			lock(readLock);
			if (--readCount == 0) {
				unlock(writeLock);
			}
			unlock(readLock);
		};

		slot = hash & (capacity - 1);

		while (auto identifier = table[slot]) {
			if (identifier->hash == hash && String(identifier->characters, identifier->length) == name)
				return identifier;

			slot++;
			slot &= capacity - 1;
		}
	}

	Identifier *result = (Identifier *)malloc(sizeof(Identifier) + name.length);
	result->hash = hash;
	result->length = name.length;
	memcpy(result->characters, name.characters, name.length);

	lock(writeLock);
	at_exit{
		unlock(writeLock);
	};

	if (++count * 2 > capacity) {
		identifierRehash();
		slot = hash & (capacity - 1);
	}

	while (auto identifier = table[slot]) {
		// Another thread may have written to the table between the time we did a lookup 
		// and this insertion so check again to see if it exisits

		if (identifier->hash == hash && String(identifier->characters, identifier->length) == name) {
			free(result); // We waste a bit of time allocating and freeing here but this case should be extremely rare
			return identifier;
		}

		slot++;
		slot &= capacity - 1;
	}

	table[slot] = result;

	return result;
}
