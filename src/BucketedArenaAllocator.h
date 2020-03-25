#pragma once

#include "String.h"


struct ArenaAllocatorBucket {
	ArenaAllocatorBucket *next;
	char *memory;
	u64 remaining;
};

ArenaAllocatorBucket *makeBucket(u32 size);

struct BucketedArenaAllocator {
	ArenaAllocatorBucket *first;
	ArenaAllocatorBucket *current;
	u64 bucketSize;
	u64 totalSize;

	BucketedArenaAllocator(u64 bucketSize) : bucketSize(bucketSize), totalSize(0) {
		assert((bucketSize & 7) == 0);

		current = makeBucket(bucketSize);
		first = current;
	}

	void *allocate(u64 size);

	inline void free() {
		while (first) {
			ArenaAllocatorBucket *bucket = first;
			first = first->next;

			std::free(reinterpret_cast<char *>(bucket) - bucketSize);
		}
	}

	u8 *add1(u8 value);
	u16 *add2(u16 value);
	u32 *add4(u32 value);
	u64 *add8(u64 value);
	void addNullTerminatedString(String string);
};

