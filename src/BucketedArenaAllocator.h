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
	void *allocateUnaligned(u64 size);

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
	void add(void *value, u64 size);
	void addNullTerminatedString(String string);
};

template<typename T, u64 bucketSize = 1024> 
struct BucketArray {
	BucketedArenaAllocator allocator;

	BucketArray() : allocator(sizeof(T) *bucketSize) {}

	void add(const T &value) {
		allocator.add(&value, sizeof(value));
	}

	u64 count() const {
		return allocator.totalSize / sizeof(T);
	}
};

