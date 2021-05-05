#pragma once

#include "String.h"


struct ArenaAllocatorBucket {
	ArenaAllocatorBucket *next;
	char *memory;
	u32 remaining;
};

ArenaAllocatorBucket *makeBucket(u32 size);

struct BucketedArenaAllocator {
	ArenaAllocatorBucket *first;
	ArenaAllocatorBucket *current;
	u32 bucketSize;
	u32 totalSize;

	BucketedArenaAllocator(u32 bucketSize) : bucketSize(bucketSize), totalSize(0) {
		assert((bucketSize & 7) == 0);

		current = makeBucket(bucketSize);
		first = current;
	}

	void *allocate(u32 size);
	void *allocateUnaligned(u32 size);

	inline void free() {
		while (first) {
			ArenaAllocatorBucket *bucket = first;
			first = first->next;

			std::free(reinterpret_cast<char *>(bucket) - bucketSize);
		}
	}

	u8  *add1(u8 value);
	u16 *add2(u16 value);
	u32 *add4(u32 value);
	u64 *add8(u64 value);
	u8  *add1Unchecked(u8 value);
	u16 *add2Unchecked(u16 value);
	u32 *add4Unchecked(u32 value);
	u64 *add8Unchecked(u64 value);
	void add(const void *value, u32 size);
	void *addUnchecked(const void *value, u32 size);
	void addNullTerminatedString(String string);
	void addNullTerminatedString(const char *string);
	void addString(String string);
	void addString(const char *string);
	void ensure(u32 size);
};

template<typename T, u32 bucketSize = 1024>
struct BucketArray {
	BucketedArenaAllocator allocator;

	BucketArray() : allocator(sizeof(T) *bucketSize) {}

	T *add(const T &value) {
		allocator.ensure(sizeof(value));

		T *result = reinterpret_cast<T *>(allocator.current->memory);
		allocator.add(&value, sizeof(value));

		return result;
	}

	u32 count() const {
		return allocator.totalSize / sizeof(T);
	}
};

