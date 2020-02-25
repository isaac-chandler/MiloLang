#pragma once


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

	BucketedArenaAllocator(u64 bucketSize) : bucketSize(bucketSize) {
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
};

