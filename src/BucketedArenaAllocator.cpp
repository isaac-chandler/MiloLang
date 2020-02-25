#include "Basic.h"
#include "BucketedArenaAllocator.h"

#define Align16(a) ((reinterpret_cast<u64>(a) + 15) & ~15)

void *BucketedArenaAllocator::allocate(u64 size) {
	if (size > bucketSize) {
		return malloc(size); // @Leak
	}

	assert(size < bucketSize);

	char *aligned = (char *) Align16(current->memory);

	current->remaining -= static_cast<u64>(aligned - current->memory);
	current->memory = aligned;


	if (current->remaining >= size) {
		char *result = current->memory;
		current->remaining -= size;
		current->memory += size;

		return result;
	}
	else {
		current->next = makeBucket(bucketSize);
		current = current->next;

		// We know that makeBucket uses malloc so it is already guaranteed to be 16 byte aligned, and we have already checked that size is less than bucketSize

		char *result = current->memory;
		current->remaining -= size;
		current->memory += size;

		return result;
	}
}

ArenaAllocatorBucket *makeBucket(u32 size) {
	char *block = static_cast<char *>(malloc(size + sizeof(ArenaAllocatorBucket)));

#if BUILD_DEBUG
	memset(block, 0xAA, size);
#endif

	ArenaAllocatorBucket *result = reinterpret_cast<ArenaAllocatorBucket *>(block + size);
	result->next = nullptr;
	result->remaining = size;
	result->memory = block;

	return result;
}
