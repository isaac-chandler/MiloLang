#include "Basic.h"
#include "BucketedArenaAllocator.h"

#define Align16(a) ((reinterpret_cast<u64>(a) + 15) & ~15)

void expand(BucketedArenaAllocator *allocator) {
	allocator->current->next = makeBucket(allocator->bucketSize);
	allocator->current = allocator->current->next;
}

void *BucketedArenaAllocator::allocate(u64 size) {
	if (size > bucketSize) {
		assert(false);
		return nullptr;
	}

	char *aligned = (char *) Align16(current->memory);

	totalSize += static_cast<u64>(aligned - current->memory) + size;

	current->remaining -= static_cast<u64>(aligned - current->memory);
	current->memory = aligned;


	if (current->remaining < size) {
		expand(this);
	}


	char *result = current->memory;
	current->remaining -= size;
	current->memory += size;

	return result;
}

void BucketedArenaAllocator::addNullTerminatedString(String string) {
	totalSize += string.length;

	while (current->remaining < string.length) {
		memcpy(current->memory, string.characters, current->remaining);
		string.characters += current->remaining;
		string.length -= current->remaining;
		current->memory += current->remaining;
		current->remaining = 0;
		expand(this);
	}

	memcpy(current->memory, string.characters, string.length);
	current->memory += string.length;
	current->remaining -= string.length;

	add1(0);
}

#define addN(name, type)											\
type *BucketedArenaAllocator::name(type value) {					\
	if (current->remaining < sizeof(type)) {						\
		expand(this);												\
	}																\
	totalSize += sizeof(type);										\
	type *location = reinterpret_cast<type *>(current->memory);		\
	current->memory += sizeof(type);								\
	current->remaining -= sizeof(type);								\
																	\
	*location = value;												\
	return location;												\
}

addN(add1, u8)
addN(add2, u16)
addN(add4, u32)
addN(add8, u64)

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
