#include "Basic.h"
#include "BucketedArenaAllocator.h"

#define Align16(a) ((reinterpret_cast<u64>(a) + 15) & ~15)

void expand(BucketedArenaAllocator *allocator) {
	allocator->current->next = makeBucket(allocator->bucketSize);
	allocator->current = allocator->current->next;
}

void BucketedArenaAllocator::ensure(u32 size) {
	if (size > current->remaining) {
		expand(this);
	}
}

void *BucketedArenaAllocator::allocate(u32 size) {
	if (size > bucketSize) {
		assert(false);
		return nullptr;
	}

	char *aligned = (char *) Align16(current->memory);

	totalSize += static_cast<u32>(aligned - current->memory) + size;

	current->remaining -= static_cast<u32>(aligned - current->memory);
	current->memory = aligned;


	ensure(size);


	char *result = current->memory;
	current->remaining -= size;
	current->memory += size;

	return result;
}

void *BucketedArenaAllocator::allocateUnaligned(u32 size) {
	if (size > bucketSize) {
		assert(false);
		return nullptr;
	}

	totalSize += size;


	ensure(size);

	char *result = current->memory;
	current->remaining -= size;
	current->memory += size;

	return result;
}

void BucketedArenaAllocator::addNullTerminatedString(String string) {
	addString(string);
	add1(0);
}

void BucketedArenaAllocator::addString(String string) {
	add(string.characters, string.length);
}

void BucketedArenaAllocator::addNullTerminatedString(const char *string) {
	addString(string);
	add1(0);
}

void BucketedArenaAllocator::addString(const char *string) {
	add(string, static_cast<u32>(strlen(string)));
}

void BucketedArenaAllocator::add(const void *value, u32 size) {
	const u8 *bytes = static_cast<const u8 *>(value);
	totalSize += size;

	while (current->remaining < size) {
		memcpy(current->memory, bytes, current->remaining);
		bytes += current->remaining;
		size -= current->remaining;
		current->memory += current->remaining;
		current->remaining = 0;
		expand(this);
	}

	memcpy(current->memory, bytes, size);
	current->memory += size;
	current->remaining -= size;
}

void *BucketedArenaAllocator::addUnchecked(const void *value, u32 size) {
	assert(size <= current->remaining);
	auto dest = current->memory;

	memcpy(current->memory, value, size);
	current->memory += size;
	current->remaining -= size;
	totalSize += size;

	return dest;
}

#define addN(name, type)											\
type *BucketedArenaAllocator::name(type value) {					\
	ensure(sizeof(type));											\
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

#define addNUnchecked(name, type)									\
type *BucketedArenaAllocator::name(type value) {					\
	assert(sizeof(type) <= current->remaining);						\
	totalSize += sizeof(type);										\
	type *location = reinterpret_cast<type *>(current->memory);		\
	current->memory += sizeof(type);								\
	current->remaining -= sizeof(type);								\
																	\
	*location = value;												\
	return location;												\
}

addNUnchecked(add1Unchecked, u8)
addNUnchecked(add2Unchecked, u16)
addNUnchecked(add4Unchecked, u32)
addNUnchecked(add8Unchecked, u64)

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
