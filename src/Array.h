#pragma once

#include "Basic.h"
#include "OS.h"

inline u32 findHigherPowerOf2(u32 value) {
	if (value == 0) return 1;

	u32 index = bitScanReverse(value);

	if ((value & (value - 1)) == 0) [[likely]] {
		return index;
	}
	else {
		return index + 1;
	}
}


struct ArrayAllocatorBlock {
	ArrayAllocatorBlock *next;
	u64 data[1];
};

template<u32 itemSize>
struct ArrayAllocator {

	static constexpr u32 blocksToAllocate = 64;

	ArrayAllocatorBlock **freeBlocks;

	ArrayAllocator() {
		freeBlocks = new ArrayAllocatorBlock * [32] {};
	}

	void createBlocks(u32 powerOf2) {
		u32 blockSize = 8 + itemSize * (1 << powerOf2);

		u8 *blocks = (u8 *) malloc(blockSize * blocksToAllocate);

		for (u32 i = 0; i < blocksToAllocate; i++) {
			ArrayAllocatorBlock *block = (ArrayAllocatorBlock *) blocks;

			block->next = freeBlocks[powerOf2];
			freeBlocks[powerOf2] = block;

			blocks += blockSize;
		}
	}

	void *allocate(u32 powerOf2) {
		if (!freeBlocks[powerOf2]) {
			createBlocks(powerOf2);
		}

		auto block = freeBlocks[powerOf2];
		freeBlocks[powerOf2] = block->next;

		return block->data;
	}

	void free(u32 size, u8 *data) {
		assert(size && (size & size - 1) == 0);
		u32 powerOf2 = bitScanReverse(size);

		ArrayAllocatorBlock *block = (ArrayAllocatorBlock *) (data - 8);

		block->next = freeBlocks[powerOf2];
		freeBlocks[powerOf2] = block;
	}

	static thread_local ArrayAllocator instance;
};

template<u32 itemSize>
inline thread_local ArrayAllocator<itemSize> ArrayAllocator<itemSize>::instance;

template <typename T>
class Array {
	using Allocator = ArrayAllocator<sizeof(T)>;

public:
	T *storage = 0;
	u32 count = 0;
	u32 capacity = 0;

	void resize(u32 newCapacity) {
		PROFILE_FUNC();
		assert(newCapacity >= count);

		if (newCapacity < 4) {
			newCapacity = 2;
		}
		else {
			newCapacity = findHigherPowerOf2(newCapacity);
		}

		T *newStorage = (T *) Allocator::instance.allocate(newCapacity);

		if (storage) {	
			memcpy(newStorage, storage, sizeof(T) * count);
			Allocator::instance.free(capacity, (u8 *) storage);
		}

		storage = newStorage;

		capacity = 1U << newCapacity;
	}

	void reserve(u32 capacity) {
		if (this->capacity < capacity) {
			resize(capacity);
		}
	}

	Array() {}

	Array(u32 capacity) {
		resize(capacity);
	}


	Array(std::initializer_list<T> values) {
		count = static_cast<u32>(values.size());
		resize(count);

		memcpy(storage, values.begin(), sizeof(T) * count);
	}

	template<typename Range, typename = std::enable_if_t<!std::is_integral<Range>::value>>
	explicit Array(const Range& values) {
		count = static_cast<u32>(values.end() - values.begin());
		resize(count);

		memcpy(begin(), values.begin(), sizeof(T) * count);
	}

	T &add() {
		add(T{});

		return storage[count - 1];
	}

	T &add(const T &value) {
		if (count >= capacity) {
			resize(capacity * 2);
		}

		storage[count] = value;
		count++;

		return storage[count - 1];
	}

	const T &operator[] (u32 index) const {
		assert(index < count);
		return storage[index];
	}

	T &operator[] (u32 index) {
		assert(index < count);
		return storage[index];
	}

	T &peek() {
		assert(count);
		return storage[count - 1];
	}

	T *begin() const {
		return storage;
	}

	T *end() const {
		return storage + count;
	}

	void free() {
		if (storage) {
			Allocator::instance.free(capacity, (u8 *) storage);
			storage = nullptr;
		}

		capacity = 0;
		count = 0;
	}

	bool unordered_remove(const T &value) {
		for (u32 i = 0; i < count; i++) {
			if (storage[i] == value) {
				unordered_remove(i);
				return true;
			}
		}

		return false;
	}

	void unordered_remove(u32 index) {
		if (index != count - 1) {
			storage[index] = storage[count - 1];
		}

		--count;
	}

	void unordered_remove(T *location) {
		assert(location >= begin() && location < end());

		if (location != end() - 1) {
			*location = *(end() - 1);
		}

		--count;
	}

	void clear() {
		count = 0;
	}

	T pop() {
		assert(count);
		unordered_remove(end() - 1);

		// @Volatile: only works because we never automatically shrink
		return *end();
	}
};