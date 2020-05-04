#pragma once

#include "Basic.h"
#include "Array.h"
#include <intrin.h>

template <typename T> 
class MPSCWorkQueue {
public:
	Array<T> input;
	Array<T> output;
	u64 outputCursor = 0;

	std::mutex inputLock;
	
	volatile bool empty = true;

	void add(T job) {
		PROFILE_FUNC();
		std::unique_lock<std::mutex> lock(inputLock);

		input.add(job);
		_ReadWriteBarrier();
		empty = false;
	}

	T take() {
		PROFILE_FUNC();
		if (outputCursor >= output.count) {
			while (empty) {}

			std::unique_lock<std::mutex> lock(inputLock);

			output.clear();
			outputCursor = 0;
			std::swap(input, output);
			empty = true;
		}

		return output[outputCursor++];
	}
};

template <typename T>
class MPMCWorkQueue {
public:
	Array<T> input;
	Array<T> output;
	volatile s64 outputCursor = 0;

	std::mutex inputLock;

	volatile bool empty = true;

	void add(T job) {
		PROFILE_FUNC();
		std::unique_lock<std::mutex> lock(inputLock);

		input.add(job);
		_ReadWriteBarrier();
		empty = false;
	}

	T take() {
		PROFILE_FUNC();

		u64 index;

		while ((index = _InterlockedExchangeAdd64(&outputCursor, 1) ) > output.count) {}

		if (index == output.count) {
			while (empty) {}

			std::unique_lock<std::mutex> lock(inputLock);

			output.clear();
			index = 0;
			std::swap(input.storage, output.storage);
			std::swap(input.capacity, output.capacity);

			_ReadWriteBarrier();

			std::swap(input.count, output.count);
			empty = true;

			_ReadWriteBarrier();

			outputCursor = 1;
		}

		return output[index];
	}
};