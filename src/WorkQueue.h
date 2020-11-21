#pragma once

#include "Basic.h"
#include <intrin.h>

template <typename T>
T *CompareExchangePointers(T *volatile *dest, T *value, T *compare) {
	return reinterpret_cast<T *>(_InterlockedCompareExchange64(reinterpret_cast<volatile s64 *>(dest), reinterpret_cast<s64>(value), reinterpret_cast<s64>(compare)));
}

template <typename T>
struct alignas(64) MPMCWorkQueue{
	struct Buffer {
		Buffer *volatile next;
		T data[128];
	};

	alignas(64) HANDLE semaphore;
	Buffer *volatile free = nullptr;


	alignas(64) Buffer *volatile input;
	s64 volatile inputCursor = 0;

	alignas(64) Buffer *volatile output;
	s64 volatile outputCursor = 0;

	

	MPMCWorkQueue() {
		semaphore = CreateSemaphoreA(NULL, 0, LONG_MAX, NULL);
		input = static_cast<Buffer *>(malloc(sizeof(Buffer)));
		input->next = nullptr;
		output = input;
	}

	void add(T job) {
		PROFILE_FUNC();

		u64 index;

		while ((index = _InterlockedExchangeAdd64(&inputCursor, 1)) > 128) {}

		if (index == 128) {

			Buffer *newInput;

			while (true) {
				newInput = free;

				if (!newInput) {
					newInput = static_cast<Buffer *>(malloc(sizeof(Buffer)));
					break;
				}

				Buffer *newFree = newInput->next;

				if (CompareExchangePointers(&free, newFree, newInput) == newInput)
					break;
			}

			_ReadWriteBarrier();

			newInput->next = nullptr;

			_WriteBarrier();

			input->next = newInput;

			input = newInput;

			index = 0;
			_WriteBarrier();
			inputCursor = 1;
		}

		input->data[index] = job;
		ReleaseSemaphore(semaphore, 1, NULL);
	}

	T take() {
		PROFILE_FUNC();

		WaitForSingleObject(semaphore, INFINITE);

		u64 index;

		while ((index = _InterlockedExchangeAdd64(&outputCursor, 1)) > 128) {}

		if (index == 128) {

			while (!output->next) {}

			Buffer *newOutput = output->next;
			_ReadWriteBarrier();

			while (true) {
				auto oldFree = free;
				output->next = oldFree;

				if (CompareExchangePointers(&free, output, oldFree) == oldFree) {
					break;
				}
			}

			_ReadWriteBarrier();

			output = newOutput;
			index = 0;
			_WriteBarrier();
			outputCursor = 1;
		}

		return output->data[index];
	}
};