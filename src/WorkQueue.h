#pragma once

#include "Basic.h"
#include <intrin.h>

template <typename T>
T *CompareExchangePointers(T *volatile *dest, T *value, T *compare) {
	return reinterpret_cast<T *>(_InterlockedCompareExchange64(reinterpret_cast<volatile s64 *>(dest), reinterpret_cast<s64>(value), reinterpret_cast<s64>(compare)));
}


template <typename T>
struct alignas(64) SPSCWorkQueue {
	static constexpr s64 BUFFER_SIZE = 1024;

	struct Buffer {
		Buffer *volatile next;
		T data[BUFFER_SIZE];
	};

	alignas(64) s64 volatile inCompleteCount = 0;

	alignas(64) Buffer *volatile free = nullptr;


	alignas(64) Buffer *input;
	s64 insertSize = BUFFER_SIZE;

	alignas(64) Buffer *output;
	s64 removeSize = BUFFER_SIZE;
	s64 outStartCount = 0;


	SPSCWorkQueue() {
		input = static_cast<Buffer *>(malloc(sizeof(Buffer)));
		input->next = nullptr;
		output = input;
	}

	void add(T job) {
		PROFILE_FUNC();

		if (inCompleteCount == insertSize) {
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
				_mm_pause();
			}

			newInput->next = nullptr;
			input->next = newInput;

			input = newInput;
			insertSize += BUFFER_SIZE;
		}

		input->data[inCompleteCount & (BUFFER_SIZE - 1)] = job;
		_ReadWriteBarrier();

		++inCompleteCount;
	}

	T take() {
		PROFILE_FUNC();

		s64 index;

		{
			s64 count = 0;


			while (true) {
				if (outStartCount < inCompleteCount)
					break;

				_mm_pause();

				if (count++ > 128) {
					SwitchToThread();
				}
			}
		}

		if (outStartCount == removeSize) {
			Buffer *newOutput = output->next;

			while (true) {
				auto oldFree = free;
				output->next = oldFree;

				if (CompareExchangePointers(&free, output, oldFree) == oldFree) {
					break;
				}
			}

			output = newOutput;
			removeSize += BUFFER_SIZE;
		}

		return output->data[outStartCount++ & (BUFFER_SIZE - 1)];
	}
};

template <typename T>
struct alignas(64) MPMCWorkQueue {
	static constexpr s64 BUFFER_SIZE = 1024;

	struct Buffer {
		Buffer *volatile next;
		bool completed[BUFFER_SIZE];
		T data[BUFFER_SIZE];
	};

	alignas(64) s64 volatile inCompleteCount = 0;

	alignas(64) s64 volatile outStartCount = 0;
	alignas(64) s64 volatile outCompleteCount = 0;

	alignas(64) Buffer *volatile free = nullptr;


	alignas(64) Buffer *input;
	alignas(64) s64 volatile inStartCount = 0;
	alignas(64) s64 volatile insertSize = BUFFER_SIZE;

	alignas(64) Buffer *output;
	alignas(64) s64 volatile removeSize = BUFFER_SIZE;
	

	MPMCWorkQueue() {
		input = static_cast<Buffer *>(malloc(sizeof(Buffer)));
		memset(input->completed, 0, sizeof(input->completed));
		input->next = nullptr;
		output = input;
	}

	void add(T job) {
		PROFILE_FUNC();

		s64 index = _InterlockedExchangeAdd64(&inStartCount, 1);

		while (index > insertSize) _mm_pause();

		if (index == insertSize) {

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
				_mm_pause();
			}

			memset(newInput->completed, 0, sizeof(newInput->completed));
			newInput->next = nullptr;

			while (inCompleteCount < insertSize) _mm_pause();
			input->next = newInput;

			input = newInput;
			_ReadWriteBarrier();

			_InlineInterlockedAdd64(&insertSize, BUFFER_SIZE);
		}

		input->data[index & (BUFFER_SIZE - 1)] = job;
		_ReadWriteBarrier();

		input->completed[index & (BUFFER_SIZE - 1)] = true;
		_InterlockedExchangeAdd64(&inCompleteCount, 1);
	}

	T take() {
		PROFILE_FUNC();

		s64 index = _InterlockedExchangeAdd64(&outStartCount, 1);

		{
			s64 count = 0;
			while (index > removeSize) {
				_mm_pause();

				if (count++ > 128) {
					SwitchToThread();
				}
			}
		}

		if (index == removeSize) {
			{
				s64 count = 0;
				while (!output->next) {
					_mm_pause();

					if (count++ > 128) {
						SwitchToThread();
					}
				}
			}

			Buffer *newOutput = output->next;
			

			while (outCompleteCount < removeSize) _mm_pause();

			while (true) {
				auto oldFree = free;
				output->next = oldFree;

				if (CompareExchangePointers(&free, output, oldFree) == oldFree) {
					break;
				}
			}
			
			output = newOutput;
			_ReadWriteBarrier();

			_InterlockedExchangeAdd64(&removeSize, BUFFER_SIZE);
		}

		{
			s64 count = 0;
			while (!output->completed[index & (BUFFER_SIZE - 1)]) {
				_mm_pause();

				if (count++ > 128) {
					SwitchToThread();
				}
			}
		}

		auto result = output->data[index & (BUFFER_SIZE - 1)];
		_ReadWriteBarrier();
		_InterlockedExchangeAdd64(&outCompleteCount, 1);
		return result;
	}
};