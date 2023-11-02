#pragma once

#include "Basic.h"
#include "OS.h"

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

				if (CompareExchange((volatile u64 *)&free, (u64)newFree, (u64)newInput) == (u64)newInput)
					break;
				_mm_pause();
			}

			newInput->next = nullptr;
			input->next = newInput;

			input = newInput;
			insertSize += BUFFER_SIZE;
		}

		input->data[inCompleteCount & (BUFFER_SIZE - 1)] = job;
		read_write_barrier();

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
					yieldThread();
				}
			}
		}

		if (outStartCount == removeSize) {
			Buffer *newOutput = output->next;

			while (true) {
				auto oldFree = free;
				output->next = oldFree;

				if (CompareExchange((volatile u64 *)&free, (u64)output, (u64)oldFree) == (u64)oldFree) {
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

		s64 index = atomicFetchAdd(&inStartCount, 1);

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

				if (CompareExchange((volatile u64 *)&free, (u64) newFree, (u64)newInput) == (u64)newInput)
					break;
				_mm_pause();
			}

			memset(newInput->completed, 0, sizeof(newInput->completed));
			newInput->next = nullptr;

			while (inCompleteCount < insertSize) _mm_pause();
			input->next = newInput;

			input = newInput;
			read_write_barrier();

			atomicFetchAdd(&insertSize, BUFFER_SIZE);
		}

		input->data[index & (BUFFER_SIZE - 1)] = job;
		read_write_barrier();

		input->completed[index & (BUFFER_SIZE - 1)] = true;
		atomicFetchAdd(&inCompleteCount, 1);
	}

	T take() {
		PROFILE_FUNC();

		s64 index = atomicFetchAdd(&outStartCount, 1);

		{
			s64 count = 0;
			while (index > removeSize) {
				_mm_pause();

				if (count++ > 128) {
					yieldThread();
				}
			}
		}

		if (index == removeSize) {
			{
				s64 count = 0;
				while (!output->next) {
					_mm_pause();

					if (count++ > 128) {
						yieldThread();
					}
				}
			}

			Buffer *newOutput = output->next;
			

			while (outCompleteCount < removeSize) _mm_pause();

			while (true) {
				auto oldFree = free;
				output->next = oldFree;

				if (CompareExchange((volatile u64 *)&free, (u64) output, (u64) oldFree) == (u64)oldFree) {
					break;
				}
			}
			
			output = newOutput;
			read_write_barrier();

			atomicFetchAdd(&removeSize, BUFFER_SIZE);
		}

		{
			s64 count = 0;
			while (!output->completed[index & (BUFFER_SIZE - 1)]) {
				_mm_pause();

				if (count++ > 128) {
					yieldThread();
				}
			}
		}

		auto result = output->data[index & (BUFFER_SIZE - 1)];
		read_write_barrier();
		atomicFetchAdd(&outCompleteCount, 1);
		return result;
	}
};