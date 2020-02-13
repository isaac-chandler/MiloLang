#pragma once

#include "Basic.h"

#define WORK_QUEUE_MAX_PENDING 64 // Must be a power of 2
#define WORK_QUEUE_MASK (WORK_QUEUE_MAX_PENDING - 1)

#if 0
#include <intrin.h>

// This is an extremely slow work queue implementation, make it better later if it is a bottleneck

template<typename T>
struct SPMCQueue {
	T jobs[WORK_QUEUE_MAX_PENDING];
	
	volatile u8 addLocation = 0;
	volatile u8 takeLocation = 0;

	std::condition_variable full;



	void add(T job) {
		// @Improvement, don't burn the CPU
		if (addLocation - takeLocation == WORK_QUEUE_MAX_PENDING) ; // Only we touch addLocation so we shouldn't need to do lock stuff, tail is always increasing so this won't be invalidated

		jobs[addLocation & WORK_QUEUE_MASK] = job;

		_WriteBarrier(); // Don't increment the head until we've actually added the job

		++addLocation;
	}

	T take(T job) {
		while (true) {
			u64 oldTail = takeLocation;

			if (head != oldTail) {
				if (InterlockedCompareExchange64()
			}
		}


	}

};
#endif

template <typename T> 
class WorkQueue {
public:
	T jobs[WORK_QUEUE_MAX_PENDING];
	u64 head = 0;
	u64 tail = 0;

	std::mutex mutex;

	std::condition_variable empty;
	std::condition_variable overflow;

	void add(T job) {
		PROFILE_FUNC();
		std::unique_lock<std::mutex> lock(mutex);

		overflow.wait(lock, WorkQueueAdd(head, tail));

		//overflow.wait(lock, [&head, &tail]() {
		//	return tail + WORK_QUEUE_MAX_PENDING > head;
		//	});
		

		jobs[head++ & WORK_QUEUE_MASK] = job;
		
		empty.notify_one();
	}

	T take() {
		PROFILE_FUNC();
		std::unique_lock<std::mutex> lock(mutex);

		empty.wait(lock, WorkQueueTake(head, tail));


		//empty.wait(lock, [&head, &tail]() {
		//	return tail < head;
		//	});

		T job = jobs[tail++ & WORK_QUEUE_MASK];

		overflow.notify_one();

		return job;
	}

	void clear() {
		head = 0;
		tail = 0;
	}
};

struct WorkQueueAdd {
	u64 &head;
	u64 &tail;

	WorkQueueAdd(u64 &head, u64 &tail) : head(head), tail(tail) {}

	inline bool operator()() const { return tail + WORK_QUEUE_MAX_PENDING > head; }
};

struct WorkQueueTake {
	u64 &head;
	u64 &tail;

	WorkQueueTake(u64 &head, u64 &tail) : head(head), tail(tail) {}

	inline bool operator()() const { return tail < head; }
};
