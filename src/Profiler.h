#pragma once

#include "Basic.h"
#include "OS.h"

#if BUILD_PROFILE
#define PROFILE_FUNC(...)       Profiler_Timer CONCAT(timer,__LINE__)(__FUNCTION__, __VA_ARGS__)
#define PROFILE_ZONE(name, ...) Profiler_Timer CONCAT(timer,__LINE__)(name, __VA_ARGS__)
#else
#define PROFILE_FUNC(...)
#define PROFILE_ZONE(...)
#endif

struct Profile {
	const char *name;
	u64 time;
};

#define MAX_PROFILER_THREADS 16

#if BUILD_PROFILE
void profiler_init(u64 max_profiler_threads, u64 max_profiles_per_thread);

// The tracing JSON format stores timestamps as an integer number of microseconds, 
// for higher resolution profiling timescale can be increased. Setting timescale to 
// 1000 will save an integer number of nanoseconds instead. This will still display in 
// a tracing viewer as microseconds, so 1ms = 1us, 1us=1ns etc...
void profiler_write_tracing_json(const char *filename, double timescale = 1.0);

void profiler_write_binary(const char *filename);

void profiler_register_this_thread();
#else

#define profiler_init(...)
#define profiler_write_tracing_json(...)
#define profiler_register_this_thread(...)
#define profiler_write_binary(...)

#endif

inline thread_local volatile Profile *profile_index;

struct Profiler_Timer {
	__forceinline Profiler_Timer(const char *name) {

		volatile Profile *write = profile_index++;

		write->name = name;

		read_write_barrier();
		write->time = __rdtsc();
		read_write_barrier();
	}

	__forceinline ~Profiler_Timer() {
		read_write_barrier();
		profile_index->time = __rdtsc();
		read_write_barrier();
		profile_index++;
	}
};