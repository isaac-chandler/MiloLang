#pragma once

#include "Basic.h"

bool fileExists(const char *file);
bool directoryExists(const char *file);
FILE *fopen_utf8(const char *filename, const char *mode);
void yieldThread();
char *exePath();
char *fullPath(const char *filename);
void setThreadName(std::thread &thread, const char *name);
char *findLibrary(char *path, const char *name);
void user_library_path(const char *path);
void *open_library(const char *name);
void *library_find(void *library, const char *symbol);
void set_working_directory(char *directory);

#if BUILD_WINDOWS


template <typename T>
T CompareExchange(volatile T *dest, T value, T compare);

template<>
inline u32 CompareExchange<u32>(volatile u32 *dest, u32 value, u32 compare) {
	return _InterlockedCompareExchange((volatile unsigned long *)dest, value, compare);
}

template<>
inline u64 CompareExchange<u64>(volatile u64 *dest, u64 value, u64 compare) {
	return (u64)_InterlockedCompareExchange64((volatile long long *) dest, (long long)value, (long long)compare);
}

#define read_write_barrier() _ReadWriteBarrier()

inline u32 bitScanReverse(u32 val) {
	u32 result;
	BitScanReverse((unsigned long *) &result, val);
	return result;
}
inline u64 bitScanForward64(u64 val) {
	u32 result;
	BitScanForward64((unsigned long *) &result, val);
	return result;
}

inline s64 atomicFetchAdd(volatile s64 *target, s64 value) {
	return InterlockedExchangeAdd64(target, value);
}

inline u64 performance_time() {
	u64 time;
	QueryPerformanceCounter((LARGE_INTEGER *) &time);
	return time;
}

inline u64 performance_frequency() {
	u64 frequency;
	QueryPerformanceFrequency((LARGE_INTEGER *) &frequency);
	return frequency;
}

#elif BUILD_LINUX

#include <sched.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dlfcn.h>
#include <dirent.h>
#include <deque>

bool fileExists(const char *file);

bool directoryExists(const char *file);

FILE *fopen_utf8(const char *filename, const char *mode);

inline u32 bitScanReverse(u32 val) {
    return 31 - (u32)__builtin_clz(val);
}
inline u64 bitScanForward64(u64 val) {
    return (u64)__builtin_ctz(val);
}

template <typename T>
bool CompareExchange(T volatile *dest, T value, T compare) {
	__atomic_compare_exchange_n(dest, &compare, value, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
	return compare;
}

inline s64 atomicFetchAdd(volatile s64 *target, s64 value) {
	return __atomic_fetch_add(target, value, __ATOMIC_SEQ_CST);
}

#define read_write_barrier() asm volatile("": : :"memory")


inline u64 performance_time() {
	timespec tp;
	
	clock_gettime(CLOCK_MONOTONIC_RAW, &tp);

	return (u64) tp.tv_sec * 1'000'000'000ULL + (u64) tp.tv_nsec;
}

inline u64 performance_frequency() {
	return 1'000'000'000ULL;
}

#endif

