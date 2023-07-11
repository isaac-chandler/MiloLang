#pragma once

#include "Basic.h"

#if BUILD_WINDOWS

template <typename T>
bool CompareExchangePointers(T *volatile *dest, T *value, T *compare) {
	reinterpret_cast<T *>(_InterlockedCompareExchange64(reinterpret_cast<volatile s64 *>(dest), reinterpret_cast<s64>(value), reinterpret_cast<s64>(compare))) == compare;
}

#define read_write_barrier() _ReadWriteBarrier()

#elif BUILD_LINUX

#include <sched.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dlfcn.h>
#include <dirent.h>
#include <deque>

inline u32 bitScanReverse(u32 val) {
    return 31 - (u32)__builtin_clz(val);
}
inline u64 bitScanForward64(u64 val) {
    return (u64)__builtin_ctz(val);
}

template <typename T>
bool CompareExchange(T volatile *dest, T value, T compare) {
	return __atomic_compare_exchange_n(dest, &value, compare, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}

inline s64 atomicFetchAdd(volatile s64 *target, s64 value) {
	return __atomic_fetch_add(target, value, __ATOMIC_SEQ_CST);
}

#define read_write_barrier() asm volatile("": : :"memory")

inline void yieldThread() {
	sched_yield();
}

inline u64 performance_time() {
	timespec tp;
	
	clock_gettime(CLOCK_MONOTONIC_RAW, &tp);

	return (u64) tp.tv_sec * 1'000'000'000ULL + (u64) tp.tv_nsec;
}

inline u64 performance_frequency() {
	return 1'000'000'000ULL;
}

inline bool fileExists(const char *file) {
	struct stat buf;

	if (stat(file, &buf) == 0) {
		return S_ISREG(buf.st_mode);
	}

	return false;
}

inline bool directoryExists(const char *file) {
	struct stat buf;

	if (stat(file, &buf) == 0) {
		return S_ISDIR(buf.st_mode);
	}

	return false;
}

inline char *exePath() {
	char *buffer = nullptr;

	for (int buf_size = 256;; buf_size *= 2) {
		buffer = static_cast<char *>(realloc(buffer, buf_size));

		int result = readlink("/proc/self/exe", buffer, buf_size);

		if (result < 0)
			return nullptr;
		if (result < buf_size || buffer[buf_size - 1] == 0)
			return buffer;
	}
}

inline char *fullPath(const char *filename) {
	return realpath(filename, nullptr);
}

inline void setThreadName(std::thread &thread, const char *name) {
	pthread_setname_np(thread.native_handle(), name);
}

inline FILE *fopen_utf8(const char *filename, const char *mode) {
	return fopen(filename, mode);
}



inline char *findLibrary(char *path, const char *name) {
	extern char *mprintf(const char *format, ...);

	u64 nameLen = strlen(name);

	std::deque<char *> searchDirs;

	searchDirs.push_back(path);

	while (!searchDirs.empty()) {
		auto currentPath = searchDirs.front();
		searchDirs.pop_front();

		auto dir = opendir(currentPath);

		if (dir) {
			struct dirent *entry;

			while (entry = readdir(dir)) {
				if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, "..")) {
					continue;
				}

				if (entry->d_type == DT_DIR) {
					searchDirs.push_back(mprintf("%s/%s", currentPath, entry->d_name));
				} else {
					if (!strcmp(entry->d_name, name)) {
						closedir(dir);

						auto result =  mprintf("%s/%s", currentPath, entry->d_name);

						free(currentPath);

						for (auto dir : searchDirs) {
							free(dir);
						}

						return result;
					}

					if (!strncmp(entry->d_name, "lib", 3) && !strncmp(entry->d_name + 3, name, nameLen) && !strncmp(entry->d_name + 3 + nameLen, ".so.", 4)) {
						closedir(dir);

						auto result =  mprintf("%s/%s", currentPath, entry->d_name);

						free(currentPath);

						for (auto dir : searchDirs) {
							free(dir);
						}

						return result;
					}
				}
			}
			

			closedir(dir);
		}

		free(currentPath);
	}

	for (auto dir : searchDirs) {
		free(dir);
	}
	
	return nullptr;
}

inline void *open_library(const char *name) {
	extern char *copyString(const char*);

	auto path = findLibrary(copyString("/lib"), name);

	if (!path)
		path = findLibrary(copyString("/usr/lib"), name);

	if (!path)
		return nullptr;

	auto result = dlopen(path, RTLD_LAZY);

	free(path);

	return result;
}

inline void *library_find(void *library, const char *symbol) {
	return dlsym(library, symbol);
}

#endif

