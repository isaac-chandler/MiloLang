#pragma once

#include <iostream>
#include <Windows.h>
#include <assert.h>
#include <cstring>
#include <stdint.h>
#include <initializer_list>
#include <atomic>
#include <fstream>
#include <sstream>
#include <thread>
#include <algorithm>
#include <stdlib.h>
#include <mutex>
#include <immintrin.h>
#include <string_view>
#include <cinttypes>
#include <stdarg.h>


#undef small
#undef min
#undef max
#undef CONST
#undef TRUE
#undef FALSE
#undef VOID



#pragma warning(push)
#pragma warning(disable: 4141 4146 4244 4267 4530 4624 4996)
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/Host.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#pragma warning(pop)

#pragma warning(error: 4715)

// Magic color names for chrome://tracing
#define PROFILE_BLUE "vsync_highlight_color"
#define PROFILE_BLACK "black"
#define PROFILE_LIGHT_BLUE "thread_state_runnable"
#define PROFILE_OLIVE "olive"
#define PROFILE_YELLOW "yellow"
#define PROFILE_WHITE "white"
#define PROFILE_ORANGE "bad"
#define PROFILE_LIGHT_GREEN "thread_state_running"
#define PROFILE_DARK_RED "terrible"


#if BUILD_PROFILE
#define CONCAT2(x, y) x##y
#define CONCAT(x, y) CONCAT2(x, y)
#define PROFILE_FUNC(...)       Timer CONCAT(timer,__LINE__)(__FUNCTION__, __VA_ARGS__)
#define PROFILE_ZONE(name, ...) Timer CONCAT(timer,__LINE__)(name, __VA_ARGS__)
#else
#define PROFILE_FUNC(...)
#define PROFILE_ZONE(...)
#endif

#define ARRAY_COUNT(arr) (sizeof(arr) / (sizeof(arr[0])))

#define FLAGS_ARE_SAME(a, b, flags) (	(( (a)  ^  (b) ) & (flags) ) == 0	)

#define AlignPO2(a, alignment) ((static_cast<u32>(a) + (static_cast<u32>(alignment) - 1)) & ~(static_cast<u32>(alignment) - 1))

#define prefetch(address, hint) (_mm_prefetch(reinterpret_cast<const CHAR *>(address), (hint)))


typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

#define CAST_FROM_SUBSTRUCT(castTo, subStruct, castFrom) (	reinterpret_cast<castTo *>(reinterpret_cast<u8 *>(castFrom) - offsetof(castTo, subStruct)) + ((decltype(castFrom)) 0 - (decltype(&((castTo *) 0)->subStruct)) 0)	)

struct EndLocation {
	u32 locationInMemory;
	u32 line;
	u32 column;
};


struct CodeLocation : EndLocation {
	u32 fileUid;
};

#define my_min(a, b) ((a) < (b) ? (a) : (b))
#define my_max(a, b) ((a) > (b) ? (a) : (b))

struct ScopeLock {
	std::mutex &lock;

	ScopeLock(std::mutex &lock) : lock(lock) { lock.lock(); }
	~ScopeLock() { lock.unlock(); }
};

#if BUILD_PROFILE
struct Profile {
	const char *name;
	const char *color;
	u64 time;
	s32 threadId;
};

inline Profile profiles[10000000];
inline std::atomic_uint32_t profileIndex = 0;

struct Timer {
	Timer(const char *name) {
		u32 write = profileIndex++;

		profiles[write].name = name;

		QueryPerformanceCounter(reinterpret_cast<LARGE_INTEGER *>(&profiles[write].time));

		profiles[write].threadId = *reinterpret_cast<s32 *>(reinterpret_cast<u8 *>(__readgsqword(0x30)) + 0x48);
	}

	Timer(const char *name, const char *color) {
		u32 write = profileIndex++;

		profiles[write].name = name;
		profiles[write].color = color;

		QueryPerformanceCounter(reinterpret_cast<LARGE_INTEGER *>(&profiles[write].time));

		profiles[write].threadId = *reinterpret_cast<s32 *>(reinterpret_cast<u8 *>(__readgsqword(0x30)) + 0x48);
	}

	__declspec(noinline) ~Timer() {
		u32 write = profileIndex++;

		QueryPerformanceCounter(reinterpret_cast<LARGE_INTEGER *>(&profiles[write].time));

		profiles[write].threadId = *reinterpret_cast<s32 *>(reinterpret_cast<u8 *>(__readgsqword(0x30)) + 0x48);
	}
};

#endif