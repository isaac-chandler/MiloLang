#pragma once

#define _SILENCE_CXX17_ITERATOR_BASE_CLASS_DEPRECATION_WARNING

#include <iostream>

#if BUILD_WINDOWS
#include <Windows.h>
#include <Shlwapi.h>
#endif

#if !BUILD_NO_LLVM
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
#include <llvm/InitializePasses.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/CodeGen/CommandFlags.h>
#include <llvm/DebugInfo/PDB/PDB.h>
#pragma warning(pop)
#else
namespace llvm {
	class Value;
	class GlobalVariable;
	class Function;
	class Type;
	class DIType;
	class BasicBlock;
}

#endif

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
#include <string_view>
#include <cinttypes>
#include <stdarg.h>
#include <immintrin.h>


#undef small
#undef min
#undef max
#undef CONST
#undef TRUE
#undef FALSE
#undef VOID

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

#define CONCAT2(x, y) x##y
#define CONCAT(x, y) CONCAT2(x, y)

#if BUILD_PROFILE
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

template<typename T>
struct AtExit {
	T lambda;
	AtExit(T lambda) :lambda(lambda) {}
	~AtExit() { lambda(); }
};

class AtExitHelp {
public:
	template<typename T>
	AtExit<T> operator+(T t) { return t; }
};

#define at_exit const auto &CONCAT(at_exit_, __LINE__) = AtExitHelp() + [&]()

template<typename T>
T validate_(T value) {
	assert(value);
	return value;
}

#if BUILD_DEBUG
#define validate(x) validate_(x)
#else
#define validate(...)
#endif


typedef unsigned char      u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
typedef unsigned long long u64;

typedef signed char s8;
typedef short       s16;
typedef int         s32;
typedef long long   s64;

template<typename T>
struct MiloArray {
	T *data;
	u64 count;
};

struct MiloString {
	u8 *data;
	u64 count;
};

#if BUILD_LINUX
#pragma GCC diagnostic ignored "-Winvalid-offsetof"
#pragma GCC diagnostic ignored "-Wvolatile"
#endif

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
};

#define PROFILER_THREADS 10


inline Profile *volatile profiles[PROFILER_THREADS];
inline std::atomic_int32_t perThreadIndex(0);
inline thread_local Profile *profileIndex;

struct Timer {
	__forceinline Timer(const char *name) {

		Profile *write = profileIndex++;

		write->name = name;

		read_write_barrier();
		write->time = __rdtsc();
		read_write_barrier();
	}

	__forceinline Timer(const char *name, const char *color) {
		Profile *write = profileIndex++;

		write->name = name;
		write->color = color;

		read_write_barrier();
		write->time = __rdtsc();
		read_write_barrier();
	}

	__forceinline ~Timer() {
		Profile *write = profileIndex++;

		read_write_barrier();
		write->time = __rdtsc();
		read_write_barrier();
	}
};

#endif