#pragma once

#include "String.h"
#include "Basic.h"
#include "ArraySet.h"

inline bool SHUFFLE_JOBS = false;
inline volatile bool hadError = false;
inline std::thread::id mainThread;


struct Build_Options {
	enum class Backend : u64 {
		X64 = 0,
		LLVM = 1
	};

	struct C_Runtime_Library {
		static const u64 DYNAMIC = 0x1;
		static const u64 DEBUG   = 0x2;
		static const u64 FORCED = 0x4;

		static const u64  STATIC       = 0;
		static const u64  STATIC_DEBUG = STATIC  | DEBUG;
		static const u64 DYNAMIC_DEBUG = DYNAMIC | DEBUG;
	};

	Backend backend = Backend::X64;
	u64 c_runtime_library = C_Runtime_Library::STATIC;
	MiloArray<MiloString> llvm_options = { nullptr, 0 };
	MiloString output_name;
	MiloString icon_name;
	bool show_console = true;
};

inline Build_Options buildOptions;
inline MiloArray<MiloString> buildArguments;
inline char *objectFileName;

struct FileInfo { // @Platform
	String path;
	u32 fileUid;

	char *data;
	u32 size;

	struct Module *module;

	u32 offsetInStringTable;
};

FileInfo *getFileInfoByUid(u32 fileUid);

inline Array<FileInfo *> compilerFiles;
inline Array<struct Module *> modules;

struct Module *getModule(String name);

struct Library {
	String name;
	HMODULE handle;

	bool operator==(const Library &other) const {
		return other.name == name;
	}
};

inline struct Module *runtimeModule;
inline struct Module *mainModule;

inline bool printDiagnostics = false;
inline bool noDce = false;

inline u32 loadsPending = 0;

void loadNewFile(String file, struct Module *module);

inline ArraySet<Library> libraries;

String msprintf(const char *format, ...);

inline bool linkLibC = false;

inline char modulePath[1024];

#define PROGRAM_START "__program_start"
inline struct ExprFunction *programStart;
inline volatile long totalLines;