#pragma once

#include "String.h"
#include "Basic.h"
#include "ArraySet.h"

inline volatile bool hadError = false;
inline std::thread::id mainThread;


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

inline u32 loadsPending = 0;

void loadNewFile(String file, struct Module *module);

inline ArraySet<Library> libraries;

String msprintf(const char *format, ...);