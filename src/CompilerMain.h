#pragma once

#include "String.h"
#include "Basic.h"
#include "ArraySet.h"

inline volatile bool hadError = false;
inline std::thread::id inferThread;
inline std::thread::id mainThread;


struct FileInfo { // @Platform
	u64 fileIndex;
	u32 volumeSerialNumber;
	u32 fileUid;
	String path;
	HANDLE handle;

	char *data;
	u64 size;

	u32 offsetInStringTable;

	bool operator==(const FileInfo &other) const {
		return fileIndex == other.fileIndex && volumeSerialNumber == other.volumeSerialNumber;
	}
};

bool loadNewFile(String file);
FileInfo *getFileInfoByUid(u32 fileUid);

Array<FileInfo> getAllFilesNoLock();

struct Library {
	String name;
	HMODULE handle;

	bool operator==(const Library &other) const {
		return other.name == name;
	}
};

inline ArraySet<Library> libraries;

#ifdef BUILD_WINDOWS
#define CHECK_PRINTF _Printf_format_string_
#else
// @Incomplete: other compilers have versions of this
#define CHECK_PRINTF
#endif

void reportError(struct Expr *location, CHECK_PRINTF const char *format, ...);
void reportError(struct Declaration *location, CHECK_PRINTF const char *format, ...);
void reportError(struct Token *location, CHECK_PRINTF const char *format, ...);
void reportError(struct CodeLocation *location, CHECK_PRINTF const char *format, ...);
void reportError(CodeLocation *start, EndLocation *end, CHECK_PRINTF const char *format, ...);
void reportExpectedError(struct Token *location, CHECK_PRINTF const char *format, ...);
void reportError(CHECK_PRINTF const char *format, ...);