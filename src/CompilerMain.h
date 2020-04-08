#pragma once

#include "String.h"

inline volatile bool hadError = false;



struct FileInfo { // @Platform
	u64 fileIndex;
	u32 volumeSerialNumber;
	u32 fileUid;
	String path;
	HANDLE handle;

	char *data;

	bool operator==(const FileInfo &other) const {
		return fileIndex == other.fileIndex && volumeSerialNumber == other.volumeSerialNumber;
	}
};

bool loadNewFile(String file);
FileInfo *getFileInfoByUid(u32 fileUid);

void reportError(struct Expr *location, const char *format, ...);
void reportError(struct Declaration *location, const char *format, ...);
void reportError(struct Token *location, const char *format, ...);
void reportExpectedError(struct Token *location, const char *format, ...);
void reportError(const char *format, ...);