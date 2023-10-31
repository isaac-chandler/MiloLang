#include "Basic.h"

#include "OS.h"

#include "UTF.h"

#if BUILD_WINDOWS

void set_working_directory(char *directory) {
	auto dir = utf8ToWString(directory);
	SetCurrentDirectoryW(dir);
	free(dir);
}

void yieldThread() {
	SwitchToThread();
}

bool fileExists(const char *file) {
	wchar_t *filename = utf8ToWString(file);

	u32 attributes = GetFileAttributesW(filename);

	free(filename);
	return attributes != INVALID_FILE_ATTRIBUTES && !(attributes & FILE_ATTRIBUTE_DIRECTORY);
}

bool directoryExists(const char *file) {
	wchar_t *filename = utf8ToWString(file);

	u32 attributes = GetFileAttributesW(filename);

	free(filename);
	return attributes != INVALID_FILE_ATTRIBUTES && (attributes & FILE_ATTRIBUTE_DIRECTORY);
}

char *exePath() {
	u32 bufferSize = 256;

	while (true) {
		wchar_t *buffer = new wchar_t[bufferSize];

		if (GetModuleFileNameW(nullptr, buffer, bufferSize) < bufferSize) {
			char *path = wStringToUtf8(buffer);
			delete[] buffer;
			return path;
		}

		delete[] buffer;
		bufferSize *= 2;
	}
}

char *fullPath(const char *filename) {
	wchar_t *wfilename = utf8ToWString(filename);

	wchar_t dummy;
	u32 charCount = GetFullPathNameW(wfilename, 1, &dummy, nullptr);
	wchar_t *buffer = new wchar_t[charCount];
	GetFullPathNameW(wfilename, charCount, buffer, nullptr);
	free(wfilename);

	char *result = wStringToUtf8(buffer);
	delete[] buffer;

	return result;
}

FILE *fopen_utf8(const char *filename, const char *mode) {
	wchar_t wmode[4];

	int i = 0;
	while (i + 1 < sizeof(wmode) && mode[i]) {
		wmode[i] = mode[i];
		i++;
	}
	wmode[i] = 0;

	wchar_t *wfilename = utf8ToWString(filename);
	FILE *file;

	if (_wfopen_s(&file, wfilename, wmode)) {
		free(wfilename);
		return nullptr;
	}
	free(wfilename);

	return file;
}

void *open_library(const char *name) {
	wchar_t *wname = utf8ToWString(name);
	HMODULE lib = LoadLibraryW(wname);

	free(wname);
	return lib;
}

void *library_find(void *library, const char *symbol) {
	return GetProcAddress(static_cast<HMODULE>(library), symbol);
}

void set_working_directory(const char *directory) {
	wchar_t *wname = utf8ToWString(directory);
	SetCurrentDirectoryW(wname);
	free(wname);
}

void setThreadName(std::thread &thread, const char *name) {
	SetThreadDescription(thread.native_handle(), utf8ToWString(name));
}


#elif BUILD_LINUX

#include <sched.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dlfcn.h>
#include <dirent.h>
#include <deque>

void yieldThread() {
	sched_yield();
}

bool fileExists(const char *file) {
	struct stat buf;

	if (stat(file, &buf) == 0) {
		return S_ISREG(buf.st_mode);
	}

	return false;
}

bool directoryExists(const char *file) {
	struct stat buf;

	if (stat(file, &buf) == 0) {
		return S_ISDIR(buf.st_mode);
	}

	return false;
}

char *exePath() {
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

char *fullPath(const char *filename) {
	return realpath(filename, nullptr);
}

void setThreadName(std::thread &thread, const char *name) {
	pthread_setname_np(thread.native_handle(), name);
}

FILE *fopen_utf8(const char *filename, const char *mode) {
	return fopen(filename, mode);
}



char *findLibrary(char *path, const char *name) {
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

void *open_library(const char *name) {
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

void *library_find(void *library, const char *symbol) {
	return dlsym(library, symbol);
}

void set_working_directory(char *directory) {
	chdir(directory);
}

#endif

