#include "Basic.h"

#include "CompilerMain.h"
#include "Parser.h"
#include "Infer.h"
#include "IrGenerator.h"
#include "CoffWriter.h"
#include "ArraySet.h"
#include "UTF.h"
#include "Lexer.h"
#include "TypeTable.h"
#include "Find.h"

#if BUILD_WINDOWS
bool doColorPrint = false; // False by default, set at startup if color can be enabled
#else
bool doColorPrint = true;
#endif


std::mutex filesMutex;

static ArraySet<FileInfo> files;

bool loadNewFile(String file) {
	// Open the file here and keep it open until we parse it to avoid race condition
	HANDLE handle = createFileUtf8(file, GENERIC_READ, FILE_SHARE_READ, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN); // @Platform

	if (handle == INVALID_HANDLE_VALUE) {
		reportError("Error: failed to open file: %.*s", STRING_PRINTF(file));
		return false;
	}

	BY_HANDLE_FILE_INFORMATION fileInfo;

	if (!GetFileInformationByHandle(handle, &fileInfo)) {
		reportError("Error: failed to read file information for file: %.*s", STRING_PRINTF(file));
		return false;
	}

	if (fileInfo.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) { // Are there any other attributes we should check for?
		reportError("Error: %.*s is a directory, you can only compile files", STRING_PRINTF(file));
		return false;
	}

	FileInfo info;
	info.path = file;
	info.handle = handle;
	info.volumeSerialNumber = fileInfo.dwVolumeSerialNumber;
	info.fileIndex = (static_cast<u64>(fileInfo.nFileIndexHigh) << 32ULL) | static_cast<u64>(fileInfo.nFileIndexLow);


	{
		ScopeLock fileLock(filesMutex);
		info.fileUid = static_cast<u32>(files.size());

		if (!files.add(info)) {
			CloseHandle(handle);
		}
	}

	return true;
}

FileInfo *getFileInfoByUid(u32 fileUid) {
	FileInfo *info;

	{
		ScopeLock fileLock(filesMutex);
		info = &files[fileUid];
	}

	assert(info->fileUid == fileUid);

	return info;
}

Array<FileInfo> getAllFilesNoLock() {
	return files.array;
}

void displayErrorLocation(CodeLocation *start, EndLocation *end) {
	auto info = getFileInfoByUid(start->fileUid);

	char *file = info->data;
	char *errorStart = file + start->locationInMemory;
	char *errorEnd = file + end->locationInMemory;

	do {
		--errorStart;
		assert(errorStart >= file);
	} while (!utf8ByteCount(*errorStart));

	char *lineStart = errorStart;

	while (lineStart != file) {
		--lineStart;

		if (*lineStart == '\n' || *lineStart == '\r') {
			++lineStart;
			break;
		}
	}

	char *lineEnd = errorEnd;

	while (lineEnd != file + info->size) {
		if (*lineEnd == '\n' || *lineEnd == '\r') {
			break;
		}

		++lineEnd;
	}

	String pre = { lineStart, errorStart };
	String error = { errorStart, errorEnd };
	String post = { errorEnd, lineEnd };

	if (doColorPrint) {
		printf("%.*s\x1b[91m%.*s\x1b[0m%.*s\n", STRING_PRINTF(pre), STRING_PRINTF(error), STRING_PRINTF(post));
	}
	else {
		printf("%.*s%.*s%.*s\n", STRING_PRINTF(pre), STRING_PRINTF(error), STRING_PRINTF(post));
	}
}

void printErrorLocation(CodeLocation *location) {
	String filename = getFileInfoByUid(location->fileUid)->path;

	printf("%.*s:%" PRIu32 ",%" PRIu32 " ", STRING_PRINTF(filename), location->line, location->column);

}

void reportError(CHECK_PRINTF const char *format, ...) {
	hadError = true;

	va_list args;
	va_start(args, format);

	vprintf(format, args);
	puts("");

	va_end(args);

}

void reportError(Expr *location, CHECK_PRINTF const char *format, ...) {
	printErrorLocation(&location->start);
	hadError = true;

	va_list args;
	va_start(args, format);

	vprintf(format, args);
	puts("");

	va_end(args);

	displayErrorLocation(&location->start, &location->end);
}



void reportError(CodeLocation *start, EndLocation *end, CHECK_PRINTF const char *format, ...) {
	printErrorLocation(start);
	hadError = true;

	va_list args;
	va_start(args, format);

	vprintf(format, args);
	puts("");

	va_end(args);

	displayErrorLocation(start, end);
}

void reportError(CodeLocation *location, CHECK_PRINTF const char *format, ...) {
	printErrorLocation(location);

	hadError = true;

	va_list args;
	va_start(args, format);

	vprintf(format, args);
	puts("");

	va_end(args);
}


void reportError(Declaration *location, CHECK_PRINTF const char *format, ...) {
	printErrorLocation(&location->start);

	hadError = true;

	va_list args;
	va_start(args, format);

	vprintf(format, args);
	puts("");

	va_end(args);

	displayErrorLocation(&location->start, &location->end);
}

void reportError(Token *location, CHECK_PRINTF const char *format, ...) {
	printErrorLocation(&location->start);

	hadError = true;

	va_list args;
	va_start(args, format);

	vprintf(format, args);
	puts("");

	va_end(args);

	displayErrorLocation(&location->start, &location->end);
}

char *mprintf(const char *format, ...) {

	va_list args1;
	va_start(args1, format);

	va_list args2;
	va_copy(args2, args1);

	s64 size = 1LL + vsnprintf(NULL, 0, format, args1);
	va_end(args1);

	char *buffer = static_cast<char *>(malloc(size));

	vsprintf_s(buffer, size, format, args2);

	va_end(args2);

	return buffer;
}

wchar_t *mprintf(const wchar_t *format, ...) {

	va_list args1;
	va_start(args1, format);

	va_list args2;
	va_copy(args2, args1);

	s64 size = 1LL + _vsnwprintf(NULL, 0, format, args1);
	va_end(args1);

	wchar_t *buffer = static_cast<wchar_t *>(malloc(sizeof(wchar_t) * size));

	vswprintf_s(buffer, size, format, args2);

	va_end(args2);

	return buffer;
}

void reportExpectedError(Token *location, CHECK_PRINTF const char *format, ...) {
	if (location->type == TokenT::INVALID) { // If it was invalid assume that the lexer already reported an error, don't print the error so we don't double report
		assert(hadError);
	}
	else {
		printErrorLocation(&location->start);

		hadError = true;

		va_list args;
		va_start(args, format);

		vprintf(format, args);

		va_end(args);

		String token = getTokenString(location);
		printf(" but got %.*s", STRING_PRINTF(token));
		puts("");

		displayErrorLocation(&location->start, &location->end);
	}
}

void stompLastBackslash(char *name) {
	char *lastBackslash = nullptr;
	for (char *cursor = name; *cursor; ++cursor) {
		if (*cursor == '\\') lastBackslash = cursor;
	}

	if (lastBackslash) {
		*lastBackslash = 0;
	}
}

void stompLastBackslash(wchar_t *name) {
	wchar_t *lastBackslash = nullptr;
	for (wchar_t *cursor = name; *cursor; ++cursor) {
		if (*cursor == '\\') lastBackslash = cursor;
	}

	if (lastBackslash) {
		*lastBackslash = 0;
	}
}

bool fileExists(wchar_t *file) {
	DWORD attributes = GetFileAttributesW(file);

	return (attributes != INVALID_FILE_ATTRIBUTES &&
		!(attributes & FILE_ATTRIBUTE_DIRECTORY));
}

bool directoryExists(wchar_t *file) {
	DWORD attributes = GetFileAttributesW(file);

	return (attributes != INVALID_FILE_ATTRIBUTES &&
		(attributes & FILE_ATTRIBUTE_DIRECTORY));
}

int main(int argc, char *argv[]) {
#if BUILD_WINDOWS
	{
		HANDLE console = GetStdHandle(STD_OUTPUT_HANDLE);
		if (console != INVALID_HANDLE_VALUE) {
			DWORD mode;

			if (GetConsoleMode(console, &mode)) {
				if (SetConsoleMode(console, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING)) {
					doColorPrint = true;
				}
			}
		}
	}
#endif
	using namespace std::chrono;

#if BUILD_PROFILE
	u64 startTime;
	QueryPerformanceCounter(reinterpret_cast<LARGE_INTEGER *>(&startTime));
#endif

	char *input = nullptr;
	bool useLlvm = false;

	for (int i = 1; i < argc; i++) {
		if (strcmp("-llvm", argv[i]) == 0) {
			if (useLlvm) {
				reportError("Error: Cannot specify LLVM codegen more than once");
				return 1;
			}

			useLlvm = true;
		}
		else {
			if (input) {
				reportError("Error: Cannot more than one input file");
				return 1;
			}

			input = argv[i];
		}
	}

	if (!input) {
		reportError("Error: Expected name of input file");
		reportError("Usage: %s <file>", argv[0]);
	}

	auto start = high_resolution_clock::now();

	if (!hadError && loadNewFile("runtime.milo") && loadNewFile(String(input))) {
		setupTypeTable();

		std::thread infer(runInfer);
		std::thread irGenerator(runIrGenerator);
		std::thread backend = std::thread(useLlvm ? runLlvm : runCoffWriter);

		mainThread = std::this_thread::get_id();
		inferThread = infer.get_id();

		SetThreadDescription(infer.native_handle(), L"Infer");
		SetThreadDescription(irGenerator.native_handle(), L"Ir Generator");
		SetThreadDescription(backend.native_handle(), useLlvm ? L"LLVM" : L"Coff Writer");

		infer.detach();
		irGenerator.detach();
		{
			u64 i;
			for (i = 0; i < files.size(); i++) {
				parseFile(getFileInfoByUid(static_cast<u32>(i)));

				if (hadError) {
					break;
				}
			}

			inferQueue.add(static_cast<Declaration *>(nullptr));

			for (++i; i < files.size(); i++) { // In the event we had an error so stopped parsing files mid way through
				CloseHandle(files[i].handle);
			}
		}

		backend.join();
	}

#if BUILD_WINDOWS

	if (!hadError) {
		u64 totalQueued = totalDeclarations + totalFunctions + totalTypesSized + totalImporters;

		printf(
			"Total queued: %llu\n"
			"  %llu declarations\n"
			"  %llu functions\n"
			"  %llu types\n"
			"  %llu importers\n"
			"Total infers: %llu, %.1f infers/queued\n"
			"Total sizes: %llu, %.1f sizes/type\n",
			totalQueued, totalDeclarations, totalFunctions, totalTypesSized, totalImporters, totalInfers, static_cast<float>(totalInfers) / totalQueued, totalSizes, static_cast<float>(totalSizes) / totalTypesSized);

		std::cout << "Compiler Time: " << (duration_cast<microseconds>(duration<double>(
			high_resolution_clock::now() - start)).count() / 1000.0) << "ms\n";

		auto linkerStart = high_resolution_clock::now();

		wchar_t buffer[1024];

		wchar_t *linkerPath;
		wchar_t *libPath;


		{
			wchar_t name[1024]; // @Robustness
			GetModuleFileNameW(NULL, name, sizeof(name));

			stompLastBackslash(name);

			wchar_t *cacheFile = mprintf(L"%s\\%s", name, L"linker.cache");

			bool linkerFindFailed = false;

			if (FILE *cache = _wfopen(cacheFile, L"rb")) {
				u16 length;

#define read(dest, count) ((count) == fread(dest, sizeof(*(dest)), count, cache))

				if (!read(&length, 1)) {
					printf("Failed to read linker cache\n");
					fclose(cache);
					goto linkerCacheFail;
				}

				linkerPath = new wchar_t[length];

				if (!read(linkerPath, length)) {
					printf("Failed to read linker cache\n");
					fclose(cache);
					goto linkerCacheFail;
				}

				if (!read(&length, 1)) {
					printf("Failed to read linker cache\n");
					fclose(cache);
					goto linkerCacheFail;
				}

				libPath = new wchar_t[length];

				if (!read(libPath, length)) {
					printf("Failed to read linker cache\n");
					fclose(cache);
					goto linkerCacheFail;
				}

				fclose(cache);

				if (!fileExists(linkerPath)) {
					printf("Linker cache had invalid linker\n");
					goto linkerCacheFail;
				}

				if (!directoryExists(libPath)) {
					printf("Linker cache had invalid library path\n");
					goto linkerCacheFail;
				}
			}
			else {
				printf("Failed to find cached linker, searching for linker\n");

			linkerCacheFail:

				PROFILE_ZONE("Find linker");


				Find_Result result = find_visual_studio_and_windows_sdk();

				if (!result.vs_exe_path) {
					reportError("Couldn't find linker");
					return 1;
				}
				else if (!result.windows_sdk_um_library_path) {
					reportError("Couldn't find libraries");
					return 1;
				}

				linkerPath = mprintf(L"%s\\%s", result.vs_exe_path, L"link.exe");
				libPath = result.windows_sdk_um_library_path;

				if (FILE *cache = _wfopen(cacheFile, L"wb")) {
					u16 length = lstrlenW(linkerPath) + 1;

#define write(src, count) fwrite(src, sizeof(*(src)), count, cache)

					write(&length, 1);
					write(linkerPath, length);


					length = lstrlenW(libPath) + 1;

					write(&length, 1);
					write(libPath, length);

					fclose(cache);
				}
			}

			_snwprintf(buffer, 1024, L"\"%s\" out.obj /debug /entry:main kernel32.lib user32.lib gdi32.lib opengl32.lib \"/libpath:%s\" /incremental:no /nologo", linkerPath, libPath);
		}


		fwprintf(stdout, L"Linker command: %s\n", buffer);

		STARTUPINFOW startup = {};
		startup.cb = sizeof(STARTUPINFOW);

		PROCESS_INFORMATION info;

		if (!CreateProcessW(NULL, buffer, NULL, NULL, false, 0, NULL, NULL, &startup, &info)) {
			std::cout << "Failed to run linker command" << std::endl;
		}

		CloseHandle(info.hThread);
		{
			PROFILE_ZONE("Wait for linker");
			WaitForSingleObject(info.hProcess, INFINITE);
		}
		CloseHandle(info.hProcess);

		std::cout << "Linker Time: " << (duration_cast<microseconds>(duration<double>(
			high_resolution_clock::now() - linkerStart)).count() / 1000.0) << "ms\n";
	}
#else
	// @Platform
#error "Non windows builds are not supported" 
#endif

#if BUILD_PROFILE
	{
		std::ofstream out("profile.json", std::ios::out | std::ios::trunc);

		u64 pcf;
		QueryPerformanceFrequency(reinterpret_cast<LARGE_INTEGER *>(&pcf));

		out << '[';
		for (u32 i = 0; i < static_cast<u32>(profileIndex); i++) {
			Profile p = profiles[i];

			out << "{\"cat\":\"function\",\"pid\":0,\"tid\":" << p.threadId << ",\"ts\":" << ((p.time - startTime) * 1.0e9 / (double) pcf);

			if (p.name) {
				out << ",\"ph\":\"B\",\"name\":\"" << p.name;

				if (p.data) {
					out << "\",\"args\":{\"data\":\"" << p.data << "\"}}";
				}
				else {
					out << "\"}";
				}
			}
			else {
				out << ",\"ph\":\"E\"}";
			}

			if (i != static_cast<u32>(profileIndex) - 1) {
				out << ",\n";
			}

		}

		out << ']';
	}

#endif



#if BUILD_DEBUG
	std::cin.get();
#endif

	return hadError;
}

