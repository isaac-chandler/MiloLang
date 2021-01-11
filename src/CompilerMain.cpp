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
#include "Error.h"

#define NUM_PARSE_THREADS 4

#if BUILD_PROFILE

void NTAPI tls_callback(PVOID DllHandle, DWORD dwReason, PVOID) {
	if (dwReason == DLL_THREAD_ATTACH) {
		s32 thread = perThreadIndex.fetch_add(1, std::memory_order_relaxed);

		profileIndex = profiles[thread];
	}
}

#ifdef _WIN64
#pragma comment (linker, "/INCLUDE:_tls_used")  // See p. 1 below
#pragma comment (linker, "/INCLUDE:tls_callback_func")  // See p. 3 below
#else
#pragma comment (linker, "/INCLUDE:__tls_used")  // See p. 1 below
#pragma comment (linker, "/INCLUDE:_tls_callback_func")  // See p. 3 below
#endif

// Explained in p. 3 below
#ifdef _WIN64
#pragma const_seg(".CRT$XLF")
EXTERN_C const
#else
#pragma data_seg(".CRT$XLF")
EXTERN_C
#endif
PIMAGE_TLS_CALLBACK tls_callback_func = tls_callback;
#ifdef _WIN64
#pragma const_seg()
#else
#pragma data_seg()
#endif //_WIN64

#endif


std::mutex filesMutex;

void loadNewFile(String file, Module *module) {
	// Open the file here and keep it open until we parse it to avoid race condition
	FileInfo *info = new FileInfo;
	info->path = file;
	info->module = module;


	{
		ScopeLock fileLock(filesMutex);
		info->fileUid = compilerFiles.count;

		compilerFiles.add((info));

	}

	loadsPending++;
	parserQueue.add(info);
}

FileInfo *getFileInfoByUid(u32 fileUid) {
	FileInfo *info;

	{
		ScopeLock fileLock(filesMutex);
		info = compilerFiles[fileUid];
	}

	assert(info->fileUid == fileUid);

	return info;
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

String msprintf(const char *format, ...) {

	va_list args1;
	va_start(args1, format);

	va_list args2;
	va_copy(args2, args1);

	u32 size = 1 + (u32) vsnprintf(NULL, 0, format, args1);
	va_end(args1);

	char *buffer = static_cast<char *>(malloc(size));

	vsprintf_s(buffer, size, format, args2);

	va_end(args2);

	return { buffer, size - 1 };
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

Module *getModule(String name) {
	for (auto module : modules) {
		if (module->name == name) {
			return module;
		}
	}

	auto module = new Module;
	module->members.flavor = BlockFlavor::GLOBAL;
	module->name = name;
	
	if (name.length) {		
		loadNewFile("", module);
	}

	modules.add(module);

	if (runtimeModule) {
		auto name = new ExprStringLiteral;
		name->flavor = ExprFlavor::STRING_LITERAL;
		name->start = {};
		name->end = {};
		name->string = "Runtime";
		name->type = &TYPE_STRING;

		auto import = new ExprLoad;
		import->flavor = ExprFlavor::IMPORT;
		import->start = {};
		import->end = {};
		import->file = name;
		import->module = module;

		auto importer = new Importer;
		importer->import = import;

		inferQueue.add(InferQueueJob(importer, module));
	}

	return module;
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

#if 1
int main(int argc, char *argv[]) {
	setlocale(LC_ALL, "");
	initPrinter();

	using namespace std::chrono;

#if BUILD_PROFILE
	for (u32 i = 0; i < PROFILER_THREADS; i++) {
		profiles[i] = static_cast<Profile *>(VirtualAlloc(nullptr, sizeof(Profile) * 1 << 23, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE));
	}

	tls_callback(nullptr, DLL_THREAD_ATTACH, nullptr);
	u64 startTime;
	u64 startTsc = __rdtsc();
	QueryPerformanceCounter(reinterpret_cast<LARGE_INTEGER *>(&startTime));
#endif

	char *input = nullptr;
	bool useLlvm = false;

	bool noThreads = false;

	for (int i = 1; i < argc; i++) {
		if (strcmp("-llvm", argv[i]) == 0) {
			if (useLlvm) {
				reportError("Error: Cannot specify LLVM codegen more than once");
				return 1;
			}

			useLlvm = true;
		}
		if (strcmp("-no_threads", argv[i]) == 0) {
			if (noThreads) {
				reportError("Error: Cannot specify no threads more than once");
				return 1;
			}

			noThreads = true;
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

	runtimeModule = getModule("Runtime");
	mainModule = getModule("");

	if (!hadError && runtimeModule && mainModule) {
		setupTypeTable();


		for (u32 i = 0; i < (noThreads ? 1 : NUM_PARSE_THREADS); i++) {
			wchar_t name[] = L"Parser  ";

			name[7] = static_cast<wchar_t>(i + '1');

			std::thread parserThread(runParser);
			SetThreadDescription(parserThread.native_handle(), name);
			
			parserThread.detach();
		}
		std::thread irGenerator(runIrGenerator);

		mainThread = std::this_thread::get_id();

		SetThreadDescription(irGenerator.native_handle(), L"Ir Generator");

		irGenerator.detach();

		runInfer(input);

		std::cout << "Frontend Time: " << (duration_cast<microseconds>(duration<double>(
			high_resolution_clock::now() - start)).count() / 1000.0) << "ms\n";
	}


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


		auto backendStart = high_resolution_clock::now();
		if (useLlvm) {
			runLlvm();
		}
		else {
			runCoffWriter();
		}

		std::cout << (useLlvm ? "LLVM" : "Coff Writer") << " Time: " << (duration_cast<microseconds>(duration<double>(
			high_resolution_clock::now() - backendStart)).count() / 1000.0) << "ms\n";

		std::cout << "Compiler Time: " << (duration_cast<microseconds>(duration<double>(
			high_resolution_clock::now() - start)).count() / 1000.0) << "ms\n";
	}

#if BUILD_WINDOWS
	if (!hadError) {
		auto linkerStart = high_resolution_clock::now();

		wchar_t *linkerCommand;

		wchar_t *linkerPath;
		wchar_t *windowsLibPath;
		wchar_t *crtLibPath;


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

				windowsLibPath = new wchar_t[length];

				if (!read(windowsLibPath, length)) {
					printf("Failed to read linker cache\n");
					fclose(cache);
					goto linkerCacheFail;
				}

				if (!read(&length, 1)) {
					printf("Failed to read linker cache\n");
					fclose(cache);
					goto linkerCacheFail;
				}

				crtLibPath = new wchar_t[length];

				if (!read(crtLibPath, length)) {
					printf("Failed to read linker cache\n");
					fclose(cache);
					goto linkerCacheFail;
				}

				fclose(cache);

				if (!fileExists(linkerPath)) {
					printf("Linker cache had invalid linker\n");
					goto linkerCacheFail;
				}

				if (!directoryExists(windowsLibPath)) {
					printf("Linker cache had invalid library path\n");
					goto linkerCacheFail;
				}

				if (!directoryExists(crtLibPath)) {
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
				else if (!result.windows_sdk_ucrt_library_path) {
					reportError("Couldn't find libraries");
					return 1;
				}

				linkerPath = mprintf(L"%s\\%s", result.vs_exe_path, L"link.exe");
				windowsLibPath = result.windows_sdk_um_library_path;
				crtLibPath = result.vs_library_path;

				if (FILE *cache = _wfopen(cacheFile, L"wb")) {
					u16 length = lstrlenW(linkerPath) + 1;

#define write(src, count) fwrite(src, sizeof(*(src)), count, cache)

					write(&length, 1);
					write(linkerPath, length);


					length = lstrlenW(windowsLibPath) + 1;

					write(&length, 1);
					write(windowsLibPath, length);


					length = lstrlenW(crtLibPath) + 1;

					write(&length, 1);
					write(crtLibPath, length);

					fclose(cache);
				}
			}

			char libBuffer[1024]; // @Robustness

			{
				char *libOffset = libBuffer;

				for (auto lib : libraries) {
					*(libOffset++) = ' ';
					memcpy(libOffset, lib.name.characters, lib.name.length);
					libOffset += lib.name.length;
					*(libOffset++) = '.';
					*(libOffset++) = 'l';
					*(libOffset++) = 'i';
					*(libOffset++) = 'b';
				}

				*libOffset = 0;
			}


			linkerCommand = mprintf(L"\"%s\" out.obj __milo_chkstk.obj /debug /entry:main%S \"/libpath:%s\" \"/libpath:%s\" /incremental:no /nologo /natvis:milo.natvis",
				linkerPath, libBuffer, windowsLibPath, crtLibPath);
		}


		fwprintf(stdout, L"Linker command: %s\n", linkerCommand);

		STARTUPINFOW startup = {};
		startup.cb = sizeof(STARTUPINFOW);

		PROCESS_INFORMATION info;

		if (!CreateProcessW(NULL, linkerCommand, NULL, NULL, false, 0, NULL, NULL, &startup, &info)) {
			std::cout << "Failed to run linker command" << std::endl;
		}

		CloseHandle(info.hThread);
		{
			PROFILE_ZONE("Wait for linker");
			WaitForSingleObject(info.hProcess, INFINITE);
		}

		// Make sure a failing exit code is returned if the linker fails
		DWORD exitCode;
		GetExitCodeProcess(info.hProcess, &exitCode);
		if (exitCode) {
			hadError = true;
		}
		
		CloseHandle(info.hProcess);

		std::cout << "Linker Time: " << (duration_cast<microseconds>(duration<double>(
			high_resolution_clock::now() - linkerStart)).count() / 1000.0) << "ms\n";

		std::cout << "Total Time: " << (duration_cast<microseconds>(duration<double>(
			high_resolution_clock::now() - start)).count() / 1000.0) << "ms\n";
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

		u64 endTime;
		u64 endTsc = __rdtsc();

		QueryPerformanceCounter(reinterpret_cast<LARGE_INTEGER *>(&endTime));

		double tscFactor = 1.0e9 * (double) (endTime - startTime) / (double) (endTsc - startTsc) / (double) pcf;

		out << '[';
		bool first = true;

		for (u32 j = 0; j < PROFILER_THREADS; j++) {

			if (!profiles[j])
				continue;

			for (Profile *i = profiles[j]; i->time; i++) {
				if (!first) {
					out << ",\n";
				}
				first = false;
			
				Profile p = *i;

				out << "{\"cat\":\"function\",\"pid\":0,\"tid\":" << j << ",\"ts\":" << ((p.time - startTsc) * tscFactor);

				if (p.name) {
					out << ",\"ph\":\"B\",\"name\":\"" << p.name;

					if (p.color) {
						out << "\",\"cname\":\"" << p.color;
					}

					out << "\"}";
				}
				else {
					out << ",\"ph\":\"E\"}";
				}
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
#else

using namespace std::chrono;

MPMCWorkQueue<long> queue1;
MPMCWorkQueue<long> queue2;

double sInsertSpeed;
double sRemoveSpeed;

double mInsertSpeed;
double mRemoveSpeed;

std::mutex speedLock;

#define TSIZE 10'000'000


DWORD testSPSCInserter(void *param) {
	auto start = high_resolution_clock::now();

	for (u64 i = 1; i <= TSIZE; i++) {
		queue1.add(i);
	}


	sInsertSpeed = 10.0 / duration<double, seconds::period>(high_resolution_clock::now() - start).count();

	return 0;
}

DWORD testSPSCExtractor(void *param) {
	auto start = high_resolution_clock::now();

	for (u64 i = 1; i <= TSIZE; i++) {
		int x = queue1.take();

		if (x != i)
			printf("Error\n");
	}

	sRemoveSpeed = 10.0 / duration<double, seconds::period>(high_resolution_clock::now() - start).count();

	return 0;
}

long long volatile mpmcTest[TSIZE];

DWORD testMPMCInserter(void *param) {
	auto start = high_resolution_clock::now();

	for (u64 i = 1; i <= TSIZE; i++) {
		queue2.add(i);
	}

	queue2.add(0);

	speedLock.lock();
	mInsertSpeed += 10.0 / duration<double, seconds::period>(high_resolution_clock::now() - start).count();
	speedLock.unlock();

	return 0;
}

DWORD testMPMCExtractor(void *param) {
	auto start = high_resolution_clock::now();

	while (true) {
		long x = queue2.take();

		if (x == 0)
			break;
		else {
			_InterlockedExchangeAdd64(&mpmcTest[x - 1], 1);
		}

		x = 0;
	}

	speedLock.lock();
	mRemoveSpeed += 10.0 / duration<double, seconds::period>(high_resolution_clock::now() - start).count();
	speedLock.unlock();

	return 0;
}

void main() {
	CreateThread(NULL, 0, testSPSCInserter, NULL, 0, NULL);
	auto t2 = CreateThread(NULL, 0, testSPSCExtractor, NULL, 0, NULL);

	WaitForSingleObject(t2, INFINITE);

	std::cout << sInsertSpeed << "M inserts/s\n";
	std::cout << sRemoveSpeed << "M removes/s\n";

	for (u64 i = 0; i < 6; i++) {
		CreateThread(NULL, 0, testMPMCInserter, NULL, 0, NULL);
	}

	HANDLE t6[6];
	for (u64 i = 0; i < 6; i++) {
		t6[i] = CreateThread(NULL, 0, testMPMCExtractor, NULL, 0, NULL);
	}

	WaitForMultipleObjects(6, t6, 1, INFINITE);

	for (u64 i = 0; i < TSIZE; i++) {
		if (mpmcTest[i] != 6) {
			printf("MPMC Error\n");
		}
	}

	std::cout << mInsertSpeed << "M inserts/s\n";
	std::cout << mRemoveSpeed << "M removes/s\n";

	std::cin.get();
}
#endif
