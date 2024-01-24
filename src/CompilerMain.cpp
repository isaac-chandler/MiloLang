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
#include "Error.h"
#include "Find.h"
#include "IdentTable.h"

#define NUM_PARSE_THREADS 4


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

	vsnprintf(buffer, size, format, args2);

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

	vsnprintf(buffer, size, format, args2);

	va_end(args2);

	return { buffer, size - 1 };
}

Module *getModule(String name) {
	for (auto module : modules) {
		if (module->name == name) {
			return module;
		}
	}

	auto module = new Module;
	module->members.flavor = BlockFlavor::GLOBAL;
	module->members.module = true;
	module->name = name;
	
	module->moduleId = modules.count;
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
	
	if (name.length) {		
		loadNewFile("", module);
		compilerDirectories.add(msprintf("%s/%.*s", modulePath, STRING_PRINTF(name)));
	}
	else {
		compilerDirectories.add(initialFilePath);
	}

	return module;
}

char *lastSlash(char *path) {
	char *last = nullptr;
	
	while (*path) {
		path++;

		if (*path == '/' || *path == '\\') {
			last = path;
		}
	}

	return last;
}

void chopFinalDir(char *path) {
	auto slash = lastSlash(path);

	if (slash)
		*slash = 0;
}

char *getFileName(char *path) {
	char *slash = lastSlash(path);

	if (slash)
		return slash + 1;
	else
		return path;
}

#if BUILD_WINDOWS
static const char *getLibC() {
	switch (buildOptions.c_runtime_library & ~Build_Options::C_Runtime_Library::FORCED) {
	case Build_Options::C_Runtime_Library::STATIC:        return  "libcmt.lib libvcruntime.lib libucrt.lib kernel32.lib";
	case Build_Options::C_Runtime_Library::STATIC_DEBUG:  return "libcmtd.lib libvcruntimed.lib libucrtd.lib kernel32.lib";
	case Build_Options::C_Runtime_Library::DYNAMIC:       return  "msvcrt.lib vcruntime.lib ucrt.lib kernel32.lib";
	case Build_Options::C_Runtime_Library::DYNAMIC_DEBUG: return "msvcrtd.lib vcruntimed.lib ucrtd.lib kernel32.lib";
	default: assert(false); return "";
	}
}
#endif

#if 1
int main(int argc, char *argv[]) {
	profiler_init(16, 1024 * 1024);
	profiler_register_this_thread();
	
	setlocale(LC_ALL, "");
	initPrinter();

	using namespace std::chrono;


	char *input = nullptr;

	bool noThreads = false;

	bool backendGiven = false;

	int firstBuildArgument = argc;

	char *installPath = exePath();
	chopFinalDir(installPath);
	chopFinalDir(installPath);

	modulePath = mprintf("%s/modules", installPath);

	for (int i = 1; i < argc; i++) {
		if (strcmp("-llvm", argv[i]) == 0) {
			if (backendGiven) {
				reportError("Error: Cannot specify backend more than once");
				return 1;
			}

			buildOptions.backend = Build_Options::Backend::LLVM;
			backendGiven = true;
		}
		else if (strcmp("-x64", argv[i]) == 0) {
			if (backendGiven) {
				reportError("Error: Cannot specify backend more than once");
				return 1;
			}

			buildOptions.backend = Build_Options::Backend::X64;
			backendGiven = true;
		}
		else if (strcmp("-no_threads", argv[i]) == 0) {
			noThreads = true;
		}
		else if (strcmp("-diagnostic", argv[i]) == 0) {
			printDiagnostics = true;
		}
		else if (strcmp("-no_dce", argv[i]) == 0) {
			noDce = true;
		}
		else if (strcmp("--", argv[i]) == 0) {
			firstBuildArgument = i + 1;
			break;
		}
		else if (strcmp("-seed", argv[i]) == 0) {

			
			u64 seed;
			if (i + 1 < argc) {
				char *end;
				seed = strtol(argv[i + 1], &end, 10);
				

				if (*end)
					seed = performance_time();
				else
					i++;
			}
			else {
				seed = performance_time();
			}

			SHUFFLE_JOBS = true;

			srand((s32) seed);
			printf("Job ordering seed: %d\n", (s32) seed);
		}
		else if (argv[i][0] == '-') {
			reportError("Option %s not recognized", argv[i]);
			return 1;
		}
		else {
			if (input) {
				reportError("Error: Cannot more than one input file");
				return 1;
			}

			input = argv[i];
		}
	}

	buildArguments.count = argc - static_cast<u64>(firstBuildArgument);

	buildArguments.data = new MiloString[buildArguments.count];

	for (u64 i = 0; i < buildArguments.count; i++) {
		buildArguments.data[i].data = (u8 *) argv[firstBuildArgument + i];
		buildArguments.data[i].count = strlen(argv[firstBuildArgument + i]);
	}

	if (!input) {
		reportError("Error: Expected name of input file");
		reportError("Usage: %s <file>", argv[0]);
	}

	initialFilePath = fullPath(input);

	if (initialFilePath) {
		input = getFileName(initialFilePath);

		chopFinalDir(initialFilePath);
		set_working_directory(initialFilePath);

		char *lastDot = strrchr(input, '.');

		if (lastDot && lastDot != input) {
			buildOptions.output_name.count = lastDot - input;
		} else {
			buildOptions.output_name.count = strlen(input);
		}
	}
	else {
		reportError("Error: File %s does not exist", input);
		return 1;
	}

	compilerDirectories.add(initialFilePath);
	compilerDirectories.add(modulePath);

	buildOptions.output_name.data = new u8[buildOptions.output_name.count];
	memcpy(buildOptions.output_name.data, input, buildOptions.output_name.count);
	

	auto start = high_resolution_clock::now();

	runtimeModule = getModule("Runtime");
	mainModule = getModule("");

	if (!hadError && runtimeModule && mainModule) {
		initIdentTable();
		setupTypeTable();

		{
			PROFILE_ZONE("Create threads");
			for (u32 i = 0; i < (noThreads ? 1 : NUM_PARSE_THREADS); i++) {
				char name[] = "Parser  ";

				name[7] = i + '1';

				std::thread parserThread(runParser);
				setThreadName(parserThread, name);

				parserThread.detach();
			}
			std::thread irGenerator(runIrGenerator);

			mainThread = std::this_thread::get_id();

			setThreadName(irGenerator, "Ir Generator");

			irGenerator.detach();
		}

		runInfer(input);

		reportInfo("Lines parsed: %ld", totalLines);
		reportInfo("Frontend Time: %.1fms", duration_cast<microseconds>(duration<double>(
			high_resolution_clock::now() - start)).count() / 1000.0);


		if (printDiagnostics) {
			u64 totalQueued = totalDeclarations + totalFunctions + totalSizes + totalImporters + totalRuns;
			reportInfo("Total queued: %llu", totalQueued);
			reportInfo("  %-5llu declarations (%-5llu skipped,       %-5llu type infers, %-5llu value infers)", 
				totalDeclarations, totalDeclarations - totalInferredDeclarations, totalInferDeclarationTypes, totalInferDeclarationValues);
			reportInfo("  %-5llu functions    (%-5llu header infers, %-5llu body infers)", totalFunctions, totalInferFunctionHeaders, totalInferFunctionBodies);
			reportInfo("  %-5llu types        (%-5llu struct sizes,  %-5llu enum sizes,  %-5llu array sizes)", totalSizes, totalInferStructSizes, totalInferEnumSizes, totalInferArraySizes);
			reportInfo("  %-5llu importers    (%-5llu infers)", totalImporters, totalInferImporters);
			reportInfo("  %-5llu runs         (%-5llu infers)", totalRuns, totalInferRuns);
			reportInfo("Total infers: %llu, %.1f infers/queued, %.1f iterations/infer", totalFlattenedInfers, static_cast<float>(totalFlattenedInfers) / totalQueued, static_cast<float>(totalInferIterations) / totalFlattenedInfers);
			reportInfo("Total types: %llu, %.1f sizes/type", totalSizes, static_cast<float>(totalInferStructSizes + totalInferEnumSizes + totalInferArraySizes) / totalSizes);

			reportInfo("IR Instructions: %u", irInstructions);
		}
	}

	String outputFileName;

	if (buildOptions.c_runtime_library & Build_Options::C_Runtime_Library::FORCED) {
		linkLibC = true;
	}
	else {
		for (auto library : libraries) {
			if (library.name == "c") {
				linkLibC = true;
			}
		}
	}

	if (!hadError && buildOptions.output_name.count) {
#if BUILD_WINDOWS
		MiloString output = buildOptions.output_name;
		u8 *outputFile = output.data;
		u64 outputCount = output.count;

		u8 *lastSlash = nullptr;
		u8 *lastDot = nullptr;

		while (outputCount--) {
			u8 c = *outputFile;

			if (c == '\\' || c == '/')
				lastSlash = outputFile;
			else if (c == '.')
				lastDot = outputFile;

			outputFile++;
		}

		if (lastDot <= lastSlash) {
			outputFileName.characters = mprintf("%.*s.exe", buildOptions.output_name.count, buildOptions.output_name.data);
			outputFileName.length = strlen(outputFileName.characters);
		}
		else {
			outputFileName = { (char *)buildOptions.output_name.data, static_cast<u32>(buildOptions.output_name.count) };
		}

		objectFileName = mprintf("%.*s.obj", STRING_PRINTF(outputFileName));
#elif BUILD_LINUX
		outputFileName = { (char *)buildOptions.output_name.data, static_cast<u32>(buildOptions.output_name.count) };
		objectFileName = mprintf("%.*s.o", STRING_PRINTF(outputFileName));

#endif

		auto backendStart = high_resolution_clock::now();
		decltype(backendStart) backendEnd;
		if (buildOptions.backend == Build_Options::Backend::LLVM) {
			runLlvm();

			backendEnd = high_resolution_clock::now();

			reportInfo("LLVM Time: %.1fms", duration_cast<microseconds>(duration<double>(
				backendEnd - backendStart)).count() / 1000.0);
		}
		else if (buildOptions.backend == Build_Options::Backend::X64) {
			runCoffWriter();

			backendEnd = high_resolution_clock::now();

			reportInfo("x64 Time: %.1fms", duration_cast<microseconds>(duration<double>(
				backendEnd - backendStart)).count() / 1000.0);
		}


		reportInfo("Compiler Time: %.1fms", duration_cast<microseconds>(duration<double>(
			backendEnd - start)).count() / 1000.0);
	}

	if (!hadError && buildOptions.output_name.count) {
		auto linkerStart = high_resolution_clock::now();

#if BUILD_WINDOWS
		char *linkerCommand;

		char *linkerPath;
		char *windowsLibPath;
		char *windowsSdkPath;
		char *crtLibPath;
		char *ucrtLibPath;


		{
			char *name = exePath();
			
			char *lastBackslash = strrchr(name, '\\');
			if (lastBackslash)
				*lastBackslash = 0;

			char *cacheFile = mprintf("%s\\%s", name, "linker.cache");

			bool linkerFindFailed = false;

			if (FILE *cache = fopen_utf8(cacheFile, "rb")) {
				u8 magic[3];

#define read(dest, count) ((count) == fread(dest, sizeof(*(dest)), count, cache))

				if (!read(magic, sizeof(magic)) || magic[0] != 'l' || magic[1] != 'p' || magic[2] != 1) {
					reportInfo("Failed to read linker cache");
					fclose(cache);
					goto linkerCacheFail;
				}

				u16 length;


				if (!read(&length, 1)) {
					reportInfo("Failed to read linker cache");
					fclose(cache);
					goto linkerCacheFail;
				}

				linkerPath = new char[length];

				if (!read(linkerPath, length)) {
					reportInfo("Failed to read linker cache");
					fclose(cache);
					goto linkerCacheFail;
				}

				if (!read(&length, 1)) {
					reportInfo("Failed to read linker cache");
					fclose(cache);
					goto linkerCacheFail;
				}

				windowsLibPath = new char[length];

				if (!read(windowsLibPath, length)) {
					reportInfo("Failed to read linker cache");
					fclose(cache);
					goto linkerCacheFail;
				}

				if (!read(&length, 1)) {
					reportInfo("Failed to read linker cache");
					fclose(cache);
					goto linkerCacheFail;
				}

				crtLibPath = new char[length];

				if (!read(crtLibPath, length)) {
					reportInfo("Failed to read linker cache");
					fclose(cache);
					goto linkerCacheFail;
				}

				if (!read(&length, 1)) {
					reportInfo("Failed to read linker cache");
					fclose(cache);
					goto linkerCacheFail;
				}

				ucrtLibPath = new char[length];

				if (!read(ucrtLibPath, length)) {
					reportInfo("Failed to read linker cache");
					fclose(cache);
					goto linkerCacheFail;
				}

				if (!read(&length, 1)) {
					reportInfo("Failed to read linker cache");
					fclose(cache);
					goto linkerCacheFail;
				}

				windowsSdkPath = new char[length];

				if (!read(windowsSdkPath, length)) {
					reportInfo("Failed to read linker cache");
					fclose(cache);
					goto linkerCacheFail;
				}

				fclose(cache);

				if (!fileExists(linkerPath)) {
					reportInfo("Linker cache had invalid linker");
					goto linkerCacheFail;
				}

				if (!directoryExists(windowsLibPath)) {
					reportInfo("Linker cache had invalid windows library path");
					goto linkerCacheFail;
				}

				if (!directoryExists(crtLibPath)) {
					reportInfo("Linker cache had invalid crt library path");
					goto linkerCacheFail;
				}

				if (!directoryExists(ucrtLibPath)) {
					reportInfo("Linker cache had invalid windows ucrt library path");
					goto linkerCacheFail;
				}

				if (!directoryExists(windowsSdkPath)) {
					reportInfo("Linker cache had invalid windows sdk path");
					goto linkerCacheFail;
				}
			}
			else {
				reportInfo("Failed to find cached linker, searching for linker");

			linkerCacheFail:

				PROFILE_ZONE("Find linker");


				Find_Result result = find_visual_studio_and_windows_sdk();

				if (!result.vs_exe_path) {
					reportError("Couldn't find linker");
					return 1;
				}
				else if (!result.windows_sdk_um_library_path) {
					reportError("Couldn't find windows sdk um");
					return 1;
				}
				else if (!result.windows_sdk_ucrt_library_path) {
					reportError("Couldn't find windows sdk ucrt");
					return 1;
				}
				else if (!result.windows_sdk_bin) {
					reportError("Couldn't find windows sdk");
					return 1;
				}

				linkerPath = mprintf("%s\\%s", wStringToUtf8(result.vs_exe_path), "link.exe");
				windowsLibPath = wStringToUtf8(result.windows_sdk_um_library_path);
				crtLibPath = wStringToUtf8(result.vs_library_path);
				ucrtLibPath = wStringToUtf8(result.windows_sdk_ucrt_library_path);
				windowsSdkPath = wStringToUtf8(result.windows_sdk_bin);

				if (FILE *cache = fopen_utf8(cacheFile, "wb")) {
					u16 length = strlen(linkerPath) + 1;

#define write(src, count) fwrite(src, sizeof(*(src)), count, cache)

					write("lp\1", 3);

					write(&length, 1);
					write(linkerPath, length);


					length = strlen(windowsLibPath) + 1;

					write(&length, 1);
					write(windowsLibPath, length);


					length = strlen(crtLibPath) + 1;

					write(&length, 1);
					write(crtLibPath, length);


					length = strlen(ucrtLibPath) + 1;

					write(&length, 1);
					write(ucrtLibPath, length);


					length = strlen(windowsSdkPath) + 1;

					write(&length, 1);
					write(windowsSdkPath, length);

					fclose(cache);
				}
			}

			bool hadLibC = false;

			char *libBuffer = mprintf("");
			{
				for (auto lib : libraries) {
					char *oldLibBuffer = libBuffer;

#if BUILD_WINDOWS
					if (lib.name == "c") {
						hadLibC = true;
						libBuffer = mprintf("%s %s", oldLibBuffer, getLibC());
					}
					else
#endif
						libBuffer = mprintf("%s %.*s.lib", oldLibBuffer, STRING_PRINTF(lib.name));

					free(oldLibBuffer);
				}
			}

			if (!hadLibC && (buildOptions.c_runtime_library & Build_Options::C_Runtime_Library::FORCED)) {
				libBuffer = mprintf("%s %s", libBuffer, getLibC());
			}

			if (buildOptions.icon_name.count) {
				auto iconFile = fopen("icon.rc", "w");
				fprintf(iconFile, "MAINICON ICON %.*s\r\n", buildOptions.icon_name.count, buildOptions.icon_name.data);
				fclose(iconFile);

				auto rcCommand = mprintf("\"%s\\rc.exe\" /nologo icon.rc", windowsSdkPath);

				printf("RC command: %s\n", rcCommand);

				STARTUPINFOW startup = {};
				startup.cb = sizeof(STARTUPINFOW);

				PROCESS_INFORMATION info;

				if (!CreateProcessW(NULL, utf8ToWString(rcCommand), NULL, NULL, false, 0, NULL, NULL, &startup, &info)) {
					reportInfo("Failed to run rc command");
				}

				CloseHandle(info.hThread);
				{
					PROFILE_ZONE("Wait for rc");
					WaitForSingleObject(info.hProcess, INFINITE);
				}

				// Make sure a failing exit code is returned if the linker fails
				DWORD exitCode;
				GetExitCodeProcess(info.hProcess, &exitCode);
				if (exitCode) {
					hadError = true;
					return 1;
				}
			}

			linkerCommand = mprintf("\"%s\" %s -nodefaultlib -out:%.*s /debug %s %s\\%s \"-libpath:%s\"  \"-libpath:%s\" \"-libpath:%s\" -incremental:no -nologo -natvis:%s\\milo.natvis -SUBSYSTEM:%s%s",
				linkerPath, 
				objectFileName, 
				STRING_PRINTF(outputFileName), 
				libBuffer, 
				installPath,
				linkLibC ? "__milo_cmain.obj" :  "__milo_chkstk.obj -entry:__program_start", 
				windowsLibPath, 
				crtLibPath,
				ucrtLibPath, 
				installPath,
				buildOptions.show_console ? "CONSOLE" : "WINDOWS", 
				buildOptions.icon_name.count ? " icon.res" : "");
		}

		{
			printf("Linker command: %s\n", linkerCommand);

			STARTUPINFOW startup = {};
			startup.cb = sizeof(STARTUPINFOW);

			PROCESS_INFORMATION info;

			if (!CreateProcessW(NULL, utf8ToWString(linkerCommand), NULL, NULL, false, 0, NULL, NULL, &startup, &info)) {
				reportInfo("Failed to run linker command");
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
		}
#else
		char *libBuffer = mprintf("");
		for (auto lib : libraries) {
			char *oldLibBuffer = libBuffer;
			libBuffer = mprintf("%s -l%.*s", oldLibBuffer, STRING_PRINTF(lib.name));
			free(oldLibBuffer);
		}

		char *linkerCommand = mprintf("gcc -gdwarf-5%s %s __milo_cmain.o -o %.*s", libBuffer, objectFileName, STRING_PRINTF(outputFileName));
		printf("Linker command: %s\n", linkerCommand);

		if (system(linkerCommand) != 0) {
			hadError = true;
			return 1;
		}
#endif

	error:;

		reportInfo("Linker Time: %.1fms", duration_cast<microseconds>(duration<double>(
			high_resolution_clock::now() - linkerStart)).count() / 1000.0);

		reportInfo("Total Time: %.1fms", duration_cast<microseconds>(duration<double>(
			high_resolution_clock::now() - start)).count() / 1000.0);
	}

	profiler_write_tracing_json("profile.json");
	profiler_write_binary("profile.mprof");

#if BUILD_DEBUG && BUILD_WINDOWS
	if (IsDebuggerPresent()) {
		std::cin.get();
	}
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
