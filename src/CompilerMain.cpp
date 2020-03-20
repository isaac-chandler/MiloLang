#include "Basic.h"

#include "CompilerMain.h"
#include "Parser.h"
#include "Infer.h"
#include "Lexer.h"
#include "IrGenerator.h"
#include "CoffWriter.h"

int main(int argc, char *argv[]) {

	if (argc != 2) {
		std::cout << "Expected argument to be name of input file" << std::endl;
	}

	char *input = argv[1];

	if (!PathFileExistsA(input)) {
		std::cout << input << " doesn't exist" << std::endl;
		return 1;
	}

	if (PathIsDirectoryA(input)) {
		std::cout << input << " is a directory" << std::endl;
		return 1;
	}

	auto start = std::chrono::high_resolution_clock::now();

	std::thread infer(runInfer);
	std::thread irGenerator(runIrGenerator);
	std::thread coffWriter(runCoffWriter);
	infer.detach();
	irGenerator.detach();


	LexerFile lexer = parseFile(reinterpret_cast<u8 *>(argv[1]));

	coffWriter.join();


	lexer.close();

#if PROFILE
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

	std::cout << "It took me " << (std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::duration<double>(
		std::chrono::high_resolution_clock::now() - start)).count() / 1000.0) << "ms";



#if BUILD_DEBUG
	std::cin.get();
#endif

	return 0;
}


#if PROFILE
Profile profiles[10000000];
std::atomic_uint32_t profileIndex;
#endif

