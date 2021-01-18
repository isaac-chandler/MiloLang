#include "Basic.h"
#include "Error.h"
#include "UTF.h"
#include "CompilerMain.h"
#include "Ast.h"
#include "Lexer.h"

static std::mutex errorMutex;

#if BUILD_WINDOWS
bool doColorPrint = false; // False by default, set at startup if color can be enabled
#else
bool doColorPrint = true;
#endif

void initPrinter() {
#if BUILD_WINDOWS
	HANDLE console = GetStdHandle(STD_OUTPUT_HANDLE);
	if (console != INVALID_HANDLE_VALUE) {
		DWORD mode;

		if (GetConsoleMode(console, &mode)) {
			if (SetConsoleMode(console, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING)) {
				doColorPrint = true;
			}
		}
	}
#endif
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
	ScopeLock lock(errorMutex);
	hadError = true;

	va_list args;
	va_start(args, format);

	vprintf(format, args);
	puts("");

	va_end(args);

}

void reportError(Expr *location, CHECK_PRINTF const char *format, ...) {
	ScopeLock lock(errorMutex);
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
	ScopeLock lock(errorMutex);
	printErrorLocation(start);
	hadError = true;

	va_list args;
	va_start(args, format);

	vprintf(format, args);
	puts("");

	va_end(args);

	displayErrorLocation(start, end);
}

void reportError(struct Ir *ir, CHECK_PRINTF const char *format, ...) {
	for (; ir->op != IrOp::LINE_MARKER; ir--); // There had better be a location

	ScopeLock lock(errorMutex);
	printErrorLocation(&ir->location.start);
	hadError = true;

	va_list args;
	va_start(args, format);

	vprintf(format, args);
	puts("");

	va_end(args);

	displayErrorLocation(&ir->location.start, &ir->location.end);
}

void reportError(CodeLocation *location, CHECK_PRINTF const char *format, ...) {
	ScopeLock lock(errorMutex);
	printErrorLocation(location);

	hadError = true;

	va_list args;
	va_start(args, format);

	vprintf(format, args);
	puts("");

	va_end(args);
}


void reportError(Declaration *location, CHECK_PRINTF const char *format, ...) {
	ScopeLock lock(errorMutex);
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
	ScopeLock lock(errorMutex);
	printErrorLocation(&location->start);

	hadError = true;

	va_list args;
	va_start(args, format);

	vprintf(format, args);
	puts("");

	va_end(args);

	displayErrorLocation(&location->start, &location->end);
}

void reportInfo(CHECK_PRINTF const char *format, ...) {
	ScopeLock lock(errorMutex);

	va_list args;
	va_start(args, format);

	vprintf(format, args);
	puts("");

	va_end(args);
}

void reportExpectedError(Token *location, CHECK_PRINTF const char *format, ...) {
	ScopeLock lock(errorMutex);
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
