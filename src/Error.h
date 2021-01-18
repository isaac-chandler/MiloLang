#pragma once
#include "Basic.h"


#ifdef BUILD_WINDOWS
#define CHECK_PRINTF _Printf_format_string_
#else
// @Incomplete: other compilers have versions of this
#define CHECK_PRINTF
#endif

void initPrinter();
void reportError(struct Expr *location, CHECK_PRINTF const char *format, ...);
void reportError(struct Declaration *location, CHECK_PRINTF const char *format, ...);
void reportError(struct Ir *ir, CHECK_PRINTF const char *format, ...);
void reportError(struct Token *location, CHECK_PRINTF const char *format, ...);
void reportError(CodeLocation *location, CHECK_PRINTF const char *format, ...);
void reportError(CodeLocation *start,  EndLocation *end, CHECK_PRINTF const char *format, ...);
void reportExpectedError(struct Token *location, CHECK_PRINTF const char *format, ...);
void reportError(CHECK_PRINTF const char *format, ...);
void reportInfo(CHECK_PRINTF const char *format, ...);