#pragma once

#include "Basic.h"
#include "Array.h"

#define STRING_PRINTF(val) ((val).length), ((val).characters)

struct String;


struct String {
	char *characters;
	u32 length;

	String() : length(0), characters(0) {};
	String(const char *cString);
	String(char *characters, u32 length) : length(length), characters(characters) {};
	String(const char *begin, const char *end);


	bool operator==(const String &other) const;
	bool operator!=(const String &other) const;

	operator bool() const {
		return length != 0;
	}

	inline void free() { std::free(characters); }
};

inline char *copyString(const char *s) {
	size_t len = strlen(s);

	char *ss = (char *) malloc(len + 1);

	assert(ss);
	strcpy_s(ss, len + 1, s);

	return ss;
}

std::ostream &operator<<(std::ostream &out, const String &str);

inline char *toCString(const String &s) {
	char *c = static_cast<char *>(malloc(s.length + 1));
	memcpy(c, s.characters, s.length);
	c[s.length] = 0;

	return c;
}

inline String copyString(const String &s) {
	String result;
	result.length = s.length;
	result.characters = static_cast<char *>(malloc(s.length));

	memcpy(result.characters, s.characters, s.length);

	return result;
}