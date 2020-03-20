#pragma once

#include "Basic.h"
#include "Array.h"

struct String;


struct String {
	u64 length;
	char *characters;
	u32 hash;

	String() : hash(0), length(0), characters(0) {};
	String(const char *cString);
	String(const char *begin, const char *end);


	bool operator==(const String &other) const;

	operator bool() const {
		return length != 0;
	}

	inline void free() { std::free(characters); }
};

inline u32 updateHash(u32 hash, char c) {
	hash = (hash << 4) + c;
	u32 high = hash & 0xF000'0000;

	hash ^= high >> 24;
	hash &= 0x0FFF'FFFF;

	return hash;
}

inline char *copyString(const char *s) {
	size_t len = strlen(s);

	char *ss = (char *) malloc(len + 1);

	assert(ss);
	strcpy_s(ss, len + 1, s);

	return ss;
}

std::ostream &operator<<(std::ostream &out, const String &str);

inline char *toCString(const String &s) {
	char *c = static_cast<char *>(malloc((size_t)s.length + 1));
	memcpy(c, s.characters, s.length);
	c[s.length] = 0;

	return c;
}

inline String copyString(const String &s) {
	String result;
	result.length = s.length;
	result.hash = s.hash;
	result.characters = static_cast<char *>(malloc(s.length));

	memcpy(result.characters, s.characters, s.length);

	return result;
}