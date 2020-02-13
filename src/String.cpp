#include "Basic.h"
#include "String.h"

String::String(const char *cString) {
	const char *s = cString;

	hash = 0;
	length = 0;
	while (*s) {
		hash = updateHash(hash, *s);
		length++;
		s++;
	}

	characters = static_cast<char *>(malloc(length));
	memcpy(characters, cString, length);
}

String::String(const char *begin, const char *end) {
	assert(end >= begin);
	hash = 0;
	length = (u32)(end - begin);

	characters = static_cast<char *>(malloc(length));

	for (u32 i = 0; i < length; i++) {
		characters[i] = begin[i];
		hash = updateHash(hash, begin[i]);
	}
}

bool String::operator==(const String &other) const {
	return length == other.length && hash == other.hash && !memcmp(characters, other.characters, length);
}

std::ostream &operator<<(std::ostream &out, const String &str) {
	return out << std::string_view(str.characters, str.length);
}
