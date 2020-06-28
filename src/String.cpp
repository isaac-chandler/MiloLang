#include "Basic.h"
#include "String.h"

String::String(const char *cString) {
	length = strlen(cString);
	
	const char *s = cString;
	characters = static_cast<char *>(malloc(length));
	memcpy(characters, cString, length);
}

String::String(const char *begin, const char *end) {
	assert(end >= begin);
	length = (u32)(end - begin);

	characters = static_cast<char *>(malloc(length));

	for (u32 i = 0; i < length; i++) {
		characters[i] = begin[i];
	}
}

bool String::operator==(const String &other) const {
	if (length != other.length) return false;

	return memcmp(characters, other.characters, length) == 0;
}


bool String::operator!=(const String &other) const {
	if (length != other.length) return true;

	return memcmp(characters, other.characters, length) != 0;
}

std::ostream &operator<<(std::ostream &out, const String &str) {
	return out << std::string_view(str.characters, str.length);
}
