#include "Basic.h"
#include "String.h"

String::String(const char *cString) {
	length = static_cast<u32>(strlen(cString));
	
	const char *s = cString;
	characters = static_cast<char *>(malloc(length));
	memcpy(characters, cString, length);
}

String::String(const char *begin, const char *end) {
	assert(end >= begin);
	length = static_cast<u32>(end - begin);

	characters = static_cast<char *>(malloc(length));

	memcpy(characters, begin, length);
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
