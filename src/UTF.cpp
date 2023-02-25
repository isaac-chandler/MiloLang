#include "Basic.h"

#include "UTF.h"



u8 *isInvalidUtf8(u8 *string) {
	while (*string) {
		u8 count = utf8ByteCount(*string);

#define TRAILING_ERROR(i) if ((string[i] & UTF8_TRAILING_BYTE_MASK) != UTF8_TRAILING_BYTE) return &string[i]

		if (count == 1) {
		}
		else if (count == 2) {
			TRAILING_ERROR(1);

			if ((string[0] & 0b000'1111'0) == 0) return &string[0]; // The encoding is overlong if the 4msb are 0
		}
		else if (count == 3) {
			TRAILING_ERROR(1);
			TRAILING_ERROR(2);


			u32 utf32 = utf32Build(string, 3);

			if (utf32 < utf8Min[3 - 1]) return &string[0]; // The encoding is overlong if it is smaller than the utf8 3-byte min (could be encoded in fewer bytes)
			if (IS_UTF16_RESERVED(utf32)) return &string[0]; // This is not valid unicded, as it breaks UTF-16
		}
		else if (count == 4) {
			TRAILING_ERROR(1);
			TRAILING_ERROR(2);
			TRAILING_ERROR(3);

			u32 utf32 = utf32Build(string, 4);

			if (!UTF8_IN_RANGE(utf32, 4)) return &string[0]; // Out of Unicode range
		}
		else {
			return &string[0];
		}

		string += count;
	}

#undef TRAILING_ERROR

	return nullptr;
}

u8 *isInvalidUtf8(String string) {
	while (string.length) {
		u8 count = utf8ByteCount(string);

		if (string.length < count) {
			return reinterpret_cast<u8 *>(string.characters + string.length);
		}

#define TRAILING_ERROR(i) if ((string.characters[i] & UTF8_TRAILING_BYTE_MASK) != UTF8_TRAILING_BYTE) return reinterpret_cast<u8 *>(&string.characters[i]);

		if (count == 1) {
		}
		else if (count == 2) {
			TRAILING_ERROR(1);

			if ((string.characters[0] & 0b000'1111'0) == 0) reinterpret_cast<u8 *>(&string.characters[0]); // The encoding is overlong if the 4msb are 0
		}
		else if (count == 3) {
			TRAILING_ERROR(1);
			TRAILING_ERROR(2);


			u32 utf32 = utf32Build(reinterpret_cast<u8 *>(string.characters), 3);

			if (utf32 < utf8Min[3 - 1]) return reinterpret_cast<u8 *>(&string.characters[0]); // The encoding is overlong if it is smaller than the utf8 3-byte min (could be encoded in fewer bytes)
			if (IS_UTF16_RESERVED(utf32)) return reinterpret_cast<u8 *>(&string.characters[0]); // This is not valid unicded, as it breaks UTF-16
		}
		else if (count == 4) {
			TRAILING_ERROR(1);
			TRAILING_ERROR(2);
			TRAILING_ERROR(3);

			u32 utf32 = utf32Build(reinterpret_cast<u8 *>(string.characters), 4);

			if (!UTF8_IN_RANGE(utf32, 4)) reinterpret_cast<u8 *>(&string.characters[0]); // Out of Unicode range
		}
		else {
			return reinterpret_cast<u8 *>(&string.characters[0]);
		}

		string.characters += count;
		string.length -= count;
	}

#undef TRAILING_ERROR

	return nullptr;
}

u32 getSingleUtf32FromUtf8(u8 *string, u8 **newString) {
	u8 *dummy = 0;

	newString = newString ? newString : &dummy;

	u8 count = utf8ByteCount(*string);

	*newString = string + count;
	return utf32Build(string, count);
}

u32 getSingleUtf32FromUtf8(String string, String *newString) {
	String dummy;

	newString = newString ? newString : &dummy;

	u8 count = utf8ByteCount(*string.characters);

	newString->characters = string.characters + count;
	newString->length -= count;
	return utf32Build(reinterpret_cast<u8 *>(string.characters), count);
}

u64 utf8Len(u8 *string) {
	assert(!isInvalidUtf8(string));

	u64 len = 0;

	while (*string) {
		string += utf8ByteCount(*string);

		++len;
	}

	return len;
}

static u64 utf8LenForUtf16(u8 *string) {
	assert(!isInvalidUtf8(string));

	u64 len = 0;

	while (*string) {
		u8 count = utf8ByteCount(*string);
		len += count == 4;
		++len;
		string += count;
	}

	return len;
}

static u64 utf8LenForUtf16(String string) {
	assert(!isInvalidUtf8(string));

	u64 len = 0;

	while (string.length) {
		u8 count = utf8ByteCount(string);
		len += count == 4;
		++len;
		string.characters += count;
		string.length -= count;
	}

	return len;
}

u32 *utf8ToUtf32(u8 *string) {
	assert(!isInvalidUtf8(string));

	u64 len = utf8Len(string);

	u32 *utf32 = new u32[len + 1];

	for (u64 i = 0; i < len; i++) {
		utf32[i] = getSingleUtf32FromUtf8(string, &string);
	}

	utf32[len] = 0;
	return utf32;
}

u8 writeUtf16(u16 *buffer, u32 character) {
	if (character > 0xFFFF) {
		character -= 0x10000;

		assert(character <= 0xFFFFF);

		buffer[0] = 0xD800 + (character >> 10);
		buffer[1] = 0xDC00 + (character & 0x3FF);
		return 2;
	}
	else {
		buffer[0] = static_cast<u16>(character);
		return 1;
	}
}


void appendUtf32ToUtf8(Array<u8> &string, u32 c) {
	for (u32 i = 0; i < 4; i++) {
		if (c <= utf8Max[i]) {
			string.add(static_cast<u8>((c >> (6 * i)) | utf8Byte[i]));

			for (s32 j = i - 1; j >= 0; j--) {
				string.add(static_cast<u8>(
					((c >> (6 * j)) & ~UTF8_TRAILING_BYTE_MASK) | UTF8_TRAILING_BYTE
					));
			}

			return;
		}
	}

	assert(false);
}

#if BUILD_WINDOWS
wchar_t *utf8ToWString(String filename) {
	static_assert(sizeof(wchar_t) == sizeof(u16), "wchar_t should be UTF16");
	u64 len = utf8LenForUtf16(filename);

	u16 *utf16 = (u16 *)malloc((len + 1) * sizeof(u16));

	for (u64 i = 0; i < len;) {
		i += writeUtf16(&utf16[i], getSingleUtf32FromUtf8(filename, &filename));
	}

	utf16[len] = 0;


	return reinterpret_cast<wchar_t *>(utf16);
}

wchar_t *utf8ToWString(const char *filename) {
	static_assert(sizeof(wchar_t) == sizeof(u16), "wchar_t should be UTF16");
	u64 len = utf8LenForUtf16(filename);

	u16 *utf16 = (u16 *) malloc((len + 1) * sizeof(u16));

	for (u64 i = 0; i < len;) {
		i += writeUtf16(&utf16[i], getSingleUtf32FromUtf8((u8 *)filename, (u8 **)&filename));
	}

	utf16[len] = 0;


	return reinterpret_cast<wchar_t *>(utf16);
}
#endif