#pragma once

#include "Basic.h"
#include "Array.h"
#include "String.h"

#define UNICODE_MAX   0x10FFFF
#define UTF8_CAPACITY 0x1FFFFF
#define UTF16_RESERVED_MIN 0xD800
#define UTF16_RESERVED_MAX 0xDFFF

#define IS_UTF16_RESERVED(c) (((c) >= UTF16_RESERVED_MIN) && ((c) <= UTF16_RESERVED_MAX))

#define UTF8_ONE_BYTE_MASK      0b1'0000000
#define UTF8_TRAILING_BYTE_MASK 0b11'000000
#define UTF8_TWO_BYTE_MASK      0b111'00000
#define UTF8_THREE_BYTE_MASK    0b1111'0000
#define UTF8_FOUR_BYTE_MASK     0b11111'000

constexpr u8 utf8ByteMask[4] = {
	UTF8_ONE_BYTE_MASK,
	UTF8_TWO_BYTE_MASK,
	UTF8_THREE_BYTE_MASK,
	UTF8_FOUR_BYTE_MASK,
};

#define UTF8_ONE_BYTE      0b0'0000000
#define UTF8_TRAILING_BYTE 0b10'000000
#define UTF8_TWO_BYTE      0b110'00000
#define UTF8_THREE_BYTE    0b1110'0000
#define UTF8_FOUR_BYTE     0b11110'000

constexpr u8 utf8Byte[4] = {
	UTF8_ONE_BYTE,
	UTF8_TWO_BYTE,
	UTF8_THREE_BYTE,
	UTF8_FOUR_BYTE,
};

static_assert(sizeof(char) == sizeof(u8), "u8 must be the same size as char");

#define UTF8_IS_BYTE(value, num) (((value) & utf8ByteMask[(num) - 1]) == utf8Byte[(num) - 1])

#define UTF8_ONE_BYTE_MIN   0x00
#define UTF8_ONE_BYTE_MAX   0x7F
#define UTF8_TWO_BYTE_MIN   0x80
#define UTF8_TWO_BYTE_MAX   0x7FF
#define UTF8_THREE_BYTE_MIN 0x800
#define UTF8_THREE_BYTE_MAX 0xFFFF
#define UTF8_FOUR_BYTE_MIN  0x10000
#define UTF8_FOUR_BYTE_MAX  UNICODE_MAX


constexpr u32 utf8Min[4] = {
	UTF8_ONE_BYTE_MIN, 
	UTF8_TWO_BYTE_MIN, 
	UTF8_THREE_BYTE_MIN, 
	UTF8_FOUR_BYTE_MIN
};

constexpr u32 utf8Max[4] = {
	UTF8_ONE_BYTE_MAX,
	UTF8_TWO_BYTE_MAX,
	UTF8_THREE_BYTE_MAX,
	UTF8_FOUR_BYTE_MAX
};

#define UTF8_IN_RANGE(value, num) ((value) >= utf8Min[(num) - 1] && (value) <= utf8Max[(num) - 1])

inline u8 utf8ByteCount(u8 leadingByte) {
	if (UTF8_IS_BYTE(leadingByte, 1)) return 1;
	else if (UTF8_IS_BYTE(leadingByte, 2)) return 2;
	else if (UTF8_IS_BYTE(leadingByte, 3)) return 3;
	else if (UTF8_IS_BYTE(leadingByte, 4)) return 4;

	return 0;
}

inline u32 utf32Build(const u8 *characters, u8 count) {
	assert(count >= 1);
	assert(count <= 4);
	u32 total = characters[0] & ~utf8ByteMask[count - 1] << (6 * (count - 1));

	for (u32 i = 1; i < count; i++) {
		total |= (characters[i] & ~UTF8_TRAILING_BYTE_MASK) << (6 * (count - 1 - i));
	}

	return total;
}


u8 *isInvalidUtf8(u8 *string);
u8 *isInvalidUtf8(String string);

u32 *utf8ToUtf32(u8 *string);

u64 utf8Len(u8 *string);

u32 getSingleUtf32FromUtf8(u8 *string, u8 **newString);
u32 getSingleUtf32FromUtf8(String string, String *newString);

void appendUtf32ToUtf8(Array<u8> &string, u32 c);

wchar_t *utf8ToWString(String filename);
char *wStringToUtf8(const wchar_t *filename);