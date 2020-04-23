#pragma once

#include "Basic.h"
#include "String.h"
#include "CompilerMain.h"

#define LITERAL_IS_DECIMAL 0x1
#define LITERAL_IS_HEX 0x2
#define LITERAL_IS_BINARY 0x4

#define LITERAL_IS_BINARY_EXPONENT 0x8

#define LITERAL_IS_CHARACTER 0x8

#define TOKEN(x) (static_cast<TokenT>(x))

String getTokenString(Token *token);

// Call it TokenT instead of TokenType since Windows.h defines TokenType, @Sigh
enum class TokenT : u8 {
	INVALID = 0, 
	END_OF_FILE = 128, // Start above the ascii range so single character tokens can be their character, @Unicode for now the only place where non-ASCII characters are valid is in comments, character literals and string literals. Currently unicode idents are not allowed because unicode has a whole bunch of weird shit that could cause ridiculous problems like zero-width space characters

	STRING_LITERAL, 
	INT_LITERAL, 
	FLOAT_LITERAL, 

	IDENTIFIER, 

	S8, 
	S16, 
	S32, 
	S64, 
	U8, 
	U16, 
	U32, 
	U64, 

	F32, 
	F64, 

	STRING, 

	BOOL,

	VOID, 

	TYPE, 

	STRUCT, 

	TRUE, 
	FALSE, 

	NULL_, 

	SIZE_OF, 
	TYPE_OF, 

	FOR, 
	WHILE, 
	BREAK, 
	CONTINUE, 
	IF, 
	ELSE, 
	RETURN, 

	CAST, 

	EQUAL, 
	NOT_EQUAL, 
	GREATER_EQUAL, 
	LESS_EQUAL, 

	LOGIC_AND, 
	LOGIC_OR, 

	SHIFT_LEFT, 
	SHIFT_RIGHT, 

	PLUS_EQUALS, 
	MINUS_EQUALS, 
	TIMES_EQUALS, 
	DIVIDE_EQUALS, 
	MOD_EQUALS, 

	AND_EQUALS, 
	OR_EQUALS,
	XOR_EQUALS, 

	SHIFT_LEFT_EQUALS, 
	SHIFT_RIGHT_EQUALS, 

	DOUBLE_DASH, 
	DOUBLE_DOT, 
	ARROW, 

	EXTERNAL, 
	
	ARRAY_TYPE, 
	LOAD, 
	UNION, 
	COMPLETED
};

struct Token {
	CodeLocation start;
	EndLocation end;
	TokenT type;

	union {
		String text;

		u64 unsignedValue;
		double floatValue;

	};
	u8 flags = 0;

	Token() {}
};

struct BigInt {
	SmallArray<u32, 8> numbers;

	void zero();
	void multiplyAdd(u32 multiplyBy, u32 add);

	bool getU64(u64 *value);
	double getDouble();
};

struct LexerFile {
	char *text;

	u64 bytesRemaining;
	Array<u8> stringBuilder;
	BigInt bigInt;

	CodeLocation location;
	
	Token token;

	CodeLocation undoLocation;
	EndLocation previousTokenEnd;

	void peekTokenTypes(u64 count, TokenT *buffer);
	void advance();

	bool open(FileInfo *file);
};