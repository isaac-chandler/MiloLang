#include "Basic.h"

#include "Lexer.h"
#include "String.h"


#include "UTF.h"

struct Keyword {
	String name;
	TokenT type;
};

static const Keyword keywords[] = {
	{"s8", TokenT::S8},
	{"s16", TokenT::S16},
	{"s32", TokenT::S32},
	{"s64", TokenT::S64},
	{"u8", TokenT::U8},
	{"u16", TokenT::U16},
	{"u32", TokenT::U32},
	{"u64", TokenT::U64},
	{"f32", TokenT::F32}, 
	{"f64", TokenT::F64}, 
	{"string", TokenT::STRING}, 
	{"void", TokenT::VOID}, 
	{"bool", TokenT::BOOL},
	{"type", TokenT::TYPE},
	{"struct", TokenT::STRUCT},
	{"for", TokenT::FOR},
	{"while", TokenT::WHILE},
	{"break", TokenT::BREAK},
	{"continue", TokenT::CONTINUE},
	{"if", TokenT::IF},
	{"else", TokenT::ELSE},
	{"return", TokenT::RETURN},
	{"cast", TokenT::CAST},
	{"true", TokenT::TRUE},
	{"false", TokenT::FALSE},
	{"null", TokenT::NULL_}, 
	{"size_of", TokenT::SIZE_OF},
	{"type_of", TokenT::TYPE_OF},
	{"#external", TokenT::EXTERNAL}, 
	{"#load", TokenT::LOAD}, 
	{"union", TokenT::UNION}, 
	{"completed", TokenT::COMPLETED}
};

void BigInt::zero() {
	numbers.clear();
	numbers.add(0);
}

void BigInt::multiplyAdd(u32 multiplyBy, u32 add) {
	u64 carry = add;

	for (u32 i = 0; i < numbers.count; i++) {
		u64 value = static_cast<u64>(numbers[i]) * static_cast<u64>(multiplyBy) + carry;
		carry = value >> 32ULL;
		numbers[i] = static_cast<u32>(value);
	}

	if (carry) {
		numbers.add(static_cast<u32>(carry));
	}
}


bool BigInt::getU64(u64 *value) {
	if (numbers.count == 1) {
		*value = numbers[0];
		return true;
	}
	else if (numbers.count == 2) {
		*value = (static_cast<u64>(numbers[1])) << 32ULL | static_cast<u64>(numbers[0]);
		return true;
	}
	else {
		assert(numbers.count);

#if BUILD_DEBUG
		assert(numbers[numbers.count] - 1 != 0);
#endif
		return false;
	}
}

double BigInt::getDouble() {
	double value = 0;

	double multiply = 1;

	// @Efficiency, we could just look at the 53 most significant bits, but how often do we have giant float literals
	for (u64 i = 0; i < numbers.count; i++) {
		value += numbers[i] * multiply;
		multiply *= 0x1P+32;
	}

	return value;
}

String getTokenString(Token *token) {
	char *file = getFileInfoByUid(token->start.fileUid)->data;
	char *start = file + token->start.locationInMemory;
	char *end = file + token->end.locationInMemory;

	do {
		--start;
		assert(start >= file);
	} while (!utf8ByteCount(*start));

	return String(start, end);
}

static u32 readCharacter(LexerFile *file, bool *endOfFile, bool silent = false) {
	file->undoLocation = file->location;

	if (file->bytesRemaining) {
		*endOfFile = false;

		u8 count = utf8ByteCount(file->text[file->location.locationInMemory]);
		if (!count) {
			return 0;
		}

		if (file->bytesRemaining < count) {
			return 0;
		}

		file->bytesRemaining -= count;

		u32 utf32 = utf32Build(reinterpret_cast<u8 *>(file->text + file->location.locationInMemory), count);

		if (!UTF8_IN_RANGE(utf32, count)) {
			if (!silent) {
				reportError(&file->location, "Error: Invalid UTF8 character");
			}

			return 0;
		}

		if (IS_UTF16_RESERVED(utf32)) {
			return 0;
		}

		file->location.locationInMemory += count;

		file->location.column++;
		return utf32;
	}
	else {
		*endOfFile = true;
		return 0;
	}
}

static void undoReadChar(LexerFile *lexer, u32 character) {
	lexer->bytesRemaining += lexer->location.locationInMemory - lexer->undoLocation.locationInMemory;
	lexer->location = lexer->undoLocation;
}

static s64 getDigitForBase(u32 c, u64 base) {
	if (base > 10) {
		u32 lowercase = c | 0x20; // Setting this bit will convert A-F -> a-f (from MSVC char_conv impl)

		if (c - '0' < 10) {
			return c - '0';
		}
		if (lowercase - 'a' < base - 10) {
			return lowercase - ('a' - 10);
		}
	}
	else {
		if (c - '0' < base) {
			return c - '0';
		}
	}

	return -1;
}

static TokenT advanceTokenType(LexerFile *lexer) {
	PROFILE_FUNC();
	bool endOfFile;
	u32 c;

	u64 commentNestCount = 0;

	s64 digit;
	u64 base;
	u64 exponentBase;

whitespace:
	c = readCharacter(lexer, &endOfFile, true);

	if (endOfFile) {
	fileEnd:
		return TokenT::END_OF_FILE;
	}

#define EQUAL_AFTER_OR_DOUBLE(character, tokenType, doubleType) \
	case (character): {                                         \
		c = readCharacter(lexer, &endOfFile, true);				\
		                                                        \
		if (c == (character)) {                                 \
		    return TokenT::doubleType;                          \
		}                                                       \
		else if (c == '=') {			                        \
			return TokenT::tokenType;                           \
		}                                                       \
		else {                                                  \
			undoReadChar(lexer, c);                             \
			return TOKEN((character));                          \
		}                                                       \
	}                                                           \


#define EQUAL_AFTER(character, tokenType)           \
	case (character): {                             \
		c = readCharacter(lexer, &endOfFile, true); \
		                                            \
		if (c == '=') {						        \
			return TokenT::tokenType;               \
		}                                           \
		else {                                      \
			undoReadChar(lexer, c);                 \
			return TOKEN((character));              \
		}                                           \
	}                                               \


	switch (c) {
		case ' ':
		case '\t':
		case '\r':
		case '\n':
			goto whitespace;
		EQUAL_AFTER('=', EQUAL)
		EQUAL_AFTER('!', NOT_EQUAL)
		EQUAL_AFTER('+', PLUS_EQUALS)
		EQUAL_AFTER('*', TIMES_EQUALS)
		EQUAL_AFTER('%', MOD_EQUALS)
		EQUAL_AFTER('^', XOR_EQUALS)

		EQUAL_AFTER_OR_DOUBLE('&', AND_EQUALS, LOGIC_AND)
		EQUAL_AFTER_OR_DOUBLE('|', OR_EQUALS, LOGIC_OR)


		case '>':
			c = readCharacter(lexer , &endOfFile);

			if (c == '=') {
				return TokenT::GREATER_EQUAL;
			}
			else if (c == '>') {
				c = readCharacter(lexer, &endOfFile, true);

				if (c == '=') {
					return TokenT::SHIFT_RIGHT_EQUALS;
				}
				else {
					undoReadChar(lexer, c);
					return TokenT::SHIFT_RIGHT;
				}
			}
			else {
				undoReadChar(lexer, c);
				return TOKEN('>');
			}
		case '<':
			c = readCharacter(lexer, &endOfFile, true);

			if (c == '=') {
				return TokenT::LESS_EQUAL;
			}
			else if (c == '<') {
				c = readCharacter(lexer, &endOfFile, true);

				if (c == '=') {
					return TokenT::SHIFT_LEFT_EQUALS;
				}
				else {
					undoReadChar(lexer, c);
					return TokenT::SHIFT_LEFT;
				}
			}
			else {
				undoReadChar(lexer, c);
				return TOKEN('<');
			}
		case '-':
			c = readCharacter(lexer, &endOfFile, true);

			if (c == '=') {
				return TokenT::MINUS_EQUALS;
			}
			else if (c == '>') {
				return TokenT::ARROW;
			}
			else if (c == '-') {
				return TokenT::DOUBLE_DASH;
			}
			else {
				undoReadChar(lexer, c);
				return TOKEN('-');
			}
		case '/':
			goto slash;

		case '"':
			goto stringLiteral;
		case '\'':
			goto charLiteral;
		case '0':
			goto zero;
		case '?':
		case ':':
		case ';':
		case '(':
		case ')':
		case '[':
		case ']':
		case '{':
		case '}':
		case '~':
		case ',':
			return TOKEN(c);
		case '.':
			goto dot;
		default:
			if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_' || c == '#') {
				lexer->token.text.characters = lexer->text + lexer->undoLocation.locationInMemory;
				lexer->token.text.length = 1;
				goto identifier;
			}
			else if (c >= '1' && c <= '9') {
				goto integerLiteral;
			}
			else {
				return TokenT::INVALID;
			}

			assert(false);
	}
#undef EQUAL_AFTER
#undef EQUAL_AFTER_OR_DOUBLE

	charLiteral :
	c = readCharacter(lexer, &endOfFile, true);

	if (endOfFile) {
		return TokenT::INVALID;
	}

	if (c == '\\') {
		goto charEscape;
	}
	else if (c < ' ') {
		return TokenT::INVALID;
	}

	c = readCharacter(lexer, &endOfFile, true);

charLiteralEnd:
	if (c == '\'') {
		return TokenT::INT_LITERAL;
	}
	else {
		return TokenT::INVALID;
	}

charEscape:
	c = readCharacter(lexer, &endOfFile, true);

	if (c == '\\') {
		c = readCharacter(lexer, &endOfFile, true);
		goto charLiteralEnd;
	}
	else if (c == '\'') {

		c = readCharacter(lexer, &endOfFile, true);

		if (c == '\'') {
			return TokenT::INT_LITERAL;
		}
		else {
			undoReadChar(lexer, c);
			return TokenT::INT_LITERAL;
		}
	}
	else if (c == '"') {
		c = readCharacter(lexer, &endOfFile, true);
		goto charLiteralEnd;
	}
	else if (c == 't') {
		c = readCharacter(lexer, &endOfFile, true);
		goto charLiteralEnd;
	}
	else if (c == 'r') {
		c = readCharacter(lexer, &endOfFile, true);
		goto charLiteralEnd;
	}
	else if (c == 'n') {
		c = readCharacter(lexer, &endOfFile, true);
		goto charLiteralEnd;
	}
	else if (c == 'e') {
		c = readCharacter(lexer, &endOfFile, true);
		goto charLiteralEnd;
	}
	else if (c == 'b') {
		c = readCharacter(lexer, &endOfFile, true);
		goto charLiteralEnd;
	}
	else if (c == 'f') {
		c = readCharacter(lexer, &endOfFile, true);
		goto charLiteralEnd;
	}
	else if (c == 'v') {
		c = readCharacter(lexer, &endOfFile, true);
		goto charLiteralEnd;
	}
	else if (c == 'x') {
		base = 16;
		goto charHexEscape;
	}
	else if (c >= '0' && c <= '9') {
		base = 10;
		goto charNumericEscape;
	}
	else if (c == 'u') {
		base = 16;
		goto charHexEscape;
	}
	else {
		return TokenT::INVALID;
	}

charHexEscape:
	c = readCharacter(lexer, &endOfFile, true);

	digit = getDigitForBase(c, base);

	if (digit >= 0) {
	}
	else {
		return TokenT::INVALID;
	}

charNumericEscape:
	c = readCharacter(lexer, &endOfFile, true);

	digit = getDigitForBase(c, base);

	if (digit >= 0) {
	}
	else if (c == '\\') {
		c = readCharacter(lexer, &endOfFile, true);
		goto charLiteralEnd;
	}
	else {
		goto charLiteralEnd;
	}

stringLiteral:
	c = readCharacter(lexer, &endOfFile, true);

stringLiteralRead:
	if (endOfFile) {
		return TokenT::INVALID;
	}
	else if (c == '"') {
		return TokenT::STRING_LITERAL;
	}
	else if (c == '\\') {
		goto stringEscape;
	}
	else if (c < ' ') {
		return TokenT::INVALID;
	}
	else {
		goto stringLiteral;
	}

stringEscape:
	c = readCharacter(lexer, &endOfFile, true);

	if (c == '\\') {
		goto stringLiteral;
	}
	else if (c == '\'') {
		goto stringLiteral;
	}
	else if (c == '"') {
		goto stringLiteral;
	}
	else if (c == 't') {
		goto stringLiteral;
	}
	else if (c == 'r') {
		goto stringLiteral;
	}
	else if (c == 'n') {
		goto stringLiteral;
	}
	else if (c == 'e') {
		goto stringLiteral;
	}
	else if (c == 'b') {
		goto stringLiteral;
	}
	else if (c == 'f') {
		goto stringLiteral;
	}
	else if (c == 'v') {
		goto stringLiteral;
	}
	else if (c == 'x') {
		base = 16;
		goto stringHexEscape;
	}
	else if (c >= '0' && c <= '9') {
		base = 10;
		goto stringNumericEscape;
	}
	else if (c == 'u') {
		base = 16;
		goto stringHexEscape;
	}
	else {
		goto stringLiteralRead;
	}

stringHexEscape:
	c = readCharacter(lexer, &endOfFile, true);

	digit = getDigitForBase(c, base);

	if (digit >= 0) {
	}
	else {
		return TokenT::INVALID;
	}

stringNumericEscape:
	c = readCharacter(lexer, &endOfFile, true);

	digit = getDigitForBase(c, base);

	if (digit >= 0) {
	}
	else if (c == '\\') {
		goto stringLiteral;
	}
	else {
		goto stringLiteralRead;
	}

zero:
	c = readCharacter(lexer, &endOfFile, true);

	if (c >= '0' && c <= '9') {
		goto integerLiteral;
	}
	else if (c == 'x' || c == 'X') {
		base = 16;
		goto integerLiteral;
	}
	else if (c == 'b' || c == 'B') {
		base = 2;
		goto integerLiteral;
	}
	else {
		undoReadChar(lexer, c);
		return TokenT::INT_LITERAL;
	}

dot:
	c = readCharacter(lexer, &endOfFile, true);

	if (c >= '0' && c <= '9') {
		base = 10;
		goto decimalPoint;
	}
	else if (c == '.') {
		return TokenT::DOUBLE_DOT;
	}
	else {
		undoReadChar(lexer, c);
		return TOKEN('.');
	}

slash:
	c = readCharacter(lexer, &endOfFile, true);
	switch (c) {
		case '/':
			goto lineComment;
		case '*':
			++commentNestCount;
			goto blockComment;
		case '=':
			return TokenT::DIVIDE_EQUALS;
		default:
			undoReadChar(lexer, c);
			return TOKEN('/');
	}

lineComment:
	c = readCharacter(lexer, &endOfFile, true);

	if (endOfFile)
		goto fileEnd;

	if (c == '\n')
		goto whitespace;
	else if (c == '\r')
		goto whitespace;
	else
		goto lineComment;

blockComment:
	c = readCharacter(lexer, &endOfFile, true);

	if (endOfFile)
		goto fileEnd;

	if (c == '*')
		goto blockCommentEnd;
	else if (c == '/')
		goto blockCommentSlash;

	goto blockComment;

blockCommentSlash:
	c = readCharacter(lexer, &endOfFile, true);

	if (endOfFile)
		goto fileEnd;

	if (c == '*')
		++commentNestCount;

	goto blockComment;

blockCommentEnd:
	c = readCharacter(lexer, &endOfFile, true);

	if (endOfFile)
		goto fileEnd;

	if (c == '/' && --commentNestCount == 0)
		goto whitespace;
	else
		goto blockComment;

identifier:
	c = readCharacter(lexer, &endOfFile, true);

	if ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_') {
		++lexer->token.text.length;
		goto identifier;
	}
	else {
		undoReadChar(lexer, c);


		for (auto &keyword : keywords) {
			if (keyword.name == lexer->token.text) {
				return keyword.type;
			}
		}


		
		if (lexer->token.text.characters[0] == '#') {
			return TokenT::INVALID;
		}

		return TokenT::IDENTIFIER;
	}

integerLiteral:
	c = readCharacter(lexer, &endOfFile, true);

	digit = getDigitForBase(c, base);

	if (digit >= 0) {
		goto integerLiteral;
	}
	else if (c == '.') {
		goto decimalPoint;
	}
	else if (c == '_') {
		goto integerLiteral;
	}
	else if (c == 'e' || c == 'E') {
		exponentBase = 10;
		goto exponentSign;
	}
	else if (c == 'p' || c == 'P') {
		exponentBase = 2;
		goto exponentSign;
	}
	else {
		undoReadChar(lexer, c);
		return TokenT::INT_LITERAL;
	}


decimalPoint:
	c = readCharacter(lexer, &endOfFile, true);

	digit = getDigitForBase(c, base);

	if (digit >= 0) {
		goto decimalPoint;
	}
	else if (c == '_') {
		goto decimalPoint;
	}
	else if (c == 'e' || c == 'E') {
		exponentBase = 10;
		goto exponentSign;
	}
	else if (c == 'p' || c == 'P') {
		exponentBase = 2;
		goto exponentSign;
	}
	else {
		undoReadChar(lexer, c);
		return TokenT::FLOAT_LITERAL;
	}

exponentSign:
	c = readCharacter(lexer, &endOfFile, true);

	digit = getDigitForBase(c, exponentBase);

	if (digit >= 0) {
		goto exponent;
	}
	else if (c == '_') {
		goto exponentSign;
	}
	else if (c == '+') {
		goto exponentDigit;
	}
	else if (c == '-') {
		goto exponentDigit;
	}
	else {
		return TokenT::INVALID;
	}

exponentDigit:
	c = readCharacter(lexer, &endOfFile, true);

	digit = getDigitForBase(c, exponentBase);

	if (digit >= 0) {
		goto exponent;
	}
	else if (c == '_') {
		goto exponentDigit;
	}
	else {
		return TokenT::INVALID;
	}

exponent:
	c = readCharacter(lexer, &endOfFile, true);

	digit = getDigitForBase(c, exponentBase);

	if (digit >= 0) {
		goto exponent;
	}
	else if (c == '_') {
		goto exponentDigit;
	}
	else {
		undoReadChar(lexer, c);
		return TokenT::FLOAT_LITERAL;
	}

	assert(false);
}

void LexerFile::peekTokenTypes(u64 count, TokenT *buffer) {
	Token oldToken = token;
	CodeLocation oldLocation = location;
	CodeLocation oldUndoLocation = undoLocation;
	u64 oldBytesRemaining = bytesRemaining;

	for (u64 i = 0; i < count; i++) {
		TokenT type = advanceTokenType(this);
		
		if (type == TokenT::INVALID || type == TokenT::END_OF_FILE) {
			for (; i < count; i++) {
				buffer[i] = type;
			}

			break;
		}
		else {
			buffer[i] = type;
		}
	}
	token = oldToken;
	location = oldLocation;
	undoLocation = oldUndoLocation;
	bytesRemaining = oldBytesRemaining;
}

void LexerFile::advance() {
	PROFILE_FUNC();
	bool endOfFile;
	u32 c;

	u64 commentNestCount = 0;

	token.flags = 0;

	token.unsignedValue = 0;
	s64 digitsAfterDecimal = 0;
	u64 base;
	u64 exponentBase;
	s64 exponent = 0;
	u64 exponentIsNegative;

	u64 escapeMaxValue;
	bool escapeIsUnicode;

	s64 digit;

	previousTokenEnd = token.end;

	bigInt.zero();

whitespace:
	c = readCharacter(this, &endOfFile);

whitespaceAlreadyRead:
	if (endOfFile) {
	fileEnd:
		token.start = location;
		token.end = location;
		token.type = TokenT::END_OF_FILE;
		return;
	}
	
#define EQUAL_AFTER_OR_DOUBLE(character, tokenType, doubleType) \
	case (character): {                                         \
		token.start = location;                                 \
		c = readCharacter(this, &endOfFile);                    \
		                                                        \
		if (c == (character)) {                                 \
			token.end = location;                               \
		    token.type = TokenT::doubleType;                    \
		    return;                                             \
		}                                                       \
		else if (c == '=') {			                        \
			token.end = location;                               \
			token.type = TokenT::tokenType;                     \
			return;                                             \
		}                                                       \
		else {                                                  \
			undoReadChar(this, c);                              \
			token.end = location;                               \
			token.type = TOKEN((character));                    \
			return;                                             \
		}                                                       \
	}                                                           \


#define EQUAL_AFTER(character, tokenType)     \
	case (character): {                       \
		token.start = location;               \
		c = readCharacter(this, &endOfFile);  \
		                                      \
		if (c == '=') {						  \
			token.end = location;             \
			token.type = TokenT::tokenType;   \
			return;                           \
		}                                     \
		else {                                \
			undoReadChar(this, c);            \
			token.end = location;             \
			token.type = TOKEN((character));  \
			return;                           \
		}                                     \
	}                                         \


	switch (c) {
		case ' ':
		case '\t':
			goto whitespace;
		case '\r':
			goto carriageReturn;
		case '\n':
			goto newLine;
		EQUAL_AFTER('=', EQUAL)
		EQUAL_AFTER('!', NOT_EQUAL)
		EQUAL_AFTER('+', PLUS_EQUALS)
		EQUAL_AFTER('*', TIMES_EQUALS)
		EQUAL_AFTER('%', MOD_EQUALS)
		EQUAL_AFTER('^', XOR_EQUALS)

		EQUAL_AFTER_OR_DOUBLE('&', AND_EQUALS, LOGIC_AND)
		EQUAL_AFTER_OR_DOUBLE('|', OR_EQUALS, LOGIC_OR)


		case '>':
			token.start = location;
			c = readCharacter(this, &endOfFile);

			if (c == '=') {
				token.end = location;
				token.type = TokenT::GREATER_EQUAL;
				return;
			}
			else if (c == '>') {
				c = readCharacter(this, &endOfFile);

				if (c == '=') {
					token.end = location;
					token.type = TokenT::SHIFT_RIGHT_EQUALS;
					return;
				}
				else {
					undoReadChar(this, c);
					token.end = location;
					token.type = TokenT::SHIFT_RIGHT;
					return;
				}
			}
			else {
				undoReadChar(this, c);
				token.end = location;
				token.type = TOKEN('>');
				return;
			}
		case '<':
			token.start = location;
			c = readCharacter(this, &endOfFile);

			if (c == '=') {
				token.end = location;
				token.type = TokenT::LESS_EQUAL;
				return;
			}
			else if (c == '<') {
				c = readCharacter(this, &endOfFile);

				if (c == '=') {
					token.end = location;
					token.type = TokenT::SHIFT_LEFT_EQUALS;
					return;
				}
				else {
					undoReadChar(this, c);
					token.end = location;
					token.type = TokenT::SHIFT_LEFT;
					return;
				}
			}
			else {
				undoReadChar(this, c);
				token.end = location;
				token.type = TOKEN('<');
				return;
			}
		case '-':
			token.start = location;
			c = readCharacter(this, &endOfFile);

			if (c == '=') {
				token.end = location;
				token.type = TokenT::MINUS_EQUALS;
				return;
			}
			else if (c == '>') {
				token.end = location;
				token.type = TokenT::ARROW;
				return;
			}
			else if (c == '-') {
				token.end = location;
				token.type = TokenT::DOUBLE_DASH;
				return;
			}
			else {
				undoReadChar(this, c);
				token.end = location;
				token.type = TOKEN('-');
				return;
			}
		case '/':
			goto slash;

		case '"':
			token.start = location;
			stringBuilder.clear();
			goto stringLiteral;
		case '\'':
			token.start = location;
			goto charLiteral;
		case '0':
			token.start = location;
			goto zero;
		case '?':
		case ':':
		case ';':
		case '(':
		case ')':
		case '[':
		case ']':
		case '{':
		case '}':
		case '~':
		case ',':
			token.start = location;
			token.end = location;
			token.type = TOKEN(c);
			return;
		case '.':
			token.start = location;
			goto dot;
		default:
			if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_' || c == '#') {
				token.start = location;
				token.text.characters = text + undoLocation.locationInMemory;
				token.text.length = 1;
				goto identifier;
			}
			else if (c >= '1' && c <= '9') {
				token.start = location;
				bigInt.multiplyAdd(10, c - '0');
				token.flags |= LITERAL_IS_DECIMAL;
				base = 10;
				goto integerLiteral;
			}
			else {
				token.start = location;
				token.end = location;
				token.type = TokenT::INVALID;

				reportError(&location, "Error: Invalid character"); // Don't try to print the character in case its invalid UTF8 or a control character
				return;
			}

			assert(false);
	}
#undef EQUAL_AFTER
#undef EQUAL_AFTER_OR_DOUBLE

charLiteral:
	c = readCharacter(this, &endOfFile);

	if (endOfFile) {
		token.end = location;
		token.type = TokenT::INVALID;
		reportError(&location, "Error: Expected a ' to close character literal but hit the end of the file");
		return;
	}

	if (c == '\\') {
		goto charEscape;
	}
	else if (c < ' ') {
		token.end = location;
		token.type = TokenT::INVALID;
		reportError(&location, "Error: Expected a ' to close character literal but hit the end of the file");
		return;
	}
	else {
		token.unsignedValue = c;
	}

	c = readCharacter(this, &endOfFile);

charLiteralEnd:
	if (c == '\'') {
		token.end = location;
		token.type = TokenT::INT_LITERAL;
		token.flags |= LITERAL_IS_CHARACTER;
		return;
	}
	else {
		token.end = location;
		token.type = TokenT::INVALID;
		reportError(&location, "Error: Expected a ' to close character, character literals may only be 1 character long");
		return;
	}

charEscape:
	c = readCharacter(this, &endOfFile);

	if (c == '\\') {
		token.unsignedValue = '\\';
		goto charLiteralEnd;
	}
	else if (c == '\'') {
		
		c = readCharacter(this, &endOfFile);

		if (c == '\'') {
			token.unsignedValue = '\'';
			token.end = location;
			token.type = TokenT::INT_LITERAL;
			token.flags |= LITERAL_IS_CHARACTER;
			return;
		}
		else {
			undoReadChar(this, c);
			token.unsignedValue = '\\';
			token.end = location;
			token.type = TokenT::INT_LITERAL;
			token.flags |= LITERAL_IS_CHARACTER;
			return;
		}
	}
	else if (c == '"') {
		token.unsignedValue = '"';
		c = readCharacter(this, &endOfFile);
		goto charLiteralEnd;
	}
	else if (c == 't') {
		token.unsignedValue = '\t';
		c = readCharacter(this, &endOfFile);
		goto charLiteralEnd;
	}
	else if (c == 'r') {
		token.unsignedValue = '\r';
		c = readCharacter(this, &endOfFile);
		goto charLiteralEnd;
	}
	else if (c == 'n') {
		token.unsignedValue = '\n';
		c = readCharacter(this, &endOfFile);
		goto charLiteralEnd;
	}
	else if (c == 'e') {
		token.unsignedValue = '\x1b';
		c = readCharacter(this, &endOfFile);
		goto charLiteralEnd;
	}
	else if (c == 'b') {
		token.unsignedValue = '\b';
		c = readCharacter(this, &endOfFile);
		goto charLiteralEnd;
	}
	else if (c == 'f') {
		token.unsignedValue = '\f';
		c = readCharacter(this, &endOfFile);
		goto charLiteralEnd;
	}
	else if (c == 'v') {
		token.unsignedValue = '\v';
		c = readCharacter(this, &endOfFile);
		goto charLiteralEnd;
	}
	else if (c == 'x') {
		base = 16;
		escapeMaxValue = UINT64_MAX;
		escapeIsUnicode = false;
		goto charHexEscape;
	}
	else if (c >= '0' && c <= '9') {
		base = 10;
		token.unsignedValue = c - '0';
		escapeMaxValue = UINT64_MAX;
		escapeIsUnicode = false;
		goto charNumericEscape;
	}
	else if (c == 'u') {
		base = 16;
		escapeMaxValue = UNICODE_MAX;
		escapeIsUnicode = true;
		goto charHexEscape;
	}
	else {
		token.end = location;
		token.type = TokenT::INVALID;
		reportError(&location, "Error: Invalid escape in character literal");
		return;
	}

charHexEscape:
	c = readCharacter(this, &endOfFile);

	digit = getDigitForBase(c, base);

	if (digit >= 0) {
		token.unsignedValue = digit;
	}
	else {
		token.end = location;
		token.type = TokenT::INVALID;
		reportError(&location, "Error: Invalid hex digit in escape");
		return;
	}

charNumericEscape:
	c = readCharacter(this, &endOfFile);

	digit = getDigitForBase(c, base);

	if (digit >= 0) {
		if (token.unsignedValue > UINT64_MAX / base) {
			token.end = location;
			token.type = TokenT::INVALID;
			reportError(&location, "Error: Character escape value is too large");
			return;
		}

		token.unsignedValue *= base;
		token.unsignedValue += digit;
	}
	else if (c == '\\') {
		if (token.unsignedValue > escapeMaxValue) {
			token.end = location;
			token.type = TokenT::INVALID;
			reportError(&location, "Error: Character escape value is too large");
			return;
		}

		if (escapeIsUnicode) {
			if (IS_UTF16_RESERVED(token.unsignedValue)) {
				token.end = location;
				token.type = TokenT::INVALID;
				reportError(&location, "Error: Character escape value is invalid unicode");
				return;
			}
		}

		c = readCharacter(this, &endOfFile);
		goto charLiteralEnd;
	}
	else {
		if (token.unsignedValue > escapeMaxValue) {
			token.end = location;
			token.type = TokenT::INVALID;
			reportError(&location, "Error: Character escape value is too large");
			return;
		}

		if (escapeIsUnicode) {
			if (IS_UTF16_RESERVED(token.unsignedValue)) {
				token.end = location;
				token.type = TokenT::INVALID;
				reportError(&location, "Error: Character escape value is invalid unicode");
				return;
			}
		}
		goto charLiteralEnd;
	}

stringLiteral:
	c = readCharacter(this, &endOfFile);

stringLiteralRead:
	if (endOfFile) {
		token.end = location;
		token.type = TokenT::INVALID;
		reportError(&location, "Error: File ended before string literal");
		return;
	}
	else if (c == '"') {
		token.text = String(reinterpret_cast<char *>(stringBuilder.begin()), reinterpret_cast<char *>(stringBuilder.end()));
		token.end = location;
		token.type = TokenT::STRING_LITERAL;
		return;
	}
	else if (c == '\\') {
		goto stringEscape;
	}
	else if (c < ' ') {
		token.end = location;
		token.type = TokenT::INVALID;
		reportError(&location, "Error: Invalid character in string literal");
		return;
	}
	else {
		appendUtf32ToUtf8(stringBuilder, c);
		goto stringLiteral;
	}

stringEscape:
	c = readCharacter(this, &endOfFile);

	if (c == '\\') {
		stringBuilder.add('\\');
		goto stringLiteral;
	}
	else if (c == '\'') {
		stringBuilder.add('\'');
		goto stringLiteral;
	}
	else if (c == '"') {
		stringBuilder.add('"');
		goto stringLiteral;
	}
	else if (c == 't') {
		stringBuilder.add('\t');
		goto stringLiteral;
	}
	else if (c == 'r') {
		stringBuilder.add('\r');
		goto stringLiteral;
	}
	else if (c == 'n') {
		stringBuilder.add('\n');
		goto stringLiteral;
	}
	else if (c == 'e') {
		stringBuilder.add('\x1b');
		goto stringLiteral;
	}
	else if (c == 'b') {
		stringBuilder.add('\b');
		goto stringLiteral;
	}
	else if (c == 'f') {
		stringBuilder.add('\f');
		goto stringLiteral;
	}
	else if (c == 'v') {
		stringBuilder.add('\v');
		goto stringLiteral;
	}
	else if (c == 'x') {
		base = 16;
		escapeMaxValue = 255;
		escapeIsUnicode = false;
		goto stringHexEscape;
	}
	else if (c >= '0' && c <= '9') {
		base = 10;
		token.unsignedValue = c - '0';
		escapeMaxValue = 255;
		escapeIsUnicode = false;
		goto stringNumericEscape;
	}
	else if (c == 'u') {
		base = 16;
		escapeMaxValue = UNICODE_MAX;
		escapeIsUnicode = true;
		goto stringHexEscape;
	}
	else {
		stringBuilder.add('\\');
		goto stringLiteralRead;
	}

stringHexEscape:
	c = readCharacter(this, &endOfFile);

	digit = getDigitForBase(c, base);

	if (digit >= 0) {
		token.unsignedValue = digit;
	}
	else {
		token.end = location;
		token.type = TokenT::INVALID;
		reportError(&location, "Error: Invalid hex digit in string escape");
		return;
	}

stringNumericEscape:
	c = readCharacter(this, &endOfFile);

	digit = getDigitForBase(c, base);

	if (digit >= 0) {
		if (token.unsignedValue > UINT64_MAX / base) {
			token.end = location;
			token.type = TokenT::INVALID;
			reportError(&location, "Error: String escape value is too large");
			return;
		}

		token.unsignedValue *= base;
		token.unsignedValue += digit;
	}
	else if (c == '\\') {
		if (token.unsignedValue > escapeMaxValue) {
			token.end = location;
			token.type = TokenT::INVALID;
			reportError(&location, "Error: String escape value is too large");
			return;
		}

		if (escapeIsUnicode) {
			if (IS_UTF16_RESERVED(token.unsignedValue)) {
				token.end = location;
				token.type = TokenT::INVALID;
				reportError(&location, "Error: String escape value is too large");
				return;
			}

			appendUtf32ToUtf8(stringBuilder, static_cast<u32>(token.unsignedValue));
		}
		else {
			stringBuilder.add(static_cast<u8>(token.unsignedValue));
		}
		goto stringLiteral;
	}
	else {
		if (token.unsignedValue > escapeMaxValue) {
			token.end = location;
			token.type = TokenT::INVALID;
			reportError(&location, "Error: String escape value is too large");
			return;
		}

		if (escapeIsUnicode) {
			if (IS_UTF16_RESERVED(token.unsignedValue)) {
				token.end = location;
				token.type = TokenT::INVALID;
				reportError(&location, "Error: String escape value is too large");
				return;
			}

			appendUtf32ToUtf8(stringBuilder, static_cast<u32>(token.unsignedValue));
		}
		else {
			stringBuilder.add(static_cast<u8>(token.unsignedValue));
		}
		goto stringLiteralRead;
	}

zero:
	c = readCharacter(this, &endOfFile);

	if (c >= '0' && c <= '9') {
		bigInt.multiplyAdd(10, c - '0');

		base = 10;
		token.flags |= LITERAL_IS_DECIMAL;
		goto integerLiteral;
	}
	else if (c == 'x' || c == 'X') {
		base = 16;
		token.flags |= LITERAL_IS_HEX;
		goto integerLiteral;
	}
	else if (c == 'b' || c == 'B') {
		base = 2;
		token.flags |= LITERAL_IS_BINARY;
		goto integerLiteral;
	}
	else {
		undoReadChar(this, c);
		token.flags |= LITERAL_IS_DECIMAL;
		token.end = location;
		token.type = TokenT::INT_LITERAL;
		return;
	}

dot:
	c = readCharacter(this, &endOfFile);

	if (c >= '0' && c <= '9') {
		base = 10;
		token.unsignedValue = c - '0';
		token.flags |= LITERAL_IS_DECIMAL;
		token.end = location;
		++digitsAfterDecimal;
		goto decimalPoint;
	}
	else if (c == '.') {
		token.end = location;
		token.type = TokenT::DOUBLE_DOT;
		return;
	}
	else {
		undoReadChar(this, c);
		token.end = location;
		token.type = TOKEN('.');
		return;
	}

newLine:
	location.column = 0;
	++location.line;

	goto whitespace;


carriageReturn:
	c = readCharacter(this, &endOfFile);

	location.column = 0;
	++location.line;

	if (c == '\n')
		goto whitespace;
	else
		goto whitespaceAlreadyRead;

slash:
	token.start = undoLocation;

	c = readCharacter(this, &endOfFile);
	switch (c) {
		case '/':
			goto lineComment;
		case '*':
			++commentNestCount;
			goto blockComment;
		case '=':
			token.end = location;
			token.type = TokenT::DIVIDE_EQUALS;
			return;
		default:
			undoReadChar(this, c);
			token.end = location;
			token.type = TOKEN('/');
			return;
	}

lineComment:
	c = readCharacter(this, &endOfFile);

	if (endOfFile)
		goto fileEnd;

	if (c == '\n')
		goto newLine;
	else if (c == '\r')
		goto carriageReturn;
	else
		goto lineComment;

blockComment:
	c = readCharacter(this, &endOfFile);

blockCommentAlreadyRead:
	if (endOfFile)
		goto fileEnd;

	if (c == '*')
		goto blockCommentEnd;
	else if (c == '/')
		goto blockCommentSlash;
	else if (c == '\r') {
		goto blockCommentCarriageReturn;
	}
	else if (c == '\n') {
	blockCommentNewLine:
		location.column = 0;
		++location.line;
	}

	goto blockComment;

blockCommentCarriageReturn:
	c = readCharacter(this, &endOfFile);

	location.column = 0;
	++location.line;

	if (c == '\n')
		goto blockComment;
	else
		goto blockCommentAlreadyRead;


blockCommentSlash:
	c = readCharacter(this, &endOfFile);

	if (endOfFile)
		goto fileEnd;

	if (c == '*')
		++commentNestCount;
	else if (c == '\n')
		goto blockCommentNewLine;
	else if (c == '\r')
		goto blockCommentCarriageReturn;

	goto blockComment;

blockCommentEnd:
	c = readCharacter(this, &endOfFile);

	if (endOfFile)
		goto fileEnd;

	if (c == '/' && --commentNestCount == 0)
		goto whitespace;
	else
		goto blockComment;

identifier:
	c = readCharacter(this, &endOfFile);

	if ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_') {
		++token.text.length;
		goto identifier;
	}
	else {
		undoReadChar(this, c);
		
		token.end = location;
		

		for (auto &keyword : keywords) {
			if (keyword.name == token.text) {
				token.type = keyword.type;
				return;
			}
		}
		
		if (token.text.characters[0] == '#') {
			assert(false);
			token.type = TokenT::INVALID;
			reportError(&location, "Error: Invalid compiler directive '%.*s", STRING_PRINTF(token.text));
			return;
		}

		token.type = TokenT::IDENTIFIER;
		return;
	}

integerLiteral:
	c = readCharacter(this, &endOfFile);

	digit = getDigitForBase(c, base);

	if (digit >= 0) {
		bigInt.multiplyAdd(base, digit);
		goto integerLiteral;
	}
	else if (c == '.') {
		goto decimalPoint;
	}
	else if (c == '_') {
		goto integerLiteral;
	}
	else if (c == 'e' || c == 'E') {
		exponentBase = 10;
		goto exponentSign;
	}
	else if (c == 'p' || c == 'P') {
		token.flags |= LITERAL_IS_BINARY_EXPONENT;
		exponentBase = 2;
		goto exponentSign;
	}
	else {
		undoReadChar(this, c);
		token.end = location;
		token.type = TokenT::INT_LITERAL;

		if (!bigInt.getU64(&token.unsignedValue)) {
			token.type = TokenT::INVALID;
			reportError(&location, "Error: integer literal too large, the maximum value is %" PRIu64, UINT64_MAX);
		}

		return;
	}


decimalPoint:
	c = readCharacter(this, &endOfFile);

	digit = getDigitForBase(c, base);

	if (digit >= 0) {
		bigInt.multiplyAdd(base, digit);
		++digitsAfterDecimal;
		goto decimalPoint;
	}
	else if (c == '_') {
		goto decimalPoint;
	}
	else if (c == 'e' || c == 'E') {
		exponentBase = 10;
		goto exponentSign;
	}
	else if (c == 'p' || c == 'P') {
		token.flags |= LITERAL_IS_BINARY_EXPONENT;
		exponentBase = 2;
		goto exponentSign;
	}
	else {
		undoReadChar(this, c);
		token.end = location;
		token.type = TokenT::FLOAT_LITERAL; // @Incomplete: bounds check float literals
		token.floatValue = bigInt.getDouble() *          // @Improvement, do this calculation in such a way that we get the maximum possible precision for float literals
			pow(static_cast<double>(base), -digitsAfterDecimal);
		return;
	}

exponentSign:
	c = readCharacter(this, &endOfFile);

	digit = getDigitForBase(c, exponentBase);

	if (digit >= 0) {
		exponentIsNegative = false;
		exponent *= exponentBase;
		exponent += static_cast<u64>(digit);
		goto exponent;
	}
	else if (c == '_') {
		goto exponentSign;
	}
	else if (c == '+') {
		exponentIsNegative = false;
		goto exponentDigit;
	}
	else if (c == '-') {
		exponentIsNegative = true;
		goto exponentDigit;
	}
	else {
		token.end = undoLocation;
		token.type = TokenT::INVALID;
		reportError(&location, "Error: Expected float literal exponent");
		return;
	}

exponentDigit:
	c = readCharacter(this, &endOfFile);

	digit = getDigitForBase(c, exponentBase);

	if (digit >= 0) {
		exponent *= exponentBase;
		exponent += static_cast<u64>(digit);

		goto exponent;
	}
	else if (c == '_') {
		goto exponentDigit;
	}
	else {
		token.end = undoLocation;
		token.type = TokenT::INVALID;
		reportError(&location, "Error: Expected float literal exponent");
		return;
	}

exponent:
	c = readCharacter(this, &endOfFile);

	digit = getDigitForBase(c, exponentBase);

	if (digit >= 0) {
		exponent *= exponentBase;
		exponent += static_cast<u64>(digit);


		if (exponentIsNegative) {
			if (exponent > 1074) {
				token.end = location;
				token.type = TokenT::INVALID;
				reportError(&location, "Error: Expected float literal exponent to small, the minimum is -1074");
				return;
			}
		}
		else {

			if (exponent > 1023) {
				token.end = location;
				token.type = TokenT::INVALID;
				reportError(&location, "Error: Expected float literal exponent to large, the minimum is 1023");
				return;
			}
		}

		goto exponent;
	}
	else if (c == '_') {
		goto exponentDigit;
	}
	else {
		undoReadChar(this, c);
		token.end = location;
		token.type = TokenT::FLOAT_LITERAL; // @Incomplete: bounds check float literals
		token.floatValue = bigInt.getDouble() *          // @Improvement, do this calculation in such a way that we get the maximum possible precision for float literals
			pow(static_cast<double>(base), -digitsAfterDecimal) * 
			pow(static_cast<double>(exponentBase), exponentIsNegative ? -exponent : exponent);
		return;
	}

	assert(false);
}

bool LexerFile::open(FileInfo *file) {
	assert(file->handle != INVALID_HANDLE_VALUE);

	LARGE_INTEGER size;

	GetFileSizeEx(file->handle, &size);

	text = static_cast<char *>(malloc(size.QuadPart));
	file->data = text;
	
	DWORD bytesRead = 0;

	u64 toRead = size.QuadPart;

	char *buffer = text;

	while (toRead) {
		u32 readAmount = static_cast<u32>(my_min(toRead, UINT32_MAX));

		if (!ReadFile(file->handle, buffer, readAmount, &bytesRead, nullptr) || bytesRead != readAmount) {
			CloseHandle(file->handle);
			reportError("Error: failed to read file: %.*s", STRING_PRINTF(file->path));
			return false;
		}

		toRead -= readAmount;
		buffer += readAmount;
	}

	CloseHandle(file->handle);

	location.line = 1;
	location.column = 0;
	location.locationInMemory = 0;
	location.fileUid = file->fileUid;
	bytesRemaining = size.QuadPart;

	file->size = size.QuadPart;


	return true;
}