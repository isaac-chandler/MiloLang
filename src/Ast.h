#pragma once

#include "Basic.h"
#include "String.h"
#include "Array.h"
#include "BucketedArenaAllocator.h"

inline BucketedArenaAllocator typeArena(1024 * 1024);
#define TYPE_NEW(T) new (static_cast<T *>(typeArena.allocate(sizeof(T)))) T
#define TYPE_NEW_ARRAY(T, C) new (static_cast<T *>(typeArena.allocate((C) * sizeof(T)))) T[C]

enum class IrOp : u8 {
	ADD,
	ADD_CONSTANT,
	SUB,
	MUL,
	MUL_BY_CONSTANT,
	DIV,
	DIVIDE_BY_CONSTANT,
	MOD,
	AND,
	OR,
	XOR,
	NOT,
	SHIFT_LEFT,
	SHIFT_RIGHT,
	READ,
	WRITE,
	SET,
	GOTO,
	IF_Z_GOTO,
	IF_NZ_GOTO,
	LESS,
	GREATER,
	LESS_EQUAL,
	GREATER_EQUAL,
	NOT_EQUAL,
	EQUAL,
	ADDRESS_OF_GLOBAL,
	ADDRESS_OF_LOCAL,
	IMMEDIATE,
	FLOAT_TO_INT,
	INT_TO_FLOAT,
	RETURN,
	CALL,
	NEG,
	NOOP,
	FUNCTION, 
	STRING, 
	STRING_EQUAL
};

#define IR_SIGNED_OP 0x1
#define IR_FLOAT_OP 0x2


inline bool isStandardSize(u64 size) {
	return size == 1 || size == 2 || size == 4 || size == 8;
}

#define DEST_NONE UINT64_MAX

struct Argument {
	u64 number;
	struct Type *type;
};

struct FunctionCall {
	struct Type *returnType;
	u64 argCount;
	Argument args[];
};

struct Ir {
	u64 dest;

	union {
		u64 a;
		struct Declaration *declaration;
		struct ExprFunction *function;
		struct ExprStringLiteral *string;
	};

	union {
		u64 b;
		u64 destSize;
		FunctionCall *arguments;
	};

	u64 flags = 0;

	u8 opSize;

	IrOp op;
};

struct Loop {
	struct ExprLoop *loop;
	u64 start;
	Array<u64> endPatches;
};

struct IrState {
	u64 nextRegister = 1;
	u64 callAuxStorage = 0;
	u64 parameterSpace = 0;
	Array<Ir> ir;

	BucketedArenaAllocator allocator;

	Array<Loop> loopStack;
	u64 loopCount = 0;

	IrState() : allocator(1024) {}
};


enum class TokenT : u8;

enum class TypeFlavor : u8 {
	VOID, 
	INTEGER,
	FLOAT, 
	POINTER, 
	BOOL, 
	FUNCTION, 
	TYPE, 
	AUTO_CAST, 
	STRING, 
	ARRAY
};

#define TYPE_INTEGER_IS_SIGNED 0x2
#define TYPE_IS_INTERNAL      0x4 // This means that this type should never be accessible to the user, i.e. the type of null, which is special to give it casting properties, but the program should never be able to reference it.
#define TYPE_IS_INTERNAL      0x4 // This means that this type should never be accessible to the user, i.e. the type of null, which is special to give it casting properties, but the program should never be able to reference it.
#define TYPE_ARRAY_IS_FIXED 0x8
#define TYPE_ARRAY_IS_DYNAMIC 0x10

struct Type {
	u64 size;
	u64 alignment;
	String name;
	u16 flags = 0;
	TypeFlavor flavor;

	struct TypePointer *pointerPrototype = nullptr;
	struct TypeArray *arrayPrototype = nullptr;
	struct TypeArray *dynamicArrayPrototype = nullptr;
};

struct TypePointer : Type {
	Type *pointerTo;
};

struct TypeArray : Type {
	Type *arrayOf;
	u64 count;
};

inline Type TYPE_S8 = { 1, 1, "s8", TYPE_INTEGER_IS_SIGNED, TypeFlavor::INTEGER };
inline Type TYPE_S16 = { 2, 2, "s16", TYPE_INTEGER_IS_SIGNED, TypeFlavor::INTEGER };
inline Type TYPE_S32 = { 4, 4, "s32", TYPE_INTEGER_IS_SIGNED, TypeFlavor::INTEGER };
inline Type TYPE_S64 = { 8, 8, "S64", TYPE_INTEGER_IS_SIGNED, TypeFlavor::INTEGER };

inline Type TYPE_U8 = { 1, 1, "u8", 0, TypeFlavor::INTEGER };
inline Type TYPE_U16 = { 2, 2, "u16", 0, TypeFlavor::INTEGER };
inline Type TYPE_U32 = { 4, 4, "u32", 0, TypeFlavor::INTEGER };
inline Type TYPE_U64 = { 8, 8, "u64", 0, TypeFlavor::INTEGER };

inline Type TYPE_UNSIGNED_INT_LITERAL = { 0, 0, "<int literal>", TYPE_IS_INTERNAL, TypeFlavor::INTEGER };
inline Type TYPE_SIGNED_INT_LITERAL = { 0, 0, "<signed int literal>", TYPE_INTEGER_IS_SIGNED | TYPE_IS_INTERNAL, TypeFlavor::INTEGER };

inline Type TYPE_F32 = { 4, 4, "f32", 0, TypeFlavor::FLOAT };
inline Type TYPE_F64 = { 8, 8, "f64", 0, TypeFlavor::FLOAT };

inline Type TYPE_FLOAT_LITERAL = { 0, 0, "<float literal>", TYPE_IS_INTERNAL, TypeFlavor::FLOAT };

inline Type TYPE_VOID = { 1, 1, "void", 0, TypeFlavor::VOID, new TypePointer {8, 8, "*void", 0, TypeFlavor::POINTER } }; // Give void size and alignment of 1, so *void math just adds raw memory
inline Type TYPE_BOOL = { 1, 1, "bool", 0, TypeFlavor::BOOL };

inline Type TYPE_TYPE = { 0, 0, "type", 0, TypeFlavor::TYPE };

inline Type TYPE_AUTO_CAST = { 0, 0, "<auto cast>", TYPE_IS_INTERNAL, TypeFlavor::AUTO_CAST };

// @StringFormat make strings length based, currently they are the same as *u8
inline Type TYPE_STRING = { 8, 8, "string", 0, TypeFlavor::STRING };

inline TypePointer *getPointerTo(Type *type) {
	if (!type->pointerPrototype) {
		type->pointerPrototype = TYPE_NEW(TypePointer);

		type->pointerPrototype->size = 8;
		type->pointerPrototype->alignment = 8;
		type->pointerPrototype->flavor = TypeFlavor::POINTER;
		type->pointerPrototype->pointerTo = type;

		type->pointerPrototype->name.length = type->name.length + 1;
		type->pointerPrototype->name.characters = TYPE_NEW_ARRAY(char, type->pointerPrototype->name.length);

		type->pointerPrototype->name.characters[0] = '*';
		memcpy(type->pointerPrototype->name.characters + 1, type->name.characters, type->name.length);
	}

	return type->pointerPrototype;
}

inline TypeArray *getArrayOf(Type *type) {
	if (!type->arrayPrototype) {
		type->arrayPrototype = TYPE_NEW(TypeArray);

		type->arrayPrototype->size = 16;
		type->arrayPrototype->alignment = 8;
		type->arrayPrototype->flavor = TypeFlavor::ARRAY;
		type->arrayPrototype->arrayOf = type;
		type->arrayPrototype->count = 0;

		type->arrayPrototype->name.length = type->name.length + 2;
		type->arrayPrototype->name.characters = TYPE_NEW_ARRAY(char, type->arrayPrototype->name.length);

		type->arrayPrototype->name.characters[0] = '[';
		type->arrayPrototype->name.characters[1] = ']';
		memcpy(type->arrayPrototype->name.characters + 2, type->name.characters, type->name.length);
	}

	return type->arrayPrototype;
}

inline TypeArray *getDynamicArrayOf(Type *type) {
	if (!type->dynamicArrayPrototype) {
		type->dynamicArrayPrototype = TYPE_NEW(TypeArray);

		type->dynamicArrayPrototype->size = 24;
		type->dynamicArrayPrototype->alignment = 8;
		type->dynamicArrayPrototype->flavor = TypeFlavor::ARRAY;
		type->dynamicArrayPrototype->arrayOf = type;
		type->dynamicArrayPrototype->count = 0;
		type->dynamicArrayPrototype->flags = TYPE_ARRAY_IS_DYNAMIC;

		type->dynamicArrayPrototype->name.length = type->name.length + 4;
		type->dynamicArrayPrototype->name.characters = TYPE_NEW_ARRAY(char, type->dynamicArrayPrototype->name.length);

		type->dynamicArrayPrototype->name.characters[0] = '[';
		type->dynamicArrayPrototype->name.characters[1] = '.';
		type->dynamicArrayPrototype->name.characters[2] = '.';
		type->dynamicArrayPrototype->name.characters[3] = ']';
		memcpy(type->dynamicArrayPrototype->name.characters + 4, type->name.characters, type->name.length);
	}

	return type->dynamicArrayPrototype;
}

constexpr inline u64 getDigitCount(u64 value) {
	u64 count = 0;

	do {
		++count;
	} while (value /= 10);

	return count;
}

inline TypeArray *getFixedArrayOf(Type *type, u64 count) {
	auto array = TYPE_NEW(TypeArray);

	array->size = type->size * count;
	array->alignment = type->alignment;
	array->flavor = TypeFlavor::ARRAY;
	array->arrayOf = type;
	array->count = count;
	array->flags = TYPE_ARRAY_IS_FIXED;

	constexpr u64 digitCountInU64 = getDigitCount(UINT64_MAX);

	array->name.characters = TYPE_NEW_ARRAY(char, type->name.length + 2 + digitCountInU64);

	array->name.characters[0] = '[';
	_ui64toa(count, array->name.characters + 1, 10);

	u64 offset = strlen(array->name.characters);

	array->name.length = offset + 1 + type->name.length;

	array->name.characters[offset] = ']';

	memcpy(array->name.characters + offset + 1, type->name.characters, type->name.length);

	return array;
}

struct Expr;


struct TypeFunction : Type {
	Expr *returnType;

	u64 argumentCount;
	Expr **argumentTypes;
};


enum class ExprFlavor : u8 {
	INT_LITERAL,
	FLOAT_LITERAL, 
	STRING_LITERAL,
	TYPE_LITERAL,
	ARRAY, 
	IDENTIFIER, 

	UNARY_OPERATOR, 
	BINARY_OPERATOR, 
	FUNCTION_CALL, 

	STRUCT_ACCESS, 

	FUNCTION, 

	BLOCK, 
	FOR, 
	WHILE, 
	BREAK, 
	CONTINUE, 
	RETURN, 
	IF, 

};

struct Declaration;

struct Expr {
	CodeLocation start;
	EndLocation end;
	u16 flags = 0;
	ExprFlavor flavor;

	Type *type = nullptr;

	Declaration *valueOfDeclaration = nullptr;

#if BUILD_DEBUG // So I don't have to cast it to view the actual expression type in the debugger
	virtual ~Expr() {}
#endif
};

#define EXPR_FOR_BY_POINTER 0x1
#define EXPR_CAST_IS_IMPLICIT 0x2
#define EXPR_HAS_STORAGE 0x4
#define EXPR_FUNCTION_IS_EXTERNAL 0x8
#define EXPR_ARRAY_IS_DYNAMIC 0x10
#define EXPR_IDENTIFIER_RESOLVING_IN_OUTER_FUNCTION 0x20
#define EXPR_IDENTIFIER_IS_BREAK_OR_CONTINUE_LABEL 0x40


struct ExprLiteral : Expr {
	union {
		u64 unsignedValue;
		s64 signedValue;
		double floatValue;
		Type *typeValue;
	};
};

struct ExprStringLiteral : Expr {
	String string;

	union Symbol *symbol;
	u32 physicalStorage;
};

struct ExprArray : Expr {
	Expr **storage;
	u64 count;
};

struct ExprBinaryOperator : Expr {
	Expr *left;
	Expr *right;
	TokenT op;
};

struct ExprUnaryOperator : Expr {
	Expr *value;
	TokenT op;
};

struct Block;

struct ExprIdentifier : Expr {
	String name;
	Block *resolveFrom;
	u64 indexInBlock;

	Declaration *declaration;
};

struct ExprFunctionCall : Expr {
	Expr *function;
	u64 argumentCount;
	Expr **arguments;
};

struct ExprStructAccess : Expr {
	Expr *left;
	String name;
	Declaration *declaration;
};

#define BLOCK_IS_ARGUMENTS 0x1
#define BLOCK_IS_COMPLETE 0x2
#define BLOCK_IS_LOOP 0x4

struct Block {
	Array<Declaration *> declarations;
	Block *parentBlock = nullptr;
	u64 indexInParent;
	u64 flags = 0;
};

struct ExprBlock : Expr {
	Block declarations;

	Array<Expr *> exprs;
};

struct ExprFunction : Expr {
	Block arguments;
	Expr *returnType;
	IrState state;

	Expr *body;

	union Symbol *symbol;
	u32 physicalStorage;
};

struct ExprReturn : Expr {
	ExprFunction *returnsFrom;
	Expr *value;
};

struct ExprLoop : Expr {
	union {
		Expr *forBegin;
		Expr *whileCondition;
	};
	Expr *forEnd;

	Expr *body;

	u64 irPointer;
	Block iteratorBlock;
};

struct ExprBreakOrContinue : Expr {
	Expr *label;
	ExprLoop *refersTo;
};

struct ExprIf : Expr {
	Expr *condition;
	Expr *ifBody;
	Expr *elseBody;
};

#define DECLARATION_IS_CONSTANT 0x1
#define DECLARATION_IS_UNINITIALIZED 0x2
#define DECLARATION_IS_ITERATOR 0x4
#define DECLARATION_IS_ITERATOR_INDEX 0x8
#define DECLARATION_VALUE_IS_READY 0x10
#define DECLARATION_TYPE_IS_READY 0x20
#define DECLARATION_IS_ARGUMENT 0x40
#define DECLARATION_HAS_STORAGE 0x80

inline void generateTypeNameForFunction(TypeFunction *function) {
	function->name.length = 6; // "() -> "

	function->name.length += static_cast<ExprLiteral *>(function->returnType)->typeValue->name.length;

	for (u64 i = 0; i < function->argumentCount; i++) {
		function->name.length += static_cast<ExprLiteral *>(function->argumentTypes[i])->typeValue->name.length;

		if (i + 1 != function->argumentCount) {
			function->name.length += 2;
		}
	}

	function->name.characters = TYPE_NEW_ARRAY(char, function->name.length);

	char *cursor = function->name.characters;
	*cursor++ = '(';

	for (u64 i = 0; i < function->argumentCount; i++) {
		String name = static_cast<ExprLiteral *>(function->argumentTypes[i])->typeValue->name;

		memcpy(cursor, name.characters, name.length);
		cursor += name.length;

		if (i + 1 != function->argumentCount) {
			*cursor++ = ',';
			*cursor++ = ' ';
		}
	}

	*cursor++ = ')';
	*cursor++ = ' ';
	*cursor++ = '-';
	*cursor++ = '>';
	*cursor++ = ' ';


	String name = static_cast<ExprLiteral *>(function->returnType)->typeValue->name;

	memcpy(cursor, name.characters, name.length);
}


struct Declaration {
	CodeLocation start;
	EndLocation end;
	String name;
	Expr *type;
	Expr *initialValue;
	Block *enclosingScope;

	union Symbol *symbol;
	u64 physicalStorage;

	u64 flags = 0;
	
};


inline bool typesAreSame(Type *a, Type *b) {
	if (a == b) return true;
	if (a->flavor != b->flavor) return false;

	switch (a->flavor) {
		case TypeFlavor::BOOL:
		case TypeFlavor::VOID:
		case TypeFlavor::TYPE:
		case TypeFlavor::STRING:
			return true;
		case TypeFlavor::INTEGER:
			return a->size == b->size && FLAGS_ARE_SAME(a->flags, b->flags, TYPE_INTEGER_IS_SIGNED);
		case TypeFlavor::FLOAT:
			return a->size == b->size;
		case TypeFlavor::POINTER: {
			return typesAreSame(static_cast<TypePointer *>(a)->pointerTo, static_cast<TypePointer *>(b)->pointerTo);
		}
		case TypeFlavor::FUNCTION: {
			auto fa = static_cast<TypeFunction *>(a);
			auto fb = static_cast<TypeFunction *>(b);



			if (fa->argumentCount != fb->argumentCount) return false;

			auto ra = fa->returnType;
			auto rb = fb->returnType;

			assert(ra->flavor == ExprFlavor::TYPE_LITERAL); // The types should be resolved to a type literal at this point
			assert(rb->flavor == ExprFlavor::TYPE_LITERAL); // The types should be resolved to a type literal at this point

			if (!typesAreSame(static_cast<ExprLiteral *>(ra)->typeValue, static_cast<ExprLiteral *>(rb)->typeValue))
				return false;

			for (u64 i = 0; i < fa->argumentCount; i++) {
				auto aa = fa->argumentTypes[i];
				auto ab = fb->argumentTypes[i];

				assert(aa->flavor == ExprFlavor::TYPE_LITERAL); // The types should be resolved to a type literal at this point
				assert(ab->flavor == ExprFlavor::TYPE_LITERAL); // The types should be resolved to a type literal at this point

				if (!typesAreSame(static_cast<ExprLiteral *>(aa)->typeValue, static_cast<ExprLiteral *>(ab)->typeValue))
					return false;
			}

			return true;
		}
		case TypeFlavor::ARRAY: {
			auto ta = static_cast<TypeArray *>(a);
			auto tb = static_cast<TypeArray *>(b);

			if ((ta->flags ^ tb->flags) & (TYPE_ARRAY_IS_DYNAMIC | TYPE_ARRAY_IS_FIXED)) return false;

			if ((ta->flags & TYPE_ARRAY_IS_FIXED) && ta->count != tb->count) return false;
			
			return typesAreSame(ta->arrayOf, tb->arrayOf);
		}
		default:
			assert(false);
			return false;
	}
}