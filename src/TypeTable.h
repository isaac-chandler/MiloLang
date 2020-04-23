#pragma once

#include "Basic.h"
#include "String.h"
#include "BucketedArenaAllocator.h"


extern BucketedArenaAllocator typeArena;


#if 1
#define TYPE_NEW(T) new (static_cast<T *>(typeArena.allocate(sizeof(T)))) T
#define TYPE_NEW_ARRAY(T, C) new (static_cast<T *>(typeArena.allocate((C) * sizeof(T)))) T[C]
#else
#define TYPE_NEW(T) new T
#define TYPE_NEW_ARRAY(T, C) new T[C]
#endif

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
	ARRAY, 
	STRUCT
};

#define TYPE_INTEGER_IS_SIGNED 0x2
#define TYPE_IS_INTERNAL      0x4 // This means that this type should never be accessible to the user, i.e. the type of null, which is special to give it casting properties, but the program should never be able to reference it.
#define TYPE_IS_INTERNAL      0x4 // This means that this type should never be accessible to the user, i.e. the type of null, which is special to give it casting properties, but the program should never be able to reference it.
#define TYPE_ARRAY_IS_FIXED 0x8
#define TYPE_ARRAY_IS_DYNAMIC 0x10
#define TYPE_STRUCT_IS_UNION 0x20

struct Type {
	u64 size;
	u64 alignment;
	String name;
	u32 hash;
	u16 flags = 0;
	TypeFlavor flavor;
	struct InferJob *sizeJob = nullptr;
};

struct TypePointer : Type {
	Type *pointerTo;
};

struct TypeArray : Type {
	Type *arrayOf;
	u64 count;
};

constexpr inline u64 getDigitCount(u64 value) {
	u64 count = 0;

	do {
		++count;
	} while (value /= 10);

	return count;
}

struct Expr;


struct TypeFunction : Type {
	Type *returnType;

	u64 argumentCount;
	Type **argumentTypes;
};

#define BLOCK_IS_ARGUMENTS 0x1
#define BLOCK_IS_COMPLETE 0x2
#define BLOCK_IS_LOOP 0x4

struct Block {
	Array<struct Declaration *> declarations;
	Block *parentBlock = nullptr;
	u64 indexInParent;
	u64 flags = 0;
};

struct TypeStruct : Type {
	Block members;
};

inline Type TYPE_S8 = { 1, 1, "s8", 2, TYPE_INTEGER_IS_SIGNED, TypeFlavor::INTEGER };
inline Type TYPE_S16 = { 2, 2, "s16", 3, TYPE_INTEGER_IS_SIGNED, TypeFlavor::INTEGER };
inline Type TYPE_S32 = { 4, 4, "s32", 5, TYPE_INTEGER_IS_SIGNED, TypeFlavor::INTEGER };
inline Type TYPE_S64 = { 8, 8, "S64", 7, TYPE_INTEGER_IS_SIGNED, TypeFlavor::INTEGER };

inline Type TYPE_U8 = { 1, 1, "u8", 11, 0, TypeFlavor::INTEGER };
inline Type TYPE_U16 = { 2, 2, "u16", 13, 0, TypeFlavor::INTEGER };
inline Type TYPE_U32 = { 4, 4, "u32", 17, 0, TypeFlavor::INTEGER };
inline Type TYPE_U64 = { 8, 8, "u64", 19, 0, TypeFlavor::INTEGER };

inline Type TYPE_UNSIGNED_INT_LITERAL = { 0, 0, "<int literal>", 23, TYPE_IS_INTERNAL, TypeFlavor::INTEGER };
inline Type TYPE_SIGNED_INT_LITERAL = { 0, 0, "<signed int literal>", 29, TYPE_INTEGER_IS_SIGNED | TYPE_IS_INTERNAL, TypeFlavor::INTEGER };

inline Type TYPE_F32 = { 4, 4, "f32", 31, 0, TypeFlavor::FLOAT };
inline Type TYPE_F64 = { 8, 8, "f64", 37, 0, TypeFlavor::FLOAT };

inline Type TYPE_FLOAT_LITERAL = { 0, 0, "<float literal>", 41, TYPE_IS_INTERNAL, TypeFlavor::FLOAT };

inline Type TYPE_VOID = { 1, 1, "void", 43, 0, TypeFlavor::VOID }; // Give void size and alignment of 1, so *void math just adds raw memory
inline Type TYPE_BOOL = { 1, 1, "bool", 47, 0, TypeFlavor::BOOL };

inline Type TYPE_TYPE = { 8, 8, "type", 53, 0, TypeFlavor::TYPE };

inline Type TYPE_AUTO_CAST = { 0, 0, "<auto cast>", 59, TYPE_IS_INTERNAL, TypeFlavor::AUTO_CAST };

// @StringFormat make strings length based, currently they are the same as *u8
inline Type TYPE_STRING = { 8, 8, "string", 61, 0, TypeFlavor::STRING };

inline Type *TYPE_VOID_POINTER;

TypePointer *getPointer(Type *type);

TypeArray *getArray(Type *type);

TypeArray *getDynamicArray(Type *type);

TypeArray *getStaticArray(Type *type, u64 count);

TypeFunction *getFunctionType(struct ExprFunction *expr);

void setupTypeTable();
