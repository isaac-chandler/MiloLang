#pragma once

#include "Basic.h"
#include "String.h"
#include "BucketedArenaAllocator.h"
#include "Block.h"


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
	STRUCT, 
	ENUM, 
	NAMESPACE
};

#define TYPE_INTEGER_IS_SIGNED 0x2
#define TYPE_IS_INTERNAL      0x4 // This means that this type should never be accessible to the user, i.e. integer literals, which is special to give it casting properties, but the program should never be able to reference it
#define TYPE_ARRAY_IS_FIXED 0x8
#define TYPE_ARRAY_IS_DYNAMIC 0x10
#define TYPE_STRUCT_IS_UNION 0x20
#define TYPE_STRUCT_IS_PACKED 0x40
#define TYPE_ENUM_IS_FLAGS 0x80

struct Type {
	u64 size;
	u64 alignment;
	String name;
	u32 hash;
	u16 flags = 0;
	TypeFlavor flavor;
	Array<struct SubJob *> sleepingOnMe;
	struct SizeJob *sizeJob = nullptr;

	union Symbol *symbol = nullptr;
	u32 physicalStorage;
};

struct TypePointer : Type {
	Type *pointerTo;
};

struct TypeStruct : Type {
	Block members;
};

struct TypeEnum : TypeStruct {
	Type *integerType;
	Block *values;
};

struct TypeArray : TypeStruct {
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
	u64 returnCount;
	Type **returnTypes;

	u64 argumentCount;
	Type **argumentTypes;
};

inline Type TYPE_S8 = { 1, 1, "s8", 2, TYPE_INTEGER_IS_SIGNED, TypeFlavor::INTEGER };
inline Type TYPE_S16 = { 2, 2, "s16", 3, TYPE_INTEGER_IS_SIGNED, TypeFlavor::INTEGER };
inline Type TYPE_S32 = { 4, 4, "s32", 5, TYPE_INTEGER_IS_SIGNED, TypeFlavor::INTEGER };
inline Type TYPE_S64 = { 8, 8, "s64", 7, TYPE_INTEGER_IS_SIGNED, TypeFlavor::INTEGER };

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

inline Type TYPE_UNARY_DOT = { 0, 0, "<unary dot>", 67, TYPE_IS_INTERNAL, TypeFlavor::AUTO_CAST };

inline Type *TYPE_VOID_POINTER;

inline Type *TYPE_ANY;
inline Type *TYPE_TYPE_INFO;
inline Type *TYPE_TYPE_INFO_INTEGER;
inline Type *TYPE_TYPE_INFO_POINTER;
inline Type *TYPE_TYPE_INFO_FUNCTION;
inline Type *TYPE_TYPE_INFO_ARRAY;
inline Type *TYPE_TYPE_INFO_STRUCT;
inline Type *TYPE_TYPE_INFO_ENUM;

void addStruct(TypeStruct *struct_);

TypePointer *getPointer(Type *type);

TypeArray *getArray(Type *type);

TypeArray *getDynamicArray(Type *type);

TypeArray *getStaticArray(Type *type, u64 count);

TypeFunction *getFunctionType(struct ExprFunction *expr);

void setupTypeTable();

struct TypeTableEntry {
	Type *value;
	u32 hash = 0;
};

inline TypeTableEntry *typeTableEntries;
inline u32 typeTableCapacity;

// These structs are parallel to the ones defined in runtime.milo

template<typename T> 
struct MiloArray {
	T *data;
	u64 count;
};

struct Type_Info {
	enum class Tag : u64 {
		VOID = 0,
		INTEGER = 1,
		FLOAT = 2,
		POINTER = 3,
		BOOL = 4,
		FUNCTION = 5,
		TYPE = 6,
		STRING = 7,
		ARRAY = 8,
		STRUCT = 9,
		ENUM = 10
	};

	Tag tag;
	u64 size;
	u64 alignment;
	char *name;
};

struct Type_Info_Integer : Type_Info {
	bool signed_;
};

struct Type_Info_Pointer : Type_Info {
	Type_Info *value_type;
};

struct Type_Info_Function : Type_Info {
	MiloArray<Type_Info *> arguments;
	MiloArray<Type_Info *> returns;
};

struct Type_Info_Array : Type_Info {
	enum class Flavor : u64 {
		FIXED   =  0, 
		NORMAL  = 1, 
		DYNMAIC = 2
	};

	Flavor flavor;
	Type_Info *element_type;
	u64 count;
};

struct Type_Info_Struct : Type_Info {
	struct Member {
		struct Flags {
			constexpr static u64 UNINITIALIZED = 0x1;
			constexpr static u64 CONSTANT      = 0x2;
			constexpr static u64 USING         = 0x4;
		};

		char *name;
		u64 offset;
		Type_Info *member_type;
		void *initial_value;
		u64 flags;
	};

	struct Flags {
		constexpr static u64 UNION  = 0x1;
		constexpr static u64 PACKED = 0x2;
	};

	u64 flags;
	MiloArray<Member> members;
};

struct Type_Info_Enum : Type_Info {
	struct Value {
		char *name;
		u64 value;
	};

	Type_Info_Integer *base_type;
	bool is_flags;
	MiloArray<Value> values;
};