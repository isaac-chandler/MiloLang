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
	NAMESPACE, 

	MAX
};

constexpr const char *TYPE_FLAVOR_NAMES[] = {
	"void",
	"integer",
	"float",
	"pointer",
	"bool",
	"function",
	"type",
	"auto cast", 
	"string", 
	"array", 
	"struct", 
	"enum", 
	"namespace"
};

#define TYPE_FLAVOR_NAME(x) (TYPE_FLAVOR_NAMES[static_cast<u8>(x)])

static_assert(sizeof(TYPE_FLAVOR_NAMES) / sizeof(const char *) == static_cast<u8>(TypeFlavor::MAX));

#define TYPE_INTEGER_IS_SIGNED 0x2
#define TYPE_IS_INTERNAL      0x4 // This means that this type should never be accessible to the user, i.e. integer literals, which is special to give it casting properties, but the program should never be able to reference it
#define TYPE_ARRAY_IS_FIXED 0x8
#define TYPE_ARRAY_IS_DYNAMIC 0x10
#define TYPE_STRUCT_IS_UNION 0x20
#define TYPE_STRUCT_IS_PACKED 0x40
#define TYPE_ENUM_IS_FLAGS 0x80
#define TYPE_FUNCTION_IS_C_CALL 0x100
#define TYPE_USED_IN_OUTPUT 0x200
#define TYPE_IS_ANONYMOUS 0x400

struct Type {
	u32 size;
	u32 alignment;
	String name;
	u32 hash;
	u16 flags = 0;
	TypeFlavor flavor;
	Array<struct SubJob *> sleepingOnMe;
	struct SizeJob *sizeJob = nullptr;

	llvm::Type *llvmType = nullptr;

	union Symbol *symbol = nullptr;
	u32 physicalStorage;
	u32 codeviewTypeIndex = 0;

	struct Expr *defaultValue = nullptr;
	class llvm::GlobalVariable *llvmStorage = nullptr;
	struct Type_Info *runtimeTypeInfo = nullptr;
};

struct TypePointer : Type {
	Type *pointerTo;
};

struct TypeStruct : Type {
	struct Block *enclosingScope; // This field is only used for debug info for struct, union, enum, enum_flags	
	Block members;
};

struct TypeEnum : TypeStruct {
	Type *integerType;
	TypeStruct values; // Does not contain other fields i.e. integer_type
};

struct TypeArray : TypeStruct {
	Type *arrayOf;
	u32 count;
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
	Type **argumentTypes;
	Type **returnTypes;

	u32 argumentCount;
	u32 returnCount;
	bool isVarargs;
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

inline Type TYPE_MODULE = { 0, 0, "<module>", 59, TYPE_IS_INTERNAL, TypeFlavor::NAMESPACE };

inline TypeStruct TYPE_STRING = { 16, 8, "string", 61, 0, TypeFlavor::STRING };

inline Type TYPE_UNARY_DOT = { 0, 0, "<unary dot>", 67, TYPE_IS_INTERNAL, TypeFlavor::AUTO_CAST };

inline Type TYPE_OVERLOAD_SET = { 0, 0, "<overload set>", 71, TYPE_IS_INTERNAL, TypeFlavor::AUTO_CAST };

inline Type TYPE_POLYMORPHIC_FUNCTION = { 0, 0, "<polymorhpic function>", 73, TYPE_IS_INTERNAL, TypeFlavor::AUTO_CAST };

inline Type *TYPE_VOID_POINTER;
inline Type *TYPE_U8_POINTER;
inline Type *TYPE_U8_ARRAY;

inline Type *TYPE_ANY;
inline Type *TYPE_TYPE_INFO;
inline Type *TYPE_TYPE_INFO_INTEGER;
inline Type *TYPE_TYPE_INFO_POINTER;
inline Type *TYPE_TYPE_INFO_FUNCTION;
inline Type *TYPE_TYPE_INFO_ARRAY;
inline Type *TYPE_TYPE_INFO_STRUCT;
inline Type *TYPE_TYPE_INFO_STRUCT_MEMBER;
inline Type *TYPE_TYPE_INFO_ENUM;
inline Type *TYPE_TYPE_INFO_ENUM_VALUE;

void addStruct(TypeStruct *struct_);

TypePointer *getPointer(Type *type);

TypeArray *getArray(Type *type);

TypeArray *getDynamicArray(Type *type);

TypeArray *getStaticArray(Type *type, u32 count);

TypeFunction *getFunctionType(struct ExprFunction *expr);

void setupTypeTable();

struct TypeTableEntry {
	Type *value;
	u32 hash = 0;
};

inline TypeTableEntry *typeTableEntries;
inline u32 typeTableCapacity;

// These structs are parallel to the ones defined in runtime.milo

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
	String name;
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
	bool c_call;
	bool varargs;
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

		String name;
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
		String name;
		u64 value;
	};

	Type_Info_Integer *base_type;
	bool is_flags;
	MiloArray<Value> values;
};