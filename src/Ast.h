#pragma once

#include "Basic.h"
#include "String.h"
#include "Array.h"
#include "BucketedArenaAllocator.h"

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
	u64 flags = 0;
	TypeFlavor flavor;

	TypePointer *pointerPrototype = nullptr;
	TypeArray *arrayPrototype = nullptr;
	TypeArray *dynamicArrayPrototype = nullptr;
};

inline Type TYPE_S8 = { 1, 1, TYPE_INTEGER_IS_SIGNED, TypeFlavor::INTEGER };
inline Type TYPE_S16 = { 2, 2, TYPE_INTEGER_IS_SIGNED, TypeFlavor::INTEGER };
inline Type TYPE_S32 = { 4, 4, TYPE_INTEGER_IS_SIGNED, TypeFlavor::INTEGER };
inline Type TYPE_S64 = { 8, 8, TYPE_INTEGER_IS_SIGNED, TypeFlavor::INTEGER };

inline Type TYPE_U8 = { 1, 1, 0, TypeFlavor::INTEGER };
inline Type TYPE_U16 = { 2, 2, 0, TypeFlavor::INTEGER };
inline Type TYPE_U32 = { 4, 4, 0, TypeFlavor::INTEGER };
inline Type TYPE_U64 = { 8, 8, 0, TypeFlavor::INTEGER };

inline Type TYPE_UNSIGNED_INT_LITERAL = { 0, 0, TYPE_IS_INTERNAL, TypeFlavor::INTEGER };
inline Type TYPE_SIGNED_INT_LITERAL = { 0, 0, TYPE_INTEGER_IS_SIGNED | TYPE_IS_INTERNAL, TypeFlavor::INTEGER };

inline Type TYPE_F32 = { 4, 4, 0, TypeFlavor::FLOAT };
inline Type TYPE_F64 = { 8, 8, 0, TypeFlavor::FLOAT };

inline Type TYPE_FLOAT_LITERAL = { 0, 0, TYPE_IS_INTERNAL, TypeFlavor::FLOAT };

inline Type TYPE_VOID = { 1, 1, 0, TypeFlavor::VOID }; // Give void size and alignment of 1, so *void math just adds raw memory
inline Type TYPE_BOOL = { 1, 1, 0, TypeFlavor::BOOL };

inline Type TYPE_TYPE = { 0, 0, 0, TypeFlavor::TYPE };

inline Type TYPE_AUTO_CAST = { 0, 0, TYPE_IS_INTERNAL, TypeFlavor::AUTO_CAST };

// @StringFormat make strings length based, currently they are the same as *u8
inline Type TYPE_STRING = { 8, 8, 0, TypeFlavor::STRING };


struct TypePointer : Type {
	Type *pointerTo;
};

struct TypeArray : Type {
	Type *arrayOf;
	u64 count;
};

inline TypePointer *getPointerTo(Type *type) {
	if (!type->pointerPrototype) {
		type->pointerPrototype = new TypePointer;

		type->pointerPrototype->size = 8;
		type->pointerPrototype->alignment = 8;
		type->pointerPrototype->flavor = TypeFlavor::POINTER;
		type->pointerPrototype->pointerTo = type;
	}

	return type->pointerPrototype;
}

inline TypeArray *getArrayOf(Type *type) {
	if (!type->arrayPrototype) {
		type->arrayPrototype = new TypeArray;

		type->arrayPrototype->size = 16;
		type->arrayPrototype->alignment = 8;
		type->arrayPrototype->flavor = TypeFlavor::ARRAY;
		type->arrayPrototype->arrayOf = type;
		type->arrayPrototype->count = 0;
	}

	return type->arrayPrototype;
}

inline TypeArray *getDynamicArrayOf(Type *type) {
	if (!type->dynamicArrayPrototype) {
		type->dynamicArrayPrototype = new TypeArray;

		type->dynamicArrayPrototype->size = 24;
		type->dynamicArrayPrototype->alignment = 8;
		type->dynamicArrayPrototype->flavor = TypeFlavor::ARRAY;
		type->dynamicArrayPrototype->arrayOf = type;
		type->dynamicArrayPrototype->count = 0;
	}

	return type->dynamicArrayPrototype;
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
	ExprFlavor flavor;
	EndLocation end;
	u64 flags = 0;

	Declaration *declaration = nullptr;

	Type *type = nullptr;

#if BUILD_DEBUG // So I don't have to cast it to view the actual expression type in the debugger
	virtual ~Expr() {}
#endif
};

#define EXPR_FOR_BY_POINTER 0x1
#define EXPR_CAST_IS_IMPLICIT 0x2
#define EXPR_HAS_STORAGE 0x4
#define EXPR_FUNCTION_IS_EXTERNAL 0x8
#define EXPR_ARRAY_IS_DYNAMIC 0x10


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
	Declaration *declaration;
	Block *resolveFrom;
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

struct Block {
	Array<Declaration *> declarations;
	Block *parentBlock = nullptr;
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
	Block iteratorBlock;

	union {
		Expr *forBegin;
		Expr *whileCondition;
	};
	Expr *forEnd;

	Expr *body;

	u64 irPointer;
};

struct ExprBreakOrContinue : Expr {
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