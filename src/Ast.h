#pragma once

#include "Basic.h"
#include "String.h"

enum class TokenT : u8;

enum class TypeFlavor : u8 {
	VOID, 
	INTEGER,
	FLOAT, 
	POINTER, 
	BOOL, 
	FUNCTION, 
	TYPE, 
	AUTO_CAST
};

#define TYPE_INTEGER_IS_SIGNED 0x2
#define TYPE_IS_INTERNAL      0x4 // This means that this type should never be accessible to the user, i.e. the type of null, which is special to give it casting properties, but the program should never be able to reference it.
#define TYPE_IS_INTERNAL      0x4 // This means that this type should never be accessible to the user, i.e. the type of null, which is special to give it casting properties, but the program should never be able to reference it.

struct Type {
	u64 size;
	u64 alignment;
	u64 flags = 0;
	TypeFlavor flavor;

};

extern Type TYPE_S8;
extern Type TYPE_S16;
extern Type TYPE_S32;
extern Type TYPE_S64;

extern Type TYPE_U8;
extern Type TYPE_U16;
extern Type TYPE_U32;
extern Type TYPE_U64;

extern Type TYPE_SIGNED_INT_LITERAL;
extern Type TYPE_UNSIGNED_INT_LITERAL;

extern Type TYPE_F32;
extern Type TYPE_F64;

extern Type TYPE_FLOAT_LITERAL;

extern Type TYPE_VOID;
extern Type TYPE_BOOL;

extern Type TYPE_TYPE;

extern Type TYPE_AUTO_CAST;

struct TypePointer : Type {
	Type *pointerTo;
};

extern TypePointer TYPE_VOID_POINTER;

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

	Expr *body;
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


struct Declaration {
	CodeLocation start;
	EndLocation end;
	String name;
	Expr *type;
	Expr *initialValue;
	Block *enclosingScope;

	u64 irRegister;

	u64 flags = 0;
	
};


inline bool typesAreSame(Type *a, Type *b) {
	if (a == b) return true;
	if (a->flavor != b->flavor) return false;

	switch (a->flavor) {
		case TypeFlavor::BOOL:
		case TypeFlavor::VOID:
		case TypeFlavor::TYPE:
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
		default:
			assert(false);
			return false;
	}
}