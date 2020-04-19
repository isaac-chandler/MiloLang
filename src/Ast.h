#pragma once

#include "Basic.h"
#include "String.h"
#include "Array.h"
#include "BucketedArenaAllocator.h"
#include "TypeTable.h"

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

	FUNCTION_PROTOTYPE
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


struct Declaration {
	CodeLocation start;
	EndLocation end;
	String name;
	Expr *type;
	Expr *initialValue;
	Block *enclosingScope;

	struct InferJob *inferJob;

	union Symbol *symbol;
	u64 physicalStorage;

	u64 flags = 0;
	
};