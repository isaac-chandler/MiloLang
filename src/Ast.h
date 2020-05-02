#pragma once

#include "Basic.h"
#include "String.h"
#include "Array.h"
#include "BucketedArenaAllocator.h"
#include "Block.h"

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
	STRING_EQUAL, 
	LINE_MARKER
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
	union {
		struct {
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

			u8 opSize;
		};

		struct {
			CodeLocation start;
			EndLocation end;
		} location;
	};

	u64 flags = 0;
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

	FUNCTION, 

	BLOCK, 
	FOR, 
	WHILE, 
	BREAK, 
	CONTINUE, 
	REMOVE, 
	RETURN, 
	IF, 

	FUNCTION_PROTOTYPE, 
	STRUCT_DEFAULT
};

struct Declaration;

struct Expr {
	CodeLocation start;
	EndLocation end;
	u16 flags = 0;
	ExprFlavor flavor;

	struct Type *type = nullptr;

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
#define EXPR_IDENTIFIER_RESOLVING_ONLY_CONSTANTS 0x20
#define EXPR_IDENTIFIER_IS_BREAK_OR_CONTINUE_LABEL 0x40
#define EXPR_ASSIGN_IS_IMPLICIT_INITIALIZER 0x80
// :EvaluatedBinaryLiterals
// Set for consistency in rules that only a literal can be assigned to a constant even though we do 'constexpr' evaluate 
// int + int -> int binary expressions so that expressions such as 3 + 5 can still be converted to unsigned implicitly 
// instead of being stuck as s64. We could allow these to be constants but it would be inconsistent which binary ops are 
// allowed as constants since we don't evaluate comparisons
#define EXPR_WAS_EVALUATED_BINARY 0x100
#define EXPR_VALUE_NOT_REQUIRED 0x200


struct ExprLiteral : Expr {
	union {
		u64 unsignedValue;
		s64 signedValue;
		double floatValue;
		struct Type *typeValue;
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
	Expr *structAccess;
	u64 indexInBlock;

	Declaration *declaration;
};

struct ExprFunctionCall : Expr {
	Expr *function;
	u64 argumentCount;
	Expr **arguments;
	String *argumentNames;
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

	Expr *completedBody;

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