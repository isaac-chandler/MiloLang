#pragma once

#include "Basic.h"
#include "String.h"
#include "Array.h"
#include "BucketedArenaAllocator.h"
#include "Block.h"
#include "TypeTable.h"


enum class CoffJobFlavor : u8 {
	FUNCTION,
	GLOBAL_DECLARATION,
	TYPE
};

struct CoffJob {
	union {
		struct Declaration *declaration;
		struct ExprFunction *function;
		struct Type *type;
	};

	CoffJobFlavor flavor;
};

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
	ZERO_MEMORY, 
	COPY, 
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
	STACK_ADDRESS,
	IMMEDIATE,
	FLOAT_TO_INT,
	INT_TO_FLOAT,
	FLOAT_CAST, 
	EXTEND_INT, 
	RETURN,
	CALL,
	NEG,
	NOOP,
	FUNCTION, 
	STRING, 
	ARRAY_LITERAL, 
	STRUCT_LITERAL, 
	LINE_MARKER, 
	TYPE, 
	TYPE_INFO, 
	BLOCK, 
	POP_COUNT, 
	BIT_SCAN_FORWARD
};

#define IR_SIGNED_OP 0x1
#define IR_FLOAT_OP 0x2
#define IR_C_CALL 0x4


inline bool isStandardSize(u64 size) {
	return size == 1 || size == 2 || size == 4 || size == 8;
}

struct Argument {
	u32 number;
	struct Type *type;
};

struct FunctionCall {
	struct Type *returnType;
	u32 argCount;
	Argument args[];
};

struct Ir {
	union {
		struct {
			u32 dest;
			u32 opSize;
			union {
				u32 a;
				struct ExprFunction *function;
			};

			union {
				u32 b;
				u64 immediate;
				void *data;
			};

		};

		struct {
			CodeLocation start;
			EndLocation end;
		} location;
	};

	u8 flags = 0;
	IrOp op;
};

struct IrState {
	u32 nextRegister = 0;
	u32 contextRegister = 0;
	u32 parameters = 0;
	u32 stackSpace = 0;
	u32 maxCallArguments = 0;
	Array<Ir> ir;

	BucketedArenaAllocator allocator;

	IrState() : allocator(1024) {}
};


enum class TokenT : u8;

enum class ExprFlavor : u8 {
	INT_LITERAL,
	FLOAT_LITERAL, 
	STRING_LITERAL,
	TYPE_LITERAL,
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
	ENUM, 
	ENUM_INCREMENT, 

	COMMA_ASSIGNMENT, 

	SWITCH, 

	STATIC_IF, 
	DEFER, 

	SLICE, 
	RUN, 

	LOAD, 
	IMPORT, 

	OVERLOAD_SET, 

	ARRAY_LITERAL, 
	STRUCT_LITERAL, 

	CONTEXT, 
	PUSH_CONTEXT, 
	ENTRY_POINT,
	ADD_CONTEXT
};

struct Declaration;

struct Expr {
	CodeLocation start;
	EndLocation end;
	ExprFlavor flavor;

	u64 flags = 0;

	struct Type *type = nullptr;

	Declaration *valueOfDeclaration = nullptr;

#if BUILD_DEBUG // So I don't have to cast it to view the actual expression type in the debugger
	virtual ~Expr() {}
#endif
};

struct ExprOverloadSet : Expr {
	ExprIdentifier *identifier;
	
	Declaration *currentOverload;
	Block *block;
};

struct ExprAddContext : Expr {
	Declaration *declaration;
	Importer *using_;
};

struct ExprSlice : Expr {
	Expr *array;
	Expr *sliceStart;
	Expr *sliceEnd;
};

struct ExprRun : Expr {
	Expr *returnValue = nullptr;
	struct RunJob *runJob = nullptr;
	struct Module *module;
	Array<struct SubJob *> sleepingOnMe;
	Expr *function;
};

struct ExprDefer : Expr {
	Expr *expr;
	Block *enclosingScope;
};


// The for loop value is being iterated by pointer
#define EXPR_FOR_BY_POINTER 0x1

// This cast was implicitly inserted during type checking
#define EXPR_CAST_IS_IMPLICIT 0x2

// This expression has already been given storage space in the output executable
#define EXPR_HAS_STORAGE 0x4

#define EXPR_FUNCTION_IS_EXTERNAL 0x8


#define EXPR_ARRAY_IS_DYNAMIC 0x10

// Set when identifier resolving passes out of a struct or out of a function, this stops a variable from outside the function being referenced
// since we don't support captures
#define EXPR_IDENTIFIER_RESOLVING_ONLY_CONSTANTS 0x20
#define EXPR_IDENTIFIER_IS_BREAK_OR_CONTINUE_LABEL 0x40

// This is an initializer assignment that was inserted by a variable 
// intitialization in a runtime scope
#define EXPR_ASSIGN_IS_IMPLICIT_INITIALIZER 0x80

#define EXPR_FUNCTION_IS_C_CALL 0x100

// These two flags are used to check #must
#define EXPR_FUNCTION_CALL_IS_STATEMENT_LEVEL 0x400     // This is set if the function call is a statement so all return values are ignored
#define EXPR_FUNCTION_CALL_IS_IN_COMMA_ASSIGNMENT 0x800 // This is set if the function call is used as a value in an expression so only the first return value is received

// Set if a comma assignment is a := assignment not just an = assignment, so it should declare its arguments
#define EXPR_COMMA_ASSIGNMENT_IS_DECLARATION 0x1000

// This flag tells us if an integer literal 0 was a -0 that has been evaluated to 0, 
// this is required in the case that the literal is implicitly converted to a float, 
// when we will want the float to be -0.0 instead of just 0
#define EXPR_INTEGER_LITERAL_IS_NEGATIVE_ZERO 0x2000

// When we generate the cases for a switch we create an == ExprBinaryOperator node
// so that we don't have to add an extra case in typechecking, the left side is the 
// value we are switching which is already typechecked so we don't typecheck that again
#define EXPR_EQUALS_IS_IMPLICIT_SWITCH 0x4000

// If a switch if is marked as #complete this flag is set, 
// this means that the switch must include a case for every possible
// value of an enum
#define EXPR_SWITCH_IS_COMPLETE 0x8000

// If a cast is marked as bitwise, the two types must be the same size, 
// there will be no conversion made, the bits are simply reinterpreted
#define EXPR_CAST_IS_BITWISE 0x1'0000

#define EXPR_FUNCTION_HAS_VARARGS 0x2'0000

#define EXPR_FUNCTION_RUN_READY 0x8'0000
#define EXPR_FUNCTION_RUN_CHECKED 0x10'0000

#define EXPR_IS_SPREAD 0x20'0000

#define EXPR_FUNCTION_IS_COMPILER 0x40'0000

#define EXPR_IMPORT_IS_EXPR 0x80'0000

#define EXPR_IDENTIER_DEFINES_POLYMORPH_VARIABLE 0x100'0000

#define EXPR_FUNCTION_IS_POLYMORPHIC 0x200'0000

#define EXPR_STRUCT_LITERAL_UNSPECIFIED_MEMBERS_UNINITIALZIED 0x400'0000

#define EXPR_INVERT_UNARY_DOT 0x800'0000

#define EXPR_FUNCTION_IS_INSTRINSIC 0x1000'0000
#define EXPR_CONTEXT_AVAILABLE 0x2000'0000

#define ENUM_SPECIAL_MEMBER_COUNT 1

struct ExprLoad : Expr {
	Expr *file;
	struct Module *module;
};

struct ExprLiteral : Expr {
	union {
		u64 unsignedValue;
		s64 signedValue;
		double floatValue;
		struct Type *typeValue;
	};
};

struct ExprSwitch : Expr {
	struct Case {
		Expr *condition;
		Expr *block;
		bool fallsThrough; // @Memory this wastes 7 bytes

		class llvm::BasicBlock *llvmCaseBlock;

		u32 irBranch;
		u32 irSkip;

	};

	Expr *condition;

	Array<Case> cases;
};

struct ExprStringLiteral : Expr {
	String string;

	union Symbol *symbol;
	u32 physicalStorage;
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
	u32 serial;
	Block *resolveFrom;
	Block *enclosingScope;
	Expr *structAccess;
	Module *module;

	Declaration *declaration;
};

// This is an internal compiler expression that is used when the value of an enum is not specified
// it tells the type checker to give it the value next after the previous declaration
struct ExprEnumIncrement : Expr {
	Declaration *previous;
};

struct Arguments {
	Expr **values;
	String *names;
	Declaration **declarations = nullptr;
	u32 count;

};

struct ExprFunctionCall : Expr {
	Expr *function;
	Arguments arguments;
};

struct ExprBlock : Expr {
	Block declarations;

	Array<Expr *> exprs;
};

struct ExprArrayLiteral : Expr {
	Expr *typeValue;
	Expr **values;
	u32 count;

	union {
		struct {
			union Symbol *symbol;
			u32 physicalStorage;
		};

		llvm::GlobalVariable *llvmStorage = nullptr;
	};

	void *runtimeValue = nullptr;
};

struct ExprStructLiteral : Expr {
	Expr *typeValue;
	Arguments initializers;

	union {
		struct {
			union Symbol *symbol;
			u32 physicalStorage;
		};

		llvm::GlobalVariable *llvmStorage = nullptr;
	};

	void *runtimeValue = nullptr;
};

struct ExprFunction : Expr {
	Block constants;
	Block arguments;
	Block returns;
	IrState state;

	Expr *body;
	
	Array<ExprFunction *> polymorphs;
	Array<struct SubJob *> sleepingOnInfer;
	Array<struct SubJob *> sleepingOnIr;

	union {
		struct {
			union Symbol *symbol;
			u32 physicalStorage;
		};

		llvm::Function *llvmStorage;
	};

	void *loadedFunctionPointer = nullptr;

	ExprFunction() : llvmStorage(nullptr) {}
};

struct ExprReturn : Expr {
	ExprFunction *returnsFrom;
	Arguments returns;
};

struct ExprLoop : Expr {
	union {
		Expr *forBegin;
		Expr *whileCondition;
	};
	Expr *forEnd;

	Expr *body;

	Expr *completedBody;

	u32 irPointer;
	u32 arrayPointer;
	class llvm::Value *llvmPointer;
	class llvm::Value *llvmArrayPointer;

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

struct ExprEnum : Expr {
	TypeEnum struct_;
};

struct ExprCommaAssignment : Expr {
	Expr **left;

	Expr *call;
	u32 exprCount;
};

inline Type *getTypeForExpr(Expr *expr) {
	auto type = expr->type;

	if (type == &TYPE_SIGNED_INT_LITERAL) {
		assert(expr->flavor == ExprFlavor::INT_LITERAL);
		return &TYPE_S64;
	}
	else if (type == &TYPE_UNSIGNED_INT_LITERAL) {
		assert(expr->flavor == ExprFlavor::INT_LITERAL);

		if (static_cast<ExprLiteral *>(expr)->unsignedValue < static_cast<u64>(INT64_MAX)) {
			return &TYPE_S64;
		}
		else {
			return &TYPE_U64;
		}
	}
	else if (type == &TYPE_FLOAT_LITERAL) {
		assert(expr->flavor == ExprFlavor::FLOAT_LITERAL);
		return &TYPE_F64;
	}

	return type;
}



inline bool isLiteral(Expr *expr);

inline bool arrayIsLiteral(ExprArrayLiteral *array) {
	for (u64 i = 0; i < array->count; i++)
		if (!isLiteral(array->values[i]))
			return false;

	return true;
}

inline bool structIsLiteral(ExprStructLiteral *struct_) {
	for (u64 i = 0; i < struct_->initializers.count; i++)
		if (!isLiteral(struct_->initializers.values[i]))
			return false;

	return true;
}

inline bool isLiteral(Expr *expr) {
	return expr->flavor == ExprFlavor::INT_LITERAL ||
		expr->flavor == ExprFlavor::FLOAT_LITERAL ||
		expr->flavor == ExprFlavor::TYPE_LITERAL ||
		expr->flavor == ExprFlavor::STRING_LITERAL ||
		expr->flavor == ExprFlavor::FUNCTION ||
		expr->flavor == ExprFlavor::IMPORT ||
		(expr->flavor == ExprFlavor::ARRAY_LITERAL && arrayIsLiteral(static_cast<ExprArrayLiteral *>(expr))) ||
		(expr->flavor == ExprFlavor::STRUCT_LITERAL && structIsLiteral(static_cast<ExprStructLiteral *>(expr)));
}

inline Type *getDeclarationType(Declaration *declaration) {
	assert(declaration->flags & DECLARATION_TYPE_IS_READY);
	assert(declaration->type);
	assert(declaration->type->flavor == ExprFlavor::TYPE_LITERAL);

	return static_cast<ExprLiteral *>(declaration->type)->typeValue;
}