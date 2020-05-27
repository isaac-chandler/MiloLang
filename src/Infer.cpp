#include "Basic.h"
#include "Infer.h"
#include "Array.h"
#include "Parser.h"
#include "Lexer.h"
#include "IrGenerator.h"
#include "CoffWriter.h"
#include "TypeTable.h"


u64 totalDeclarations = 0;
u64 totalFunctions = 0;
u64 totalTypesSized = 0;
u64 totalInfers = 0;

enum class InferType {
	FUNCTION_BODY,
	DECLARATION,
	TYPE_TO_SIZE
};

ExprLiteral *inferMakeTypeLiteral(CodeLocation &start, EndLocation &end, Type *type) {
	ExprLiteral *literal = new ExprLiteral;
	literal->flavor = ExprFlavor::TYPE_LITERAL;
	literal->start = start;
	literal->end = end;
	literal->typeValue = type;
	literal->type = &TYPE_TYPE;

	return literal;
}

struct InferJob {
	union {
		Declaration *declaration;
		ExprFunction *function;
		Type *type;
	} infer;

	InferType type;
	Block *waitingOnBlock = nullptr;

	Array<Expr **> typeFlattened;
	union {
		u64 typeFlattenedIndex = 0;
		u64 sizingIndexInMembers;
	};

	Array<Expr **> valueFlattened;
	union {
		u64 valueFlattenedIndex = 0;
		u64 runningSize;
	};

	Array<Type *> sizeDependencies;

	InferJob *nextFree;

	InferJob() : valueFlattened(20) {}
};

Array<InferJob *> inferJobs;


void flatten(Array<Expr **> &flattenTo, Expr **expr) {
	PROFILE_FUNC();
	switch ((*expr)->flavor) {
		case ExprFlavor::IDENTIFIER: {
			auto identifier = static_cast<ExprIdentifier *>(*expr);

			if (identifier->structAccess) {
				flatten(flattenTo, &identifier->structAccess);
			}

			flattenTo.add(expr);
			break;
		}
		case ExprFlavor::FUNCTION: {
			ExprFunction *function = static_cast<ExprFunction *>(*expr);

			flatten(flattenTo, &function->returnType);

			flattenTo.add(expr);

			// The function arguments are handled separately since the arguments are declarations of their own so will be type-checked separately
			// The function body is type-checked separately from the head so we don't get circular dependencies with recursive functions
			// And the body's types are irrelevant to the expression using the function

			break;
		}
		case ExprFlavor::FUNCTION_PROTOTYPE: {
			ExprFunction *function = static_cast<ExprFunction *>(*expr);

			flatten(flattenTo, &function->returnType);

			for (auto argument : function->arguments.declarations) {
				flatten(flattenTo, &argument->type);
			}

			flattenTo.add(expr);

			break;
		}
		case ExprFlavor::TYPE_LITERAL:
		case ExprFlavor::STRING_LITERAL:
		case ExprFlavor::FLOAT_LITERAL:
		case ExprFlavor::INT_LITERAL:
			break;
		case ExprFlavor::BREAK:
		case ExprFlavor::CONTINUE:
		case ExprFlavor::REMOVE: {
			auto continue_ = static_cast<ExprBreakOrContinue *>(*expr);

			if (continue_->label) {
				flatten(flattenTo, &continue_->label);
			}

			if (!continue_->refersTo) {
				flattenTo.add(expr);
			}

			break;
		}
		case ExprFlavor::BINARY_OPERATOR: {
			ExprBinaryOperator *binary = static_cast<ExprBinaryOperator *>(*expr);

			if (binary->left) {
				flatten(flattenTo, &binary->left);
			}

			if (!(binary->flags & EXPR_ASSIGN_IS_IMPLICIT_INITIALIZER)) {
				flatten(flattenTo, &binary->right);
			}

			if (binary->op == TokenT::CAST && !binary->left) {
				// Don't add auto-casts to the flattened list, there is nothing to infer
			}
			else {
				flattenTo.add(expr);
			}

			break;
		}
		case ExprFlavor::BLOCK: {
			ExprBlock *block = static_cast<ExprBlock *>(*expr);

			for (auto &subExpr : block->exprs)
				flatten(flattenTo, &subExpr);

			flattenTo.add(expr);

			break;
		}
		case ExprFlavor::FOR: {
			ExprLoop *loop = static_cast<ExprLoop *>(*expr);

			flatten(flattenTo, &loop->forBegin);

			if (loop->forEnd)
				flatten(flattenTo, &loop->forEnd);

			flattenTo.add(expr);

			if (loop->body)
				flatten(flattenTo, &loop->body);

			if (loop->completedBody)
				flatten(flattenTo, &loop->completedBody);

			break;
		}
		case ExprFlavor::FUNCTION_CALL: {
			ExprFunctionCall *call = static_cast<ExprFunctionCall *>(*expr);

			for (u64 i = 0; i < call->argumentCount; i++) {
				flatten(flattenTo, &call->arguments[i]);
			}

			flatten(flattenTo, &call->function);
			flattenTo.add(expr);

			break;
		}
		case ExprFlavor::IF: {
			ExprIf *ifElse = static_cast<ExprIf *>(*expr);

			flatten(flattenTo, &ifElse->condition);
			flattenTo.add(expr);

			if (ifElse->ifBody)
				flatten(flattenTo, &ifElse->ifBody);

			if (ifElse->elseBody)
				flatten(flattenTo, &ifElse->elseBody);

			break;
		}
		case ExprFlavor::RETURN: {
			ExprReturn *return_ = static_cast<ExprReturn *>(*expr);

			if (return_->value)
				flatten(flattenTo, &return_->value);

			flattenTo.add(expr);

			break;
		}
		case ExprFlavor::UNARY_OPERATOR: {
			ExprUnaryOperator *unary = static_cast<ExprUnaryOperator *>(*expr);

			flatten(flattenTo, &unary->value);
			flattenTo.add(expr);

			break;
		}
		case ExprFlavor::WHILE: {
			ExprLoop *loop = static_cast<ExprLoop *>(*expr);

			flatten(flattenTo, &loop->whileCondition);
			flattenTo.add(expr);

			if (loop->body)
				flatten(flattenTo, &loop->body);

			if (loop->completedBody)
				flatten(flattenTo, &loop->completedBody);

			break;
		}
		case ExprFlavor::ENUM_INCREMENT: {
			flattenTo.add(expr);
			break;
		}
		case ExprFlavor::ENUM: {
			auto enum_ = static_cast<ExprEnum *>(*expr);

			flatten(flattenTo, &enum_->integerType);

			flattenTo.add(expr);
			break;
		}
		default:
			assert(false);
	}
}

Type *getDeclarationType(Declaration *declaration) {
	assert(declaration->flags & DECLARATION_TYPE_IS_READY);
	assert(declaration->type);
	assert(declaration->type->flavor == ExprFlavor::TYPE_LITERAL);

	return static_cast<ExprLiteral *>(declaration->type)->typeValue;
}

void trySolidifyNumericLiteralToDefault(Expr *expr) {
	if (expr->type == &TYPE_UNSIGNED_INT_LITERAL) {
		assert(expr->type == &TYPE_UNSIGNED_INT_LITERAL);
		assert(expr->flavor == ExprFlavor::INT_LITERAL);

		auto literal = static_cast<ExprLiteral *>(expr);

		if (literal->unsignedValue > static_cast<u64>(std::numeric_limits<s64>::max())) {
			expr->type = &TYPE_U64;
		}
		else {
			expr->type = &TYPE_S64;
		}
	}
	else if (expr->type == &TYPE_SIGNED_INT_LITERAL) {
		assert(expr->flavor == ExprFlavor::INT_LITERAL);
		expr->type = &TYPE_S64;
	}
	else if (expr->type == &TYPE_FLOAT_LITERAL) {
		assert(expr->flavor == ExprFlavor::FLOAT_LITERAL);
		expr->type = &TYPE_F64;
	}
}

// @Incomplete more specific handling so we can print the bounds that were violated
bool boundsCheckImplicitConversion(Expr *location, Type *convertTo, ExprLiteral *convertFrom) {
	if (convertFrom->type->flags & TYPE_INTEGER_IS_SIGNED) {
		if (convertTo == &TYPE_U64) {
			if (convertFrom->signedValue < 0) {
				reportError(location, "Error: Integer literal too small for u64 (min: 0, given: %" PRIi64 ")", convertFrom->signedValue);
				return false;
			}
			return true;
		}
		else if (convertTo == &TYPE_U32) {
			if (convertFrom->signedValue < 0) {
				reportError(location, "Error: Integer literal too small for u32 (min: 0, given: %" PRIi64 ")", convertFrom->signedValue);
				return false;
			}
			if (convertFrom->signedValue > static_cast<s64>(UINT32_MAX)) {
				reportError(location, "Error: Integer literal too large for u32 (max: %" PRIu32 ", given: %" PRIu64 ")", UINT32_MAX, convertFrom->unsignedValue);

				return false;
			}

			return true;
		}
		else if (convertTo == &TYPE_U16) {
			if (convertFrom->signedValue < 0) {
				reportError(location, "Error: Integer literal too small for u16 (min: 0, given: %" PRIi64 ")", convertFrom->signedValue);
				return false;
			}
			if (convertFrom->unsignedValue > static_cast<s64>(UINT16_MAX)) {
				reportError(location, "Error: Integer literal too large for u16 (max: %" PRIu16 ", given: %" PRIu64 ")", UINT16_MAX, convertFrom->unsignedValue);

				return false;
			}

			return true;
		}
		else if (convertTo == &TYPE_U8) {
			if (convertFrom->signedValue < 0) {
				reportError(location, "Error: Integer literal too small for u8 (min: 0, given: %" PRIi64 ")", convertFrom->signedValue);
				return false;
			}
			if (convertFrom->unsignedValue > static_cast<s64>(UINT8_MAX)) {
				reportError(location, "Error: Integer literal too large for u8 (max: %" PRIu8 ", given: %" PRIu64 ")", UINT16_MAX, convertFrom->unsignedValue);

				return false;
			}

			return true;
		}
		if (convertTo == &TYPE_S64) {
			return true;
		}
		else if (convertTo == &TYPE_S32) {
			if (convertFrom->signedValue > INT32_MAX) {
				reportError(location, "Error: Integer literal too large for s32 (max: %" PRIi32 ", given: %" PRIi64 ")", INT32_MAX, convertFrom->signedValue);

				return false;
			}
			else if (convertFrom->signedValue < INT32_MIN) {
				reportError(location, "Error: Integer literal too small for s32 (min: %" PRIi32 ", given: %" PRIi64 ")", INT32_MIN, convertFrom->signedValue);

				return false;
			}

			return true;
		}
		else if (convertTo == &TYPE_S16) {
			if (convertFrom->signedValue > INT16_MAX) {
				reportError(location, "Error: Integer literal too large for s16 (max: %" PRIi16 ", given: %" PRIi64 ")", INT16_MAX, convertFrom->signedValue);

				return false;
			}
			else if (convertFrom->signedValue < INT16_MIN) {
				reportError(location, "Error: Integer literal too small for s16 (min: %" PRIi16 ", given: %" PRIi64 ")", INT16_MIN, convertFrom->signedValue);

				return false;
			}

			return true;
		}
		else if (convertTo == &TYPE_S8) {
			if (convertFrom->signedValue > INT8_MAX) {
				reportError(location, "Error: Integer literal too large for s8 (max: %" PRIi8 ", given: %" PRIi64 ")", INT8_MAX, convertFrom->signedValue);

				return false;
			}
			else if (convertFrom->signedValue < INT16_MIN) {
				reportError(location, "Error: Integer literal too small for s8 (min: %" PRIi8 ", given: %" PRIi64 ")", INT8_MIN, convertFrom->signedValue);

				return false;
			}

			return true;
		}
		else {
			assert(false);
			return false;
		}
	}
	else {
		if (convertTo == &TYPE_U64) {
			return true;
		}
		else if (convertTo == &TYPE_U32) {
			if (convertFrom->unsignedValue > UINT32_MAX) {
				reportError(location, "Error: Integer literal too large for u32 (max: %" PRIu32 ", given: %" PRIu64 ")", UINT32_MAX, convertFrom->unsignedValue);

				return false;
			}

			return true;
		}
		else if (convertTo == &TYPE_U16) {
			if (convertFrom->unsignedValue > UINT16_MAX) {
				reportError(location, "Error: Integer literal too large for u16 (max: %" PRIu16 ", given: %" PRIu64 ")", UINT16_MAX, convertFrom->unsignedValue);

				return false;
			}

			return true;
		}
		else if (convertTo == &TYPE_U8) {
			if (convertFrom->unsignedValue > UINT8_MAX) {
				reportError(location, "Error: Integer literal too large for u8 (max: %" PRIu8 ", given: %" PRIu64 ")", UINT16_MAX, convertFrom->unsignedValue);

				return false;
			}

			return true;
		}
		else if (convertTo == &TYPE_S64) {
			if (convertFrom->unsignedValue > static_cast<u64>(INT64_MAX)) {
				reportError(location, "Error: Integer literal too large for s64 (max: %" PRIi64 ", given: %" PRIu64 ")", INT64_MAX, convertFrom->unsignedValue);

				return false;
			}

			return true;
		}
		else if (convertTo == &TYPE_S32) {
			if (convertFrom->unsignedValue > static_cast<u64>(INT32_MAX)) {
				reportError(location, "Error: Integer literal too large for s32 (max: %" PRIi32 ", given: %" PRIu64 ")", INT32_MAX, convertFrom->unsignedValue);

				return false;
			}

			return true;
		}
		else if (convertTo == &TYPE_S16) {
			if (convertFrom->unsignedValue > static_cast<u64>(INT16_MAX)) {
				reportError(location, "Error: Integer literal too large for s16 (max: %" PRIi16 ", given: %" PRIu64 ")", INT16_MAX, convertFrom->unsignedValue);

				return false;
			}

			return true;
		}
		else if (convertTo == &TYPE_S8) {
			if (convertFrom->unsignedValue > static_cast<u64>(INT8_MAX)) {
				reportError(location, "Error: Integer literal too large for s8 (max: %" PRIi8 ", given: %" PRIu64 ")", INT8_MAX, convertFrom->unsignedValue);

				return false;
			}

			return true;
		}
		else {
			assert(false);
			return false;
		}
	}
}


bool solidifyOneLiteral(ExprBinaryOperator *binary) {
	auto left = binary->left;
	auto right = binary->right;

	if (left->type == &TYPE_FLOAT_LITERAL) {
		assert(right->type->flavor == TypeFlavor::FLOAT);

		left->type = right->type;
	}
	else if (left->type == &TYPE_UNSIGNED_INT_LITERAL) {
		assert(right->type->flavor == TypeFlavor::INTEGER || right->type->flavor == TypeFlavor::FLOAT);

		if (right->type->flavor == TypeFlavor::INTEGER) {
			if (!boundsCheckImplicitConversion(binary, right->type, static_cast<ExprLiteral *>(left))) {
				return false;
			}
		}
		else {
			assert(right->type->flavor == TypeFlavor::FLOAT);


			auto literal = static_cast<ExprLiteral *>(left);

			if (right->type->size == 4) {
				if (literal->unsignedValue > 1ULL << (FLT_MANT_DIG + 1ULL)) {
					reportError(right, "Error: Cannot implicitly convert %" PRIu64 " to f32, precision would be lost. The maximum exact integer for an f32 is %" PRIu64, literal->unsignedValue, 1ULL << (FLT_MANT_DIG + 1ULL));
					return false;
				}
			}
			else if (right->type->size == 8) {
				if (literal->unsignedValue > 1ULL << (DBL_MANT_DIG + 1ULL)) {
					reportError(right, "Error: Cannot implicitly convert %" PRIu64 " to f64, precision would be lost. The maximum exact integer for an f64 is %" PRIu64, literal->unsignedValue, 1ULL << (DBL_MANT_DIG + 1ULL));
					return false;
				}
			}

			literal->flavor = ExprFlavor::FLOAT_LITERAL;
			literal->floatValue = static_cast<double>(literal->unsignedValue);
		}

		left->type = right->type;
	}
	else if (left->type == &TYPE_SIGNED_INT_LITERAL) {
		assert(right->type->flavor == TypeFlavor::INTEGER);


		if (right->type->flavor == TypeFlavor::INTEGER) {
			if (!(right->flags & TYPE_INTEGER_IS_SIGNED)) {
				reportError(binary, "Error: Signed-unsigned mismatch, cannot convert s64 to %.*s", STRING_PRINTF(right->type->name));
				return false;
			}

			if (!boundsCheckImplicitConversion(binary, right->type, static_cast<ExprLiteral *>(left))) {
				return false;
			}
		}
		else {
			assert(right->type->flavor == TypeFlavor::FLOAT);

			auto literal = static_cast<ExprLiteral *>(left);

			if (right->type->size == 4) {
				if (std::abs(literal->signedValue) > 1LL << (FLT_MANT_DIG + 1LL)) {
					reportError(right, "Error: Cannot implicitly convert %" PRIi64 " to f32, precision would be lost. The maximum exact integer for an f32 is %" PRIi64, literal->signedValue, 1LL << (FLT_MANT_DIG + 1LL));
					return false;
				}
			}
			else if (right->type->size == 8) {
				if (std::abs(literal->signedValue) > 1LL << (DBL_MANT_DIG + 1LL)) {
					reportError(right, "Error: Cannot implicitly convert %" PRIi64 " to f64, precision would be lost. The maximum exact integer for an f64 is %" PRIi64, literal->signedValue, 1LL << (DBL_MANT_DIG + 1LL));
					return false;
				}
			}

			literal->flavor = ExprFlavor::FLOAT_LITERAL;
			literal->floatValue = static_cast<double>(literal->signedValue);
		}

		left->type = right->type;
	}
	else if (right->type == &TYPE_FLOAT_LITERAL) {
		assert(left->type->flavor == TypeFlavor::FLOAT);

		right->type = left->type;
	}
	else if (right->type == &TYPE_UNSIGNED_INT_LITERAL) {
		assert(left->type->flavor == TypeFlavor::INTEGER || left->type->flavor == TypeFlavor::FLOAT);

		if (left->type->flavor == TypeFlavor::INTEGER) {
			if (!boundsCheckImplicitConversion(binary, left->type, static_cast<ExprLiteral *>(right))) {
				return false;
			}
		}
		else {
			assert(left->type->flavor == TypeFlavor::FLOAT);

			auto literal = static_cast<ExprLiteral *>(right);

			if (left->type->size == 4) {
				if (literal->unsignedValue > 1ULL << (FLT_MANT_DIG + 1ULL)) {
					reportError(left, "Error: Cannot implicitly convert %" PRIu64 " to f32, precision would be lost. The maximum exact integer for an f32 is %" PRIu64, literal->unsignedValue, 1ULL << (FLT_MANT_DIG + 1ULL));
					return false;
				}
			}
			else if (left->type->size == 8) {
				if (literal->unsignedValue > 1ULL << (DBL_MANT_DIG + 1ULL)) {
					reportError(left, "Error: Cannot implicitly convert %" PRIu64 " to f64, precision would be lost. The maximum exact integer for an f64 is %" PRIu64, literal->unsignedValue, 1ULL << (DBL_MANT_DIG + 1ULL));
					return false;
				}
			}

			literal->flavor = ExprFlavor::FLOAT_LITERAL;
			literal->floatValue = static_cast<double>(literal->unsignedValue);
		}

		right->type = left->type;
	}
	else if (right->type == &TYPE_SIGNED_INT_LITERAL) {
		assert(left->type->flavor == TypeFlavor::INTEGER);


		if (left->type->flavor == TypeFlavor::INTEGER) {
			if (!(left->flags & TYPE_INTEGER_IS_SIGNED)) {
				reportError(binary, "Error: Signed-unsigned mismatch, cannot convert %.*s to s64", STRING_PRINTF(left->type->name));
				return false;
			}

			if (!boundsCheckImplicitConversion(binary, left->type, static_cast<ExprLiteral *>(right))) {
				return false;
			}
		}
		else {
			assert(left->type->flavor == TypeFlavor::FLOAT);

			auto literal = static_cast<ExprLiteral *>(right);

			if (left->type->size == 4) {
				if (std::abs(literal->signedValue) > 1LL << (FLT_MANT_DIG + 1LL)) {
					reportError(left, "Error: Cannot implicitly convert %" PRIi64 " to f32, precision would be lost. The maximum exact integer for an f32 is %" PRIi64, literal->signedValue, 1LL << (FLT_MANT_DIG + 1LL));
					return false;
				}
			}
			else if (left->type->size == 8) {
				if (std::abs(literal->signedValue) > 1LL << (DBL_MANT_DIG + 1LL)) {
					reportError(left, "Error: Cannot implicitly convert %" PRIi64 " to f64, precision would be lost. The maximum exact integer for an f64 is %" PRIi64, literal->signedValue, 1LL << (DBL_MANT_DIG + 1LL));
					return false;
				}
			}

			literal->flavor = ExprFlavor::FLOAT_LITERAL;
			literal->floatValue = static_cast<double>(literal->signedValue);
		}

		right->type = left->type;
	}

	return true;
}


InferJob *allocateJob();

bool isValidCast(Type *to, Type *from) {
	if (to->flavor == from->flavor) {
		if (from->flavor == TypeFlavor::AUTO_CAST || from->flavor == TypeFlavor::VOID || from->flavor == TypeFlavor::NAMESPACE) {
			return false;
		}

		if (to->flavor == TypeFlavor::ARRAY) {
			if (to->flags & (TYPE_ARRAY_IS_FIXED | TYPE_ARRAY_IS_DYNAMIC)) {
				return to == from;
			}
			else {
				auto toArray = static_cast<TypeArray *>(to);
				auto fromArray = static_cast<TypeArray *>(from);

				return toArray->arrayOf == fromArray->arrayOf;
			}
		}

		if (to->flavor == TypeFlavor::STRUCT) {
			return to == from;
		}

		return true;
	}

	if (from->flavor == TypeFlavor::BOOL) {
		return to->flavor == TypeFlavor::INTEGER;
	}
	else if (from->flavor == TypeFlavor::AUTO_CAST || from->flavor == TypeFlavor::TYPE || from->flavor == TypeFlavor::VOID || from->flavor == TypeFlavor::STRUCT || from->flavor == TypeFlavor::NAMESPACE) {
		return false;
	}
	else if (from->flavor == TypeFlavor::FLOAT) {
		return to->flavor == TypeFlavor::INTEGER;
	}
	else if (from->flavor == TypeFlavor::ENUM) {
		return to->flavor == TypeFlavor::INTEGER || to->flavor == TypeFlavor::BOOL;
	}
	else if (from->flavor == TypeFlavor::FUNCTION) {
		return to->flavor == TypeFlavor::BOOL || (to->flavor == TypeFlavor::INTEGER && to->size == 8) || to == TYPE_VOID_POINTER;
	}
	else if (from->flavor == TypeFlavor::INTEGER) {
		return to->flavor == TypeFlavor::ENUM || to->flavor == TypeFlavor::BOOL || to->flavor == TypeFlavor::FLOAT || 
			(from->size == 8 && (to->flavor == TypeFlavor::FUNCTION || to->flavor == TypeFlavor::POINTER));
	}
	else if (from->flavor == TypeFlavor::POINTER) {
		return to->flavor == TypeFlavor::BOOL || (to->flavor == TypeFlavor::INTEGER && to->size == 8) ||
			(from == getPointer(&TYPE_U8) && to->flavor == TypeFlavor::STRING) || (from == TYPE_VOID_POINTER && to->flavor == TypeFlavor::FUNCTION);
	}
	else if (from->flavor == TypeFlavor::STRING) {
		// @StringFormat when strings change to be equivalent to [] u8, stop casting to *u8
		return to->flavor == TypeFlavor::BOOL || to == getPointer(&TYPE_U8) || to == TYPE_VOID_POINTER;
	}
	else if (from->flavor == TypeFlavor::ARRAY) {
		// Casts between array types are handled above
		return to->flavor == TypeFlavor::BOOL || (to->flavor == TypeFlavor::POINTER &&
			(static_cast<TypePointer *>(to)->pointerTo == static_cast<TypeArray *>(from)->arrayOf || to == TYPE_VOID_POINTER));
	}
	else {
		assert(false); // Invalid code path
		return false;
	}
}

u64 convertToUnsigned(u64 value, u64 size) {
	u64 msb = (1ULL << (size * 8 - 1));
	u64 mask = msb | (msb - 1);

	return value & mask;
}

u64 convertToSigned(u64 value, u64 size) {
	u64 msb = (1ULL << (size * 8 - 1));
	u64 mask = msb | (msb - 1);

	value &= mask;

	if (value & msb) { // The value is a negative number
		value |= ~mask; // Sign extend
	}

	return value;
}

void addSizeDependency(InferJob *job, Type *type) {
	if (job && type->size == 0 && !(type->flags & TYPE_IS_INTERNAL)) {
		job->sizeDependencies.add(type);
	}
}

void checkedRemoveLastSizeDependency(InferJob *job, Type *type) {
	if (job && type->size == 0 && !(type->flags & TYPE_IS_INTERNAL)) {
		if (job->sizeDependencies.pop() != type) {
			assert(false);
		}
	}
}

void doConstantCast(Expr **cast) {
	auto binary = static_cast<ExprBinaryOperator *>(*cast);
	auto castTo = static_cast<ExprLiteral *>(binary->left)->typeValue;
	auto expr = binary->right;

	if (expr->type == castTo) {
		*cast = expr;
	}

	if (expr->flavor == ExprFlavor::INT_LITERAL) {
		auto old = static_cast<ExprLiteral *>(expr);


		if (castTo->flavor == TypeFlavor::INTEGER || castTo->flavor == TypeFlavor::ENUM) {
			auto newLiteral = new ExprLiteral;
			newLiteral->start = binary->start;
			newLiteral->end = old->end;
			newLiteral->valueOfDeclaration = old->valueOfDeclaration;
			newLiteral->flavor = ExprFlavor::INT_LITERAL;

			newLiteral->type = castTo;
			newLiteral->unsignedValue = (castTo->flags & TYPE_INTEGER_IS_SIGNED) ?
				convertToSigned(old->unsignedValue, castTo->size) : convertToUnsigned(old->unsignedValue, castTo->size);

			*cast = newLiteral;
		}
		else if (castTo->flavor == TypeFlavor::FLOAT) {
			auto newLiteral = new ExprLiteral;
			newLiteral->start = binary->start;
			newLiteral->end = old->end;
			newLiteral->valueOfDeclaration = old->valueOfDeclaration;
			newLiteral->flavor = ExprFlavor::FLOAT_LITERAL;

			newLiteral->type = castTo;
			newLiteral->floatValue = (old->type->flags & TYPE_INTEGER_IS_SIGNED) ?
				static_cast<double>(old->signedValue) : static_cast<double>(old->unsignedValue);

			*cast = newLiteral;
		}
		else if (castTo->flavor == TypeFlavor::BOOL) {
			auto newLiteral = new ExprLiteral;
			newLiteral->start = binary->start;
			newLiteral->end = old->end;
			newLiteral->valueOfDeclaration = old->valueOfDeclaration;
			newLiteral->flavor = ExprFlavor::INT_LITERAL;

			newLiteral->type = castTo;
			newLiteral->unsignedValue = old->unsignedValue == 0 ? 0 : 1;

			*cast = newLiteral;
		}
		else if (castTo->flavor == TypeFlavor::POINTER) {
			auto newLiteral = new ExprLiteral;
			newLiteral->start = binary->start;
			newLiteral->end = old->end;
			newLiteral->valueOfDeclaration = old->valueOfDeclaration;
			newLiteral->flavor = ExprFlavor::INT_LITERAL;

			newLiteral->type = castTo;
			newLiteral->unsignedValue = old->unsignedValue;

			*cast = newLiteral;
		}
	}
	else if (expr->flavor == ExprFlavor::FLOAT_LITERAL) {
		auto old = static_cast<ExprLiteral *>(expr);


		if (castTo->flavor == TypeFlavor::INTEGER) {
			auto newLiteral = new ExprLiteral;
			newLiteral->start = binary->start;
			newLiteral->end = old->end;
			newLiteral->valueOfDeclaration = old->valueOfDeclaration;
			newLiteral->flavor = ExprFlavor::INT_LITERAL;

			newLiteral->type = castTo;

			if (castTo->flags & TYPE_INTEGER_IS_SIGNED) {
				newLiteral->unsignedValue = static_cast<u64>(old->floatValue);
			}
			else {
				newLiteral->unsignedValue = static_cast<u64>(old->floatValue);
			}
			*cast = newLiteral;
		}
		else if (castTo->flavor == TypeFlavor::FLOAT) {
			auto newLiteral = new ExprLiteral;
			newLiteral->start = binary->start;
			newLiteral->end = old->end;
			newLiteral->valueOfDeclaration = old->valueOfDeclaration;
			newLiteral->flavor = ExprFlavor::FLOAT_LITERAL;

			newLiteral->type = castTo;
			newLiteral->floatValue = old->floatValue;

			*cast = newLiteral;
		}
	}
}

void insertImplicitCast(InferJob *job, Expr **castFrom, Type *castTo) {
	ExprBinaryOperator *cast = new ExprBinaryOperator;
	cast->flavor = ExprFlavor::BINARY_OPERATOR;
	cast->op = TokenT::CAST;
	cast->type = castTo;
	cast->right = *castFrom;
	cast->start = cast->right->start;
	cast->end = cast->right->end;
	cast->flags |= EXPR_CAST_IS_IMPLICIT;
	cast->left = inferMakeTypeLiteral(cast->right->start, cast->right->end, castTo);

	cast->valueOfDeclaration = cast->right->valueOfDeclaration;

	*castFrom = cast;

	addSizeDependency(job, castTo);

	assert(isValidCast(castTo, cast->right->type));

	doConstantCast(castFrom);
}

// Passing null for location means that an error shouldn't be reported, and null should be returned if the expression doesn't have a namespace
TypeStruct *getExpressionNamespace(Expr *expr, bool *onlyConstants, Expr *location) {
	if (expr->type->flavor == TypeFlavor::STRUCT || expr->type->flavor == TypeFlavor::ARRAY) {
		*onlyConstants = false;
		return static_cast<TypeStruct *>(expr->type);
	}
	else if (expr->type->flavor == TypeFlavor::POINTER) {
		auto type = static_cast<TypePointer *>(expr->type)->pointerTo;

		if (type->flavor == TypeFlavor::STRUCT || type->flavor == TypeFlavor::ARRAY) {
			*onlyConstants = false;
			return static_cast<TypeStruct *>(type);
		}
		else {
			if (location)
				reportError(location, "Error: A %.*s does not have fields", STRING_PRINTF(expr->type->name));
			return nullptr;
		}
	}
	else if (expr->type->flavor == TypeFlavor::TYPE) {
		if (expr->flavor != ExprFlavor::TYPE_LITERAL) {
			if (location)
				reportError(location, "Error: Cannot access the fields of a non-constant type");
			return nullptr;
		}

		auto type = static_cast<ExprLiteral *>(expr)->typeValue;

		if (type->flavor == TypeFlavor::STRUCT || type->flavor == TypeFlavor::ARRAY || type->flavor == TypeFlavor::NAMESPACE || type->flavor == TypeFlavor::ENUM) {
			*onlyConstants = true;
			return static_cast<TypeStruct *>(type);
		}
		else {
			if (location)
				reportError(location, "Error: A %.*s prototype does not have fields", STRING_PRINTF(type->name));
			return nullptr;
		}
	}
	else {
		if (location)
			reportError(location, "Error: A %.*s does not have fields", STRING_PRINTF(expr->type->name));
		return nullptr;
	}
}

void copyLiteral(Expr **exprPointer, Expr *expr) {
	switch (expr->flavor) {
	case ExprFlavor::FUNCTION: // Functions are unique
	case ExprFlavor::STRUCT_DEFAULT: // Struct defaults have no mutable data
	case ExprFlavor::STRING_LITERAL: // Don't duplicate string literals this will bloat the binary
	case ExprFlavor::ARRAY: { // Don't duplicate array literals this will bloat the binary
		*exprPointer = expr;
		break;
	}
	case ExprFlavor::FLOAT_LITERAL:
	case ExprFlavor::TYPE_LITERAL:
	case ExprFlavor::INT_LITERAL: {
		ExprLiteral *newLiteral = new ExprLiteral;
		*newLiteral = *static_cast<ExprLiteral *>(expr);
		newLiteral->start = (*exprPointer)->start;
		newLiteral->end = (*exprPointer)->end;

		*exprPointer = newLiteral;

		break;
	}
	}
}


Expr *pushStructAccessDown(ExprIdentifier *accesses, Expr *base, CodeLocation start, EndLocation end) {
	if (!accesses) {
		return base;
	}

	auto result = new ExprIdentifier;
	auto current = result;

	while (true) {
		*current = *accesses;
		current->start = start;
		current->end = end;

		if (current->structAccess) {
			current->structAccess = new ExprIdentifier;
			current = static_cast<ExprIdentifier *>(current->structAccess);

			assert(accesses->structAccess->flavor == ExprFlavor::IDENTIFIER);
			accesses = static_cast<ExprIdentifier *>(accesses->structAccess);
		}
		else {
			current->structAccess = base;
			return result;
		}
	}
}

bool inferIdentifier(InferJob *job, Expr **exprPointer, ExprIdentifier *identifier, bool *yield) {
	*yield = false;

	if ((identifier->declaration->flags & DECLARATION_IMPORTED_BY_USING) && !(identifier->declaration->flags & DECLARATION_IS_CONSTANT)) {
		auto current = static_cast<ExprIdentifier *>(identifier->declaration->initialValue);

		if (job) {
			while (current->structAccess) {
				bool onlyConstants;
				addSizeDependency(job, getExpressionNamespace(current->structAccess, &onlyConstants, current->structAccess));

				current = static_cast<ExprIdentifier *>(current->structAccess);
				assert(current->flavor == ExprFlavor::IDENTIFIER);
			}
		}

		identifier->structAccess = pushStructAccessDown(static_cast<ExprIdentifier *>(identifier->declaration->initialValue), identifier->structAccess, identifier->start, identifier->end);
		identifier->declaration = identifier->declaration->import;
	}



	if (identifier->flags & EXPR_IDENTIFIER_IS_BREAK_OR_CONTINUE_LABEL) {
		// We shouldn't bother resolving types they aren't needed
		// Replacing a constant would mean we have to check if the label
		// is still an identifier when we infer break/continue and if we did that it would be a bad error message
		// so we are done

	}
	else {

		if (identifier->declaration->flags & DECLARATION_IS_ITERATOR) {
			auto loop = CAST_FROM_SUBSTRUCT(ExprLoop, iteratorBlock, identifier->declaration->enclosingScope);
			assert(loop->flavor == ExprFlavor::WHILE || loop->flavor == ExprFlavor::FOR);

			if (loop->flavor == ExprFlavor::WHILE) {
				reportError(identifier, "Error: Cannot use while loop label as a variable, it has no storage");
				reportError(identifier->declaration, "   ..: Here is the location of the loop");
				return false;
			}
		}

		if (identifier->declaration->flags & DECLARATION_TYPE_IS_READY) {
			if (!identifier->type) {
				identifier->type = getDeclarationType(identifier->declaration);
				addSizeDependency(job, identifier->type);
			}
		}
		else {
			*yield = true;
			return false;
		}

		if (identifier->declaration->flags & DECLARATION_IS_CONSTANT) {
			if (identifier->declaration->flags & DECLARATION_VALUE_IS_READY) {
				copyLiteral(exprPointer, identifier->declaration->initialValue);
			}
			else {
				*yield = true;
				return false;
			}
		}


	}

	return true;
}

bool tryUsingConversion(InferJob *job, Type *correct, Expr **exprPointer, bool *yield) {
	*yield = false;
	auto given = *exprPointer;

	if (correct == given->type) {
		return true;
	}

	bool onlyConstants = false;
	auto struct_ = getExpressionNamespace(given, &onlyConstants, nullptr);


	if (!struct_)
		return false;

	if (struct_->members.usings.count) {
		*yield = true;
		return false;
	}


	Declaration *found = nullptr;
	for (auto member : struct_->members.declarations) {
		if (member->flags & DECLARATION_MARKED_AS_USING) {
			if (!(member->flags & DECLARATION_TYPE_IS_READY)) {
				*yield = true;
				return false;
			}

			if (getDeclarationType(member) == correct) {
				found = member;
				break;
			}
			else if (given->type->flavor == TypeFlavor::POINTER && !(member->flags & DECLARATION_IS_CONSTANT) && getPointer(getDeclarationType(member)) == correct) {
				found = member;
				break;
			}
		}
	}

	if (!found)
		return false;

	if ((found->flags & DECLARATION_IS_CONSTANT) && !(found->flags & DECLARATION_VALUE_IS_READY)) {
		*yield = true;
		return false;
	}

	auto identifier = new ExprIdentifier;
	identifier->start = given->start;
	identifier->end = given->end;
	identifier->flavor = ExprFlavor::IDENTIFIER;
	identifier->name = found->name;
	identifier->resolveFrom = nullptr;
	identifier->enclosingScope = nullptr;
	identifier->structAccess = given;
	identifier->indexInBlock = 0;
	identifier->declaration = found;

	Expr *expr = identifier;

	if (!inferIdentifier(job, &expr, identifier, yield)) {
		assert(false); // This shouldn't fail
	}

	if (getDeclarationType(found) != correct) {
		assert(given->type->flavor == TypeFlavor::POINTER && !(found->flags & DECLARATION_IS_CONSTANT) && getPointer(getDeclarationType(found)) == correct);

		auto unary = new ExprUnaryOperator;

		unary->start = given->start;
		unary->end = given->end;
		unary->flavor = ExprFlavor::UNARY_OPERATOR;
		unary->op = TOKEN('*');
		unary->value = expr;
		unary->type = correct;

		expr = unary;
	}

	expr->valueOfDeclaration = given->valueOfDeclaration;
	*exprPointer = expr;

	return true;
}
bool tryAutoCast(InferJob *job, Expr **cast, Type *castTo, bool *yield) {
	*yield = false;
	auto castExpr = *cast;
	assert(castExpr->type->flavor == TypeFlavor::AUTO_CAST);
	assert(castExpr->flavor == ExprFlavor::BINARY_OPERATOR);

	auto autoCast = static_cast<ExprBinaryOperator *>(castExpr);
	assert(autoCast->op == TokenT::CAST);

	auto &castFrom = autoCast->right->type;

	assert(!autoCast->left);

	trySolidifyNumericLiteralToDefault(autoCast->right);


	assert(!(castTo->flags & TYPE_IS_INTERNAL));

	autoCast->type = castTo;
	autoCast->left = inferMakeTypeLiteral(autoCast->start, autoCast->end, castTo);

	if (!tryUsingConversion(job, castTo, &autoCast->right, yield)) {
		if (*yield) {
			return false;
		}
	}

	if (!isValidCast(castTo, castFrom)) {
		return false;
	}

	addSizeDependency(job, castTo);
	doConstantCast(cast);

	return true;
}

bool binaryOpForFloat(Expr **exprPointer) {
	auto binary = static_cast<ExprBinaryOperator *>(*exprPointer);
	auto &left = binary->left;
	auto &right = binary->right;

	if (left->type == right->type) {
		if (left->type == &TYPE_FLOAT_LITERAL) {
			ExprLiteral literal;
			literal.flavor = ExprFlavor::FLOAT_LITERAL;
			literal.type = left->type;
			literal.start = left->start;
			literal.end = right->end;
			literal.flags |= EXPR_WAS_EVALUATED_BINARY;

			auto lhs = static_cast<ExprLiteral *>(left);
			auto rhs = static_cast<ExprLiteral *>(right);

			switch (binary->op) {
			case TOKEN('+'): {
				literal.floatValue = lhs->floatValue + rhs->floatValue;
				break;
			}
			case TOKEN('-'): {
				literal.floatValue = lhs->floatValue - rhs->floatValue;
				break;
			}
			case TOKEN('*'): {
				literal.floatValue = lhs->floatValue * rhs->floatValue;
				break;
			}
			case TOKEN('/'): {
				literal.floatValue = lhs->floatValue / rhs->floatValue;
				break;
			}
			case TOKEN('%'): {
				literal.floatValue = fmod(lhs->floatValue, rhs->floatValue);
				break;
			}
			default: {
				trySolidifyNumericLiteralToDefault(left);
				trySolidifyNumericLiteralToDefault(right);

				return true;
			}
			}

			ExprLiteral *allocation = new ExprLiteral;
			*allocation = literal;

			*exprPointer = allocation;
		}
	}
	else {
		if (left->type == &TYPE_FLOAT_LITERAL || right->type == &TYPE_FLOAT_LITERAL) {
			if (!solidifyOneLiteral(binary)) return false;
		}
		else {
			assert((left->type == &TYPE_F32 && right->type == &TYPE_F64) || (left->type == &TYPE_F64 && right->type == &TYPE_F32));

			// @Incomplete should we allow this conversion in some cases, this code was originally taken
			// from == and != where float conversion definitely shouldn't be allowed, since that's alredy
			// bad enough without the compiler converting types behind your back
			reportError(binary, "Error: Cannot convert between %.*s and %.*s", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
			return false;
		}
	}

	return true;
}

bool assignOpForFloat(ExprBinaryOperator *binary) {
	auto &left = binary->left;
	auto &right = binary->right;

	if (left->type == right->type) {
		// We are done
	}
	else {
		if (right->type == &TYPE_FLOAT_LITERAL) {
			if (!solidifyOneLiteral(binary)) return false;
		}
		else {
			assert((left->type == &TYPE_F32 && right->type == &TYPE_F64) || (left->type == &TYPE_F64 && right->type == &TYPE_F32));

			// @Incomplete should we allow this conversion in some cases, this code was originally taken
			// from == and != where float conversion definitely shouldn't be allowed, since that's alredy
			// bad enough without the compiler converting types behind your back
			reportError(binary, "Error: Cannot convert from %.*s to %.*s", STRING_PRINTF(right->type->name), STRING_PRINTF(left->type->name));
			return false;
		}
	}

	return true;
}

bool binaryOpForInteger(InferJob *job, Expr **exprPointer) {
	auto binary = static_cast<ExprBinaryOperator *>(*exprPointer);
	auto &left = binary->left;
	auto &right = binary->right;

	if (left->type == &TYPE_UNSIGNED_INT_LITERAL && right->type == &TYPE_SIGNED_INT_LITERAL) {
		if (!boundsCheckImplicitConversion(binary, &TYPE_S64, static_cast<ExprLiteral *>(left))) {
			return false;
		}
		
		left->type = &TYPE_SIGNED_INT_LITERAL;
	}

	if (left->type == &TYPE_SIGNED_INT_LITERAL && right->type == &TYPE_UNSIGNED_INT_LITERAL) {
		if (!boundsCheckImplicitConversion(binary, &TYPE_S64, static_cast<ExprLiteral *>(right))) {
			return false;
		}

		right->type = &TYPE_SIGNED_INT_LITERAL;
	}

	if (left->type == right->type) {
		if (left->type == &TYPE_UNSIGNED_INT_LITERAL || left->type == &TYPE_SIGNED_INT_LITERAL) {
			ExprLiteral literal;
			literal.flavor = ExprFlavor::INT_LITERAL;
			literal.type = left->type;
			literal.start = left->start;
			literal.end = right->end;
			literal.flags |= EXPR_WAS_EVALUATED_BINARY;

			auto lhs = static_cast<ExprLiteral *>(left);
			auto rhs = static_cast<ExprLiteral *>(right);

			switch (binary->op) {
			case TOKEN('+'): {
				literal.unsignedValue = lhs->unsignedValue + rhs->unsignedValue; // Independent of signed
				break;
			}
			case TOKEN('-'): {
				if (lhs->unsignedValue < rhs->unsignedValue) {
					literal.type = &TYPE_SIGNED_INT_LITERAL; // If you typed 3 - 5 you probably mean -2, not (S64_MAX - 1)
				}

				literal.unsignedValue = lhs->unsignedValue - rhs->unsignedValue; // Independent of signed
				break;
			}
			case TOKEN('*'): {
				literal.unsignedValue = lhs->unsignedValue * rhs->unsignedValue; // Independent of signed
				break;
			}
			case TOKEN('/'): {
				if (left->type == &TYPE_UNSIGNED_INT_LITERAL) {
					literal.unsignedValue = lhs->unsignedValue / rhs->unsignedValue;
				}
				else {
					literal.signedValue = lhs->signedValue / rhs->signedValue;
				}

				break;
			}
			case TOKEN('%'): {
				if (left->type == &TYPE_UNSIGNED_INT_LITERAL) {
					literal.unsignedValue = lhs->unsignedValue % rhs->unsignedValue;
				}
				else {
					literal.signedValue = lhs->signedValue % rhs->signedValue;
				}

				break;
			}
			case TOKEN('&'): {
				literal.unsignedValue = lhs->unsignedValue & rhs->unsignedValue; // Independent of signed
				break;
			}
			case TOKEN('|'): {
				literal.unsignedValue = lhs->unsignedValue | rhs->unsignedValue; // Independent of signed
				break;
			}
			case TOKEN('^'): {
				literal.unsignedValue = lhs->unsignedValue ^ rhs->unsignedValue; // Independent of signed
				break;
			}
			case TokenT::SHIFT_LEFT: {
				literal.unsignedValue = lhs->unsignedValue << rhs->unsignedValue; // Independent of signed
				break;
			}
			case TokenT::SHIFT_RIGHT: {
				if (left->type == &TYPE_UNSIGNED_INT_LITERAL) {
					literal.unsignedValue = lhs->unsignedValue >> rhs->unsignedValue;
				}
				else {
					literal.signedValue = lhs->signedValue >> rhs->signedValue;
				}

				break;
			}
			default: {
				trySolidifyNumericLiteralToDefault(left);
				trySolidifyNumericLiteralToDefault(right);

				return true;
			}
			}

			ExprLiteral *allocation = new ExprLiteral;
			*allocation = literal;

			*exprPointer = allocation;
		}
	}
	else {
		if ((left->type->flags & TYPE_INTEGER_IS_SIGNED) == (right->type->flags & TYPE_INTEGER_IS_SIGNED)) {
			if (left->type == &TYPE_UNSIGNED_INT_LITERAL || left->type == &TYPE_SIGNED_INT_LITERAL) {
				if (!boundsCheckImplicitConversion(binary, right->type, static_cast<ExprLiteral *>(left))) {
					return false;
				}

				left->type = right->type;
			}
			else if (right->type == &TYPE_UNSIGNED_INT_LITERAL || right->type == &TYPE_SIGNED_INT_LITERAL) {
				if (!boundsCheckImplicitConversion(binary, left->type, static_cast<ExprLiteral *>(right))) {
					return false;
				}

				right->type = left->type;
			}
			else if (left->type->size > right->type->size) {
				insertImplicitCast(job, &right, left->type);
			}
			else if (right->type->size > left->type->size) {
				insertImplicitCast(job, &left, right->type);
			}
		}
		else {
			if ((left->type == &TYPE_UNSIGNED_INT_LITERAL) && (right->type->flags & TYPE_INTEGER_IS_SIGNED)) {
				if (!boundsCheckImplicitConversion(binary, right->type, static_cast<ExprLiteral *>(left))) {
					return false;
				}

				left->type = right->type;
			}
			else if ((right->type == &TYPE_UNSIGNED_INT_LITERAL) && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
				if (!boundsCheckImplicitConversion(binary, left->type, static_cast<ExprLiteral *>(right))) {
					return false;
				}

				right->type = left->type;
			}
			else {
				reportError(binary, "Error: Signed-unsigned mismatch, cannot convert between %.*s and %.*s", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
				return false;
			}
		}
	}

	return true;
}

bool assignOpForInteger(InferJob *job, ExprBinaryOperator *binary) {
	auto &left = binary->left;
	auto &right = binary->right;

	if (left->type == right->type) {
		if (left->type == &TYPE_UNSIGNED_INT_LITERAL || left->type == &TYPE_SIGNED_INT_LITERAL) {
			trySolidifyNumericLiteralToDefault(left);
			trySolidifyNumericLiteralToDefault(right);
		}
	}
	else {
		if ((left->type->flags & TYPE_INTEGER_IS_SIGNED) == (right->type->flags & TYPE_INTEGER_IS_SIGNED)) {
			if (right->type == &TYPE_UNSIGNED_INT_LITERAL || right->type == &TYPE_SIGNED_INT_LITERAL) {
				if (!boundsCheckImplicitConversion(binary, left->type, static_cast<ExprLiteral *>(right))) {
					return false;
				}

				right->type = left->type;
			}
			else if (left->type->size > right->type->size) {
				insertImplicitCast(job, &right, left->type);
			}
			else if (right->type->size > left->type->size) {
				reportError(binary, "Error: Cannot convert %.*s to %.*s, information could be lost", STRING_PRINTF(right->type->name), STRING_PRINTF(left->type->name));
				return false;
			}
		}
		else {
			if ((right->type == &TYPE_UNSIGNED_INT_LITERAL) && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
				if (!boundsCheckImplicitConversion(binary, left->type, static_cast<ExprLiteral *>(right))) {
					return false;
				}

				right->type = left->type;
			}
			else {
				reportError(binary, "Error: Signed-unsigned mismatch, cannot convert %.*s to %.*s", STRING_PRINTF(right->type->name), STRING_PRINTF(left->type->name));
				return false;
			}
		}
	}

	return true;
}

bool binaryOpForAutoCast(InferJob *job, ExprBinaryOperator *binary, bool *yield) {
	auto &left = binary->left;
	auto &right = binary->right;

	if (left->type->flavor == TypeFlavor::AUTO_CAST) {
		trySolidifyNumericLiteralToDefault(right);

		if (!tryAutoCast(job, &left, right->type, yield)) {
			if (*yield) {
				return false;
			}
			else {
				reportError(binary, "Error: Cannot cast from %.*s to %.*s", STRING_PRINTF(static_cast<ExprBinaryOperator *>(left)->right->type->name), STRING_PRINTF(right->type->name));
				return false;
			}
		}

		return true;
	}
	else if (right->type->flavor == TypeFlavor::AUTO_CAST) {
		trySolidifyNumericLiteralToDefault(left);

		if (!tryAutoCast(job, &right, left->type, yield)) {
			if (*yield) {
				return false;
			}
			else {
				reportError(binary, "Error: Cannot cast from %.*s to %.*s", STRING_PRINTF(static_cast<ExprBinaryOperator *>(right)->right->type->name), STRING_PRINTF(left->type->name));
				return false;
			}
		}

		return true;
	}

	return true;
}

bool assignOpForAutoCast(InferJob *job, ExprBinaryOperator *binary, bool *yield) {
	auto &right = binary->right;

	if (right->type->flavor == TypeFlavor::AUTO_CAST) {
		auto &left = binary->left;

		if (!tryAutoCast(job, &right, left->type, yield)) {
			if (!*yield)
				reportError(binary, "Error: Cannot cast from %.*s to %.*s", STRING_PRINTF(static_cast<ExprBinaryOperator *>(right)->right->type->name), STRING_PRINTF(left->type->name));
			return false;
		}

	}

	return true;
}

bool binaryOpForFloatAndIntLiteral(Expr **exprPointer) {
	auto binary = static_cast<ExprBinaryOperator *>(*exprPointer);
	auto &left = binary->left;
	auto &right = binary->right;

	if (left->type->flavor == TypeFlavor::FLOAT && (right->type == &TYPE_UNSIGNED_INT_LITERAL || right->type == &TYPE_SIGNED_INT_LITERAL)) {
		if (left->type == &TYPE_FLOAT_LITERAL) {
			trySolidifyNumericLiteralToDefault(right);

			if (!solidifyOneLiteral(binary))
				return false;

			left->type = &TYPE_FLOAT_LITERAL;
			right->type = &TYPE_FLOAT_LITERAL;

			binaryOpForFloat(exprPointer);
		}

		if (!solidifyOneLiteral(binary))
			return false;
	}
	else if (right->type->flavor == TypeFlavor::FLOAT && (left->type == &TYPE_UNSIGNED_INT_LITERAL || left->type == &TYPE_SIGNED_INT_LITERAL)) {
		if (right->type == &TYPE_FLOAT_LITERAL) {
			trySolidifyNumericLiteralToDefault(right);

			if (!solidifyOneLiteral(binary))
				return false;

			left->type = &TYPE_FLOAT_LITERAL;
			right->type = &TYPE_FLOAT_LITERAL;

			binaryOpForFloat(exprPointer);
		}

		if (!solidifyOneLiteral(binary))
			return false;
	}

	return true;
}

bool assignOpForFloatAndIntLiteral(ExprBinaryOperator *binary) {
	auto &left = binary->left;
	auto &right = binary->right;

	if (left->type->flavor == TypeFlavor::FLOAT && (right->type == &TYPE_UNSIGNED_INT_LITERAL || right->type == &TYPE_SIGNED_INT_LITERAL)) {
		if (!solidifyOneLiteral(binary))
			return false;
	}

	return true;
}

bool isAssignable(Expr *expr) {
	if ((expr->flavor == ExprFlavor::UNARY_OPERATOR && static_cast<ExprUnaryOperator *>(expr)->op == TokenT::SHIFT_LEFT) ||
		(expr->flavor == ExprFlavor::BINARY_OPERATOR && static_cast<ExprBinaryOperator *>(expr)->op == TOKEN('['))) {
		return true;
	}
	else if (expr->flavor == ExprFlavor::IDENTIFIER) {
		auto access = static_cast<ExprIdentifier *>(expr)->structAccess;

		if (!access) return true;

		if (access->type->flavor == TypeFlavor::ARRAY) {
			return !(access->type->flags & TYPE_ARRAY_IS_FIXED) && isAssignable(access);
		}
		else if (access->type->flavor == TypeFlavor::STRUCT) {
			return isAssignable(access);
		}
		else if (access->type->flavor == TypeFlavor::POINTER) {
			auto pointer = static_cast<TypePointer *>(access->type);

			return !(pointer->pointerTo->flags & TYPE_ARRAY_IS_FIXED);
		}
		else {
			return false;
		}
	}

	return false;
}

bool isLiteral(Expr *expr);

bool arrayIsLiteral(ExprArray *array) {
	for (u64 i = 0; i < array->count; i++) {
		if (!array->storage[i])
			break;

		if (!isLiteral(array->storage[i])) {
			return false;
		}
	}

	return true;
}

bool isLiteral(Expr *expr) {
	return ((expr->flavor == ExprFlavor::INT_LITERAL || expr->flavor == ExprFlavor::FLOAT_LITERAL) && !(expr->flags & EXPR_WAS_EVALUATED_BINARY)) /* See :EvaluatedBinaryLiterals*/
		|| expr->flavor == ExprFlavor::TYPE_LITERAL || expr->flavor == ExprFlavor::STRING_LITERAL || expr->flavor == ExprFlavor::FUNCTION || (expr->flavor == ExprFlavor::ARRAY && arrayIsLiteral(static_cast<ExprArray *>(expr))) || expr->flavor == ExprFlavor::STRUCT_DEFAULT;
}

bool defaultValueIsZero(Type *type, bool *yield) {
	*yield = false;

	switch (type->flavor) {
		case TypeFlavor::BOOL:
		case TypeFlavor::FLOAT:
		case TypeFlavor::FUNCTION:
		case TypeFlavor::INTEGER:
		case TypeFlavor::POINTER: {
			return true;
		}
		case TypeFlavor::STRING: {
			return false;
		}
		case TypeFlavor::ARRAY: {
			if (type->flags & TYPE_ARRAY_IS_FIXED) {
				return defaultValueIsZero(static_cast<TypeArray *>(type)->arrayOf, yield);
			}
			else {
				return true;
			}
		}
		case TypeFlavor::STRUCT: {
			auto struct_ = static_cast<TypeStruct *>(type);

			for (auto member : struct_->members.declarations) {
				if (member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_IMPLICIT_IMPORT | DECLARATION_IS_USING)) continue;

				if (member->flags & DECLARATION_IS_UNINITIALIZED) return false;

				if (!(member->flags & DECLARATION_VALUE_IS_READY)) {
					*yield = true;
					return false;
				}

				assert(member->initialValue);

				if ((member->initialValue->flavor == ExprFlavor::INT_LITERAL || member->initialValue->flavor == ExprFlavor::FLOAT_LITERAL)
					&& static_cast<ExprLiteral *>(member->initialValue)->unsignedValue == 0) { // Check against unsignedValue even for float literals so if the user explicitly 
																							   // sets the default value to -0, they will actually get -0, not 0
				}
				else {
					return false;
				}
			}

			return true;
		}
		case TypeFlavor::ENUM: {
			return type->flags & TYPE_ENUM_IS_FLAGS ? true : false;
		}
		default: {
			return false;
		}
	}
}

Expr *createDefaultValue(Declaration *location, Type *type, bool *shouldYield) {
	*shouldYield = false;
	bool yield;

	if (defaultValueIsZero(type, &yield)) {
		ExprLiteral *zero = new ExprLiteral;
		zero->flavor = ExprFlavor::INT_LITERAL;
		zero->unsignedValue = 0;
		zero->type = type;

		return zero;
	}

	if (yield) {
		*shouldYield = true;
		return nullptr;
	}

	switch (type->flavor) {
		case TypeFlavor::BOOL:
		case TypeFlavor::FLOAT:
		case TypeFlavor::FUNCTION:
		case TypeFlavor::INTEGER:
		case TypeFlavor::POINTER: {
			assert(false); // This should be handled by the defaultValueIsZero check
			return nullptr;
		}
		case TypeFlavor::STRUCT: {
			Expr *literal = new Expr;
			literal->flavor = ExprFlavor::STRUCT_DEFAULT;
			literal->type = type;

			return literal;
		}
		case TypeFlavor::STRING: {
			ExprStringLiteral *empty = new ExprStringLiteral;
			empty->flavor = ExprFlavor::STRING_LITERAL;
			empty->string = "";
			empty->type = type;

			return empty;

		}
		case TypeFlavor::ARRAY: {
			ExprArray *defaults = new ExprArray;
			defaults->flavor = ExprFlavor::ARRAY;
			defaults->type = type;


			if (type->flags & TYPE_ARRAY_IS_FIXED) {
				auto array = static_cast<TypeArray *>(type);
				defaults->count = array->count;


				defaults->storage = new Expr * [array->count];

				bool yield;

				Expr *value = createDefaultValue(location, array->arrayOf, &yield);

				if (yield) {
					*shouldYield = true;
					return nullptr;
				}

				if (!value) {
					return nullptr;
				}

				defaults->storage[0] = value;

				if (array->count > 1) {
					defaults->storage[1] = nullptr; // A value of nullptr signifies all remaining values are the same as the previous
				}
			}

			return defaults;
		}
		case TypeFlavor::TYPE: {
			reportError(location, "Error: There is no default value for a type");
			return nullptr;
		}
		case TypeFlavor::ENUM: {
			// Enum flags will be handled by the defaultIsZero case
			reportError(location, "Error: Enums do not have a default value");
			return nullptr;
		}
		default: {
			assert(false);
			return nullptr;
		}
	}
}

bool assignOp(InferJob *job, Expr *location, Type *correct, Expr *&given, bool *yield) {
	*yield = false;
	if (correct != given->type) {
		if (correct->flavor == given->type->flavor) {
			switch (correct->flavor) {
				case TypeFlavor::ARRAY: {
					if (correct != given->type) {
						if (given->type->flags & (TYPE_ARRAY_IS_DYNAMIC | TYPE_ARRAY_IS_FIXED)) {
							if (!(correct->flags & (TYPE_ARRAY_IS_DYNAMIC | TYPE_ARRAY_IS_FIXED))) { // We are converting from [N]T or [..]T to []T
								if (static_cast<TypeArray *>(given->type)->arrayOf == static_cast<TypeArray *>(correct)->arrayOf) {
									insertImplicitCast(job, &given, correct);
								}
								else {
									reportError(location, "Error: Cannot convert from %.*s to %.*s", STRING_PRINTF(given->type->name), STRING_PRINTF(correct->name));
									return false;
								}
							}
							else {
								reportError(location, "Error: Cannot convert from %.*s to %.*s", STRING_PRINTF(given->type->name), STRING_PRINTF(correct->name));
								return false;
							}
						}
						else {
							reportError(location, "Error: Cannot convert from %.*s to %.*s", STRING_PRINTF(given->type->name), STRING_PRINTF(correct->name));
							return false;
						}
					}
					break;
				}
				case TypeFlavor::BOOL: {
					break;
				}
				case TypeFlavor::STRING: {
					break;
				}
				case TypeFlavor::FLOAT: {
					if (given->type == &TYPE_FLOAT_LITERAL) {
						given->type = correct;
						given->type = correct;
					}
					else if (given->type->size < correct->size) {
						insertImplicitCast(job, &given, correct);
					}
					else {
						// @Incomplete should we allow this conversion in some cases, this code was originally taken
						// from == and != where float conversion definitely shouldn't be allowed, since that's already
						reportError(location, "Error: Cannot convert from %.*s to %.*s", STRING_PRINTF(given->type->name), STRING_PRINTF(correct->name));
						return false;
					}
					break;
				}
				case TypeFlavor::FUNCTION: {
					if (correct != given->type) {
						reportError(location, "Error: Cannot convert from %.*s to %.*s", STRING_PRINTF(given->type->name), STRING_PRINTF(correct->name));
						return false;
					}
				}
				case TypeFlavor::INTEGER: {
					if ((correct->flags & TYPE_INTEGER_IS_SIGNED) == (given->type->flags & TYPE_INTEGER_IS_SIGNED)) {
						if (given->type == &TYPE_UNSIGNED_INT_LITERAL || given->type == &TYPE_SIGNED_INT_LITERAL) {
							if (!boundsCheckImplicitConversion(location, correct, static_cast<ExprLiteral *>(given))) {
								return false;
							}

							given->type = correct;
						}
						else if (correct->size > given->type->size) {
							insertImplicitCast(job, &given, correct);
						}
						else if (correct->size < given->type->size) {
							reportError(location, "Error: Cannot convert from %.*s to %.*s, precision will be lost", STRING_PRINTF(given->type->name), STRING_PRINTF(correct->name));
							return false;
						}
					}
					else {
						if ((given->type == &TYPE_UNSIGNED_INT_LITERAL) && (correct->flags & TYPE_INTEGER_IS_SIGNED)) {
							if (!boundsCheckImplicitConversion(location, correct, static_cast<ExprLiteral *>(given))) {
								return false;
							}

							given->type = correct;
						}
						else {
							reportError(location, "Error: Signed-unsigned mismatch. Cannot convert from %.*s to %.*s", STRING_PRINTF(given->type->name), STRING_PRINTF(correct->name));
							return false;
						}
					}

					break;
				}
				case TypeFlavor::POINTER: {
					if (given->type != correct) {
						auto givenPointer = static_cast<TypePointer *>(given->type)->pointerTo;
						auto correctPointer = static_cast<TypePointer *>(correct)->pointerTo;

						if (correct == TYPE_VOID_POINTER) {
							insertImplicitCast(job, &given, correct);
						}
						else if (given->type == TYPE_VOID_POINTER) {
							insertImplicitCast(job, &given, correct);
						}
						else if (givenPointer->flavor == TypeFlavor::ARRAY && correctPointer->flavor == TypeFlavor::ARRAY && 
							(givenPointer->flags & TYPE_ARRAY_IS_DYNAMIC) && !(correctPointer->flags & (TYPE_ARRAY_IS_FIXED | TYPE_ARRAY_IS_DYNAMIC))) {
							if (static_cast<TypeArray *>(givenPointer)->arrayOf == static_cast<TypeArray *>(correctPointer)->arrayOf) {
								insertImplicitCast(job, &given, correct);
							}
						}
						else {
							if (!tryUsingConversion(job, correct, &given, yield)) {
								if (*yield) {
									return false;
								}
							}
							else {
								addSizeDependency(job, given->type);
							}

							if (correct != given->type) {
								reportError(location, "Error: Cannot convert from %.*s to %.*s", STRING_PRINTF(given->type->name), STRING_PRINTF(correct->name));
								return false;
							}
						}
					}

					break;
				}
				case TypeFlavor::TYPE: {
					// @Incomplete make this work
					assert(false);
					return false;
				}
				case TypeFlavor::STRUCT: {
					if (!tryUsingConversion(job, correct, &given, yield)) {
						if (*yield) {
							return false;
						}
					}
					else {
						addSizeDependency(job, given->type);
					}

					if (correct != given->type) {
						reportError(location, "Error: Cannot convert from %.*s to %.*s", STRING_PRINTF(given->type->name), STRING_PRINTF(correct->name));
						return false;
					}

					break;
				}
				case TypeFlavor::ENUM: {
					if (correct != given->type) {
						reportError(location, "Error: Cannot convert from %.*s to %.*s", STRING_PRINTF(given->type->name), STRING_PRINTF(correct->name));
						return false;
					}
				}
				default:
					assert(false);
			}
		}
		else {
			if (given->type->flavor == TypeFlavor::AUTO_CAST) {
				if (!tryAutoCast(job, &given, correct, yield)) {
					if (!yield)
						reportError(location, "Error: Cannot cast from %.*s to %.*s", STRING_PRINTF(static_cast<ExprBinaryOperator *>(given)->right->type->name), STRING_PRINTF(correct->name));
					return false;
				}
			}
			else {
				if (correct->flavor == TypeFlavor::FLOAT && given->type == &TYPE_UNSIGNED_INT_LITERAL) {
					auto literal = static_cast<ExprLiteral *>(given);

					literal->flavor = ExprFlavor::FLOAT_LITERAL;
					literal->floatValue = static_cast<double>(literal->unsignedValue);
					literal->type = correct;
				}
				else if (correct->flavor == TypeFlavor::INTEGER && given->type->flavor == TypeFlavor::ENUM && given->flavor == ExprFlavor::INT_LITERAL) {
					if (!boundsCheckImplicitConversion(location, correct, static_cast<ExprLiteral *>(given))) {
						return false;
					}

					insertImplicitCast(job, &given, correct);
				}
				else if (correct->flavor == TypeFlavor::ENUM && (correct->flags & TYPE_ENUM_IS_FLAGS) &&
					given->type->flavor == TypeFlavor::INTEGER && given->flavor == ExprFlavor::INT_LITERAL && static_cast<ExprLiteral *>(given)->unsignedValue == 0) {
					insertImplicitCast(job, &given, correct);
				}
				else if (correct->flavor == TypeFlavor::FLOAT && given->type == &TYPE_SIGNED_INT_LITERAL) {
					auto literal = static_cast<ExprLiteral *>(given);

					literal->flavor = ExprFlavor::FLOAT_LITERAL;
					literal->floatValue = static_cast<double>(literal->signedValue);
					literal->type = correct;
				}
				else if ((given->type == TYPE_VOID_POINTER && correct->flavor == TypeFlavor::FUNCTION) || (given->type->flavor == TypeFlavor::FUNCTION && correct == TYPE_VOID_POINTER)) {
					insertImplicitCast(job, &given, correct);
				}

				if (!tryUsingConversion(job, correct, &given, yield)) {
					if (*yield) {
						return false;
					}
				}
				else {
					addSizeDependency(job, given->type);
				}

				if (correct != given->type) {
					reportError(location, "Error: Cannot convert from %.*s to %.*s", STRING_PRINTF(given->type->name), STRING_PRINTF(correct->name));
					return false;
				}
			}
		}
	}

	return true;
}

bool isAddressable(Expr *expr) {
	if (expr->flavor == ExprFlavor::BINARY_OPERATOR) {
		return static_cast<ExprBinaryOperator *>(expr)->op == TOKEN('[');
	}
	else if (expr->flavor == ExprFlavor::IDENTIFIER) {
		auto access = static_cast<ExprIdentifier *>(expr)->structAccess;

		if (!access) return true;

		if (access->type->flavor == TypeFlavor::POINTER) {
			auto pointer = static_cast<TypePointer *>(access->type);
			return !(pointer->pointerTo->flags & TYPE_ARRAY_IS_FIXED);
		}
		else if (access->type->flavor == TypeFlavor::ARRAY) {
			return !(access->type->flags & TYPE_ARRAY_IS_FIXED) && isAddressable(access);
		}
		else {
			return isAddressable(access);
		}
	}
	else {
		return false;
	}
}


bool inferBinary(InferJob *job, Expr **exprPointer, bool *yield) {
	*yield = false;

	auto expr = *exprPointer;
	auto binary = static_cast<ExprBinaryOperator *>(expr);

	auto &left = binary->left;
	auto &right = binary->right;

	if (binary->flags & EXPR_ASSIGN_IS_IMPLICIT_INITIALIZER) {
		auto declaration = static_cast<ExprIdentifier *>(left)->declaration;

		if (!(declaration->flags & DECLARATION_VALUE_IS_READY)) {
			*yield = true;
			return true;
		}
		else {
			binary->right = declaration->initialValue;
			assert(binary->left->type == binary->right->type);
			return true;
		}
	}

	assert(binary->op == TokenT::ARRAY_TYPE || left); // This is safe since even though auto-casts can have a left of null, they shouldn't be added to the flattened array


	assert(right);

	if (left && right && left->type->flavor == right->type->flavor && left->type->flavor == TypeFlavor::AUTO_CAST) {
		reportError(binary, "Error: Cannot infer the type of an expression when both sides are an auto cast");
		return false;
	}

	if (left && left->type->flavor == TypeFlavor::VOID) {
		reportError(left, "Error: Cannot operate on a value of type void");
		return false;
	}


	if (right && right->type->flavor == TypeFlavor::VOID) {
		reportError(right, "Error: Cannot operate on a value of type void");
		return false;
	}

	if (left && left->type->flavor == TypeFlavor::NAMESPACE) {
		reportError(left, "Error: Cannot operate on a namespace");
		return false;
	}


	if (right && right->type->flavor == TypeFlavor::NAMESPACE) {
		reportError(right, "Error: Cannot operate on a namespace");
		return false;
	}

	switch (binary->op) {
		case TokenT::CAST: {
			if (left->type != &TYPE_TYPE) {
				reportError(left, "Error: Cast target must be a type");
				return false;
			}
			else if (left->flavor != ExprFlavor::TYPE_LITERAL) {
				reportError(left, "Error: Cast target must be a constant");
				return false;
			}


			assert(left->flavor == ExprFlavor::TYPE_LITERAL);

			Type *castTo = static_cast<ExprLiteral *>(left)->typeValue;
			trySolidifyNumericLiteralToDefault(right);

			if (!tryUsingConversion(job, castTo, &right, yield)) {
				if (*yield) {
					return false;
				}

				if (!isValidCast(castTo, right->type)) {
					reportError(binary, "Error: Cannot cast from %.*s to %.*s", STRING_PRINTF(right->type->name), STRING_PRINTF(castTo->name));
					return false;
				}
			}

			expr->type = castTo;

			doConstantCast(exprPointer);

			break;
		}
		case TOKEN('['): {
			trySolidifyNumericLiteralToDefault(right);

			if (right->type->flavor != TypeFlavor::INTEGER) {
				reportError(right, "Error: Array index must be an integer");
				return false;
			}

			if (left->type->flavor == TypeFlavor::POINTER) {
				TypePointer *pointer = static_cast<TypePointer *>(left->type);

				if (pointer->pointerTo == &TYPE_VOID) {
					reportError(binary, "Error: Cannot read from a void pointer");
					return false;
				}

				assert(!(pointer->pointerTo->flags & TYPE_IS_INTERNAL));

				expr->type = pointer->pointerTo;
			}
			else if (left->type->flavor == TypeFlavor::STRING) {
				trySolidifyNumericLiteralToDefault(right);

				expr->type = &TYPE_U8;
			}
			else if (left->type->flavor == TypeFlavor::ARRAY) {
				trySolidifyNumericLiteralToDefault(right);

				addSizeDependency(job, left->type);

				expr->type = static_cast<TypeArray *>(left->type)->arrayOf;
			}
			else {
				reportError(binary->left, "Error: Cannot index a %.*s", STRING_PRINTF(left->type->name));
				return false;
			}

			addSizeDependency(job, expr->type);

			break;
		}
		case TokenT::EQUAL:
		case TokenT::NOT_EQUAL: {
			if (left->type->flavor == right->type->flavor) {
				switch (left->type->flavor) {
					case TypeFlavor::STRING: {
						break;
					}
					case TypeFlavor::BOOL: {
						break;
					}
					case TypeFlavor::FLOAT: {
						if (!binaryOpForFloat(exprPointer)) {
							assert(false);
							return false;
						}
						break;
					}
					case TypeFlavor::FUNCTION: {
						if (left->type != right->type) {
							if (left->type == TYPE_VOID_POINTER) {
								insertImplicitCast(job, &right, left->type);
							}
							else if (right->type == TYPE_VOID_POINTER) {
								insertImplicitCast(job, &left, right->type);
							}
							else {
								reportError(binary, "Error: Cannot compare %.*s to %.*s", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
								return false;
							}
						}
					}
					case TypeFlavor::INTEGER: {
						if (!binaryOpForInteger(job, exprPointer))
							return false;

						break;
					}
					case TypeFlavor::POINTER: {
						if (left->type != right->type) {
							if (left->type == TYPE_VOID_POINTER) {
								insertImplicitCast(job, &right, left->type);
							}
							else if (right->type == TYPE_VOID_POINTER) {
								insertImplicitCast(job, &left, right->type);
							}
							else {
								reportError(binary, "Error: Cannot compare %.*s to %.*s", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
								return false;
							}
						}

						break;
					}
					case TypeFlavor::ARRAY: {
						reportError(binary, "Error: Cannot compare arrays");
						return false;
					}
					case TypeFlavor::STRUCT: {
						reportError(binary, "Error: Cannot compare structs");
						return false;
					}
					case TypeFlavor::ENUM: {
						if (left->type != right->type) {
							reportError(binary, "Error: Cannot compare %.*s to %.*s", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
							return false;
						}

						break;
					}
					case TypeFlavor::TYPE: {
						// @Incomplete make this work
						assert(false);
						return false;
					}
					default:
						assert(false);
				}
			}
			else {
				if (!binaryOpForAutoCast(job, binary, yield)) {
					return false;
				}
				else if (!binaryOpForFloatAndIntLiteral(exprPointer)) {
					return false;
				}
				else if (left->type->flavor == TypeFlavor::ENUM && (left->type->flags & TYPE_ENUM_IS_FLAGS) &&
					right->type->flavor == TypeFlavor::INTEGER && right->flavor == ExprFlavor::INT_LITERAL && static_cast<ExprLiteral *>(right)->unsignedValue == 0) {
					insertImplicitCast(job, &right, left->type);
				}
				else if (right->type->flavor == TypeFlavor::ENUM && (right->type->flags & TYPE_ENUM_IS_FLAGS) &&
					left->type->flavor == TypeFlavor::INTEGER && left->flavor == ExprFlavor::INT_LITERAL && static_cast<ExprLiteral *>(left)->unsignedValue == 0) {
					insertImplicitCast(job, &left, right->type);
				}

				if (right->type != left->type) {
					reportError(binary, "Error: Cannot compare %.*s to %.*s", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
					return false;
				}
			}

			expr->type = &TYPE_BOOL;

			break;
		}
		case TokenT::GREATER_EQUAL:
		case TokenT::LESS_EQUAL:
		case TOKEN('>'):
		case TOKEN('<'): {
			if (left->type->flavor == right->type->flavor) {
				switch (left->type->flavor) {
					case TypeFlavor::ARRAY: {
						reportError(binary, "Error: Cannot compare arrays");
						return false;
					}
					case TypeFlavor::BOOL: {
						reportError(binary, "Error: Cannot compare bools");
						return false;
					}
					case TypeFlavor::STRING: {
						reportError(binary, "Error: Cannot compare strings");
						return false;
					}
					case TypeFlavor::FLOAT: {
						if (!binaryOpForFloat(exprPointer))
							return false;
						break;
					}
					case TypeFlavor::FUNCTION: {
						reportError(binary, "Error: Cannot compare functions");
						return false;
					}
					case TypeFlavor::INTEGER: {
						if (!binaryOpForInteger(job, exprPointer))
							return false;

						break;
					}
					case TypeFlavor::POINTER: {
						if (left->type != right->type) {
							if (left->type == TYPE_VOID_POINTER) {
								insertImplicitCast(job, &right, left->type);
							}
							else if (right->type == TYPE_VOID_POINTER) {
								insertImplicitCast(job, &left, right->type);
							}
							else {
								reportError(binary, "Error: Cannot compare %.*s to %.*s", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
								return false;
							}
						}

						break;
					}
					case TypeFlavor::TYPE: {
						reportError(binary, "Error: Cannot compare types");
						return false;
					}
					case TypeFlavor::STRUCT: {
						reportError(binary, "Error: Cannot compare structs");
						return false;
					}
					case TypeFlavor::ENUM: {
						reportError(binary, "Error: Cannot compare enums");
						return false;
					}
					default:
						assert(false);
				}
			}
			else {
				if (!binaryOpForAutoCast(job, binary, yield)) {
					return false;
				}
				else if (!binaryOpForFloatAndIntLiteral(exprPointer)) {
					return false;
				}

				if (right->type != left->type) {
					reportError(binary, "Error: Cannot compare %.*s to %.*s", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
					return false;
				}
			}

			expr->type = &TYPE_BOOL;

			break;
		}
		case TOKEN('+'):
		case TOKEN('-'): {
			if (left->type->flavor == right->type->flavor) {
				switch (left->type->flavor) {
					case TypeFlavor::ARRAY: {
						reportError(binary, "Error: Cannot %s arrays", binary->op == TOKEN('+') ? "add" : "subtract");
						return false;
					}
					case TypeFlavor::BOOL: {
						reportError(binary, "Error: Cannot %s bools", binary->op == TOKEN('+') ? "add" : "subtract");
						return false;
					}
					case TypeFlavor::STRING: {
						reportError(binary, "Error: Cannot %s strings", binary->op == TOKEN('+') ? "add" : "subtract");
						return false;
					}
					case TypeFlavor::FLOAT: {
						if (!binaryOpForFloat(exprPointer))
							return false;
						break;
					}
					case TypeFlavor::FUNCTION: {
						reportError(binary, "Error: Cannot %s functions", binary->op == TOKEN('+') ? "add" : "subtract");
						return false;
					}
					case TypeFlavor::INTEGER: {
						if (!binaryOpForInteger(job, exprPointer))
							return false;

						break;
					}
					case TypeFlavor::POINTER: {
						if (binary->op == TOKEN('+')) {
							reportError(binary, "Error: Cannot add pointers");
							return false;
						}

						if (left->type != right->type) {
							reportError(binary, "Error: Cannot subtract %.*s and %.*s", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
							return false;
						}

						auto pointer = static_cast<TypePointer *>(left->type);

						addSizeDependency(job, pointer->pointerTo);

						break;
					}
					case TypeFlavor::TYPE: {
						reportError(binary, "Error: Cannot %s types", binary->op == TOKEN('+') ? "add" : "subtract");
						return false;
					}
					case TypeFlavor::STRUCT: {
						reportError(binary, "Error: Cannot %s structs", binary->op == TOKEN('+') ? "add" : "subtract");
						return false;
					}
					case TypeFlavor::ENUM: {
						reportError(binary, "Error: Cannot %s enums", binary->op == TOKEN('+') ? "add" : "subtract");
						return false;
					}
					default:
						assert(false);
				}
			}
			else {
				if (!binaryOpForAutoCast(job, binary, yield)) {
					return false;
				}
				else if (left->type->flavor == TypeFlavor::POINTER && right->type->flavor == TypeFlavor::INTEGER) {
					trySolidifyNumericLiteralToDefault(right);
					auto pointer = static_cast<TypePointer *>(left->type);

					addSizeDependency(job, pointer->pointerTo);
				}
				else if (!binaryOpForFloatAndIntLiteral(exprPointer)) {
					return false;
				} else if (right->type != left->type) {
					reportError(binary, "Error: Cannot %s %.*s and %.*s",
						binary->op == TOKEN('+') ? "add" : "subtract", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
					return false;
				}
			}

			if (right->type->flavor == TypeFlavor::POINTER) {
				expr->type = &TYPE_S64;
			}
			else { // it is already set to s64 for pointer subtraction
				expr->type = left->type;
			}

			break;
		}
		case TOKEN('&'):
		case TOKEN('|'):
		case TOKEN('^'): {
			const char *opName = binary->op == TOKEN('&') ? "and" : (binary->op == TOKEN('|') ? "or" : "xor");

			if (left->type->flavor == right->type->flavor) {
				switch (left->type->flavor) {
					case TypeFlavor::ARRAY: {
						reportError(binary, "Error: Cannot '%s' arrays", opName);
						return false;
					}
					case TypeFlavor::BOOL: {
						// We are done
					}
					case TypeFlavor::STRING: {
						reportError(binary, "Error: Cannot '%s' strings", opName);
						return false;
					}
					case TypeFlavor::FLOAT: {
						reportError(binary, "Error: Cannot '%s' floats", opName);
						return false;
					}
					case TypeFlavor::FUNCTION: {
						reportError(binary, "Error: Cannot '%s' functions", opName);
						return false;
					}
					case TypeFlavor::INTEGER: {
						if (!binaryOpForInteger(job, exprPointer))
							return false;

						break;
					}
					case TypeFlavor::POINTER: { // @Incomplete: should this be allowed, i.e. to store data in unused alignment bits
						reportError(binary, "Error: Cannot '%s' pointers", opName);
						return false;
					}
					case TypeFlavor::TYPE: {
						reportError(binary, "Error: Cannot '%s' types", opName);
						return false;
					}
					case TypeFlavor::STRUCT: {
						reportError(binary, "Error: Cannot '%s' structs", opName);
						return false;
					}
					case TypeFlavor::ENUM: {
						if (left->type->flags & right->type->flags & TYPE_ENUM_IS_FLAGS) {
							if (left->type != right->type) {
								reportError(binary, "Error: Cannot '%s' %.*s and %.*s", opName, STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
								return false;
							}
						}
						else {
							reportError(binary, "Error: Cannot '%s' enums", opName);
							return false;
						}
						
						break;
					}
					default:
						assert(false);
				}
			}
			else {
				if (!binaryOpForAutoCast(job, binary, yield)) {
					return false;
				}
				else if (left->type->flavor == TypeFlavor::ENUM && (left->type->flags & TYPE_ENUM_IS_FLAGS) &&
					right->type->flavor == TypeFlavor::INTEGER && right->flavor == ExprFlavor::INT_LITERAL && static_cast<ExprLiteral *>(right)->unsignedValue == 0) {
					insertImplicitCast(job, &right, left->type);
				}
				else if (right->type->flavor == TypeFlavor::ENUM && (right->type->flags & TYPE_ENUM_IS_FLAGS) &&
					left->type->flavor == TypeFlavor::INTEGER && left->flavor == ExprFlavor::INT_LITERAL && static_cast<ExprLiteral *>(left)->unsignedValue == 0) {
					insertImplicitCast(job, &left, right->type);
				}

				if (right->type != left->type) {
					reportError(binary, "Error: Cannot '%s' %.*s and %.*s", opName, STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
					return false;
				}
			}

			expr->type = left->type;

			break;
		}
		case TokenT::SHIFT_LEFT:
		case TokenT::SHIFT_RIGHT: {
			if (left->type->flavor == right->type->flavor) {
				switch (left->type->flavor) {
					case TypeFlavor::ARRAY: {
						reportError(binary, "Error: Cannot shift arrays");
						return false;
					}
					case TypeFlavor::BOOL: {
						reportError(binary, "Error: Cannot shift bools");
						return false;
					}
					case TypeFlavor::STRING: {
						reportError(binary, "Error: Cannot shift strings");
						return false;
					}
					case TypeFlavor::FLOAT: {
						reportError(binary, "Error: Cannot shift floats");
						return false;
					}
					case TypeFlavor::FUNCTION: {
						reportError(binary, "Error: Cannot shift functions");
						return false;
					}
					case TypeFlavor::INTEGER: {
						if (!binaryOpForInteger(job, exprPointer))
							return false;

						break;
					}
					case TypeFlavor::POINTER: {
						reportError(binary, "Error: Cannot shift pointers");
						return false;
					}
					case TypeFlavor::TYPE: {
						reportError(binary, "Error: Cannot shift types");
						return false;
					}
					case TypeFlavor::STRUCT: {
						reportError(binary, "Error: Cannot shift structs");
						return false;
					}
					case TypeFlavor::ENUM: {
						reportError(binary, "Error: Cannot shift enums");
						return false;
					}
					default:
						assert(false);
				}
			}
			else {
				if (!binaryOpForAutoCast(job, binary, yield)) {
					return false;
				}

				if (right->type != left->type) {
					reportError(binary, "Error: Cannot shift %.*s by %.*s", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
					return false;
				}
			}

			expr->type = left->type;

			break;
		}
		case TOKEN('*'):
		case TOKEN('/'):
		case TOKEN('%'): {
			const char *opName = binary->op == TOKEN('*') ? "multiply" : (binary->op == TOKEN('/') ? "divide" : "mod");

			if (left->type->flavor == right->type->flavor) {
				switch (left->type->flavor) {
					case TypeFlavor::ARRAY: {
						reportError(binary, "Error: Cannot %s arrays", opName);
						return false;
					}
					case TypeFlavor::BOOL: {
						reportError(binary, "Error: Cannot %s bools", opName);
						return false;
					}
					case TypeFlavor::STRING: {
						reportError(binary, "Error: Cannot %s strings", opName);
						return false;
					}
					case TypeFlavor::FLOAT: {
						if (!binaryOpForFloat(exprPointer))
							return false;
						break;
					}
					case TypeFlavor::FUNCTION: {
						reportError(binary, "Error: Cannot %s functions", opName);
						return false;
					}
					case TypeFlavor::INTEGER: {
						if (!binaryOpForInteger(job, exprPointer))
							return false;

						break;
					}
					case TypeFlavor::POINTER: {
						reportError(binary, "Error: Cannot %s pointers", opName);
						return false;
					}
					case TypeFlavor::TYPE: {
						reportError(binary, "Error: Cannot %s types", opName);
						return false;
					}
					case TypeFlavor::STRUCT: {
						reportError(binary, "Error: Cannot %s structs", opName);
						return false;
					}
					case TypeFlavor::ENUM: {
						reportError(binary, "Error: Cannot %s enums", opName);
						return false;
					}
					default:
						assert(false);
				}
			}
			else {
				if (!binaryOpForAutoCast(job, binary, yield)) {
					return false;
				}
				else if (!binaryOpForFloatAndIntLiteral(exprPointer)) {
					return false;
				}


				if (right->type != left->type) {
					reportError(binary, "Error: Cannot %s %.*s and %.*s", opName, STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
					return false;
				}
			}

			expr->type = left->type;

			break;
		}
		case TOKEN('='): {
			if (!isAssignable(left)) {
				// @Incomplete: better error messages here
				//  - If we were assigning to a constant
				//  - If we were assigning to a pointer

				reportError(binary, "Error: Left side of binary is not assignable");
				return false;
			}

			if (!right) {
				assert(false);
			}
			else {
				bool yield;
				if (!assignOp(job, binary, left->type, binary->right, &yield)) {
					if (yield) {
						return true;
					}
					else {
						return false;
					}
				}

				assert(binary->left->type == binary->right->type);
			}

			break;
		}
		case TokenT::PLUS_EQUALS:
		case TokenT::MINUS_EQUALS: {
			if (!isAssignable(left)) {
				// @Incomplete: better error messages here
				//  - If we were assigning to a constant
				//  - If we were assigning to a pointer

				reportError(binary, "Error: Left side of binary is not assignable");
				return false;
			}

			if (left->type->flavor == right->type->flavor) {
				switch (left->type->flavor) {
					case TypeFlavor::ARRAY: {
						reportError(binary, "Error: Cannot %s arrays", binary->op == TokenT::PLUS_EQUALS ? "add" : "subtract");
						return false;
					}
					case TypeFlavor::BOOL: {
						reportError(binary, "Error: Cannot %s bools", binary->op == TokenT::PLUS_EQUALS ? "add" : "subtract");
						return false;
					}
					case TypeFlavor::STRING: {
						reportError(binary, "Error: Cannot %s strings", binary->op == TokenT::PLUS_EQUALS ? "add" : "subtract");
						return false;
					}
					case TypeFlavor::FLOAT: {
						if (!assignOpForFloat(binary))
							return false;
						break;
					}
					case TypeFlavor::FUNCTION: {
						reportError(binary, "Error: Cannot %s functions", binary->op == TokenT::PLUS_EQUALS ? "add" : "subtract");
						return false;
					}
					case TypeFlavor::INTEGER: {
						if (!assignOpForInteger(job, binary))
							return false;

						break;
					}
					case TypeFlavor::POINTER: {
						if (binary->op == TokenT::PLUS_EQUALS) {
							reportError(binary, "Error: Cannot add pointers");
						}
						else {
							reportError(binary, "Error: Cannot '-=' two pointers, the result of '-' gives s64 which cannot be assigned to %.*s", STRING_PRINTF(left->type->name));
						}

						return false;
					}
					case TypeFlavor::TYPE: {
						reportError(binary, "Error: Cannot %s types", binary->op == TokenT::PLUS_EQUALS ? "add" : "subtract");
						return false;
					}
					case TypeFlavor::STRUCT: {
						reportError(binary, "Error: Cannot %s structs", binary->op == TokenT::PLUS_EQUALS ? "add" : "subtract");
						return false;
					}
					case TypeFlavor::ENUM: {
						reportError(binary, "Error: Cannot %s enums", binary->op == TokenT::PLUS_EQUALS ? "add" : "subtract");
						return false;
					}
					default:
						assert(false);
				}
			}
			else {
				if (!assignOpForAutoCast(job, binary, yield)) {
					return false;
				}
				else if (!assignOpForFloatAndIntLiteral(binary)) {
					return false;
				}

				if (left->type->flavor == TypeFlavor::POINTER && right->type->flavor == TypeFlavor::INTEGER) {
					trySolidifyNumericLiteralToDefault(right);
					auto pointer = static_cast<TypePointer *>(left->type);

					addSizeDependency(job, pointer->pointerTo);
				}
				else if (right->type != left->type) {
					reportError(binary, "Error: Cannot %s %.*s to %.*s",
						binary->op == TokenT::PLUS_EQUALS ? "add" : "subtract", STRING_PRINTF(right->type->name), STRING_PRINTF(left->type->name));
					return false;
				}
			}

			expr->type = left->type;

			break;
		}
		case TokenT::LOGIC_AND:
		case TokenT::LOGIC_OR: {
			if (left->type != &TYPE_BOOL) {
				if (isValidCast(&TYPE_BOOL, left->type)) {
					insertImplicitCast(job, &left, &TYPE_BOOL);
				}
				else {
					reportError(binary->left, "Error: Cannot convert %.*s to bool", STRING_PRINTF(left->type->name));
					return false;
				}
			}

			if (right->type != &TYPE_BOOL) {
				if (isValidCast(&TYPE_BOOL, right->type)) {
					insertImplicitCast(job, &right, &TYPE_BOOL);
				}
				else {
					reportError(binary->right, "Error: Cannot convert %.*s to bool", STRING_PRINTF(right->type->name));
					return false;
				}
			}

			expr->type = &TYPE_BOOL;

			break;
		}
		case TokenT::AND_EQUALS:
		case TokenT::OR_EQUALS:
		case TokenT::XOR_EQUALS: {
			if (!isAssignable(left)) {
				// @Incomplete: better error messages here
				//  - If we were assigning to a constant
				//  - If we were assigning to a pointer

				reportError(binary, "Error: Left side of binary is not assignable");
				return false;
			}

			const char *opName = binary->op == TokenT::AND_EQUALS ? "and" : (binary->op == TokenT::OR_EQUALS ? "or" : "xor");

			if (left->type->flavor == right->type->flavor) {
				switch (left->type->flavor) {
					case TypeFlavor::ARRAY: {
						reportError(binary, "Error: Cannot '%s' arrays", opName);
						return false;
					}
					case TypeFlavor::BOOL: {
						// We are done
					}
					case TypeFlavor::STRING: {
						reportError(binary, "Error: Cannot '%s' strings", opName);
						return false;
					}
					case TypeFlavor::FLOAT: {
						reportError(binary, "Error: Cannot '%s' floats", opName);
						return false;
					}
					case TypeFlavor::FUNCTION: {
						reportError(binary, "Error: Cannot '%s' functions", opName);
						return false;
					}
					case TypeFlavor::INTEGER: {
						if (!assignOpForInteger(job, binary))
							return false;

						break;
					}
					case TypeFlavor::POINTER: {
						reportError(binary, "Error: Cannot '%s' pointers", opName);
						return false;
					}
					case TypeFlavor::TYPE: {
						reportError(binary, "Error: Cannot '%s' types", opName);
						return false;
					}
					case TypeFlavor::STRUCT: {
						reportError(binary, "Error: Cannot '%s' structs", opName);
						return false;
					}
					case TypeFlavor::ENUM: {
						if (left->type->flags & right->type->flags & TYPE_ENUM_IS_FLAGS) {
							if (left->type != right->type) {
								reportError(binary, "Error: Cannot '%s' %.*s and %.*s", opName, STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
								return false;
							}
						}
						else {
							reportError(binary, "Error: Cannot '%s' enums", opName);
							return false;
						}

						break;
					}
					default:
						assert(false);
				}
			}
			else {
				if (!assignOpForAutoCast(job, binary, yield)) {
					return false;
				}
				else if (left->type->flavor == TypeFlavor::ENUM && (left->type->flags & TYPE_ENUM_IS_FLAGS) &&
					right->type->flavor == TypeFlavor::INTEGER && right->flavor == ExprFlavor::INT_LITERAL && static_cast<ExprLiteral *>(right)->unsignedValue == 0) {
					insertImplicitCast(job, &right, left->type);
				}

				if (right->type != left->type) {
					reportError(binary, "Error: Cannot %s %.*s and %.*s", opName, STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
					return false;
				}
			}

			expr->type = left->type;

			break;
		}
		case TokenT::SHIFT_LEFT_EQUALS:
		case TokenT::SHIFT_RIGHT_EQUALS: {
			if (!isAssignable(left)) {
				// @Incomplete: better error messages here
				//  - If we were assigning to a constant
				//  - If we were assigning to a pointer

				reportError(binary, "Error: Left side of binary is not assignable");
				return false;
			}

			if (left->type->flavor == right->type->flavor) {
				switch (left->type->flavor) {
					case TypeFlavor::ARRAY: {
						reportError(binary, "Error: Cannot shift arrays");
						return false;
					}
					case TypeFlavor::BOOL: {
						reportError(binary, "Error: Cannot shift bools");
						return false;
					}
					case TypeFlavor::STRING: {
						reportError(binary, "Error: Cannot shift strings");
						return false;
					}
					case TypeFlavor::FLOAT: {
						reportError(binary, "Error: Cannot shift floats");
						return false;
					}
					case TypeFlavor::FUNCTION: {
						reportError(binary, "Error: Cannot shift functions");
						return false;
					}
					case TypeFlavor::INTEGER: {
						if (!assignOpForInteger(job, binary))
							return false;

						break;
					}
					case TypeFlavor::POINTER: {
						reportError(binary, "Error: Cannot shift pointers");
						return false;
					}
					case TypeFlavor::TYPE: {
						reportError(binary, "Error: Cannot shift types");
						return false;
					}
					case TypeFlavor::STRUCT: {
						reportError(binary, "Error: Cannot shift structs");
						return false;
					}
					case TypeFlavor::ENUM: {
						reportError(binary, "Error: Cannot shift enums");
						return false;
					}
					default:
						assert(false);
				}
			}
			else {
				if (!assignOpForAutoCast(job, binary, yield)) {
					return false;
				}

				if (right->type != left->type) {
					reportError(binary, "Error: Cannot shift %.*s by %.*s", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
					return false;
				}
			}

			expr->type = left->type;

			break;
		}
		case TokenT::TIMES_EQUALS:
		case TokenT::DIVIDE_EQUALS:
		case TokenT::MOD_EQUALS: {
			if (!isAssignable(left)) {
				// @Incomplete: better error messages here
				//  - If we were assigning to a constant
				//  - If we were assigning to a pointer

				reportError(binary, "Error: Left side of binary is not assignable");
				return false;
			}

			const char *opName = binary->op == TOKEN('*') ? "multiply" : (binary->op == TOKEN('/') ? "divide" : "mod");

			if (left->type->flavor == right->type->flavor) {
				switch (left->type->flavor) {
					case TypeFlavor::ARRAY: {
						reportError(binary, "Error: Cannot '%s' arrays", opName);
						return false;
					}
					case TypeFlavor::BOOL: {
						reportError(binary, "Error: Cannot '%s' bools", opName);
						return false;
					}
					case TypeFlavor::STRING: {
						reportError(binary, "Error: Cannot '%s' strings", opName);
						return false;
					}
					case TypeFlavor::FLOAT: {
						if (!assignOpForFloat(binary))
							return false;
						break;
					}
					case TypeFlavor::FUNCTION: {
						reportError(binary, "Error: Cannot '%s' functions", opName);
						return false;
					}
					case TypeFlavor::INTEGER: {
						if (!assignOpForInteger(job, binary))
							return false;

						break;
					}
					case TypeFlavor::POINTER: {
						reportError(binary, "Error: Cannot '%s' pointers", opName);
						return false;
					}
					case TypeFlavor::TYPE: {
						reportError(binary, "Error: Cannot '%s' types", opName);
						return false;
					}
					case TypeFlavor::STRUCT: {
						reportError(binary, "Error: Cannot '%s' structs", opName);
						return false;
					}
					case TypeFlavor::ENUM: {
						reportError(binary, "Error: Cannot '%s' enums", opName);
						return false;
					}
					default:
						assert(false);
				}
			}
			else {
				if (!assignOpForAutoCast(job, binary, yield)) {
					assert(false);
				}
				else if (!assignOpForFloatAndIntLiteral(binary)) {
					return false;
				}


				if (right->type != left->type) {
					reportError(binary, "Error: Cannot %s %.*s and %.*s", opName, STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
					return false;
				}
			}

			expr->type = left->type;

			break;
		}
		case TokenT::ARRAY_TYPE: {
			if (right->type->flavor != TypeFlavor::TYPE) {
				reportError(right, "Error: Array element type must be a type");
				return false;
			}

			if (right->flavor != ExprFlavor::TYPE_LITERAL) {
				reportError(right, "Error: Array element type must be a constant");
				return false;
			}

			auto type = static_cast<ExprLiteral *>(right);

			if (type->typeValue == &TYPE_VOID) {
				reportError(right, "Error: Cannot have an array of void elements");
				return false;
			}

			type->start = expr->start;

			TypeArray *array;


			if (left) {

				if (left->type->flavor != TypeFlavor::INTEGER) {
					reportError(right, "Error: Array size must be an integer");
					return false;
				}

				if (left->flavor != ExprFlavor::INT_LITERAL) {
					reportError(right, "Error: Array size must be a constant");
					return false;
				}

				auto size = static_cast<ExprLiteral *>(left);

				if (size->unsignedValue == 0) {
					reportError(right, "Error: Array size cannot be zero");
					return false;
				}

				if ((left->type->flags & TYPE_INTEGER_IS_SIGNED) && size->signedValue < 0) {
					reportError(right, "Error: Array size must be positive (given: %" PRIi64 ")", size->signedValue);
					return false;
				}

				array = getStaticArray(type->typeValue, size->unsignedValue);

				if (type->typeValue->size == 0 && !array->sizeJob) {
					auto sizeJob = allocateJob();
					sizeJob->type = InferType::TYPE_TO_SIZE;
					sizeJob->infer.type = array;
					sizeJob->infer.type->sizeJob = sizeJob;

					inferJobs.add(sizeJob);
					++totalTypesSized;
				}
			}
			else {
				if (expr->flags & EXPR_ARRAY_IS_DYNAMIC) {
					array = getDynamicArray(type->typeValue);
				}
				else {
					array = getArray(type->typeValue);
				}
			}

			*exprPointer = inferMakeTypeLiteral(binary->start, binary->right->end, array);
			(*exprPointer)->valueOfDeclaration = expr->valueOfDeclaration;

			break;
		}
		default:
			assert(false);
			return false;
	}

	return true;
}
bool inferFlattened(InferJob *job, Array<Expr **> &flattened, u64 *index, Block ** const waitingOnBlock) {
	PROFILE_FUNC();
	++totalInfers;

	for (; *index < flattened.count; ++ * index) {
		auto exprPointer = flattened[*index];
		auto expr = *exprPointer;

		switch (expr->flavor) {
			case ExprFlavor::IDENTIFIER: {

				auto identifier = static_cast<ExprIdentifier *>(expr);
				if (!identifier->declaration) {
					if (identifier->structAccess) {
						bool onlyConstants;
						auto struct_ = getExpressionNamespace(identifier->structAccess, &onlyConstants, identifier);

						if (!struct_) {
							return false;
						}

						bool yield;
						u64 index;
						auto member = findDeclaration(&struct_->members, identifier->name, &index, &yield);

						if (yield) {
							*waitingOnBlock = &struct_->members;
							return true;
						}

						if (!member) {
							reportError(identifier, "Error: %.*s does not have member %.*s", STRING_PRINTF(struct_->name), STRING_PRINTF(identifier->name));
							return false;
						}
						else if (onlyConstants & !(member->flags & DECLARATION_IS_CONSTANT)) {
							reportError(identifier, "Error: Can only access constant members of %.*s from it's prototype", STRING_PRINTF(struct_->name));
							return false;
						}
						identifier->declaration = member;

						addSizeDependency(job, struct_);
					}
					else {
						for (; identifier->resolveFrom; identifier->resolveFrom = identifier->resolveFrom->parentBlock) {
							if (!(identifier->resolveFrom->flags & BLOCK_IS_COMPLETE)) break;

							bool yield;
							u64 index;
							if (Declaration *declaration = findDeclaration(identifier->resolveFrom, identifier->name, &index, &yield, identifier->indexInBlock)) {
								if ((declaration->flags & DECLARATION_IS_CONSTANT) && !(declaration->flags & DECLARATION_IMPORTED_BY_USING)) {
									identifier->declaration = declaration;
									break;
								}
								else {
									if ((identifier->flags & EXPR_IDENTIFIER_RESOLVING_ONLY_CONSTANTS) && !(identifier->flags & EXPR_VALUE_NOT_REQUIRED)) {
										reportError(identifier, "Error: Cannot refer to %s from outside, capture is not supported", (identifier->declaration->flags & DECLARATION_IS_ARGUMENT) ? "argument" : "variable");
										return false;
									}

									if (declaration->indexInBlock < identifier->indexInBlock) {
										identifier->declaration = declaration;
										break;
									}
									else {
										reportError(identifier, "Error: Cannot refer to variable '%.*s' before it was declared", STRING_PRINTF(identifier->name));
										reportError(declaration, "   ..: Here is the location of the declaration");
										return false;
									}
								}
							}
							else if (yield) {
								*waitingOnBlock = identifier->resolveFrom;
								return true;
							}

							identifier->indexInBlock = identifier->resolveFrom->indexInParent;

							if (identifier->resolveFrom->flags & (BLOCK_IS_ARGUMENTS | BLOCK_IS_STRUCT))
								identifier->flags |= EXPR_IDENTIFIER_RESOLVING_ONLY_CONSTANTS;
						}

						if (!identifier->declaration) {
							if (!identifier->resolveFrom) { // If we have checked all the local scopes and the
								u64 index;
								identifier->declaration = findDeclarationNoYield(&globalBlock, identifier->name, &index);

								if (!identifier->declaration) {
									*waitingOnBlock = &globalBlock;
								}
							}
							else {
								*waitingOnBlock = identifier->resolveFrom;
							}
						}
						
						if (identifier->declaration) {
							if (identifier->enclosingScope && identifier->declaration->enclosingScope != identifier->enclosingScope) {
								assert(identifier->enclosingScope != &globalBlock);

								if (!addImplicitImport(identifier->enclosingScope, identifier->name, &identifier->start, &identifier->end)) {
									return false;
								}
							}
						}
					}
				}

				if (identifier->declaration) {
					bool yield;
					if (!inferIdentifier(job, exprPointer, identifier, &yield)) {
						if (yield) {
							return true;
						}
						else {
							return false;
						}
					}
				}
				else {
					return true;
				}

				break;
			}
			case ExprFlavor::FUNCTION: {
				auto function = static_cast<ExprFunction *>(expr);


				if (function->returnType->flavor != ExprFlavor::TYPE_LITERAL) {

					if (function->returnType->type == &TYPE_TYPE) {
						reportError(function->returnType, "Error: Function return type must evaluate to a constant");
					}
					else {
						reportError(function->returnType, "Error: Function return type must be a type");
					}

					return false;
				}

				bool argumentsInferred = true;

				for (auto argument : function->arguments.declarations) {
					assert(argument);

					if (!(argument->flags & DECLARATION_TYPE_IS_READY)) {
						argumentsInferred = false;
						break;
					}


					if (argument->initialValue) {
						if (argument->flags & DECLARATION_VALUE_IS_READY) {
							if (!isLiteral(argument->initialValue)) {
								reportError(argument, "Error: Default arguments must be a constant value");
								return false;
							}
						}
						else {
							argumentsInferred = false;
							break;
						}
					}
				}

				if (argumentsInferred) {

					TypeFunction *type = new TypeFunction;
					type->argumentCount = function->arguments.declarations.count;

					if (type->argumentCount) {
						type->argumentTypes = new Type * [type->argumentCount];

						for (u64 i = 0; i < type->argumentCount; i++) {
							auto arg = static_cast<ExprLiteral *>(function->arguments.declarations[i]->type);

							assert(arg->flavor == ExprFlavor::TYPE_LITERAL);

							type->argumentTypes[i] = arg->typeValue;
						}
					}

					function->type = getFunctionType(function);
				}
				else {
					return true;
				}

				break;
			}
			case ExprFlavor::FUNCTION_PROTOTYPE: {
				auto function = static_cast<ExprFunction *>(expr);

				if (function->returnType->type != &TYPE_TYPE) {
					reportError(function->returnType, "Error: Function return type must be a type");
					return false;
				}
				else if (function->returnType->flavor != ExprFlavor::TYPE_LITERAL) {
					reportError(function->returnType, "Error: Function return type must be a constant");
					return false;
				}
				else if (static_cast<ExprLiteral *>(function->returnType)->typeValue->flavor == TypeFlavor::NAMESPACE) {
					reportError(function->returnType, "Error: Function return type cannot be a namespace");
					return false;
				}

				for (auto argument : function->arguments.declarations) {
					if (argument->type->type != &TYPE_TYPE) {
						reportError(argument->type, "Error: Function argument type must be a type");
						return false;
					}
					else if (argument->type->flavor != ExprFlavor::TYPE_LITERAL) {
						reportError(argument->type, "Error: Function argument type must be a constant");
						return false;
					}
				}

				auto type = getFunctionType(function);

				*exprPointer = inferMakeTypeLiteral(function->start, function->end, type);
				(*exprPointer)->valueOfDeclaration = expr->valueOfDeclaration;

				break;
			}
			case ExprFlavor::TYPE_LITERAL:
			case ExprFlavor::STRING_LITERAL:
			case ExprFlavor::FLOAT_LITERAL:
			case ExprFlavor::INT_LITERAL: {
				break;
			}
			case ExprFlavor::BLOCK: {
				auto block = static_cast<ExprBlock *>(expr);

				for (auto declaration : block->declarations.declarations) {
					if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IS_USING | DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_IMPLICIT_IMPORT))) {
						if (!(declaration->flags & DECLARATION_TYPE_IS_READY)) {
							return true;
						}
					}
				}

				// Do two passes over the array because if the first pass added dependencies on some declarations then yielded, it would add them again next time round
				for (auto declaration : block->declarations.declarations) {
					if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IS_USING | DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_IMPLICIT_IMPORT))) {
						addSizeDependency(job, static_cast<ExprLiteral *>(declaration->type)->typeValue);
					}
				}

				break;
			}
			case ExprFlavor::BREAK:
			case ExprFlavor::CONTINUE:
			case ExprFlavor::REMOVE: {
				auto continue_ = static_cast<ExprBreakOrContinue *>(expr);

				assert(continue_->flavor == ExprFlavor::IDENTIFIER);
				auto label = static_cast<ExprIdentifier *>(continue_->label);
				assert(label->declaration);

				if (!(label->declaration->flags & DECLARATION_IS_ITERATOR)) {
					reportError(continue_, "Error: %s label '%.*s' is not a loop iterator", continue_->flavor == ExprFlavor::CONTINUE ? "Continue" : "Break", STRING_PRINTF(label->name));
					return false;
				}

				continue_->refersTo = CAST_FROM_SUBSTRUCT(ExprLoop, iteratorBlock, label->declaration->enclosingScope);

				if (continue_->flavor == ExprFlavor::REMOVE) {
					if (continue_->refersTo->flavor == ExprFlavor::WHILE) {
						reportError(continue_, "Error: Cannot remove from a while loop");
						return false;
					}

					if (continue_->refersTo->forBegin->type->flavor != TypeFlavor::ARRAY) {
						reportError(continue_, "Error: Can only remove from an array, not a %.*s",
							STRING_PRINTF(continue_->refersTo->forBegin->type->name));

						return false;
					}

					if (continue_->refersTo->forBegin->type->flags & TYPE_ARRAY_IS_FIXED) {
						reportError(continue_, "Error: Cannot remove from a static size array");

						return false;
					}
				}

				break;
			}
			case ExprFlavor::BINARY_OPERATOR: {
				auto binary = static_cast<ExprBinaryOperator *>(expr);
				bool yield;

				// Split this into a separate function so microsofts shitty optimizer doesn't fuck us during profiling builds
				if (!inferBinary(job, exprPointer, &yield)) {
					return false;
				}

				if (yield) {
					return true;
				}

				break;
			}
			case ExprFlavor::FOR: {
				auto loop = static_cast<ExprLoop *>(expr);

				assert(loop->iteratorBlock.declarations.count >= 2);
				assert(loop->iteratorBlock.declarations[0]->flags & DECLARATION_IS_ITERATOR);
				assert(loop->iteratorBlock.declarations[1]->flags & DECLARATION_IS_ITERATOR_INDEX);

				auto &it = loop->iteratorBlock.declarations[0];
				auto &it_index = loop->iteratorBlock.declarations[1];

				if (loop->forEnd) {
					assert(loop->forBegin);

					if (loop->forBegin->type == loop->forEnd->type && loop->forBegin->type == &TYPE_AUTO_CAST) {
						reportError("Error: Cannot infer types of loop range when both are auto casted");
						return false;
					}

					if (loop->forBegin->type == &TYPE_AUTO_CAST) {
						bool yield;
						if (!tryAutoCast(job, &loop->forBegin, loop->forEnd->type, &yield))
							return yield;
					}
					else if (loop->forEnd->type == &TYPE_AUTO_CAST) {
						bool yield;
						if (!tryAutoCast(job, &loop->forEnd, loop->forBegin->type, &yield))
							return yield;
					}


					if (loop->forBegin->type->flavor == TypeFlavor::INTEGER) {
						if (loop->forEnd->type->flavor != TypeFlavor::INTEGER) {
							reportError(loop, "Error: Cannot iterate from a %.*s to a %.*s", STRING_PRINTF(loop->forBegin->type->name), STRING_PRINTF(loop->forEnd->type->name));
							return false;
						}

						if (loop->flags & EXPR_FOR_BY_POINTER) {
							reportError(loop, "Error: Cannot iterate over integers by pointer");
							return false;
						}

						if ((loop->forBegin->type->flags & TYPE_INTEGER_IS_SIGNED) == (loop->forEnd->type->flags & TYPE_INTEGER_IS_SIGNED)) {
							if (loop->forBegin->type == loop->forEnd->type) {
								trySolidifyNumericLiteralToDefault(loop->forBegin);
								trySolidifyNumericLiteralToDefault(loop->forEnd);
							}
							else if (loop->forBegin->type->size == 0) {
								if (!boundsCheckImplicitConversion(loop, loop->forEnd->type, static_cast<ExprLiteral *>(loop->forBegin))) {
									return false;
								}

								loop->forBegin->type = loop->forEnd->type;
							}
							else if (loop->forEnd->type->size == 0) {
								if (!boundsCheckImplicitConversion(loop, loop->forBegin->type, static_cast<ExprLiteral *>(loop->forEnd))) {
									return false;
								}

								loop->forEnd->type = loop->forBegin->type;
							}

							if (loop->forBegin->type->size > loop->forEnd->type->size) {
								insertImplicitCast(job, &loop->forEnd, loop->forBegin->type);
							}
							else if (loop->forBegin->type->size < loop->forEnd->type->size) {
								insertImplicitCast(job, &loop->forBegin, loop->forEnd->type);
							}

							assert(loop->forBegin->type == loop->forEnd->type);
						}
						else {
							if (loop->forBegin->type == &TYPE_UNSIGNED_INT_LITERAL) {
								trySolidifyNumericLiteralToDefault(loop->forEnd);

								if (!boundsCheckImplicitConversion(loop, loop->forEnd->type, static_cast<ExprLiteral *>(loop->forBegin))) {
									return false;
								}

								loop->forBegin->type = loop->forEnd->type;
							}
							else if (loop->forEnd->type == &TYPE_UNSIGNED_INT_LITERAL) {
								trySolidifyNumericLiteralToDefault(loop->forBegin);

								if (!boundsCheckImplicitConversion(loop, loop->forBegin->type, static_cast<ExprLiteral *>(loop->forEnd))) {
									return false;
								}

								loop->forEnd->type = loop->forBegin->type;
							}
							else {
								reportError(loop, "Error: Signed-unsigned mismatch, cannot iterate from a %.*s to a %.*s",
									STRING_PRINTF(loop->forBegin->type->name), STRING_PRINTF(loop->forEnd->type->name));
								return false;
							}
						}

						it->type = inferMakeTypeLiteral(it->start, it->end, loop->forEnd->type);
						it->flags |= DECLARATION_TYPE_IS_READY;
					}
					else if (loop->forBegin->type->flavor == TypeFlavor::POINTER) {
						if (loop->forBegin->type != loop->forEnd->type) {
							reportError(loop, "Error: Cannot iterate from a %.*s to a %.*s", STRING_PRINTF(loop->forBegin->type->name), STRING_PRINTF(loop->forEnd->type->name));
							return false;
						}

						auto pointer = static_cast<TypePointer *>(loop->forBegin->type);

						if (loop->flags & EXPR_FOR_BY_POINTER) {
							it->type = inferMakeTypeLiteral(it->start, it->end, pointer);
							it->flags |= DECLARATION_TYPE_IS_READY;
						}
						else {
							if (loop->forBegin->type == TYPE_VOID_POINTER) {
								reportError(loop, "Error: Cannot iterate over a *void by value");
								return false;
							}

							it->type = inferMakeTypeLiteral(it->start, it->end, pointer->pointerTo);

							it->flags |= DECLARATION_TYPE_IS_READY;
						}
					}
					else if (loop->forBegin->type->flavor == TypeFlavor::STRING) {
						reportError(loop, "Error: Cannot iterate over a range of strings");
						return false;
					}
					else if (loop->forBegin->type->flavor == TypeFlavor::ARRAY) {
						reportError(loop, "Error: Cannot iterate over a range of arrays");
						return false;
					}
					else {
						reportError(loop, "Error: Cannot iterate from a %.*s to a %.*s", STRING_PRINTF(loop->forBegin->type->name), STRING_PRINTF(loop->forEnd->type->name));
						return false;
					}
				}
				else {
					if (loop->forBegin->type->flavor == TypeFlavor::INTEGER) {
						if (loop->flags & EXPR_FOR_BY_POINTER) {
							reportError(loop->forBegin, "Error: Cannot iterate over integers by pointer");
							return false;
						}

						trySolidifyNumericLiteralToDefault(loop->forBegin);

						loop->forEnd = loop->forBegin;
						ExprLiteral *zero = new ExprLiteral;
						zero->flavor = ExprFlavor::INT_LITERAL;
						zero->unsignedValue = 0;
						zero->type = loop->forEnd->type;
						zero->start = loop->start;
						zero->end = loop->end;
						loop->forBegin = zero;

						it->type = inferMakeTypeLiteral(it->start, it->end, loop->forEnd->type);
						it->flags |= DECLARATION_TYPE_IS_READY;
					}
					else if (loop->forBegin->type->flavor == TypeFlavor::STRING) {
						if (loop->flags & EXPR_FOR_BY_POINTER) {
							it->type = inferMakeTypeLiteral(it->start, it->end, getPointer(&TYPE_U8));
						}
						else {
							it->type = inferMakeTypeLiteral(it->start, it->end, &TYPE_U8);
						}
						it->flags |= DECLARATION_TYPE_IS_READY;
					}
					else if (loop->forBegin->type->flavor == TypeFlavor::ARRAY) {
						auto array = static_cast<TypeArray *>(loop->forBegin->type);

						if (loop->flags & EXPR_FOR_BY_POINTER) {
							auto pointerType = getPointer(array->arrayOf);

							it->type = inferMakeTypeLiteral(it->start, it->end, pointerType);
						}
						else {
							it->type = inferMakeTypeLiteral(it->start, it->end, array->arrayOf);
						}
						it->flags |= DECLARATION_TYPE_IS_READY;
					}
					else if (loop->forBegin->type->flavor == TypeFlavor::POINTER) {
						reportError(loop->forBegin, "Error: Cannot iterate over a lone pointer, you must specify a begin and end");
						return false;
					}
					else {
						reportError(loop->forBegin, "Error: Cannot iterate over a %.*s", STRING_PRINTF(loop->forBegin->type->name));
						return false;
					}
				}

				it_index->type = inferMakeTypeLiteral(it_index->start, it_index->end, &TYPE_U64);
				it_index->flags |= DECLARATION_TYPE_IS_READY;

				addSizeDependency(job, getDeclarationType(it));

				break;
			}
			case ExprFlavor::FUNCTION_CALL: {
				auto call = static_cast<ExprFunctionCall *>(expr);

				if (call->function->type->flavor != TypeFlavor::FUNCTION) {
					reportError(call->function, "Error: Cannot call a %.*s", STRING_PRINTF(call->function->type->name));
					return false;
				}

				auto function = static_cast<TypeFunction *>(call->function->type);

				expr->type = function->returnType;

				addSizeDependency(job, expr->type);

				String functionName = call->function->valueOfDeclaration ? call->function->valueOfDeclaration->name : "function";

				ExprFunction *functionForArgumentNames = nullptr;

				if (call->function->flavor == ExprFlavor::FUNCTION) {
					functionForArgumentNames = static_cast<ExprFunction *>(call->function);
				}
				else if (call->function->valueOfDeclaration && call->function->valueOfDeclaration->initialValue && call->function->valueOfDeclaration->initialValue->flavor == ExprFlavor::FUNCTION) {
					functionForArgumentNames = static_cast<ExprFunction *>(call->function->valueOfDeclaration->initialValue);
				}

				bool hasNamedArguments = false;

				for (u64 i = 0; i < call->argumentCount; i++) {
					if (call->argumentNames[i].length != 0) {
						hasNamedArguments = true;
					}
				}

				u64 minArguments = function->argumentCount;

				if (functionForArgumentNames) {
					for (u64 i = 0; i < functionForArgumentNames->arguments.declarations.count; i++) {
						if (functionForArgumentNames->arguments.declarations[i]->initialValue) {
							--minArguments;
						}
					}
				}

				if (hasNamedArguments && !functionForArgumentNames) {
					reportError(call, "Error: Cannot use named arguments with an unknown function"); // @Improvement better message
					return false;
				}

				if (hasNamedArguments || (call->argumentCount < function->argumentCount && minArguments != function->argumentCount)) {
					Expr **sortedArguments = new Expr * [function->argumentCount]{};

					for (u64 i = 0; i < call->argumentCount; i++) {
						u64 argIndex = i;
						if (call->argumentNames[i].length) {
							auto argument = findDeclarationNoYield(&functionForArgumentNames->arguments, call->argumentNames[i], &argIndex);
							assert(!(argument->flags & DECLARATION_IMPORTED_BY_USING));

							if (!argument) {
								reportError(call->arguments[i], "Error: %.*s does not have an argument called %.*s", STRING_PRINTF(functionName), STRING_PRINTF(call->argumentNames[i]));
								return false;
							}
						}

						if (sortedArguments[argIndex]) {
							reportError(call->arguments[i], "Error: Argument %.*s was supplied twice", STRING_PRINTF(functionForArgumentNames->arguments.declarations[argIndex]->name));
							reportError(sortedArguments[argIndex], "   ..: It was previously given here");
							return false;
						}

						sortedArguments[argIndex] = call->arguments[i];
					}

					for (u64 i = 0; i < function->argumentCount; i++) {
						if (!sortedArguments[i]) {
							auto argument = functionForArgumentNames->arguments.declarations[i];

							if (argument->initialValue) {
								sortedArguments[i] = argument->initialValue;
								addSizeDependency(job, argument->initialValue->type);
							}
							else {
								reportError(call, "Error: Required argument '%.*s' was not given", STRING_PRINTF(argument->name));
								reportError(argument, "   ..: Here is the argument");
								return false;
							}
						}
					}

					call->arguments = sortedArguments;
					call->argumentCount = function->argumentCount;
				}
				else if (call->argumentCount != function->argumentCount) {

					if (call->argumentCount < function->argumentCount) {
						reportError(call, "Error: Too few arguments for %.*s (Expected: %" PRIu64 ", Given: %" PRIu64 ")",
							STRING_PRINTF(functionName), function->argumentCount, call->argumentCount);

						if (functionForArgumentNames) {
							reportError(functionForArgumentNames->arguments.declarations[call->argumentCount], "   ..: Here are the missing arguments");
						}
					}
					else if (call->argumentCount > function->argumentCount) {
						reportError(call, "Error: Too many arguments for %.*s (Expected: %" PRIu64 ", Given: %" PRIu64 ")",
							STRING_PRINTF(functionName), function->argumentCount, call->argumentCount);
					}

					return false;
				}

				for (u64 i = 0; i < call->argumentCount; i++) {
					Type *correct = function->argumentTypes[i];

					bool yield;
					if (!assignOp(job, call->arguments[i], correct, call->arguments[i], &yield)) { // @Incomplete: Give function call specific error messages instead of general type conversion errors
						return yield;
					}
				}

				break;
			}
			case ExprFlavor::IF: {
				auto ifElse = static_cast<ExprIf *>(expr);

				if (ifElse->condition->type != &TYPE_BOOL) {
					if (isValidCast(&TYPE_BOOL, ifElse->condition->type)) {
						insertImplicitCast(job, &ifElse->condition, &TYPE_BOOL);
					}
					else {
						reportError(ifElse->condition, "Error: Cannot convert %.*s to bool", STRING_PRINTF(ifElse->condition->type->name));
						return false;
					}
				}

				break;
			}
			case ExprFlavor::RETURN: {
				auto return_ = static_cast<ExprReturn *>(expr);

				if (!return_->returnsFrom->type)
					return true;

				assert(return_->returnsFrom->type->flavor == TypeFlavor::FUNCTION);

				Type *returnType = static_cast<TypeFunction *>(return_->returnsFrom->type)->returnType;

				if (returnType == &TYPE_VOID) {
					if (return_->value) {
						reportError(return_->value, "Error: A function with void return type cannot return a value");
						return false;
					}
				}
				else {
					if (!return_->value) {
						reportError(return_, "Error: A function without void return type must return a value");
						return false;
					}

					bool yield;
					if (!assignOp(job, return_, returnType, return_->value, &yield)) {
						return yield;
					}
				}

				break;
			}
			case ExprFlavor::UNARY_OPERATOR: {
				auto unary = static_cast<ExprUnaryOperator *>(expr);

				auto &value = unary->value;

				switch (unary->op) {
					case TOKEN('-'): {
						if (value->type->flavor == TypeFlavor::INTEGER) {
							if (value->type->flags & TYPE_INTEGER_IS_SIGNED) {
								if (value->type == &TYPE_SIGNED_INT_LITERAL) {
									value->start = unary->start;

									auto literal = static_cast<ExprLiteral *>(value);

									if (literal->signedValue == INT64_MIN) {
										reportError(value, "Error: Integer literal too large, the maximum value is %" PRIi64, INT64_MAX);
										return false;
									}

									literal->signedValue = -literal->signedValue;
									*exprPointer = value;
								}
								else {
									unary->type = value->type;
								}
							}
							else {
								if (value->type == &TYPE_UNSIGNED_INT_LITERAL) {
									auto literal = static_cast<ExprLiteral *>(value);
									literal->start = unary->start;

									if (literal->unsignedValue > static_cast<u64>(INT64_MAX) + 1ULL) {
										reportError(value, "Error: Integer literal too small, the minimum value is %" PRIi64, INT64_MIN);
										return false;
									}

									literal->type = &TYPE_SIGNED_INT_LITERAL;

									literal->signedValue = -literal->signedValue;
									*exprPointer = value;
								}
								else { // @Incomplete: should we allow negation of unsigned numbers, its useful for soeme bit twiddling but feels weird
									unary->type = value->type;
								}

							}
						}
						else if (value->type->flavor == TypeFlavor::FLOAT) {
							if (value->type == &TYPE_FLOAT_LITERAL) {
								value->start = unary->start;

								auto literal = static_cast<ExprLiteral *>(value);
								literal->floatValue = -literal->floatValue;
								*exprPointer = value;
							}
						}
						else {
							reportError(value, "Error: Cannot negate a %.*s", STRING_PRINTF(value->type->name));
							return false;
						}

						break;
					}
					case TOKEN('+'): {
						if (value->type->flavor == TypeFlavor::INTEGER) { // @Incomplete: Should unsigned int literals become signed?
							value->start = unary->start;
							*exprPointer = value;
						}
						else if (value->type->flavor == TypeFlavor::FLOAT) {
							value->start = unary->start;
							*exprPointer = value;
						}
						else {
							reportError(value, "Error: Cannot have a '+' in front of a %.*s", STRING_PRINTF(value->type->name));
							return false;
						}

						break;
					}
					case TOKEN('~'): {
						if (value->type->flavor == TypeFlavor::INTEGER) {
							trySolidifyNumericLiteralToDefault(value);

							unary->type = value->type;
						}
						else if (value->type->flavor == TypeFlavor::ENUM) {
							if (value->type->flags & TYPE_ENUM_IS_FLAGS) {
								unary->type = value->type;
							}
							else {
								reportError(unary, "Error: Cannot invert an enum");
								return false;
							}
						}
						else {
							reportError(unary, "Error: Cannot invert a %.*s", STRING_PRINTF(value->type->name));
							return false;
						}
					}
					case TOKEN('!'): {
						if (value->type != &TYPE_BOOL) {
							if (isValidCast(&TYPE_BOOL, value->type)) {
								insertImplicitCast(job, &value, &TYPE_BOOL);
							}
							else {
								reportError(value, "Error: Cannot convert %.*s to bool", STRING_PRINTF(value->type->name));
								return false;
							}
						}

						unary->type = &TYPE_BOOL;

						break;
					}
					case TokenT::SIZE_OF: {
						if (value->type != &TYPE_TYPE) {
							reportError(value, "Error: size_of can only accept a type (given a %.*s)", STRING_PRINTF(value->type->name));
							return false;
						}
						else if (value->flavor != ExprFlavor::TYPE_LITERAL) {
							reportError(value, "Error: size_of type must be a constant");
							return false;
						}

						auto type = static_cast<ExprLiteral *>(value);

						if (type->typeValue == &TYPE_VOID) {
							reportError(value, "Error: Cannot take the size of void");
							return false;
						}
						else if (type->typeValue == &TYPE_AUTO_CAST) {
							reportError(value, "Error: Cannot take the size of an auto casted value");
							return false;
						}
						else if (type->typeValue == &TYPE_TYPE) {
							assert(false); // @Incomplete: make this work
							return false;
						}

						if (!type->typeValue->size) {
							return true;
						}

						assert(type->typeValue->size);

						auto literal = new ExprLiteral;
						literal->flavor = ExprFlavor::INT_LITERAL;
						literal->unsignedValue = type->typeValue->size;
						literal->type = &TYPE_UNSIGNED_INT_LITERAL;
						literal->start = unary->start;
						literal->end = unary->end;
						literal->valueOfDeclaration = unary->valueOfDeclaration;

						*exprPointer = literal;

						break;
					}
					case TokenT::TYPE_OF: {
						trySolidifyNumericLiteralToDefault(value);

						if (value->type == &TYPE_AUTO_CAST) {
							reportError(value, "Error: Cannot take the type of an auto casted value");
							return false;
						}
						else if (value->type->flavor == TypeFlavor::NAMESPACE) {
							reportError(value, "Error: Cannot take the type of a namespace");
							return false;
						}

						auto literal = new ExprLiteral;
						literal->flavor = ExprFlavor::TYPE_LITERAL;
						literal->typeValue = value->type;
						literal->start = unary->start;
						literal->end = unary->end;
						literal->type = &TYPE_TYPE;
						literal->valueOfDeclaration = unary->valueOfDeclaration;

						*exprPointer = literal;

						break;
					}
					case TokenT::SHIFT_LEFT: {
						if (value->type->flavor != TypeFlavor::POINTER) {
							reportError(value, "Error: Cannot only read from a pointer, given a %.*s", STRING_PRINTF(value->type->name));
							return false;
						}

						if (value->type == TYPE_VOID_POINTER) {
							reportError(value, "Error: Cannot read from a *void");
							return false;
						}

						auto pointer = static_cast<TypePointer *>(value->type);

						unary->type = pointer->pointerTo;

						addSizeDependency(job, unary->type);

						break;
					}
					case TOKEN('*'): {
						if (value->flavor == ExprFlavor::TYPE_LITERAL) {
							auto literal = static_cast<ExprLiteral *>(value);

							if (literal->typeValue->flavor == TypeFlavor::NAMESPACE) {
								reportError(unary, "Error: Cannot take a pointer to a namespace");
								return false;
							}

							*exprPointer = inferMakeTypeLiteral(unary->start, value->end, getPointer(literal->typeValue));
							(*exprPointer)->valueOfDeclaration = expr->valueOfDeclaration;
						}
						else if (value->flavor == ExprFlavor::BINARY_OPERATOR) {
							auto binary = static_cast<ExprBinaryOperator *>(value);

							if (binary->op == TOKEN('[')) {
								checkedRemoveLastSizeDependency(job, value->type);

								auto pointer = getPointer(value->type);

								unary->type = pointer;
							}
							else {
								reportError(value, "Error: Cannot take an addres to something that has no storage");
								return false;
							}
						}
						else if (value->flavor == ExprFlavor::IDENTIFIER) {
							if (!isAddressable(value)) {
								reportError(value, "Error: Cannot take an address to something that has no storage");
								return false;
							}

							checkedRemoveLastSizeDependency(job, value->type);

							auto pointer = getPointer(value->type);

							unary->type = pointer;
						}
						else {
							reportError(value, "Error: Cannot take an addres to something that has no storage");
							return false;
						}

						break;
					}
				}

				break;
			}
			case ExprFlavor::WHILE: {
				auto loop = static_cast<ExprLoop *>(expr);

				if (loop->whileCondition->type != &TYPE_BOOL) {
					if (isValidCast(&TYPE_BOOL, loop->whileCondition->type)) {
						insertImplicitCast(job, &loop->whileCondition, &TYPE_BOOL);
					}
					else {
						reportError(loop->whileCondition, "Error: Cannot convert %.*s to bool", STRING_PRINTF(loop->whileCondition->type->name));
						return false;
					}
				}

				break;
			}
			case ExprFlavor::ENUM: {
				auto enum_ = static_cast<ExprEnum *>(expr);

				if (enum_->struct_.integerType) {
					*exprPointer = inferMakeTypeLiteral(enum_->start, enum_->end, &enum_->struct_);
				}
				else {
					if (enum_->integerType->type != &TYPE_TYPE) {
						reportError(enum_->integerType, "Error: enum type must be a type");
						return false;
					}
					else if (enum_->integerType->flavor != ExprFlavor::TYPE_LITERAL) {
						reportError(enum_->integerType, "Error: enum type must be a constant");
						return false;
					}
					else if (static_cast<ExprLiteral *>(enum_->integerType)->typeValue->flavor != TypeFlavor::INTEGER) {
						reportError(enum_->integerType, "Error: enum type must be an integer");
						return false;
					}
					else if ((static_cast<ExprLiteral *>(enum_->integerType)->typeValue->flags & TYPE_INTEGER_IS_SIGNED) && (enum_->struct_.flags & TYPE_ENUM_IS_FLAGS)) {
						reportError(enum_->integerType, "Error: enum_flags cannot have a signed type");
						return false;
					}

					enum_->struct_.integerType = static_cast<ExprLiteral *>(enum_->integerType)->typeValue;
					enum_->struct_.alignment = enum_->struct_.integerType->alignment;
					enum_->struct_.size = enum_->struct_.integerType->size;
					enum_->struct_.flags |= enum_->struct_.integerType->flags & TYPE_INTEGER_IS_SIGNED;

					auto literal = inferMakeTypeLiteral(enum_->start, enum_->end, &enum_->struct_);

					literal->valueOfDeclaration = enum_->valueOfDeclaration;

					literal->typeValue->name = literal->valueOfDeclaration ? literal->valueOfDeclaration->name : (enum_->struct_.flags & TYPE_ENUM_IS_FLAGS ? "(enum_flags)" : "(enum)");

					*exprPointer = literal;
				}
				break;
			}
			case ExprFlavor::ENUM_INCREMENT: {
				auto increment = static_cast<ExprEnumIncrement *>(expr);

				if (increment->previous->flags & DECLARATION_VALUE_IS_READY) {
					auto value = static_cast<ExprLiteral *>(increment->previous->initialValue);

					assert(value->flavor == ExprFlavor::INT_LITERAL);
					assert(value->type->flavor == TypeFlavor::ENUM);

					auto literal = new ExprLiteral;
					literal->flavor = ExprFlavor::INT_LITERAL;
					literal->start = increment->start;
					literal->end = increment->end;
					literal->type = value->type->flags & TYPE_INTEGER_IS_SIGNED ? &TYPE_SIGNED_INT_LITERAL : &TYPE_UNSIGNED_INT_LITERAL;
					literal->unsignedValue = value->type->flags & TYPE_ENUM_IS_FLAGS ? value->unsignedValue * 2 : value->unsignedValue + 1;

					*exprPointer = literal;
				}
				else {
					return true;
				}

				break;
			}
			default:
				assert(false);
		}

	}

	return true;
}

bool checkForUndeclaredIdentifier(Expr *haltedOn) {
	if (haltedOn->flavor == ExprFlavor::IDENTIFIER) {
		auto identifier = static_cast<ExprIdentifier *>(haltedOn);

		if (!identifier->declaration) {
			reportError(identifier, "Error: Could not find a declaration for '%.*s'", STRING_PRINTF(identifier->name));

			if (identifier->name == "float") {
				reportError(&identifier->start, "   ..: Did you mean 'f32'?");
			}

			return true;
		}
	}

	return false;
}

static BucketedArenaAllocator inferJobAllocator(1024 * 2024);
static InferJob *firstFreeInferJob;

InferJob *allocateJob() {
	if (firstFreeInferJob) {
		auto result = firstFreeInferJob;
		firstFreeInferJob = firstFreeInferJob->nextFree;

		result->waitingOnBlock = nullptr;
		result->typeFlattened.clear();
		result->typeFlattenedIndex = 0;
		result->valueFlattened.clear();
		result->valueFlattenedIndex = 0;
		result->sizeDependencies.clear();

		return result;
	}

	return new (inferJobAllocator.allocate(sizeof(InferJob))) InferJob;
}

void freeJob(InferJob *job) {
	job->nextFree = firstFreeInferJob;
	firstFreeInferJob = job;
}

bool addDeclaration(Declaration *declaration) {
	if (declaration->name == "__remove" && !declaration->enclosingScope) {
		if (!(declaration->flags & DECLARATION_IS_CONSTANT)) {
			reportError(declaration, "Declaration for __remove must be a constant");
			return false;
		}
		else if (!declaration->initialValue || declaration->initialValue->flavor != ExprFlavor::FUNCTION) {
			reportError(declaration, "__remove must be a function");
			return false;
		}

		removeFunction = static_cast<ExprFunction *>(declaration->initialValue);
	}

	declaration->inferJob = nullptr;

	if (declaration->flags & (DECLARATION_IS_ITERATOR | DECLARATION_IS_ITERATOR_INDEX))
		return true;

	if (!declaration->enclosingScope) {
		if (!addDeclarationToBlock(&globalBlock, declaration)) {
			return false;
		}
	}

	if (!(declaration->flags & DECLARATION_IS_USING)) {
		if (declaration->flags & DECLARATION_IS_CONSTANT) {
			if (!declaration->type && declaration->initialValue && declaration->initialValue->flavor == ExprFlavor::INT_LITERAL) {
				declaration->type = inferMakeTypeLiteral(declaration->start, declaration->end, &TYPE_UNSIGNED_INT_LITERAL);
				declaration->flags |= DECLARATION_TYPE_IS_READY | DECLARATION_VALUE_IS_READY;
				return true;
			}
			/*else if (!declaration->type && declaration->initialValue->flavor == ExprFlavor::FUNCTION && static_cast<ExprFunction *>(declaration->initialValue)->returnType->flavor == ExprFlavor::TYPE_LITERAL) {
				return true;
			}*/
		}
		else if (declaration->enclosingScope) {
			if (!declaration->type && declaration->initialValue && declaration->initialValue->flavor == ExprFlavor::INT_LITERAL)  {
				declaration->type = inferMakeTypeLiteral(declaration->start, declaration->end, &TYPE_UNSIGNED_INT_LITERAL);
				declaration->flags |= DECLARATION_TYPE_IS_READY | DECLARATION_VALUE_IS_READY;
				return true;
			}
			else if (declaration->type && declaration->type->flavor == ExprFlavor::TYPE_LITERAL && !declaration->initialValue && (declaration->flags & (DECLARATION_IS_ARGUMENT | DECLARATION_IS_UNINITIALIZED))) {
				declaration->flags |= DECLARATION_TYPE_IS_READY;
				return true;
			}
			/*else if (!declaration->type && declaration->initialValue->flavor == ExprFlavor::FUNCTION && static_cast<ExprFunction *>(declaration->initialValue)->returnType->flavor == ExprFlavor::TYPE_LITERAL) {
				return true;
			}*/
		}
	}

	++totalDeclarations;

	InferJob *job = allocateJob();
	job->type = InferType::DECLARATION;
	job->infer.declaration = declaration;
	job->infer.declaration->inferJob = job;

	if (declaration->type) {
		flatten(job->typeFlattened, &declaration->type);
	}

	if (declaration->initialValue) {
		flatten(job->valueFlattened, &declaration->initialValue);
	}

	inferJobs.add(job);

	return true;
}

static Declaration *getDependency(Expr *halted) {
	if (halted->flavor == ExprFlavor::IDENTIFIER) {
		auto identifier = static_cast<ExprIdentifier *>(halted);
		assert(identifier->declaration); // We shouldn't search for circular dependencies if there are unresolved identifiers

		return identifier->declaration;
	}
	else if (halted->flavor == ExprFlavor::FUNCTION) {
		auto function = static_cast<ExprFunction *>(halted);

		for (auto argument : function->arguments.declarations) {
			if (!(argument->flags & DECLARATION_TYPE_IS_READY)) {
				return argument;
			}
			else if (argument->initialValue && !(argument->flags & DECLARATION_VALUE_IS_READY)) {
				return argument;
			}
		}
	}

	assert(false); // There should be no other cases where we ha
	return nullptr;
}

static s64 findLoop(Array<Declaration *> &loop, Declaration *declaration) {
	loop.add(declaration);

	auto job = declaration->inferJob;

	if (job->typeFlattenedIndex != job->typeFlattened.count) {
		auto dependency = getDependency(*job->typeFlattened[job->typeFlattenedIndex]);

		for (u64 i = 0; i < loop.count; i++) {
			if (loop[i] == dependency) {
				return i;
			}
		}

		s64 loopIndex = findLoop(loop, dependency);

		if (loopIndex != -1) {
			return loopIndex;
		}
	}

	if (job->valueFlattenedIndex != job->valueFlattened.count) {
		auto dependency = getDependency(*job->valueFlattened[job->valueFlattenedIndex]);

		for (u64 i = 0; i < loop.count; i++) {
			if (loop[i] == dependency) {
				return i;
			}
		}

		s64 loopIndex = findLoop(loop, dependency);

		if (loopIndex != -1) {
			return loopIndex;
		}
	}

	loop.pop();

	return -1;
}

void getHaltStatus(Declaration *declaration, Declaration *next, Expr **haltedOn, bool *typeDependence) {
	auto job = declaration->inferJob;

	if (job->typeFlattenedIndex != job->typeFlattened.count && getDependency(*job->typeFlattened[job->typeFlattenedIndex]) == next) {
		*haltedOn = *job->typeFlattened[job->typeFlattenedIndex];
		*typeDependence = true;
	}
	else if (job->valueFlattenedIndex != job->valueFlattened.count && getDependency(*job->valueFlattened[job->valueFlattenedIndex]) == next) {
		*haltedOn = *job->valueFlattened[job->valueFlattenedIndex];
		*typeDependence = false;
	}
	else {
		assert(false);
	}
}

Array<InferJob *> waitingOnSize;

bool doInferJob(u64 *index, bool *madeProgress) {
	InferJob *job = inferJobs[*index];

	switch (job->type) {
	case InferType::FUNCTION_BODY: {
		auto function = job->infer.function;

		if (!inferFlattened(job, job->valueFlattened, &job->valueFlattenedIndex, &job->waitingOnBlock)) {
			return false;
		}

		if (function->type) {
			for (auto argument : job->infer.function->arguments.declarations) {
				addSizeDependency(job, static_cast<ExprLiteral *>(argument->type)->typeValue);
			}

			if (job->infer.function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
				assert(!job->infer.function->body);

				if (!job->infer.declaration) {
					reportError(job->infer.function, "Error: External functions must be named");
					return false;
				}

				CoffJob coff;
				coff.isFunction = true;
				coff.function = job->infer.function;

				coffWriterQueue.add(coff);

				inferJobs.unordered_remove((*index)--);
				freeJob(job);
			}
			else {
				assert(job->infer.function->body);

				if (job->valueFlattenedIndex == job->valueFlattened.count) {
					inferJobs.unordered_remove((*index)--);
					waitingOnSize.add(job);
				}
			}
		}
		break;
	}
	case InferType::DECLARATION: {
		auto declaration = job->infer.declaration;

		if (declaration->type) {
			if (!inferFlattened(nullptr, job->typeFlattened, &job->typeFlattenedIndex, &job->waitingOnBlock)) {
				return false;
			}
		}

		if (declaration->initialValue) {
			if (!inferFlattened(job, job->valueFlattened, &job->valueFlattenedIndex, &job->waitingOnBlock)) {
				return false;
			}
		}

		if (!(declaration->flags & (DECLARATION_TYPE_IS_READY | DECLARATION_IS_USING)) && declaration->type && job->typeFlattenedIndex == job->typeFlattened.count) {
			if (declaration->type->type != &TYPE_TYPE) {
				if (declaration->type->type->flavor == TypeFlavor::FUNCTION) {
					reportError(declaration->type, "Error: Declaration type cannot be a function, did you miss a colon?");
				}
				else {
					reportError(declaration->type, "Error: Declaration type must be a type, but got a %.*s", STRING_PRINTF(declaration->type->type->name));
				}
				return false;
			}
			if (declaration->type->flavor != ExprFlavor::TYPE_LITERAL) {
				reportError(declaration->type, "Error: Declaration type must be a constant");
				return false;
			}

			if (static_cast<ExprLiteral *>(declaration->type)->typeValue == &TYPE_VOID) {
				reportError(declaration->type, "Error: Declaration cannot have type void");
				return false;
			}

			if (static_cast<ExprLiteral *>(declaration->type)->typeValue->flavor == TypeFlavor::NAMESPACE) {
				reportError(declaration->type, "Error: Declaration type cannot be a namespace");
				return false;
			}

			*madeProgress = true;
			declaration->flags |= DECLARATION_TYPE_IS_READY;
		}

		if (!(declaration->flags & DECLARATION_IS_USING) && declaration->type && declaration->initialValue) {
			if (job->typeFlattenedIndex == job->typeFlattened.count && job->valueFlattenedIndex == job->valueFlattened.count) {
				assert(declaration->initialValue->type);

				Type *correct = static_cast<ExprLiteral *>(declaration->type)->typeValue;

				if (declaration->flags & DECLARATION_IS_ENUM_VALUE) {
					assert(correct->flavor == TypeFlavor::ENUM);

					if (declaration->initialValue->type == &TYPE_SIGNED_INT_LITERAL || declaration->initialValue->type == &TYPE_UNSIGNED_INT_LITERAL) {
						assert(declaration->initialValue->flavor == ExprFlavor::INT_LITERAL);
						
						if (!boundsCheckImplicitConversion(declaration->initialValue, static_cast<TypeEnum *>(correct)->integerType, static_cast<ExprLiteral *>(declaration->initialValue))) {
							reportError(CAST_FROM_SUBSTRUCT(ExprEnum, struct_, static_cast<TypeEnum *>(correct))->integerType, "   ..: Here is the enum type declaration");
							
							return false;
						}

						declaration->initialValue->type = correct;
					}
				}

				bool yield;
				if (!assignOp(job, declaration->initialValue, correct, declaration->initialValue, &yield)) {
					return yield;
				}

				if ((declaration->flags &
					(DECLARATION_IS_CONSTANT | DECLARATION_IS_ARGUMENT | DECLARATION_IS_STRUCT_MEMBER)) || declaration->enclosingScope == &globalBlock) {
					if (!isLiteral(declaration->initialValue)) {
						reportError(declaration->type, "Error: Declaration value must be a constant");
						return false;
					}
				}

				*madeProgress = true;

				inferJobs.unordered_remove((*index)--);

				declaration->flags |= DECLARATION_VALUE_IS_READY;

				if (!(declaration->flags & DECLARATION_IS_CONSTANT) && declaration->enclosingScope == &globalBlock) {
					waitingOnSize.add(job);
				}
				else {
					freeJob(job);
				}
			}
		}
		else if (!(declaration->flags & DECLARATION_IS_USING) && declaration->type) {

			if (job->typeFlattenedIndex == job->typeFlattened.count) {
				if (!(declaration->flags & (DECLARATION_IS_UNINITIALIZED | DECLARATION_IS_ITERATOR | DECLARATION_IS_ITERATOR_INDEX | DECLARATION_IS_ARGUMENT))) {
					auto type = static_cast<ExprLiteral *>(declaration->type)->typeValue;

					bool yield;
					declaration->initialValue = createDefaultValue(declaration, type, &yield);

					if (yield) return true;

					if (!declaration->initialValue) {
						return false;
					}

					declaration->initialValue->valueOfDeclaration = declaration;

					declaration->initialValue->start = declaration->start;
					declaration->initialValue->end = declaration->end;
				}

				*madeProgress = true;

				inferJobs.unordered_remove((*index)--);

				declaration->flags |= DECLARATION_VALUE_IS_READY;


				if (!(declaration->flags & DECLARATION_IS_CONSTANT) && declaration->enclosingScope == &globalBlock) {
					waitingOnSize.add(job);
				}
				else {
					freeJob(job);
				}
			}
		}
		else if (declaration->initialValue) {
			if (job->valueFlattenedIndex == job->valueFlattened.count) {
				if (declaration->flags & DECLARATION_IS_USING) {
					bool onlyConstants;
					auto struct_ = getExpressionNamespace(declaration->initialValue, &onlyConstants, declaration->initialValue);

					onlyConstants |= (declaration->flags & DECLARATION_IS_CONSTANT) != 0;

					if (!struct_) {
						return false;
					}



					if (declaration->initialValue->flavor != ExprFlavor::TYPE_LITERAL) {
						auto identifier = static_cast<ExprIdentifier *>(declaration->initialValue);

						while (identifier) {
							if (identifier->flavor != ExprFlavor::IDENTIFIER) {
								reportError(declaration->initialValue, "Error: You can only 'using' an identifier, a struct access of an identifier or a type");
								return false;
							}

							identifier = static_cast<ExprIdentifier *>(identifier->structAccess);
						}

						declaration->initialValue = pushStructAccessDown(static_cast<ExprIdentifier *>(declaration->initialValue), declaration->type, declaration->start, declaration->end);
					}

					for (auto member : struct_->members.declarations) {
						if (checkForRedeclaration(declaration->enclosingScope, member, declaration->initialValue)) {
							if (!(member->flags & (DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_IMPLICIT_IMPORT))) {
								if (!onlyConstants || (member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IS_USING))) {
									auto import = new Declaration;
									import->start = member->start;
									import->end = member->end;
									import->name = member->name;
									import->flags = member->flags;

									if (member->flags & DECLARATION_IS_USING) {
										if (onlyConstants) {
											import->flags |= DECLARATION_IS_CONSTANT;
										}

										import->initialValue = member->initialValue;
										import->type = declaration->initialValue;
										import->flags &= ~DECLARATION_USING_IS_RESOLVED;

										if (!addDeclaration(import)) {
											return false;
										}
									}
									else {
										if (member->flags & DECLARATION_IS_CONSTANT) {
											import->type = member->type;
											import->initialValue = member->initialValue;
										}
										else {
											import->import = member;
											import->initialValue = declaration->initialValue;
										}

										import->flags |= DECLARATION_IMPORTED_BY_USING;
									}

									addDeclarationToBlock(declaration->enclosingScope, import);
									import->indexInBlock = declaration->indexInBlock;
								}
							}
						}
						else {
							return false;
						}
					}

					*madeProgress = true;

					declaration->enclosingScope->usings.unordered_remove(declaration);
					declaration->flags |= DECLARATION_USING_IS_RESOLVED;
					inferJobs.unordered_remove((*index)--);
					freeJob(job);
				}
				else {
					assert(declaration->initialValue->type);

					if (!(declaration->flags & DECLARATION_IS_CONSTANT)) {
						trySolidifyNumericLiteralToDefault(declaration->initialValue);
					}

					if (declaration->initialValue->type == &TYPE_VOID) {
						reportError(declaration->type, "Error: Declaration cannot have type void");
						return false;
					}
					else if (declaration->initialValue->type == &TYPE_AUTO_CAST) {
						reportError(declaration->type, "Error: Cannot infer the type of a declaration if the value is an auto cast");
						return false;
					}
					else if (declaration->initialValue->type->flavor == TypeFlavor::NAMESPACE) {
						reportError(declaration->type, "Error: Declaration type cannot be a namespace");
						return false;
					}

					declaration->type = inferMakeTypeLiteral(declaration->start, declaration->end, declaration->initialValue->type);

					if ((declaration->flags &
						(DECLARATION_IS_CONSTANT | DECLARATION_IS_ARGUMENT | DECLARATION_IS_STRUCT_MEMBER)) || declaration->enclosingScope == &globalBlock) {
						if (!isLiteral(declaration->initialValue)) {
							reportError(declaration->type, "Error: Declaration value must be a constant");
							return false;
						}
					}

					*madeProgress = true;


					inferJobs.unordered_remove((*index)--);

					declaration->flags |= DECLARATION_VALUE_IS_READY;
					declaration->flags |= DECLARATION_TYPE_IS_READY;


					if (!(declaration->flags & DECLARATION_IS_CONSTANT) && declaration->enclosingScope == &globalBlock) {
						waitingOnSize.add(job);
					}
					else {
						freeJob(job);
					}
				}
			}
		}

		break;
	}
	case InferType::TYPE_TO_SIZE: {
		auto type = job->infer.type;

		if (type->flavor == TypeFlavor::STRUCT) {
			auto struct_ = static_cast<TypeStruct *>(type);

			while (job->sizingIndexInMembers < struct_->members.declarations.count) {
				auto member = struct_->members.declarations[job->sizingIndexInMembers];

				if (!(member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IS_USING | DECLARATION_IMPORTED_BY_USING))) {
					if (!(member->flags & DECLARATION_TYPE_IS_READY))
						break;

					auto memberType = static_cast<ExprLiteral *>(member->type)->typeValue;

					if (!memberType->size)
						break;

					if (!(struct_->flags & TYPE_STRUCT_IS_PACKED)) {
						struct_->alignment = my_max(struct_->alignment, memberType->alignment);
					}

					if (struct_->flags & TYPE_STRUCT_IS_UNION) {
						job->runningSize = my_max(job->runningSize, memberType->size);

						member->physicalStorage = 0;
					}
					else {
						if (!(struct_->flags & TYPE_STRUCT_IS_PACKED)) {
							job->runningSize = AlignPO2(job->runningSize, memberType->alignment);
						}

						member->physicalStorage = job->runningSize;

						job->runningSize += memberType->size;
					}
				}

				job->sizingIndexInMembers++;
			}

			if (job->sizingIndexInMembers == struct_->members.declarations.count) {
				if (job->runningSize == 0) { // Empty structs are allowed, but we can't have size 0 types
					struct_->alignment = 1;

					_ReadWriteBarrier(); // Make sure we update the size after alignment, as having a non-zero size means that the size info is ready

					struct_->size = 1;
				}
				else {
					if (struct_->flags & TYPE_STRUCT_IS_PACKED) {
						struct_->size = job->runningSize;
					}
					else {
						struct_->size = AlignPO2(job->runningSize, struct_->alignment);
					}
				}

				*madeProgress = true;
				freeJob(job);
				inferJobs.unordered_remove((*index)--);
			}
		}
		else if (type->flavor == TypeFlavor::ARRAY) {
			auto array = static_cast<TypeArray *>(type);

			assert(array->flags & TYPE_ARRAY_IS_FIXED);

			if (array->arrayOf->size) {
				array->size = array->arrayOf->size * array->count;
				array->alignment = array->arrayOf->alignment;

				*madeProgress = true;
				freeJob(job);
				inferJobs.unordered_remove((*index)--);
			}
		}
		else {
			assert(false);
		}

		break;
	}
	default:
		assert(false);
	}

	return true;
}

void runInfer() {
	PROFILE_FUNC();

	while (true) {
		DeclarationPack declarations = inferQueue.take();

		Block *changedBlock = nullptr;

		if (declarations.type == DeclarationPackType::EXPRESSION && !declarations.data.expr) {
			break;
		}
		else if (declarations.type == DeclarationPackType::EXPRESSION) {
			InferJob *body = allocateJob();

			if (declarations.data.expr->flavor == ExprFlavor::FUNCTION) {
				body->infer.function = static_cast<ExprFunction *>(declarations.data.expr);
				body->type = InferType::FUNCTION_BODY;

				if (body->infer.function->body) {
					flatten(body->valueFlattened, &body->infer.function->body);
				}

				++totalFunctions;

				inferJobs.add(body);

				u64 index = inferJobs.count - 1;
				bool madeProgress = false;
				if (!doInferJob(&index, &madeProgress)) {
					goto error;
				}

				continue;
			}
			else if (declarations.data.expr->flavor == ExprFlavor::TYPE_LITERAL) {
				body->infer.type = static_cast<ExprLiteral *>(declarations.data.expr)->typeValue;
				body->type = InferType::TYPE_TO_SIZE;

				if (body->infer.type->flavor == TypeFlavor::STRUCT) {
					body->infer.type->name = declarations.data.expr->valueOfDeclaration ? declarations.data.expr->valueOfDeclaration->name : "(struct)";
					addStruct(static_cast<TypeStruct *>(body->infer.type));
				}
				else {
					assert(false);
				}

				body->infer.type->sizeJob = body;
				++totalTypesSized;

				inferJobs.add(body);
			}
			else {
				assert(false);
			}
		}
		else if (declarations.type == DeclarationPackType::GLOBAL_DECLARATION) {
			changedBlock = &globalBlock;

			if (!addDeclaration(declarations.data.declaration)) {
				goto error;
			}
		}
		else {
			//inferJobs.reserve(inferJobs.count + declarations.count); // Make sure we don't do unnecessary allocations
			assert(declarations.type == DeclarationPackType::BLOCK);
			changedBlock = declarations.data.block;

			for (auto declaration : declarations.data.block->declarations) {
				if (!addDeclaration(declaration)) {
					goto error;
				}
			}
		}

		bool madeProgress;

		do {
			madeProgress = false;

			for (u64 i = 0; i < inferJobs.count; i++) {

				auto job = inferJobs[i];

				if (job->waitingOnBlock && job->waitingOnBlock != changedBlock && !madeProgress) continue;
				job->waitingOnBlock = nullptr;

				if (!doInferJob(&i, &madeProgress)) {
					goto error;
				}
			}
		} while (madeProgress && inferJobs.count);

		{
			PROFILE_ZONE("Check size dependencies");

			for (u64 i = 0; i < waitingOnSize.count; i++) {
				auto job = waitingOnSize[i];

				for (u64 j = 0; j < job->sizeDependencies.count; j++) {
					auto depend = job->sizeDependencies[j];

					if (depend->size) {
						job->sizeDependencies.unordered_remove(j--);
					}
				}

				if (job->sizeDependencies.count == 0) {
					waitingOnSize.unordered_remove(i--);

					if (job->type == InferType::DECLARATION) {
						CoffJob coffJob;
						coffJob.declaration = job->infer.declaration;
						coffJob.isFunction = false;
						coffWriterQueue.add(coffJob);
					}
					else if (job->type == InferType::FUNCTION_BODY) {
						irGeneratorQueue.add(job->infer.function);
					}

					freeJob(job);
				}
			}
		}
	}

	if (inferJobs.count && !hadError) { // We didn't complete type inference, check for undeclared identifiers or circular dependencies! If we already had an error don't spam more
		// Check for undeclared identifiers

		for (auto job : inferJobs) {
			if (job->type == InferType::TYPE_TO_SIZE) continue;

			if (job->type == InferType::TYPE_TO_SIZE) continue;
			if (job->typeFlattenedIndex != job->typeFlattened.count) {
				auto haltedOn = *job->typeFlattened[job->typeFlattenedIndex];

				if (checkForUndeclaredIdentifier(haltedOn)) {
					goto error;
				}
			}

			if (job->valueFlattenedIndex != job->valueFlattened.count) {
				auto haltedOn = *job->valueFlattened[job->valueFlattenedIndex];

				if (checkForUndeclaredIdentifier(haltedOn)) {
					goto error;
				}
			}
		}

		// Check for circular dependencies

		Array<Declaration *> loop;

		for (auto job : inferJobs) {
			if (job->type == InferType::TYPE_TO_SIZE) continue;
			if (job->type == InferType::FUNCTION_BODY) continue; // A circular dependency must contain a declaration, we start from there

			loop.clear();

			s64 loopIndex = findLoop(loop, job->infer.declaration);

			if (loopIndex == -1) continue;

			reportError(loop[loopIndex], "Error: There were circular dependencies");

			if (loopIndex + 1 == loop.count) {
				Expr *haltedOn;
				bool typeDependece;


				auto declaration = loop[loopIndex];

				getHaltStatus(declaration, declaration, &haltedOn, &typeDependece);

				reportError(haltedOn, "   ..: The %s of %.*s depends on itself", typeDependece ? "type" : "value", STRING_PRINTF(declaration->name));
			}
			else {
				for (u64 i = static_cast<u64>(loopIndex); i < loop.count; i++) {
					Expr *haltedOn;
					bool typeDependece;

					auto declaration = loop[i];
					auto next = loop[i + 1 == loop.count ? loopIndex : i + 1];

					getHaltStatus(declaration, next, &haltedOn, &typeDependece);

					reportError(haltedOn, "   ..: The %s of %.*s depends on %.*s", typeDependece ? "type" : "value", STRING_PRINTF(declaration->name), STRING_PRINTF(next->name));
				}
			}

			goto error;
		}

		reportError("Internal Compiler Error: Inference got stuck but no undelcared identifiers or circular dependencies were detected");

		for (auto job : inferJobs) {
			if (job->type == InferType::TYPE_TO_SIZE) {
				auto type = job->infer.type;

				if (type->flavor == TypeFlavor::STRUCT) {
					auto haltedOn = static_cast<TypeStruct *>(type)->members.declarations[job->sizingIndexInMembers];

					reportError(haltedOn, "%.*s sizing halted here", STRING_PRINTF(type->name));
				}
				else {
					reportError("%.*s sizing halted", STRING_PRINTF(type->name));
				}
			}
			else {
				const char *type = job->type == InferType::DECLARATION ? "declaration" : "function body";

				String name = job->type == InferType::DECLARATION ? job->infer.declaration->name :
					(job->infer.function->valueOfDeclaration ? job->infer.function->valueOfDeclaration->name : "(function)");

				if (job->typeFlattenedIndex != job->typeFlattened.count) {
					auto haltedOn = *job->typeFlattened[job->typeFlattenedIndex];

					reportError(haltedOn, "%.*s %s type halted here", STRING_PRINTF(name), type);
				}

				if (job->valueFlattenedIndex != job->valueFlattened.count) {
					auto haltedOn = *job->valueFlattened[job->valueFlattenedIndex];

					reportError(haltedOn, "%.*s %s value halted here", STRING_PRINTF(name), type);
				}
			}
		}

		goto error;
	}

	if (waitingOnSize.count) {
		reportError("Error: waiting on size @Incomplete");
		goto error;
	}

	irGeneratorQueue.add(nullptr);
	if (!hadError) {
		u64 totalQueued = totalDeclarations + totalFunctions + totalTypesSized;

		printf(
			"Total queued: %llu\n"
			"  %llu declarations\n"
			"  %llu functions\n"
			"  %llu types\n"
			"Total infers: %llu, %.1f infers/queued\n",
			totalQueued, totalDeclarations, totalFunctions, totalTypesSized, totalInfers, static_cast<float>(totalInfers) / totalQueued);
	}
	return;
error:;
	assert(hadError);

	irGeneratorQueue.add(nullptr);
}