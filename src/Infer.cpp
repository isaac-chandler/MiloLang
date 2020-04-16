#include "Basic.h"
#include "Infer.h"
#include "Array.h"
#include "Parser.h"
#include "Lexer.h"
#include "IrGenerator.h"
#include "CoffWriter.h"

enum class InferType {
	FUNCTION_BODY, 
	DECLARATION
};

Block globalBlock;

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
	} infer;

	InferType type;

	Array<Expr **> typeFlattened;
	u64 typeFlattenedIndex = 0;

	Array<Expr **> valueFlattened;
	u64 valueFlattenedIndex = 0;

	InferJob() : valueFlattened(20) {}

	InferJob *nextFree;
};

WorkQueue<DeclarationPack> inferQueue;

static Array<InferJob *> inferJobs;


void flatten(Array<Expr **> &flattenTo, Expr **expr) {
	//PROFILE_FUNC();
	switch ((*expr)->flavor) {
		case ExprFlavor::IDENTIFIER: {
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
		case ExprFlavor::CONTINUE: {
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

			if (binary->right) {
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
		case ExprFlavor::STRUCT_ACCESS: {
			ExprStructAccess *access = static_cast<ExprStructAccess *>(*expr);

			flatten(flattenTo, &access->left);
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

void solidifyUnsignedLiteralToS64OrU64(Expr *expr) {
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



// @Incomplete: should a n numeric literal 0 implicitly cast to a pointer, currently I think not as this would be a strange special case

void trySolidifyNumericLiteralToDefault(Expr *expr) {
	if (expr->type == &TYPE_UNSIGNED_INT_LITERAL) {
		solidifyUnsignedLiteralToS64OrU64(expr);
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
	if (convertFrom->type == &TYPE_UNSIGNED_INT_LITERAL) {
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
	else {
		assert(convertFrom->type == &TYPE_SIGNED_INT_LITERAL);

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

			literal->flavor = ExprFlavor::FLOAT_LITERAL;
			literal->floatValue = static_cast<double>(literal->unsignedValue);
		}

		left->type = right->type;
	}
	else if (left->type == &TYPE_SIGNED_INT_LITERAL) {
		assert(right->type->flavor == TypeFlavor::INTEGER);

		if (!(right->flags & TYPE_INTEGER_IS_SIGNED)) {
			reportError(binary, "Error: Signed-unsigned mismatch, cannot convert s64 to %.*s", STRING_PRINTF(right->type->name));
			return false;
		}

		if (right->type->flavor == TypeFlavor::INTEGER) {
			if (!boundsCheckImplicitConversion(binary, right->type, static_cast<ExprLiteral *>(left))) {
				return false;
			}
		}
		else {
			assert(right->type->flavor == TypeFlavor::FLOAT);

			auto literal = static_cast<ExprLiteral *>(left);

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

			literal->flavor = ExprFlavor::FLOAT_LITERAL;
			literal->floatValue = static_cast<double>(literal->unsignedValue);
		}

		right->type = left->type;
	}
	else if (right->type == &TYPE_SIGNED_INT_LITERAL) {
		assert(left->type->flavor == TypeFlavor::INTEGER);

		if (!(left->flags & TYPE_INTEGER_IS_SIGNED)) {
			reportError(binary, "Error: Signed-unsigned mismatch, cannot convert %.*s to s64", STRING_PRINTF(left->type->name));
			return false;
		}

		if (left->type->flavor == TypeFlavor::INTEGER) {
			if (!boundsCheckImplicitConversion(binary, left->type, static_cast<ExprLiteral *>(right))) {
				return false;
			}
		}
		else {
			assert(left->type->flavor == TypeFlavor::FLOAT);

			auto literal = static_cast<ExprLiteral *>(right);

			literal->flavor = ExprFlavor::FLOAT_LITERAL;
			literal->floatValue = static_cast<double>(literal->signedValue);
		}

		right->type = left->type;
	}

	return true;
}


bool isValidCast(Type *to, Type *from) {
	

	if (to->flavor == from->flavor) {
		if (from->flavor == TypeFlavor::AUTO_CAST || from->flavor == TypeFlavor::TYPE || from->flavor == TypeFlavor::VOID) {
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

		return true;
	}

	if (from->flavor == TypeFlavor::BOOL) {
		return to->flavor == TypeFlavor::INTEGER;
	}
	else if (from->flavor == TypeFlavor::AUTO_CAST || from->flavor == TypeFlavor::TYPE || from->flavor == TypeFlavor::VOID) {
		return false;
	}
	else if (from->flavor == TypeFlavor::FLOAT) {
		return to->flavor == TypeFlavor::INTEGER;
	}
	else if (from->flavor == TypeFlavor::FUNCTION) {
		return to->flavor == TypeFlavor::BOOL || (to->flavor == TypeFlavor::INTEGER && to->size == 8) || to == TYPE_VOID_POINTER;
	}
	else if (from->flavor == TypeFlavor::INTEGER) {
		return to->flavor == TypeFlavor::BOOL || to->flavor == TypeFlavor::FLOAT || (from->size == 8 && (to->flavor == TypeFlavor::FUNCTION || to->flavor == TypeFlavor::POINTER));
	}
	else if (from->flavor == TypeFlavor::POINTER) {
		return to->flavor == TypeFlavor::BOOL || (to->flavor == TypeFlavor::INTEGER && to->size == 8) || (from == getPointer(&TYPE_U8) && to->flavor == TypeFlavor::STRING);
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

void doConstantCast(Expr **cast) {
	auto binary = static_cast<ExprBinaryOperator *>(*cast);
	auto castTo = static_cast<ExprLiteral *>(binary->left)->typeValue;
	auto expr = binary->right;

	if (expr->type == castTo) {
		*cast = expr;
	}

	if (expr->flavor == ExprFlavor::INT_LITERAL) {
		auto old = static_cast<ExprLiteral *>(expr);


		if (castTo->flavor == TypeFlavor::INTEGER) {
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

void insertImplicitCast(Expr **castFrom, Type *castTo) {
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

	assert(isValidCast(castTo, cast->right->type));

	doConstantCast(castFrom);
}

bool tryAutoCast(Expr **cast, Type *castTo) {
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

	if (!isValidCast(castTo, castFrom)) {
		return false;
	}

	doConstantCast(cast);

	return true;
}

bool binaryOpForFloat(ExprBinaryOperator *binary) {
	auto &left = binary->left;
	auto &right = binary->right;

	if (left->type == right->type) {
		if (left->type == &TYPE_FLOAT_LITERAL) {
			trySolidifyNumericLiteralToDefault(left);
			trySolidifyNumericLiteralToDefault(right);
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

bool binaryOpForInteger(ExprBinaryOperator *binary) {
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
				insertImplicitCast(&right, left->type);
			}
			else if (right->type->size < left->type->size) {
				insertImplicitCast(&left, right->type);
			}
		}
		else {
			if ((left->type == &TYPE_UNSIGNED_INT_LITERAL) && (right->type->flags & TYPE_INTEGER_IS_SIGNED)) {
				if (right->type == &TYPE_SIGNED_INT_LITERAL) {
					right->type = &TYPE_S64;
				}

				if (!boundsCheckImplicitConversion(binary, right->type, static_cast<ExprLiteral *>(left))) {
					return false;
				}

				left->type = right->type;
			}
			else if ((right->type == &TYPE_UNSIGNED_INT_LITERAL) && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
				if (left->type == &TYPE_SIGNED_INT_LITERAL) {
					left->type = &TYPE_S64;
				}

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

bool assignOpForInteger(ExprBinaryOperator *binary) {
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
				insertImplicitCast(&right, left->type);
			}
			else if (right->type->size < left->type->size) {
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

bool binaryOpForAutoCast(ExprBinaryOperator *binary) {
	auto &left = binary->left;
	auto &right = binary->right;

	if (left->type->flavor == TypeFlavor::AUTO_CAST) {
		trySolidifyNumericLiteralToDefault(right);

		if (!tryAutoCast(&left, right->type)) {
			reportError(binary, "Error: Cannot cast from %.*s to %.*s", STRING_PRINTF(static_cast<ExprBinaryOperator *>(left)->right->type->name), STRING_PRINTF(right->type->name));
			return false;
		}

		return true;
	}
	else if (right->type->flavor == TypeFlavor::AUTO_CAST) {
		trySolidifyNumericLiteralToDefault(left);

		if (!tryAutoCast(&right, left->type)) {
			reportError(binary, "Error: Cannot cast from %.*s to %.*s", STRING_PRINTF(static_cast<ExprBinaryOperator *>(right)->right->type->name), STRING_PRINTF(left->type->name));
			return false;
		}

		return true;
	}

	return true;
}

bool assignOpForAutoCast(ExprBinaryOperator *binary) {
	auto &right = binary->right;

	if (right->type->flavor == TypeFlavor::AUTO_CAST) {
		auto &left = binary->left;

		if (!tryAutoCast(&right, left->type)) {
			reportError(binary, "Error: Cannot cast from %.*s to %.*s", STRING_PRINTF(static_cast<ExprBinaryOperator *>(right)->right->type->name), STRING_PRINTF(left->type->name));
			return false;
		}

	}

	return true;
}

void binaryOpForFloatAndIntLiteral(ExprBinaryOperator *binary) {
	auto &left = binary->left;
	auto &right = binary->right;

	if (left->type->flavor == TypeFlavor::FLOAT && (right->type == &TYPE_UNSIGNED_INT_LITERAL || right->type == &TYPE_SIGNED_INT_LITERAL)) {
		if (left->type == &TYPE_FLOAT_LITERAL) {
			trySolidifyNumericLiteralToDefault(left);
		}

		bool success = solidifyOneLiteral(binary);
		assert(success); // we should never fail to solidify to float
	}
	else if (right->type->flavor == TypeFlavor::FLOAT && (left->type == &TYPE_UNSIGNED_INT_LITERAL || left->type == &TYPE_SIGNED_INT_LITERAL)) {
		if (right->type == &TYPE_FLOAT_LITERAL) {
			trySolidifyNumericLiteralToDefault(right);
		}

		bool success = solidifyOneLiteral(binary);
		assert(success); // we should never fail to solidify to float
	}
}

void assignOpForFloatAndIntLiteral(ExprBinaryOperator *binary) {
	auto &left = binary->left;
	auto &right = binary->right;

	if (left->type->flavor == TypeFlavor::FLOAT && (right->type == &TYPE_UNSIGNED_INT_LITERAL || right->type == &TYPE_SIGNED_INT_LITERAL)) {
		bool success = solidifyOneLiteral(binary);
		assert(success); // we should never fail to solidify to float
	}
}

bool isAssignable(Expr *expr) {
	return expr->flavor == ExprFlavor::IDENTIFIER ||
		(expr->flavor == ExprFlavor::STRUCT_ACCESS && !(static_cast<ExprStructAccess *>(expr)->left->type->flags & TYPE_ARRAY_IS_FIXED)) || 
		(expr->flavor == ExprFlavor::UNARY_OPERATOR && static_cast<ExprUnaryOperator *>(expr)->op == TokenT::SHIFT_LEFT) ||
		(expr->flavor == ExprFlavor::BINARY_OPERATOR && static_cast<ExprBinaryOperator *>(expr)->op == TOKEN('['));
}

bool isLiteral(Expr *expr);

bool arrayIsLiteral(ExprArray *array) {
	for (u64 i = 0; i < array->count; i++) {
		if (!isLiteral(array->storage[i])) {
			return false;
		}
	}
	
	return true;
}

bool isLiteral(Expr *expr) {
	return expr->flavor == ExprFlavor::INT_LITERAL || expr->flavor == ExprFlavor::FLOAT_LITERAL || expr->flavor == ExprFlavor::TYPE_LITERAL || expr->flavor == ExprFlavor::STRING_LITERAL || expr->flavor == ExprFlavor::FUNCTION || (expr->flavor == ExprFlavor::ARRAY && arrayIsLiteral(static_cast<ExprArray *>(expr)));
}


bool hasDefaultValue(Type *type) {
	switch (type->flavor) {
		case TypeFlavor::BOOL:
		case TypeFlavor::FLOAT:
		case TypeFlavor::FUNCTION:
		case TypeFlavor::INTEGER:
		case TypeFlavor::POINTER:
		case TypeFlavor::STRING: {
			return true;
		}
		case TypeFlavor::ARRAY: {
			if (type->flags & TYPE_ARRAY_IS_FIXED) {
				return hasDefaultValue(static_cast<TypeArray *>(type)->arrayOf);
			}
			else {
				return true;
			}
		}
		default: {
			return false;
		}
	}
}

bool defaultValueIsZero(Type *type) {
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
				return defaultValueIsZero(static_cast<TypeArray *>(type)->arrayOf);
			}
			else {
				return true;
			}
		}
		default: {
			return false;
		}
	}
}

Expr *createDefaultValue(Declaration *location, Type *type) {
	switch (type->flavor) {
		case TypeFlavor::BOOL:
		case TypeFlavor::FLOAT:
		case TypeFlavor::FUNCTION:
		case TypeFlavor::INTEGER:
		case TypeFlavor::POINTER: {
			ExprLiteral *zero = new ExprLiteral;
			zero->flavor = type->flavor == TypeFlavor::FLOAT ? ExprFlavor::FLOAT_LITERAL : ExprFlavor::INT_LITERAL;
			zero->unsignedValue = 0;
			zero->type = type;

			return zero;
		} break;
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

				if (defaultValueIsZero(array->arrayOf)) {
					defaults->storage = nullptr;
				}
				else {
					defaults->storage = new Expr * [array->count];

					Expr *value = createDefaultValue(location, array->arrayOf);

					for (u64 i = 0; i < array->count; i++) {
						defaults->storage[i] = value;
					}
				}
			}
			else {
				defaults->storage = nullptr;
				defaults->count = 0;
			}

			return defaults;
		}
		case TypeFlavor::TYPE: {
			reportError(location, "Error: There is no default value for a type");
			return nullptr;
		}
		default: {
			assert(false);
			return nullptr;
		}
	}
}

bool assignOp(Expr *location, Type *correct, Expr *&given) {
	if (correct != given->type) {
		if (correct->flavor == given->type->flavor) {
			switch (correct->flavor) {
				case TypeFlavor::ARRAY: {
					if (correct != given->type) {
						if (given->type->flags & (TYPE_ARRAY_IS_DYNAMIC | TYPE_ARRAY_IS_FIXED)) {
							if (!(correct->flags & (TYPE_ARRAY_IS_DYNAMIC | TYPE_ARRAY_IS_FIXED))) { // We are converting from [N]T or [..]T to []T
								if (static_cast<TypeArray *>(given->type)->arrayOf == static_cast<TypeArray *>(correct)->arrayOf) {
									insertImplicitCast(&given, correct);
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
						insertImplicitCast(&given, correct);
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
						if (given->type == TYPE_VOID_POINTER) {
							insertImplicitCast(&given, correct);
						}
						else {
							reportError(location, "Error: Cannot convert from %.*s to %.*s", STRING_PRINTF(given->type->name), STRING_PRINTF(correct->name));
							return false;
						}
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
							insertImplicitCast(&given, correct);
						}
						else if (given->type->size < correct->size) {
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
						if (correct == TYPE_VOID_POINTER) {
							insertImplicitCast(&given, correct);
						}
						else if (given->type == TYPE_VOID_POINTER) {
							insertImplicitCast(&given, correct);
						}
						else {
							reportError(location, "Error: Cannot convert from %.*s to %.*s", STRING_PRINTF(given->type->name), STRING_PRINTF(correct->name));
							return false;
						}
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
			if (given->type->flavor == TypeFlavor::AUTO_CAST) {
				if (!tryAutoCast(&given, correct)) {
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
				else if (correct->flavor == TypeFlavor::FLOAT && given->type == &TYPE_SIGNED_INT_LITERAL) {
					auto literal = static_cast<ExprLiteral *>(given);

					literal->flavor = ExprFlavor::FLOAT_LITERAL;
					literal->floatValue = static_cast<double>(literal->unsignedValue);
					literal->type = correct;
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


bool inferFlattened(Array<Expr **> &flattened, u64 *index) {
	PROFILE_FUNC();
	for (; *index < flattened.count; ++ *index) {
		auto exprPointer = flattened[*index];
		auto expr = *exprPointer;

		switch (expr->flavor) {
			case ExprFlavor::IDENTIFIER: {

				auto identifier = static_cast<ExprIdentifier *>(expr);
				if (!identifier->declaration) {

					for (; identifier->resolveFrom; identifier->resolveFrom = identifier->resolveFrom->parentBlock) {
						if (!(identifier->resolveFrom->flags & BLOCK_IS_COMPLETE)) break;


						u64 index;
						if (Declaration *declaration = findDeclaration(identifier->resolveFrom, identifier->name, &index)) {
							if (declaration->flags & DECLARATION_IS_CONSTANT) {
								identifier->declaration = declaration;
								break;
							}
							else {
								if (identifier->flags & EXPR_IDENTIFIER_RESOLVING_IN_OUTER_FUNCTION) {
									reportError(identifier, "Error: Cannot refer to %s from outer function, capture is not supported", (identifier->declaration->flags & DECLARATION_IS_ARGUMENT) ? "argument" : "variable");
									return false;
								}

								if (index < identifier->indexInBlock) {
									identifier->declaration = declaration;
									break;
								}
								else {
									reportError(identifier, "Error: Cannot refer to variable '%.*s' before it was declared", STRING_PRINTF(identifier->name));
									reportError(identifier->declaration, "   ..: Here is the location of the declaration");
									return false;
								}
							}
						}

						identifier->indexInBlock = identifier->resolveFrom->indexInParent;

						if (identifier->resolveFrom->flags & BLOCK_IS_ARGUMENTS)
							identifier->flags |= EXPR_IDENTIFIER_RESOLVING_IN_OUTER_FUNCTION;
					}

					if (!identifier->resolveFrom && !identifier->declaration) { // If we have checked all the local scopes and the
						u64 index;
						identifier->declaration = findDeclaration(&globalBlock, identifier->name, &index);
					}
				}

				if (identifier->declaration) {
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
							identifier->type = getDeclarationType(identifier->declaration);
						}
						else {
							return true;
						}

						if (identifier->declaration->flags & DECLARATION_IS_CONSTANT) {
							if (identifier->declaration->flags & DECLARATION_VALUE_IS_READY) {
								*exprPointer = identifier->declaration->initialValue;
							}
							else {
								return true;
							}
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

				break;
			}
			case ExprFlavor::TYPE_LITERAL:
			case ExprFlavor::STRING_LITERAL:
			case ExprFlavor::FLOAT_LITERAL:
			case ExprFlavor::INT_LITERAL:
			case ExprFlavor::BLOCK: {
				break;
			}
			case ExprFlavor::BREAK:
			case ExprFlavor::CONTINUE: {
				auto continue_ = static_cast<ExprBreakOrContinue *>(expr);

				assert(continue_->flavor == ExprFlavor::IDENTIFIER);
				auto label = static_cast<ExprIdentifier *>(continue_->label);
				assert(label->declaration);

				if (!(label->declaration->flags & DECLARATION_IS_ITERATOR)) {
					reportError(continue_, "Error: %s label '%.*s' is not a loop iterator", continue_->flavor == ExprFlavor::CONTINUE ? "Continue" : "Break", STRING_PRINTF(label->name));
					return false;
				}

				continue_->refersTo = CAST_FROM_SUBSTRUCT(ExprLoop, iteratorBlock, label->declaration->enclosingScope);
			}
			case ExprFlavor::BINARY_OPERATOR: {
				auto binary = static_cast<ExprBinaryOperator *>(expr);

				auto &left = binary->left;
				auto &right = binary->right;

				assert(binary->op == TokenT::ARRAY_TYPE || left); // This is safe since even though auto-casts can have a left of null, they shouldn't be added to the flattened array


				assert(binary->op == TOKEN('=') || right);

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

						if (!isValidCast(castTo, right->type)) {
							reportError(binary, "Error: Cannot cast from %.*s to %.*s", STRING_PRINTF(right->type->name), STRING_PRINTF(castTo->name));
							return false;
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

							expr->type = static_cast<TypeArray *>(left->type)->arrayOf;
						}
						else {
							reportError(binary->left, "Error: Cannot index a %.*s", STRING_PRINTF(left->type->name));
							return false;
						}

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
									if (!binaryOpForFloat(binary)) {
										assert(false);
										return false;
									}
									break;
								}
								case TypeFlavor::FUNCTION: {
									if (left->type != right->type) {
										if (left->type == TYPE_VOID_POINTER) {
											insertImplicitCast(&right, left->type);
										}
										else if (right->type == TYPE_VOID_POINTER) {
											insertImplicitCast(&left, right->type);
										}
										else {
											reportError(binary, "Error: Cannot compare %.*s to %.*s", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
											return false;
										}
									}
								}
								case TypeFlavor::INTEGER: {
									if (!binaryOpForInteger(binary))
										return false;
									
									break;
								}
								case TypeFlavor::POINTER: {
									if (left->type != right->type) {
										if (left->type == TYPE_VOID_POINTER) {
											insertImplicitCast(&right, left->type);
										}
										else if (right->type == TYPE_VOID_POINTER) {
											insertImplicitCast(&left, right->type);
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
							if (!binaryOpForAutoCast(binary)) {
								return false;
							}
							else {
								binaryOpForFloatAndIntLiteral(binary);
							}

							if (right->type == left->type) {
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
									if (!binaryOpForFloat(binary))
										return false;
									break;
								}
								case TypeFlavor::FUNCTION: {
									reportError(binary, "Error: Cannot compare functions");
									return false;
								}
								case TypeFlavor::INTEGER: {
									if (!binaryOpForInteger(binary))
										return false;

									break;
								}
								case TypeFlavor::POINTER: {
									if (left->type != right->type) {
										if (left->type == TYPE_VOID_POINTER) {
											insertImplicitCast(&right, left->type);
										}
										else if (right->type == TYPE_VOID_POINTER) {
											insertImplicitCast(&left, right->type);
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
								default:
									assert(false);
							}
						}
						else {
							if (!binaryOpForAutoCast(binary)) {
								return false;
							}
							else {
								binaryOpForFloatAndIntLiteral(binary);
							}

							if (right->type == left->type) {
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
									if (!binaryOpForFloat(binary))
										return false;
									break;
								}
								case TypeFlavor::FUNCTION: {
									reportError(binary, "Error: Cannot %s functions", binary->op == TOKEN('+') ? "add" : "subtract");
									return false;
								}
								case TypeFlavor::INTEGER: {
									if (!binaryOpForInteger(binary))
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

									expr->type = &TYPE_S64;

									break;
								}
								case TypeFlavor::TYPE: {
									reportError(binary, "Error: Cannot %s types", binary->op == TOKEN('+') ? "add" : "subtract");
									return false;
								}
								default:
									assert(false);
							}
						}
						else {
							if (!binaryOpForAutoCast(binary)) {
								return false;
							}
							else if (left->type->flavor == TypeFlavor::POINTER && right->type->flavor == TypeFlavor::INTEGER) {
								trySolidifyNumericLiteralToDefault(right);
							}
							else {
								binaryOpForFloatAndIntLiteral(binary);
							}

							if (right->type == left->type) {
								reportError(binary, "Error: Cannot %s %.*s and %.*s",
									binary->op == TOKEN('+') ? "add" : "subtract", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
								return false;
							}
						}

						if (!expr->type) { // it is already set to s64 for pointer subtraction
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
									if (!binaryOpForInteger(binary))
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
								default:
									assert(false);
							}
						}
						else {
							if (!binaryOpForAutoCast(binary)) {
								return false;
							}

							if (right->type == left->type) {
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
									if (!binaryOpForInteger(binary))
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
								default:
									assert(false);
							}
						}
						else {
							if (!binaryOpForAutoCast(binary)) {
								return false;
							}

							if (right->type == left->type) {
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
									if (!binaryOpForFloat(binary))
										return false;
									break;
								}
								case TypeFlavor::FUNCTION: {
									reportError(binary, "Error: Cannot %s functions", opName);
									return false;
								}
								case TypeFlavor::INTEGER: {
									if (!binaryOpForInteger(binary))
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
								default:
									assert(false);
							}
						}
						else {
							if (!binaryOpForAutoCast(binary)) {
								return false;
							}
							else {
								binaryOpForFloatAndIntLiteral(binary);
							}


							if (right->type == left->type) {
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
							assert(binary->left->flavor == ExprFlavor::IDENTIFIER);
							binary->right = createDefaultValue(static_cast<ExprIdentifier *>(binary->left)->declaration, binary->left->type);
							
							if (!binary->right) {
								return false;
							}

							binary->right->start = binary->left->start;
							binary->right->end = binary->left->end;
							binary->right->valueOfDeclaration = static_cast<ExprIdentifier *>(binary->left)->declaration;

							assert(binary->left->type == binary->right->type);
						}
						else {
							if (!assignOp(binary, left->type, binary->right)) {
								return false;
							}

							assert(binary->left->type == binary->right->type);
						}

						break;
					}
					case TokenT::PLUS_EQUALS:
					case TokenT::MINUS_EQUALS: {
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
									if (!assignOpForInteger(binary))
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
								default:
									assert(false);
							}
						}
						else {
							if (!assignOpForAutoCast(binary)) {
								return false;
							}
							else {
								assignOpForFloatAndIntLiteral(binary);

							}
							if (left->type->flavor == TypeFlavor::POINTER && right->type->flavor == TypeFlavor::INTEGER) {
								trySolidifyNumericLiteralToDefault(right);
							}
							else if (right->type == left->type) {
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
								insertImplicitCast(&left, &TYPE_BOOL);
							}
							else {
								reportError(binary->left, "Error: Cannot convert %.*s to bool", STRING_PRINTF(left->type->name));
								return false;
							}
						}

						if (right->type != &TYPE_BOOL) {
							if (isValidCast(&TYPE_BOOL, right->type)) {
								insertImplicitCast(&right, &TYPE_BOOL);
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
									if (!assignOpForInteger(binary))
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
								default:
									assert(false);
							}
						}
						else {
							if (!assignOpForAutoCast(binary)) {
								return false;
							}

							if (right->type == left->type) {
								reportError(binary, "Error: Cannot %s %.*s and %.*s", opName, STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
								return false;
							}
						}

						expr->type = left->type;

						break;
					}
					case TokenT::SHIFT_LEFT_EQUALS:
					case TokenT::SHIFT_RIGHT_EQUALS: {
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
									if (!assignOpForInteger(binary))
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
								default:
									assert(false);
							}
						}
						else {
							if (!assignOpForAutoCast(binary)) {
								return false;
							}
							else {
								
							}
							
							if (right->type == left->type) {
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
									if (!assignOpForInteger(binary))
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
								default:
									assert(false);
							}
						}
						else {
							if (!assignOpForAutoCast(binary)) {
								assert(false);
							}
							else {
								assignOpForFloatAndIntLiteral(binary);
							}


							if (right->type == left->type) {
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
						}
						else {
							if (expr->flags & EXPR_ARRAY_IS_DYNAMIC) {
								array = getDynamicArray(type->typeValue);
							}
							else {
								array = getArray(type->typeValue);
							}
						}

						type->typeValue = array;
						*exprPointer = type;

						break;
					}
					default:
						assert(false);
						return false;
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
						if (!tryAutoCast(&loop->forBegin, loop->forEnd->type))
							return false;
					}
					else if (loop->forEnd->type == &TYPE_AUTO_CAST) {
						if (!tryAutoCast(&loop->forEnd, loop->forBegin->type))
							return false;
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
								insertImplicitCast(&loop->forEnd, loop->forBegin->type);
							}
							else if (loop->forBegin->type->size < loop->forEnd->type->size) {
								insertImplicitCast(&loop->forBegin, loop->forEnd->type);
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
									STRING_PRINTF(loop->forBegin ->type->name), STRING_PRINTF(loop->forEnd->type->name));
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

				String functionName = call->function->valueOfDeclaration ? call->function->valueOfDeclaration->name : "function";

				ExprFunction *functionForArgumentNames = nullptr;

				if (call->function->flavor == ExprFlavor::FUNCTION) {
					functionForArgumentNames = static_cast<ExprFunction *>(call->function);
				}
				else if (call->function->valueOfDeclaration && call->function->valueOfDeclaration->initialValue && call->function->valueOfDeclaration->initialValue->flavor == ExprFlavor::FUNCTION) {
					functionForArgumentNames = static_cast<ExprFunction *>(call->function->valueOfDeclaration->initialValue);
				}


				if (call->argumentCount != function->argumentCount) {

					if (call->argumentCount < function->argumentCount) {
						reportError(call, "Error: Too few arguments for %.*s, (Expected: %" PRIu64 "Given: %" PRIu64 ")", 
							STRING_PRINTF(functionName), function->argumentCount, call->argumentCount);

						if (functionForArgumentNames) {
							reportError(functionForArgumentNames->arguments.declarations[call->argumentCount], "   ..: Here are the missing arguments");
						}
					}
					else if (call->argumentCount > function->argumentCount) {
						reportError(call, "Error: Too many arguments for %.*s, (Expected: %" PRIu64 "Given: %" PRIu64 ")",
							STRING_PRINTF(functionName), function->argumentCount, call->argumentCount);
					}

					return false;
				}

				for (u64 i = 0; i < call->argumentCount; i++) {
					Type *correct = function->argumentTypes[i];


					if (!assignOp(call->arguments[i], correct, call->arguments[i])) { // @Incomplete: Give function call specific error messages instead of general type conversion errors
						return false;
					}
				}

				break;
			}
			case ExprFlavor::IF: {
				auto ifElse = static_cast<ExprIf *>(expr);

				if (ifElse->condition->type != &TYPE_BOOL) {
					if (isValidCast(&TYPE_BOOL, ifElse->condition->type)) {
						insertImplicitCast(&ifElse->condition, &TYPE_BOOL);
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

					if (!assignOp(return_, returnType, return_->value)) {
						return false;
					}
				}

				break;
			}
			case ExprFlavor::STRUCT_ACCESS: {
				auto access = static_cast<ExprStructAccess *>(expr);

				if (access->left->type->flavor == TypeFlavor::ARRAY) {
					auto array = static_cast<TypeArray *>(access->left->type);

					if (access->name == "count") {
						if (array->flags & TYPE_ARRAY_IS_FIXED) {
							ExprLiteral *literal = new ExprLiteral;
							literal->flavor = ExprFlavor::INT_LITERAL;
							literal->start = access->start;
							literal->end = access->end;
							literal->type = &TYPE_U64;
							literal->unsignedValue = array->count;

							*exprPointer = literal;
						}
						else {
							expr->type = &TYPE_U64;
						}
					}
					else if (access->name == "data") {
						if (array->flags & TYPE_ARRAY_IS_FIXED) {
							*exprPointer = access->left;
							insertImplicitCast(exprPointer, getPointer(array->arrayOf));
						}
						else {
							expr->type = getPointer(array->arrayOf);
						}
					}
					else if (access->name == "capacity") {
						if (array->flags & TYPE_ARRAY_IS_DYNAMIC) {
							expr->type = &TYPE_U64;
						}
						else {
							reportError(access, "Error: %s do not have a capacity field", (array->flags & TYPE_ARRAY_IS_FIXED) ? "Static arrays" : "Arrays");
							return false;
						}
					}
					else {
						reportError(access, "Error: Arrays do not have field '%.*s'", STRING_PRINTF(access->name));
						return false;
					}
				}
				else if (access->left->type->flavor == TypeFlavor::POINTER) {
					auto type = static_cast<TypePointer *>(access->left->type)->pointerTo;

					if (type->flavor == TypeFlavor::ARRAY) {
						auto array = static_cast<TypeArray *>(type);

						if (access->name == "count") {
							if (array->flags & TYPE_ARRAY_IS_FIXED) {
								ExprLiteral *literal = new ExprLiteral;
								literal->flavor = ExprFlavor::INT_LITERAL;
								literal->start = access->start;
								literal->end = access->end;
								literal->type = &TYPE_U64;
								literal->unsignedValue = array->count;

								*exprPointer = literal;
							}
							else {
								expr->type = &TYPE_U64;
							}
						}
						else if (access->name == "data") {
							if (array->flags & TYPE_ARRAY_IS_FIXED) {
								*exprPointer = access->left;
								insertImplicitCast(exprPointer, getPointer(array->arrayOf));
							}
							else {
								expr->type = getPointer(array->arrayOf);
							}
						}
						else if (access->name == "capacity") {
							if (array->flags & TYPE_ARRAY_IS_DYNAMIC) {
								expr->type = &TYPE_U64;
							}
							else {
								reportError(access, "Error: %s do not have a capacity field", (array->flags & TYPE_ARRAY_IS_FIXED) ? "Static arrays" : "Arrays");
								return false;
							}
						}
						else {
							reportError(access, "Error: Arrays do not have field '%.*s'", STRING_PRINTF(access->name));
							return false;
						}
					}
					else {
						reportError(access, "Error: A %.*s does not have fields", STRING_PRINTF(access->left->type->name));
						return false;
					}
				}
				else if (access->left->type->flavor == TypeFlavor::TYPE) {
					if (access->left->flavor != ExprFlavor::TYPE_LITERAL) {
						reportError(access, "Error: Cannot access the fields of a non-constant type");
						return false;
					}

					auto type = static_cast<ExprLiteral *>(access->left)->typeValue;

					if (type->flavor == TypeFlavor::ARRAY) {
						auto array = static_cast<TypeArray *>(type);

						if (type->flags & TYPE_ARRAY_IS_FIXED) {
							if (access->name == "count") {
								ExprLiteral *literal = new ExprLiteral;
								literal->flavor = ExprFlavor::INT_LITERAL;
								literal->start = access->start;
								literal->end = access->end;
								literal->type = &TYPE_U64;
								literal->unsignedValue = array->count;

								*exprPointer = literal;
							}
							else {
								reportError(access, "Error: Static array prototypes do not have field '%.*s'", STRING_PRINTF(access->name));
								return false;
							}
						}
						else {
							reportError(access, "Error: %s prototypes do not have field '%.*s'", 
								(array->flags & TYPE_ARRAY_IS_DYNAMIC) ? "Dynamic array" : "Array", STRING_PRINTF(access->name));
							return false;
						}
					}
					else {
						reportError(access, "Error: A %.*s prototype does not have fields", STRING_PRINTF(type->name));
						return false;
					}
				}
				else {
					reportError(access, "Error: A %.*s does not have fields", STRING_PRINTF(access->left->type->name));
					return false;
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
								else {
									reportError(value, "Error: Cannot negate an unsigned integer");
									return false;
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
						else {
							reportError(value, "Error: Cannot invert a %.*s", STRING_PRINTF(value->type->name));
							return false;
						}
					}
					case TOKEN('!'): {
						if (value->type != &TYPE_BOOL) {
							if (isValidCast(&TYPE_BOOL, value->type)) {
								insertImplicitCast(&value, &TYPE_BOOL);
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
						
						assert(type->typeValue->size); // @Incomplete structs

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

						break;
					}
					case TOKEN('*'): {
						if (value->flavor == ExprFlavor::TYPE_LITERAL) {
							auto literal = static_cast<ExprLiteral *>(value);

							auto pointer = getPointer(literal->typeValue);

							literal->typeValue = pointer;

							literal->start = unary->start;

							*exprPointer = literal;
						}
						else if (value->flavor == ExprFlavor::BINARY_OPERATOR) {
							auto binary = static_cast<ExprBinaryOperator *>(expr);

							if (binary->op == TOKEN('[')) {
								auto pointer = getPointer(value->type);

								unary->type = pointer;
							}
							else {
								reportError(value, "Error: Cannot take an addres to something that has not storage");
								return false;
							}
						}
						else if (value->flavor == ExprFlavor::STRUCT_ACCESS) {
							assert(false); // @Incomplete structs
							return false;
						}
						else if (value->flavor == ExprFlavor::IDENTIFIER) {
							auto pointer = getPointer(value->type);

							unary->type = pointer;
						}
						else {
							reportError(value, "Error: Cannot take an addres to something that has not storage");
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
						insertImplicitCast(&loop->whileCondition, &TYPE_BOOL);
					}
					else {
						reportError(loop->whileCondition, "Error: Cannot convert %.*s to bool", STRING_PRINTF(loop->whileCondition->type->name));
						return false;
					}
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

			return true;
		}
	}

	return false;
}

static BucketedArenaAllocator inferJobAllocator(1024 * 2024);
static InferJob *firstFreeInferJob;

void freeJob(InferJob *job) {
	job->nextFree = firstFreeInferJob;
	firstFreeInferJob = job;
}

InferJob *allocateJob() {
	if (firstFreeInferJob) {
		auto result = firstFreeInferJob;
		firstFreeInferJob = firstFreeInferJob->nextFree;

		result->typeFlattened.clear();
		result->typeFlattenedIndex = 0;
		result->valueFlattened.clear();
		result->valueFlattenedIndex = 0;

		return result;
	}

	return new (inferJobAllocator.allocate(sizeof(InferJob))) InferJob;
}

bool addDeclaration(Declaration *declaration) {
	if (declaration->flags & (DECLARATION_IS_ITERATOR | DECLARATION_IS_ITERATOR_INDEX))
		return true;

	if (!declaration->enclosingScope) {
		if (!addDeclarationToBlock(&globalBlock, declaration)) {
			return false;
		}
	}

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
			} else if (argument->initialValue && !(argument->flags & DECLARATION_VALUE_IS_READY)) {
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

void runInfer() {
	PROFILE_FUNC();
	bool exit = false;

	while (!exit) {
		DeclarationPack declarations = inferQueue.take();

		if (!declarations.count && !declarations.data.function) {
			exit = true;
		} else if (declarations.count == 0) {
			InferJob *body = allocateJob();
			body->infer.function = declarations.data.function;
			body->type = InferType::FUNCTION_BODY;

			if (body->infer.function->body) {
				flatten(body->valueFlattened, &body->infer.function->body);
			}

			inferJobs.add(body);
		}
		else if (declarations.count == 1) {
			if (!addDeclaration(declarations.data.declaration)) {
				goto error;
			}
		}
		else {
			inferJobs.reserve(inferJobs.count + declarations.count); // Make sure we don't do unnecessary allocations

			for (u64 i = 0; i < declarations.count; i++)
				if (!addDeclaration(declarations.data.declarations[i])) {
					goto error;
				}
		}

		bool madeProgress;

		do {
			madeProgress = false;

			for (u64 i = 0; i < inferJobs.count; i++) {

				auto job = inferJobs[i];

				switch (job->type) {
					case InferType::FUNCTION_BODY: {
						if (!inferFlattened(job->valueFlattened, &job->valueFlattenedIndex)) {
							goto error;
						}

						if (job->infer.function->type) {
							if (job->infer.function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
								assert(!job->infer.function->body);

								if (!job->infer.declaration) {
									reportError(job->infer.function, "Error: External functions must be named");
									goto error;
								}

								CoffJob coff;
								coff.isFunction = true;
								coff.function = job->infer.function;

								coffWriterQueue.add(coff);

								inferJobs.unordered_remove(i--);
								freeJob(job);
							}
							else {
								assert(job->infer.function->body);

								if (job->valueFlattenedIndex == job->valueFlattened.count) {
									irGeneratorQueue.add(job->infer.function);

									inferJobs.unordered_remove(i--);
									freeJob(job);
								}
							}
						}
						break;
					}
					case InferType::DECLARATION: {
						auto declaration = job->infer.declaration;

						if (declaration->type) {
							if (!inferFlattened(job->typeFlattened, &job->typeFlattenedIndex)) {
								goto error;
							}
						}

						if (declaration->initialValue) {
							if (!inferFlattened(job->valueFlattened, &job->valueFlattenedIndex)) {
								goto error;
							}
						}

						if (!(declaration->flags & DECLARATION_TYPE_IS_READY) && declaration->type && job->typeFlattenedIndex == job->typeFlattened.count) {
							if (declaration->type->type != &TYPE_TYPE) {
								reportError(declaration->type, "Error: Declaration type must be a type");
								goto error;
							}
							if (declaration->type->flavor != ExprFlavor::TYPE_LITERAL) {
								reportError(declaration->type, "Error: Declaration type must be a constant");
								goto error;
							}

							if (static_cast<ExprLiteral *>(declaration->type)->typeValue == &TYPE_VOID) {
								reportError(declaration->type, "Error: Declaration cannot have type void");
								goto error;
							}

							madeProgress = true;
							declaration->flags |= DECLARATION_TYPE_IS_READY;
						}

						if (declaration->type && declaration->initialValue) {
							if (job->typeFlattenedIndex == job->typeFlattened.count && job->valueFlattenedIndex == job->valueFlattened.count) {
								assert(declaration->initialValue->type);

								Type *correct = static_cast<ExprLiteral *>(declaration->type)->typeValue;
								
								if (!assignOp(declaration->initialValue, correct, declaration->initialValue)) {
									goto error;
								}

								if ((declaration->flags & DECLARATION_IS_CONSTANT) || declaration->enclosingScope == &globalBlock) {
									if (!isLiteral(declaration->initialValue)) {
										reportError(declaration->type, "Error: Declaration value must be a constant");
										goto error;
									}
								}

								madeProgress = true;

								inferJobs.unordered_remove(i--);
								freeJob(job);

								declaration->flags |= DECLARATION_VALUE_IS_READY;

								if (!(declaration->flags & DECLARATION_IS_CONSTANT) && declaration->enclosingScope == &globalBlock) {
									CoffJob job;
									job.declaration = declaration;
									job.isFunction = false;
									coffWriterQueue.add(job);
								}
							}
						}
						else if (declaration->type) {
							if (job->typeFlattenedIndex == job->typeFlattened.count) {
								if (!(declaration->flags & (DECLARATION_IS_UNINITIALIZED | DECLARATION_IS_ARGUMENT | DECLARATION_IS_ITERATOR_INDEX | DECLARATION_IS_ITERATOR)) && declaration->enclosingScope == &globalBlock /* Declarations in local scope will be initialized in inferFlattened for the assign op they generate */) {
									auto type = static_cast<ExprLiteral *>(declaration->type)->typeValue;

									declaration->initialValue = createDefaultValue(declaration, type);
									declaration->initialValue->valueOfDeclaration = declaration;

									if (!declaration->initialValue) {
										goto error;
									}

									declaration->initialValue->start = declaration->start;
									declaration->initialValue->end = declaration->end;
								}

								madeProgress = true;

								inferJobs.unordered_remove(i--);
								freeJob(job);

								declaration->flags |= DECLARATION_VALUE_IS_READY;


								if (!(declaration->flags & DECLARATION_IS_CONSTANT) && declaration->enclosingScope == &globalBlock) {
									CoffJob job;
									job.declaration = declaration;
									job.isFunction = false;
									coffWriterQueue.add(job);
								}
							}
						}
						else if (declaration->initialValue) {
							if (job->valueFlattenedIndex == job->valueFlattened.count) {
								assert(declaration->initialValue->type);

								trySolidifyNumericLiteralToDefault(declaration->initialValue);

								if (declaration->initialValue->type == &TYPE_VOID) {
									reportError(declaration->type, "Error: Declaration cannot have type void");
									goto error;
								}
								else if (declaration->initialValue->type == &TYPE_AUTO_CAST) {
									reportError(declaration->type, "Error: Cannot infer the type of a declaration if the value is an auto cast");
									goto error;
								}

								declaration->type = inferMakeTypeLiteral(declaration->start, declaration->end, declaration->initialValue->type);

								madeProgress = true;


								inferJobs.unordered_remove(i--);
								freeJob(job);

								declaration->flags |= DECLARATION_VALUE_IS_READY;
								declaration->flags |= DECLARATION_TYPE_IS_READY;


								if (!(declaration->flags & DECLARATION_IS_CONSTANT) && declaration->enclosingScope == &globalBlock) {
									CoffJob job;
									job.declaration = declaration;
									job.isFunction = false;
									coffWriterQueue.add(job);
								}
							}
						}

						break;
					}
				}
			}
		} while (madeProgress && inferJobs.count);
	}

	if (inferJobs.count && !hadError) { // We didn't complete type inference, check for undeclared identifiers or circular dependencies! If we already had an error don't spam more
		// Check for undeclared identifiers
		
		for (auto job : inferJobs) {
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
			const char *type = job->type == InferType::DECLARATION ? "declaration" : "function body";

			String name = job->type == InferType::DECLARATION ? job->infer.declaration->name :
				(job->infer.function->valueOfDeclaration ? job->infer.function->valueOfDeclaration->name : "(function)");

			if (job->typeFlattenedIndex != job->typeFlattened.count) {
				auto haltedOn = *job->typeFlattened[job->typeFlattenedIndex];

				reportError(haltedOn, "%.*s %s type halted here", type, STRING_PRINTF(name));
			}

			if (job->valueFlattenedIndex != job->valueFlattened.count) {
				auto haltedOn = *job->valueFlattened[job->valueFlattenedIndex];

				reportError(haltedOn, "%.*s %s value halted here", type, STRING_PRINTF(name));
			}
		}
	}


error:;
	irGeneratorQueue.add(nullptr);
}