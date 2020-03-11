#include "Basic.h"
#include "Infer.h"
#include "Array.h"
#include "Parser.h"
#include "Lexer.h"
#include "IrGenerator.h"

enum class InferType {
	// FUNCTION_HEAD, 
	FUNCTION_BODY, 
	DECLARATION
};

Block globalBlock;

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
};

WorkQueue<DeclarationPack> inferQueue;

static Array<InferJob> inferJobs;


void flatten(Array<Expr **> &flattenTo, Expr **expr) {
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
		case ExprFlavor::TYPE_LITERAL: {
			ExprLiteral *literal = static_cast<ExprLiteral *>(*expr);

			Type *type = literal->typeValue;


			if (type->flavor == TypeFlavor::FUNCTION) {
				TypeFunction *function = static_cast<TypeFunction *>(type);

				flatten(flattenTo, &function->returnType);

				for (u64 i = 0; i < function->argumentCount; i++) {
					flatten(flattenTo, &function->argumentTypes[i]);
				}

				flattenTo.add(expr);
			}

			break;
		}
		case ExprFlavor::STRING_LITERAL:
		case ExprFlavor::FLOAT_LITERAL:
		case ExprFlavor::INT_LITERAL:
		case ExprFlavor::BREAK:
		case ExprFlavor::CONTINUE:
			break;
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

bool addDeclaration(Declaration *declaration) {
	if (declaration->flags & (DECLARATION_IS_ITERATOR | DECLARATION_IS_ITERATOR_INDEX))
		return true;

	if (!declaration->enclosingScope) {
		if (!addDeclarationToBlock(&globalBlock, declaration)) {
			return false;
		}
	}

	InferJob job;
	job.type = InferType::DECLARATION;
	job.infer.declaration = declaration;

	if (declaration->type) {
		flatten(job.typeFlattened, &declaration->type);
	}

	if (declaration->initialValue) {
		flatten(job.valueFlattened, &declaration->initialValue);
	}

	inferJobs.add(job);

	return true;
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
bool boundsCheckImplicitConversion(Type *convertTo, ExprLiteral *convertFrom) {
	if (convertFrom->type == &TYPE_UNSIGNED_INT_LITERAL) {
		if (convertTo == &TYPE_U64) {
			return true;
		}
		else if (convertTo == &TYPE_U32) {
			return convertFrom->unsignedValue <= static_cast<u64>(std::numeric_limits<u32>::max());
		}
		else if (convertTo == &TYPE_U16) {
			return convertFrom->unsignedValue <= static_cast<u64>(std::numeric_limits<u16>::max());
		}
		else if (convertTo == &TYPE_U8) {
			return convertFrom->unsignedValue <= static_cast<u64>(std::numeric_limits<u8>::max());
		}
		else if (convertTo == &TYPE_S64) {
			return convertFrom->unsignedValue <= static_cast<u64>(std::numeric_limits<s64>::max());
		}
		else if (convertTo == &TYPE_S32) {
			return convertFrom->unsignedValue <= static_cast<u64>(std::numeric_limits<s32>::max());
		}
		else if (convertTo == &TYPE_S16) {
			return convertFrom->unsignedValue <= static_cast<u64>(std::numeric_limits<s16>::max());
		}
		else if (convertTo == &TYPE_S8) {
			return convertFrom->unsignedValue <= static_cast<u64>(std::numeric_limits<s8>::max());
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
			return convertFrom->signedValue <= static_cast<s64>(std::numeric_limits<s32>::max()) && 
				convertFrom->signedValue >= static_cast<s64>(std::numeric_limits<s32>::min());
		}
		else if (convertTo == &TYPE_S16) {
			return convertFrom->signedValue <= static_cast<s64>(std::numeric_limits<s16>::max()) &&
				convertFrom->signedValue >= static_cast<s64>(std::numeric_limits<s16>::min());
		}
		else if (convertTo == &TYPE_S8) {
			return convertFrom->signedValue <= static_cast<s64>(std::numeric_limits<s8>::max()) &&
				convertFrom->signedValue >= static_cast<s64>(std::numeric_limits<s8>::min());
		}
		else {
			assert(false);
			return false;
		}
	}
}

bool solidifyOneLiteralDriver(Expr *left, Expr *right, bool *success) {
	assert(left->type != right->type);
	*success = true;

	if (left->type == &TYPE_FLOAT_LITERAL) {
		assert(right->type->flavor == TypeFlavor::FLOAT);

		left->type = right->type;
	}
	else if (left->type == &TYPE_UNSIGNED_INT_LITERAL) {
		assert(right->type->flavor == TypeFlavor::INTEGER || right->type->flavor == TypeFlavor::FLOAT);

		if (right->type->flavor == TypeFlavor::INTEGER) {
			if (!boundsCheckImplicitConversion(right->type, static_cast<ExprLiteral *>(left))) {
				assert(false); // @ErrorMessage cannot implicitly cast, value out of range
				*success = false;
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
			assert(false); // @ErrorMessage signed-unsigned mismatch
			*success = false;
		}

		if (right->type->flavor == TypeFlavor::INTEGER) {
			if (!boundsCheckImplicitConversion(right->type, static_cast<ExprLiteral *>(left))) {
				assert(false); // @ErrorMessage cannot implicitly cast, value out of range
				*success = false;
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
	else {
		return false;
	}

	return true;
}


bool solidifyOneLiteral(ExprBinaryOperator *binary) {
	bool success;

	if (solidifyOneLiteralDriver(binary->left, binary->right, &success)) return success;
	if (solidifyOneLiteralDriver(binary->right, binary->left , &success)) return success;

	return false;
}

bool isVoidPointer(Type *type) {
	if (type->flavor != TypeFlavor::POINTER) return false;

	return static_cast<TypePointer *>(type)->pointerTo == &TYPE_VOID;
}


bool isValidCast(Type *to, Type *from) {
	if (to->flavor == from->flavor) {
		return true;
	}

	if (from->flavor == TypeFlavor::BOOL) {
		return to->flavor == TypeFlavor::INTEGER;
	}
	else if (from->flavor == TypeFlavor::AUTO_CAST || from->flavor == TypeFlavor::TYPE || from->flavor == TypeFlavor::VOID) {
		return false;
	}
	else if (from->flavor == TypeFlavor::FLOAT) {
		return to->flavor == TypeFlavor::BOOL || to->flavor == TypeFlavor::INTEGER;
	}
	else if (from->flavor == TypeFlavor::FUNCTION) {
		return to->flavor == TypeFlavor::BOOL || (to->flavor == TypeFlavor::INTEGER && to->size == 8) || isVoidPointer(to);
	}
	else if (from->flavor == TypeFlavor::INTEGER) {
		return to->flavor == TypeFlavor::BOOL || to->flavor == TypeFlavor::FLOAT || (from->size == 8 && (to->flavor == TypeFlavor::FUNCTION || to->flavor == TypeFlavor::POINTER));
	}
	else if (from->flavor == TypeFlavor::POINTER) {
		return to->flavor == TypeFlavor::BOOL || (to->flavor == TypeFlavor::INTEGER && to->size == 8);
	}
	else {
		assert(false); // @ErrorMessage
		return false;
	}
}

void insertImplicitCast(Expr **castFrom, Type *castTo) {
	ExprBinaryOperator *cast = new ExprBinaryOperator;
	cast->flavor = ExprFlavor::BINARY_OPERATOR;
	cast->type = castTo;
	cast->right = *castFrom;
	cast->start = cast->right->start;
	cast->end = cast->right->end;
	cast->flags |= EXPR_CAST_IS_IMPLICIT;
	cast->left = makeTypeLiteral(cast->right->start, cast->right->end, castTo);

	*castFrom = cast;

	assert(isValidCast(castTo, cast->right->type));
}

bool tryAutoCast(Expr *castExpr, Type *castTo) {
	assert(castExpr->type->flavor == TypeFlavor::AUTO_CAST);
	assert(castExpr->flavor == ExprFlavor::BINARY_OPERATOR);

	auto autoCast = static_cast<ExprBinaryOperator *>(castExpr);

	auto &castFrom = autoCast->right->type;

	assert(!autoCast->left);

	trySolidifyNumericLiteralToDefault(autoCast->right);

	if (castTo->flavor == TypeFlavor::VOID) {
		assert(false); // @ErrorMessage
		return false;
	}

	if (castFrom->flavor == TypeFlavor::AUTO_CAST) {
		assert(false); // @ErrorMessage
		return false;
	}

	if (castFrom->flavor == TypeFlavor::TYPE) {
		assert(false); // @ErrorMessage
		return false;
	}


	if (castTo->flavor == TypeFlavor::TYPE) {
		assert(false); // @ErrorMessage
		return false;
	}


	assert(!(castTo->flags & TYPE_IS_INTERNAL));

	autoCast->type = castTo;
	autoCast->left = makeTypeLiteral(autoCast->start, autoCast->end, castTo);

	return isValidCast(castTo, castFrom);
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
			assert(false); // @ErrorMessage cannot implicitly convert f32 and f64
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
			assert(false); // @ErrorMessage cannot implicitly convert f32 and f64
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
				if (!boundsCheckImplicitConversion(right->type, static_cast<ExprLiteral *>(left))) {
					assert(false); // @ErrorMessage
					return false;
				}

				left->type = right->type;
			}
			else if (right->type == &TYPE_UNSIGNED_INT_LITERAL || right->type == &TYPE_SIGNED_INT_LITERAL) {
				if (!boundsCheckImplicitConversion(left->type, static_cast<ExprLiteral *>(right))) {
					assert(false); // @ErrorMessage
					return false;
				}
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

				if (!boundsCheckImplicitConversion(right->type, static_cast<ExprLiteral *>(left))) {
					assert(false); // @ErrorMessage
					return false;
				}

				left->type = right->type;
			}
			else if ((right->type == &TYPE_UNSIGNED_INT_LITERAL) && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
				if (left->type == &TYPE_SIGNED_INT_LITERAL) {
					left->type = &TYPE_S64;
				}

				if (!boundsCheckImplicitConversion(left->type, static_cast<ExprLiteral *>(right))) {
					assert(false); // @ErrorMessage
					return false;
				}

				right->type = left->type;
			}
			else {
				assert(false); // @ErrorMessage signed-unsigned mismatch
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
				if (!boundsCheckImplicitConversion(left->type, static_cast<ExprLiteral *>(right))) {
					assert(false); // @ErrorMessage
					return false;
				}
			}
			else if (left->type->size > right->type->size) {
				insertImplicitCast(&right, left->type);
			}
			else if (right->type->size < left->type->size) {
				assert(false); // @ErrorMessage
				return false;
			}
		}
		else {
			if ((right->type == &TYPE_UNSIGNED_INT_LITERAL) && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
				if (!boundsCheckImplicitConversion(left->type, static_cast<ExprLiteral *>(right))) {
					assert(false); // @ErrorMessage
					return false;
				}

				right->type = left->type;
			}
			else {
				assert(false); // @ErrorMessage signed-unsigned mismatch
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

		if (!tryAutoCast(left, right->type)) {
			return false;
		}

		return true;
	}
	else if (right->type->flavor == TypeFlavor::AUTO_CAST) {
		trySolidifyNumericLiteralToDefault(left);

		if (!tryAutoCast(right, left->type)) {
			return false;
		}

		return true;
	}

	return true;
}

bool assignOpForAutoCast(ExprBinaryOperator *binary) {
	auto &left = binary->left;
	auto &right = binary->right;

	if (left->type->flavor == TypeFlavor::AUTO_CAST) {
		assert(false); // @ErrorMessage
		return false;
	}
	else if (right->type->flavor == TypeFlavor::AUTO_CAST) {
		if (!tryAutoCast(right, left->type)) {
			return false;
		}

		return true;
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
		expr->flavor == ExprFlavor::STRUCT_ACCESS || 
		(expr->flavor == ExprFlavor::UNARY_OPERATOR && static_cast<ExprUnaryOperator *>(expr)->op == TokenT::SHIFT_LEFT) ||
		(expr->flavor == ExprFlavor::BINARY_OPERATOR && static_cast<ExprBinaryOperator *>(expr)->op == TOKEN('['));
}

bool isLiteral(Expr *expr) {
	return expr->flavor == ExprFlavor::INT_LITERAL || expr->flavor == ExprFlavor::FLOAT_LITERAL || expr->flavor == ExprFlavor::TYPE_LITERAL || expr->flavor == ExprFlavor::STRING_LITERAL || expr->flavor == ExprFlavor::FUNCTION;
}


bool inferFlattened(Array<Expr **> &flattened, u64 *index) {
	for (; *index < flattened.count; ++ *index) {
		auto exprPointer = flattened[*index];
		auto expr = *exprPointer;

		switch (expr->flavor) {
			case ExprFlavor::IDENTIFIER: {

				auto identifier = static_cast<ExprIdentifier *>(expr);
				if (!identifier->declaration) {
					for (; identifier->resolveFrom; identifier->resolveFrom = identifier->resolveFrom->parentBlock) {
						if (!(identifier->resolveFrom->flags & BLOCK_IS_COMPLETE)) break;

						for (auto declaration : identifier->resolveFrom->declarations) {
							if (declaration->name == identifier->name) {
								if (declaration->flags & DECLARATION_IS_CONSTANT) {
									identifier->declaration = declaration;
									break;
								}
								else {
									assert(false); // @ErrorMessge Identifier cannot refer to a variable that was declared later in a block
									return false;
								}
							}
						}

						if (identifier->declaration) break;
					}

					if (!identifier->resolveFrom && !identifier->declaration) { // If we have checked all the local scopes and the 
						for (auto declaration : globalBlock.declarations) {
							if (declaration->name == identifier->name) {
								identifier->declaration = declaration;
								break;
							}
						}
					}
				}

				if (identifier->declaration) {
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
				else {
					return true;
				}

				break;
			}
			case ExprFlavor::FUNCTION: {
				auto function = static_cast<ExprFunction *>(expr);

				if (function->returnType->flavor != ExprFlavor::TYPE_LITERAL) {
					assert(false); // @ErrorMessage return type must be a type
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
							if (!isLiteral(argument->type)) {
								assert(false); // @ErrorMessage, default argument must be a constant
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
						type->argumentTypes = new Expr * [type->argumentCount];

						for (u64 i = 0; i < type->argumentCount; i++) {
							auto arg = static_cast<ExprLiteral *>(function->arguments.declarations[i]->type);

							assert(arg->flavor == ExprFlavor::TYPE_LITERAL);

							type->argumentTypes[i] = arg;
						}
					}

					type->flavor = TypeFlavor::FUNCTION;
					type->size = 8;
					type->alignment = 8;
					type->returnType = function->returnType;

					function->type = type;
				}
				else {
					return true;
				}
				
				break;
			}
			case ExprFlavor::TYPE_LITERAL:
			case ExprFlavor::STRING_LITERAL:
			case ExprFlavor::FLOAT_LITERAL:
			case ExprFlavor::INT_LITERAL:
			case ExprFlavor::BREAK:
			case ExprFlavor::CONTINUE:
			case ExprFlavor::BLOCK: {
				break;
			}
			case ExprFlavor::BINARY_OPERATOR: {
				auto binary = static_cast<ExprBinaryOperator *>(expr);

				auto &left = binary->left;
				auto &right = binary->right;

				assert(left); // This is safe since even though auto-casts can have a left of null, they shouldn't be added to the flattened array


				// assert(right); We can't do this since default initialization uses a assign operation with rhs of null

				if (left->type->flavor == right->type->flavor && left->type->flavor == TypeFlavor::AUTO_CAST) {
					assert(false); // @ErrorMessage cannot auto-cast both sides of a binary operator
					return false;
				}

				if (left->type->flavor == TypeFlavor::VOID) {
					assert(false); // @ErrorMessage
					return false;
				}


				if (right && right->type->flavor == TypeFlavor::VOID) {
					assert(false); // @ErrorMessage
					return false;
				}

				switch (binary->op) {
					case TokenT::CAST: {
						if (left->flavor != ExprFlavor::TYPE_LITERAL) {
							assert(false); // @ErrorMessage
							return false;
						}
						
						assert(left->flavor == ExprFlavor::TYPE_LITERAL);

						Type *castTo = static_cast<ExprLiteral *>(left)->typeValue;

						if (!isValidCast(castTo, right->type)) {
							return false;
						}

						expr->type = castTo;

						break;
					}
					case TOKEN('['): {
						if (left->type->flavor == TypeFlavor::POINTER) {
							TypePointer *pointer = static_cast<TypePointer *>(left->type);

							if (pointer->pointerTo == &TYPE_VOID) {
								assert(false); // @ErrorMessage cannot dereference a *void
								return false;
							}

							assert(!(pointer->pointerTo->flags & TYPE_IS_INTERNAL));

							trySolidifyNumericLiteralToDefault(right);

							if (right->type->flavor != TypeFlavor::INTEGER) {
								assert(false); // @ErrorMessage array index must be an integer
								return false;
							}

							expr->type = pointer->pointerTo;
						}
						else {
							assert(false); // @ErrorMessage can only index pointers
							return false;
						}

						break;
					}
					case TokenT::EQUAL:
					case TokenT::NOT_EQUAL: {
						if (left->type->flavor == right->type->flavor) {
							switch (left->type->flavor) {
								case TypeFlavor::BOOL: {
									break;
								}
								case TypeFlavor::FLOAT: {
									if (!binaryOpForFloat(binary)) 
										return false;
									break;
								}
								case TypeFlavor::FUNCTION: {
									if (!typesAreSame(left->type, right->type)) {
										if (isVoidPointer(left->type)) {
											insertImplicitCast(&right, left->type);
										}
										else if (isVoidPointer(right->type)) {
											insertImplicitCast(&left, right->type);
										}
										else {
											assert(false); // @ErrorMessage
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
									if (!typesAreSame(left->type, right->type)) {
										if (isVoidPointer(left->type)) {
											insertImplicitCast(&right, left->type);
										}
										else if (isVoidPointer(right->type)) {
											insertImplicitCast(&left, right->type);
										}
										else {
											assert(false); // @ErrorMessage
											return false;
										}
									}

									break;
								}
								case TypeFlavor::TYPE: {
									// @Incomplete make this work
									assert(false); // @ErrorMessage cannot compare types
									return false;
								}
								default:
									assert(false);
							}
						}
						else {
							if (!binaryOpForAutoCast(binary)) {
								assert(false);
							}
							else {
								binaryOpForFloatAndIntLiteral(binary);

								if (!typesAreSame(right->type, left->type)) {
									assert(false); // @ErrorMessage
									return false;
								}
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
								case TypeFlavor::BOOL: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::FLOAT: {
									if (!binaryOpForFloat(binary))
										return false;
									break;
								}
								case TypeFlavor::FUNCTION: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::INTEGER: {
									if (!binaryOpForInteger(binary))
										return false;

									break;
								}
								case TypeFlavor::POINTER: {
									if (!typesAreSame(left->type, right->type)) {
										assert(false); // @ErrorMessage
										return false;
									}

									break;
								}
								case TypeFlavor::TYPE: {
									assert(false); // @ErrorMessage cannot compare types
									return false;
								}
								default:
									assert(false);
							}
						}
						else {
							if (!binaryOpForAutoCast(binary)) {
								assert(false);
							}
							else {
								if (left->type->flavor == TypeFlavor::FUNCTION || left->type->flavor == TypeFlavor::BOOL || left->type->flavor == TypeFlavor::TYPE) {
									assert(false); // @ErrorMessage
									return false;
								}

								binaryOpForFloatAndIntLiteral(binary);


								if (!typesAreSame(right->type, left->type)) {
									assert(false); // @ErrorMessage
									return false;
								}
							}
						}

						expr->type = &TYPE_BOOL;

						break;
					}
					case TOKEN('+'):
					case TOKEN('-'): {
						if (left->type->flavor == right->type->flavor) {
							switch (left->type->flavor) {
								case TypeFlavor::BOOL: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::FLOAT: {
									if (!binaryOpForFloat(binary))
										return false;
									break;
								}
								case TypeFlavor::FUNCTION: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::INTEGER: {
									if (!binaryOpForInteger(binary))
										return false;

									break;
								}
								case TypeFlavor::POINTER: {
									if (binary->op == TOKEN('+')) {
										assert(false); // @ErrorMessage can't add pointers
										return false;
									}

									if (!typesAreSame(left->type, right->type)) {
										assert(false); // @ErrorMessage
										return false;
									}

									break;
								}
								case TypeFlavor::TYPE: {
									assert(false); // @ErrorMessage cannot compare types
									return false;
								}
								default:
									assert(false);
							}
						}
						else {
							if (!binaryOpForAutoCast(binary)) {
								assert(false);
							}
							else if (left->type->flavor == TypeFlavor::POINTER && right->type->flavor == TypeFlavor::INTEGER) {
								trySolidifyNumericLiteralToDefault(right);
							}
							else {
								if (left->type->flavor == TypeFlavor::FUNCTION || left->type->flavor == TypeFlavor::BOOL || left->type->flavor == TypeFlavor::TYPE) {
									assert(false); // @ErrorMessage
									return false;
								}

								binaryOpForFloatAndIntLiteral(binary);

								if (!typesAreSame(right->type, left->type)) {
									assert(false); // @ErrorMessage
									return false;
								}
							}
						}

						expr->type = left->type;

						break;
					}
					case TOKEN('&'):
					case TOKEN('|'):
					case TOKEN('^'):
					case TokenT::SHIFT_LEFT:
					case TokenT::SHIFT_RIGHT: {
						if (left->type->flavor == right->type->flavor) {
							switch (left->type->flavor) {
								case TypeFlavor::BOOL: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::FLOAT: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::FUNCTION: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::INTEGER: {
									if (!binaryOpForInteger(binary))
										return false;

									break;
								}
								case TypeFlavor::POINTER: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::TYPE: {
									assert(false); // @ErrorMessage
									return false;
								}
								default:
									assert(false);
							}
						}
						else {
							if (!binaryOpForAutoCast(binary)) {
								assert(false);
							}
							else {
								if (left->type->flavor != TypeFlavor::INTEGER) {
									assert(false); // @ErrorMessage
									return false;
								}

								if (!typesAreSame(right->type, left->type)) {
									assert(false); // @ErrorMessage
									return false;
								}
							}
						}

						expr->type = left->type;

						break;
					}
					case TOKEN('*'):
					case TOKEN('/'):
					case TOKEN('%'): {
						if (left->type->flavor == right->type->flavor) {
							switch (left->type->flavor) {
								case TypeFlavor::BOOL: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::FLOAT: {
									if (!binaryOpForFloat(binary))
										return false;
									break;
								}
								case TypeFlavor::FUNCTION: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::INTEGER: {
									if (!binaryOpForInteger(binary))
										return false;

									break;
								}
								case TypeFlavor::POINTER: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::TYPE: {
									assert(false); // @ErrorMessage cannot compare types
									return false;
								}
								default:
									assert(false);
							}
						}
						else {
							if (!binaryOpForAutoCast(binary)) {
								assert(false);
							}
							else {
								if (left->type->flavor != TypeFlavor::FLOAT && left->type->flavor != TypeFlavor::INTEGER) {
									assert(false); // @ErrorMessage
									return false;
								}

								binaryOpForFloatAndIntLiteral(binary);


								if (!typesAreSame(right->type, left->type)) {
									assert(false); // @ErrorMessage
									return false;
								}
							}
						}

						expr->type = left->type;

						break;
					}
					case TOKEN('='): {
						if (!isAssignable(left)) {
							assert(false); // @ErrorMessage
							return false;
						}

						if (!right) { // @Incomplete this is only valid for non-composite types
							ExprLiteral *zero = new ExprLiteral;
							zero->flavor = ExprFlavor::INT_LITERAL;
							zero->unsignedValue = 0;
							zero->type = left->type;
							zero->start = binary->start;
							zero->end = binary->end;
							
							binary->right = zero;
						}
						else {
							if (left->type->flavor == right->type->flavor) {
								switch (left->type->flavor) {
								case TypeFlavor::BOOL: {
									break;
								}
								case TypeFlavor::FLOAT: {
									if (!assignOpForFloat(binary))
										return false;
									break;
								}
								case TypeFlavor::FUNCTION: {
									if (!typesAreSame(left->type, right->type)) {
										if (isVoidPointer(left->type)) {
											insertImplicitCast(&right, left->type);
										}
										else if (isVoidPointer(right->type)) {
											insertImplicitCast(&right, left->type);
										}
										else {
											assert(false); // @ErrorMessage
											return false;
										}
									}
								}
								case TypeFlavor::INTEGER: {
									if (!assignOpForInteger(binary))
										return false;

									break;
								}
								case TypeFlavor::POINTER: {
									if (!typesAreSame(left->type, right->type)) {
										if (isVoidPointer(left->type)) {
											insertImplicitCast(&right, left->type);
										}
										else if (isVoidPointer(right->type)) {
											insertImplicitCast(&right, left->type);
										}
										else {
											assert(false); // @ErrorMessage
											return false;
										}
									}

									break;
								}
								case TypeFlavor::TYPE: {
									// @Incomplete make this work
									assert(false); // @ErrorMessage cannot compare types
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

									if (!typesAreSame(right->type, left->type)) {
										assert(false); // @ErrorMessage
										return false;
									}
								}
							}
						}

						break;
					}
					case TokenT::PLUS_EQUALS:
					case TokenT::MINUS_EQUALS: {
						if (left->type->flavor == right->type->flavor) {
							switch (left->type->flavor) {
								case TypeFlavor::BOOL: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::FLOAT: {
									if (!assignOpForFloat(binary))
										return false;
									break;
								}
								case TypeFlavor::FUNCTION: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::INTEGER: {
									if (!assignOpForInteger(binary))
										return false;

									break;
								}
								case TypeFlavor::POINTER: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::TYPE: {
									assert(false); // @ErrorMessage cannot compare types
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
							else if (left->type->flavor == TypeFlavor::POINTER && right->type->flavor == TypeFlavor::INTEGER) {
								trySolidifyNumericLiteralToDefault(right);
							}
							else {
								if (left->type->flavor == TypeFlavor::FUNCTION || left->type->flavor == TypeFlavor::BOOL || left->type->flavor == TypeFlavor::TYPE) {
									assert(false); // @ErrorMessage
									return false;
								}

								assignOpForFloatAndIntLiteral(binary);

								if (!typesAreSame(right->type, left->type)) {
									assert(false); // @ErrorMessage
									return false;
								}
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
								assert(false); // @ErrorMessage
								return false;
							}
						}

						if (right->type != &TYPE_BOOL) {
							if (isValidCast(&TYPE_BOOL, right->type)) {
								insertImplicitCast(&right, &TYPE_BOOL);
							}
							else {
								assert(false); // @ErrorMessage
								return false;
							}
						}

						expr->type = &TYPE_BOOL;

						break;
					}
					case TokenT::AND_EQUALS:
					case TokenT::OR_EQUALS:
					case TokenT::XOR_EQUALS:
					case TokenT::SHIFT_LEFT_EQUALS:
					case TokenT::SHIFT_RIGHT_EQUALS: {
						if (left->type->flavor == right->type->flavor) {
							switch (left->type->flavor) {
								case TypeFlavor::BOOL: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::FLOAT: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::FUNCTION: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::INTEGER: {
									if (!assignOpForInteger(binary))
										return false;

									break;
								}
								case TypeFlavor::POINTER: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::TYPE: {
									assert(false); // @ErrorMessage
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
								if (left->type->flavor != TypeFlavor::INTEGER) {
									assert(false); // @ErrorMessage
									return false;
								}

								if (!typesAreSame(right->type, left->type)) {
									assert(false); // @ErrorMessage
									return false;
								}
							}
						}

						expr->type = left->type;

						break;
					}
					case TokenT::TIMES_EQUALS:
					case TokenT::DIVIDE_EQUALS:
					case TokenT::MOD_EQUALS: {
						if (left->type->flavor == right->type->flavor) {
							switch (left->type->flavor) {
								case TypeFlavor::BOOL: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::FLOAT: {
									if (!assignOpForFloat(binary))
										return false;
									break;
								}
								case TypeFlavor::FUNCTION: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::INTEGER: {
									if (!assignOpForInteger(binary))
										return false;

									break;
								}
								case TypeFlavor::POINTER: {
									assert(false); // @ErrorMessage
									return false;
								}
								case TypeFlavor::TYPE: {
									assert(false); // @ErrorMessage cannot compare types
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
								if (left->type->flavor != TypeFlavor::FLOAT && left->type->flavor != TypeFlavor::INTEGER) {
									assert(false); // @ErrorMessage
									return false;
								}

								assignOpForFloatAndIntLiteral(binary);


								if (!typesAreSame(right->type, left->type)) {
									assert(false); // @ErrorMessage
									return false;
								}
							}
						}

						expr->type = left->type;

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
						assert(false); // @ErrorMessage
						return false;
					}

					if (loop->forBegin->type == &TYPE_AUTO_CAST) {
						if (!tryAutoCast(loop->forBegin, loop->forEnd->type))
							return false;
					}
					else if (loop->forEnd->type == &TYPE_AUTO_CAST) {
						if (!tryAutoCast(loop->forEnd, loop->forBegin->type))
							return false;
					}


					if (loop->forBegin->type->flavor == TypeFlavor::INTEGER) {
						if (loop->forEnd->type->flavor != TypeFlavor::INTEGER) {
							assert(false); // @ErrorMessage
							return false;
						}

						if (loop->flags & EXPR_FOR_BY_POINTER) {
							assert(false); //. @ErrorMessage
							return false;
						}

						if ((loop->forBegin->type->flags & TYPE_INTEGER_IS_SIGNED) == (loop->forEnd->type->flags & TYPE_INTEGER_IS_SIGNED)) {
							if (loop->forBegin->type == loop->forEnd->type) {
								trySolidifyNumericLiteralToDefault(loop->forBegin);
								trySolidifyNumericLiteralToDefault(loop->forEnd);
							}
							else if (loop->forBegin->type->size == 0) {
								if (!boundsCheckImplicitConversion(loop->forEnd->type, static_cast<ExprLiteral *>(loop->forBegin))) {
									assert(false); // @ErrorMessage
									return false;
								}

								loop->forBegin->type = loop->forEnd->type;
							}
							else if (loop->forEnd->type->size == 0) {
								if (!boundsCheckImplicitConversion(loop->forBegin->type, static_cast<ExprLiteral *>(loop->forEnd))) {
									assert(false); // @ErrorMessage
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

								if (!boundsCheckImplicitConversion(loop->forEnd->type, static_cast<ExprLiteral *>(loop->forBegin))) {
									assert(false); // @ErrorMessage
									return false;
								}

								loop->forBegin->type = loop->forEnd->type;
							}
							else if (loop->forEnd->type == &TYPE_UNSIGNED_INT_LITERAL) {
								trySolidifyNumericLiteralToDefault(loop->forBegin);

								if (!boundsCheckImplicitConversion(loop->forBegin->type, static_cast<ExprLiteral *>(loop->forEnd))) {
									assert(false); // @ErrorMessage
									return false;
								}

								loop->forEnd->type = loop->forBegin->type;
							}
							else {
								assert(false); // @ErrorMessage
								return false;
							}
						}

						it->type = makeTypeLiteral(it->start, it->end, loop->forEnd->type);
						it->flags |= DECLARATION_TYPE_IS_READY;
					}
					else if (loop->forBegin->type->flavor == TypeFlavor::POINTER) {
						if (!typesAreSame(loop->forBegin->type, loop->forEnd->type)) {
							assert(false); // @ErrorMessage
							return false;
						}

						if (isVoidPointer(loop->forBegin->type)) {
							assert(false); // @ErrorMessage
							return false;
						}

						auto pointer = static_cast<TypePointer *>(loop->forBegin->type);

						if (loop->flags & EXPR_FOR_BY_POINTER) {
							it->type = makeTypeLiteral(it->start, it->end, pointer);
							it->flags |= DECLARATION_TYPE_IS_READY;
						}
						else {
							it->type = makeTypeLiteral(it->start, it->end, pointer->pointerTo);
							it->flags |= DECLARATION_TYPE_IS_READY;
						}
					}
				}
				else {
					if (loop->forBegin->type->flavor == TypeFlavor::INTEGER) {
						if (loop->flags & EXPR_FOR_BY_POINTER) {
							assert(false); //. @ErrorMessage
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

						it->type = makeTypeLiteral(it->start, it->end, loop->forEnd->type);
						it->flags |= DECLARATION_TYPE_IS_READY;
					}
					else {
						assert(false); // @ErrorMessage
						return false;
					}
				}

				it_index->type = makeTypeLiteral(it_index->start, it_index->end, &TYPE_U64);
				it->flags |= DECLARATION_TYPE_IS_READY;

				break;
			}
			case ExprFlavor::FUNCTION_CALL: {
				auto call = static_cast<ExprFunctionCall *>(expr);

				if (call->function->type->flavor != TypeFlavor::FUNCTION) {
					assert(false); // @ErrorMessage
					return false;
				}

				auto function = static_cast<TypeFunction *>(call->function->type);

				assert(function->returnType->flavor == ExprFlavor::TYPE_LITERAL);

				expr->type = static_cast<ExprLiteral *>(function->returnType)->typeValue;

				if (call->argumentCount != function->argumentCount) {
					assert(false); // @ErrorMessage
					return false;
				}

				for (u64 i = 0; i < call->argumentCount; i++) {
					assert(function->argumentTypes[i]->flavor == ExprFlavor::TYPE_LITERAL);

					Type *correct = static_cast<ExprLiteral *>(function->argumentTypes[i])->typeValue;
					auto &given = call->arguments[i];

					if (correct != given->type) {
						if (correct->flavor == given->type->flavor) {
							switch (correct->flavor) {
								case TypeFlavor::BOOL: {
									break;
								}
								case TypeFlavor::FLOAT: {
									if (given->type == &TYPE_FLOAT_LITERAL) {
										given->type = correct;
										given->type = correct;
									}
									else {
										// @Incomplete should we allow this conversion in some cases, this code was originally taken
										// from == and != where float conversion definitely shouldn't be allowed, since that's alredy
										// bad enough without the compiler converting types behind your back
										assert(false); // @ErrorMessage cannot implicitly convert f32 and f64
										return false;
									}
									break;
								}
								case TypeFlavor::FUNCTION: {
									if (!typesAreSame(correct, given->type)) {
										if (isVoidPointer(correct)) {
											insertImplicitCast(&given, correct);
										}
										else if (isVoidPointer(given->type)) {
											insertImplicitCast(&given, correct);
										}
										else {
											assert(false); // @ErrorMessage
											return false;
										}
									}
								}
								case TypeFlavor::INTEGER: {
									if ((correct->flags & TYPE_INTEGER_IS_SIGNED) == (given->type->flags & TYPE_INTEGER_IS_SIGNED)) {
										if (given->type == &TYPE_UNSIGNED_INT_LITERAL || given->type == &TYPE_SIGNED_INT_LITERAL) {
											if (!boundsCheckImplicitConversion(correct, static_cast<ExprLiteral *>(given))) {
												assert(false); // @ErrorMessage
												return false;
											}
										}
										else if (correct->size > given->type->size) {
											insertImplicitCast(&given, correct);
										}
										else if (given->type->size < correct->size) {
											assert(false); // @ErrorMessage
											return false;
										}
									}
									else {
										if ((given->type == &TYPE_UNSIGNED_INT_LITERAL) && (correct->flags & TYPE_INTEGER_IS_SIGNED)) {
											if (!boundsCheckImplicitConversion(correct, static_cast<ExprLiteral *>(given))) {
												assert(false); // @ErrorMessage
												return false;
											}

											given->type = correct;
										}
										else {
											assert(false); // @ErrorMessage signed-unsigned mismatch
											return false;
										}
									}

									break;
								}
								case TypeFlavor::POINTER: {
									if (!typesAreSame(given->type, correct)) {
										if (isVoidPointer(correct)) {
											insertImplicitCast(&given, correct);
										}
										else if (isVoidPointer(given->type)) {
											insertImplicitCast(&given, correct);
										}
										else {
											assert(false); // @ErrorMessage
											return false;
										}
									}

									break;
								}
								case TypeFlavor::TYPE: {
									// @Incomplete make this work
									assert(false); // @ErrorMessage cannot compare types
									return false;
								}
								default:
									assert(false);
							}
						}
						else {
							if (given->type->flavor == TypeFlavor::AUTO_CAST) {
								if (!tryAutoCast(given, correct)) {
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

								if (!typesAreSame(correct, given->type)) {
									assert(false); // @ErrorMessage
									return false;
								}
							}
						}
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
						assert(false); // @ErrorMessage
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

				Expr *returnTypeExpr = static_cast<TypeFunction *>(return_->returnsFrom->type)->returnType;

				assert(returnTypeExpr->flavor == ExprFlavor::TYPE_LITERAL);

				Type *returnType = static_cast<ExprLiteral *>(returnTypeExpr)->typeValue;

				if (returnType == &TYPE_VOID) {
					if (return_->value) {
						assert(false); // @ErrorMessage
						return false;
					}
				}
				else {
					if (!return_->value) {
						assert(false); // @ErrorMessage
						return false;
					}

					if (return_->value->type == &TYPE_AUTO_CAST) {
						if (!tryAutoCast(return_->value, returnType)) {
							return false;
						}
					}

					Type *&type = return_->value->type;

					switch (returnType->flavor) {
						case TypeFlavor::BOOL: {
							if (type->flavor != TypeFlavor::BOOL) {
								assert(false); // @ErrorMessage
								return false;
							}

							break;
						}
						case TypeFlavor::FLOAT: {
							if (type == &TYPE_FLOAT_LITERAL) {
								type = returnType;
							}
							else if (type == &TYPE_UNSIGNED_INT_LITERAL) {
								auto literal = static_cast<ExprLiteral *>(return_->value);

								literal->floatValue = static_cast<double>(literal->unsignedValue);
								literal->flavor = ExprFlavor::FLOAT_LITERAL;
							}
							else if (type == &TYPE_SIGNED_INT_LITERAL) {
								auto literal = static_cast<ExprLiteral *>(return_->value);

								literal->floatValue = static_cast<double>(literal->signedValue);
								literal->flavor = ExprFlavor::FLOAT_LITERAL;
							}
							break;
						}
						case TypeFlavor::FUNCTION: {
							if (!typesAreSame(returnType, type)) {
								if (!isVoidPointer(type)) {
									assert(false); // @ErrorMessage
									return false;
								}

								insertImplicitCast(&return_->value, returnType);
							}
							
							break;
						}
						case TypeFlavor::INTEGER: {
							if (type == &TYPE_UNSIGNED_INT_LITERAL) {
								if (boundsCheckImplicitConversion(returnType, static_cast<ExprLiteral *>(return_->value))) {
									type = returnType;
								}
								else {
									assert(false); // @ErrorMessage
									return false;
								}
							}
							else if (type == &TYPE_SIGNED_INT_LITERAL) {
								if (returnType->flags & TYPE_INTEGER_IS_SIGNED) {
									if (boundsCheckImplicitConversion(returnType, static_cast<ExprLiteral *>(return_->value))) {
										type = returnType;
									}
									else {
										assert(false); // @ErrorMessage
										return false;
									}
								}
								else {
									assert(false); // @ErrorMessage
									return false;
								}
							}
							else {
								if ((type->flags & TYPE_INTEGER_IS_SIGNED) == (returnType->flags & TYPE_INTEGER_IS_SIGNED)) {
									if (type->size < returnType->size) {
										insertImplicitCast(&return_->value, returnType);
									}
									else if (type->size != returnType->size) {
										assert(false); // @ErrorMessage
									}
								}
								else {
									assert(false); // @ErrorMessage
									return false;
								}
							}

							break;
						}
						case TypeFlavor::POINTER: {
							if (!typesAreSame(returnType, type)) {
								if (isVoidPointer(type)) {
									insertImplicitCast(&return_->value, returnType);
								}
								else if (isVoidPointer(returnType) && (type->flavor == TypeFlavor::POINTER || type->flavor == TypeFlavor::FUNCTION)) {
									insertImplicitCast(&return_->value, returnType);
								}
								else {
									assert(false); // @ErrorMessage
									return false;
								}
							}
							
							break;
						}
						case TypeFlavor::TYPE: {
							assert(false); // @ErrorMessage cannot return types
							return false;
						}
						default:
							assert(false);
					}
				}

				break;
			}
			case ExprFlavor::STRUCT_ACCESS: {
				assert(false); // @Incomplete
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
									if (!boundsCheckImplicitConversion(&TYPE_S64, literal)) {
										assert(false); // @ErrorMessage
										return false;
									}

									literal->start = unary->start;

									literal->signedValue = -literal->signedValue;
									*exprPointer = value;
								}
								else {
									assert(false); // @ErrorMessage
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
							assert(false); // @ErrorMessage
							return false;
						}

						break;
					}
					case TOKEN('+'): {
						if (value->type->flavor == TypeFlavor::INTEGER) {
							value->start = unary->start;
							*exprPointer = value;
						}
						else if (value->type->flavor == TypeFlavor::FLOAT) {
							value->start = unary->start;
							*exprPointer = value;
						}
						else {
							assert(false); // @ErrorMessage
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
							assert(false); // @ErrorMessage
							return false;
						}
					}
					case TOKEN('!'): {
						if (value->type != &TYPE_BOOL) {
							if (isValidCast(&TYPE_BOOL, value->type)) {
								insertImplicitCast(&value, &TYPE_BOOL);
							}
							else {
								assert(false); // @ErrorMessage
								return false;
							}
						}

						unary->type = &TYPE_BOOL;

						break;
					}
					case TokenT::SIZE_OF: {
						if (value->flavor != ExprFlavor::TYPE_LITERAL) {
							assert(false); // @ErrorMessage
							return false;
						}

						auto type = static_cast<ExprLiteral *>(value);

						if (type->typeValue == &TYPE_VOID) {
							assert(false); // @ErrorMessage
							return false;
						}
						else if (type->typeValue == &TYPE_AUTO_CAST) {
							assert(false); // @ErrorMessage
							return false;
						}
						else if (type->typeValue == &TYPE_TYPE) {
							assert(false); // @ErrorMessage
							return false;
						}
						
						assert(type->typeValue->size); // @Incomplete structs

						auto literal = new ExprLiteral;
						literal->flavor = ExprFlavor::INT_LITERAL;
						literal->unsignedValue = type->typeValue->size;
						literal->start = unary->start;
						literal->end = unary->end;
						
						*exprPointer = literal;

						break;
					}
					case TokenT::TYPE_OF: {
						trySolidifyNumericLiteralToDefault(value);

						if (value->type == &TYPE_AUTO_CAST) {
							assert(false); // @ErrorMessage
							return false;
						}

						auto literal = new ExprLiteral;
						literal->flavor = ExprFlavor::TYPE_LITERAL;
						literal->typeValue = value->type;
						literal->start = unary->start;
						literal->end = unary->end;

						*exprPointer = literal;

						break;
					}
					case TokenT::SHIFT_LEFT: {
						if (value->type->flavor != TypeFlavor::POINTER) {
							assert(false);
							return false;
						}

						if (isVoidPointer(value->type)) {
							assert(false);
							return false;
						}

						auto pointer = static_cast<TypePointer *>(value->type);

						unary->type = pointer->pointerTo;

						break;
					}
					case TOKEN('*'): {
						if (value->flavor == ExprFlavor::TYPE_LITERAL) {
							auto literal = static_cast<ExprLiteral *>(value);

							auto pointer = new TypePointer;
							pointer->flavor = TypeFlavor::POINTER;
							pointer->size = 8;
							pointer->alignment = 8;
							pointer->pointerTo = literal->typeValue;

							literal->typeValue = pointer;

							literal->start = unary->start;

							*exprPointer = literal;
						}
						else if (value->flavor == ExprFlavor::BINARY_OPERATOR) {
							auto binary = static_cast<ExprBinaryOperator *>(expr);

							if (binary->op == TOKEN('[')) {
								auto pointer = new TypePointer;
								pointer->flavor = TypeFlavor::POINTER;
								pointer->size = 8;
								pointer->alignment = 8;
								pointer->pointerTo = value->type;

								unary->type = pointer;
							}
							else {
								assert(false); // @ErrorMessage
								return false;
							}
						}
						else if (value->flavor == ExprFlavor::STRUCT_ACCESS) {
							assert(false); // @Incomplete structs
							return false;
						}
						else if (value->flavor == ExprFlavor::IDENTIFIER) {
							auto pointer = new TypePointer;
							pointer->flavor = TypeFlavor::POINTER;
							pointer->size = 8;
							pointer->alignment = 8;
							pointer->pointerTo = value->type;

							unary->type = pointer;
						}
						else {
							assert(false);
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
						assert(false); // @ErrorMessage
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

void runInfer() {
	bool exit = false;

	while (!exit) {
		DeclarationPack declarations = inferQueue.take();

		if (!declarations.count && !declarations.data.function) {
			exit = true;
		} else if (declarations.count == 0) {
			InferJob body;
			body.infer.function = declarations.data.function;
			body.type = InferType::FUNCTION_BODY;

			flatten(body.valueFlattened, &body.infer.function->body);

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
				if (!addDeclaration(declarations.data.declarations[i]))
					goto error;
		}

		bool madeProgress;

		do {
			madeProgress = false;

			for (u64 i = 0; i < inferJobs.count; i++) {

				auto job = inferJobs[i];

				switch (job.type) {
					case InferType::FUNCTION_BODY: {
						if (!inferFlattened(job.valueFlattened, &job.valueFlattenedIndex)) {
							goto error;
						}

						if (job.infer.function->type) {
							if (job.valueFlattenedIndex == job.valueFlattened.count) {
								inferJobs.unordered_remove(i--);

								irGeneratorQueue.add(job.infer.function);
							}
						}
						break;
					}
					case InferType::DECLARATION: {
						auto declaration = job.infer.declaration;

						if (declaration->type) {
							if (!inferFlattened(job.typeFlattened, &job.typeFlattenedIndex)) {
								goto error;
							}
						}

						if (declaration->initialValue) {
							if (!inferFlattened(job.valueFlattened, &job.valueFlattenedIndex)) {
								goto error;
							}
						}

						if (declaration->type && job.typeFlattenedIndex == job.typeFlattened.count) {
							if (declaration->type->flavor != ExprFlavor::TYPE_LITERAL) {
								assert(false); // @ErrorMessage could not evaluate type of declaration
								goto error;
							}

							madeProgress = true;
							declaration->flags |= DECLARATION_TYPE_IS_READY;
						}

						if (declaration->initialValue && job.valueFlattenedIndex == job.valueFlattened.count && (declaration->flags & DECLARATION_IS_CONSTANT)) {
							if (!isLiteral(declaration->initialValue)) {
								assert(false); // @ErrorMessage the value of the declaration must be a constant
								goto error;
							}

							// We can only set this flag if no type is specified since there could be an implicit cast otherwise
							if (!declaration->type) {
								trySolidifyNumericLiteralToDefault(declaration->initialValue);
								declaration->flags |= DECLARATION_VALUE_IS_READY;
							}
						}

						if (declaration->type && declaration->initialValue) {
							if (job.typeFlattenedIndex == job.typeFlattened.count && job.valueFlattenedIndex == job.valueFlattened.count) {
								assert(declaration->initialValue->type);

								Type *correct = static_cast<ExprLiteral *>(declaration->type)->typeValue;
								Expr *given = declaration->initialValue;

								// @Paste from inferTypes(FUNCTION_CALL)
								if (correct != given->type) {
									if (correct->flavor == given->type->flavor) {
										switch (correct->flavor) {
											case TypeFlavor::BOOL: {
												break;
											}
											case TypeFlavor::FLOAT: {
												if (given->type == &TYPE_FLOAT_LITERAL) {
													given->type = correct;
													given->type = correct;
												}
												else {
													// @Incomplete should we allow this conversion in some cases, this code was originally taken
													// from == and != where float conversion definitely shouldn't be allowed, since that's alredy
													// bad enough without the compiler converting types behind your back
													assert(false); // @ErrorMessage cannot implicitly convert f32 and f64
													goto error;
												}
												break;
											}
											case TypeFlavor::FUNCTION: {
												if (!typesAreSame(correct, given->type)) {
													if (isVoidPointer(correct)) {
														insertImplicitCast(&given, correct);
													}
													else if (isVoidPointer(given->type)) {
														insertImplicitCast(&given, correct);
													}
													else {
														assert(false); // @ErrorMessage
														goto error;
													}
												}
											}
											case TypeFlavor::INTEGER: {
												if ((correct->flags & TYPE_INTEGER_IS_SIGNED) == (given->type->flags & TYPE_INTEGER_IS_SIGNED)) {
													if (given->type == &TYPE_UNSIGNED_INT_LITERAL || given->type == &TYPE_SIGNED_INT_LITERAL) {
														if (!boundsCheckImplicitConversion(correct, static_cast<ExprLiteral *>(given))) {
															assert(false); // @ErrorMessage
															goto error;
														}
													}
													else if (correct->size > given->type->size) {
														insertImplicitCast(&given, correct);
													}
													else if (given->type->size < correct->size) {
														assert(false); // @ErrorMessage
														goto error;
													}
												}
												else {
													if ((given->type == &TYPE_UNSIGNED_INT_LITERAL) && (correct->flags & TYPE_INTEGER_IS_SIGNED)) {
														if (!boundsCheckImplicitConversion(correct, static_cast<ExprLiteral *>(given))) {
															assert(false); // @ErrorMessage
															goto error;
														}

														given->type = correct;
													}
													else {
														assert(false); // @ErrorMessage signed-unsigned mismatch
														goto error;
													}
												}

												break;
											}
											case TypeFlavor::POINTER: {
												if (!typesAreSame(given->type, correct)) {
													if (isVoidPointer(correct)) {
														insertImplicitCast(&given, correct);
													}
													else if (isVoidPointer(given->type)) {
														insertImplicitCast(&given, correct);
													}
													else {
														assert(false); // @ErrorMessage
														goto error;
													}
												}

												break;
											}
											case TypeFlavor::TYPE: {
												// @Incomplete make this work
												assert(false); // @ErrorMessage cannot compare types
												goto error;
											}
											default:
												assert(false);
										}
									}
									else {
										if (given->type->flavor == TypeFlavor::AUTO_CAST) {
											if (!tryAutoCast(given, correct)) {
												goto error;
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

											if (!typesAreSame(correct, given->type)) {
												assert(false); // @ErrorMessage
												goto error;
											}
										}
									}
								}

								madeProgress = true;
								inferJobs.unordered_remove(i--);
								// @Incomplete: Queue to later compile stage

								declaration->flags |= DECLARATION_VALUE_IS_READY;
							}
						}
						else if (declaration->type) {
							if (job.typeFlattenedIndex == job.typeFlattened.count) {
								if (!(declaration->flags & (DECLARATION_IS_UNINITIALIZED | DECLARATION_IS_ARGUMENT | DECLARATION_IS_ITERATOR_INDEX | DECLARATION_IS_ITERATOR))) {
									auto type = static_cast<ExprLiteral *>(declaration->type)->typeValue;

									ExprLiteral *literal = new ExprLiteral;
									literal->start = declaration->start;
									literal->end = declaration->end;
									literal->type = type;

									switch (type->flavor) {
										case TypeFlavor::BOOL:
										case TypeFlavor::FUNCTION:
										case TypeFlavor::INTEGER:
										case TypeFlavor::POINTER: {
											literal->flavor = ExprFlavor::INT_LITERAL;
											literal->unsignedValue = 0;
											break;
										}
										case TypeFlavor::FLOAT: {
											literal->flavor = ExprFlavor::FLOAT_LITERAL;
											literal->floatValue = 0;
											break;
										}
										case TypeFlavor::TYPE:
											assert(false); // @ErrorMessage
											goto error;
										default:
											assert(false); // Invalid code path, variable type should never be void or auto-cast
											goto error;
									}

									declaration->initialValue = literal;
								}

								madeProgress = true;
								inferJobs.unordered_remove(i--);
								// @Incomplete: Queue to later compile stage

								declaration->flags |= DECLARATION_VALUE_IS_READY;
							}
						}
						else if (declaration->initialValue) {
							if (job.valueFlattenedIndex == job.valueFlattened.count) {
								assert(declaration->initialValue->type);

								trySolidifyNumericLiteralToDefault(declaration->initialValue);

								if (declaration->initialValue->type == &TYPE_VOID) {
									assert(false); // @ErrorMessage cannot declare a variable of type void
									goto error;
								}
								else if (declaration->initialValue->type == &TYPE_AUTO_CAST) {
									assert(false); // @ErrorMessage cannot infer the type of both sides of a declaration
									goto error;
								}

								declaration->type = makeTypeLiteral(declaration->start, declaration->end, declaration->initialValue->type);

								madeProgress = true;

								inferJobs.unordered_remove(i--);
								// @Incomplete: Queue to later compile stage
								declaration->flags |= DECLARATION_VALUE_IS_READY;
								declaration->flags |= DECLARATION_TYPE_IS_READY;
							}
						}

						break;
					}
				}
			}
		} while (madeProgress && inferJobs.count);
	}

	if (inferJobs.count) { // We didn't complete type inference, check for undeclared identifiers or circular dependencies!
		assert(false); // @ErrorMessage

		// @Incomplete
		// @Incomplete
		// @Incomplete
		// @Incomplete
	}


error:;
	irGeneratorQueue.add(nullptr);
}