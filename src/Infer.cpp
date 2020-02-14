#include "Basic.h"
#include "Infer.h"
#include "Array.h"
#include "Parser.h"
#include "Lexer.h"

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

extern WorkQueue<DeclarationPack> inferQueue;

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
			else if (type->flavor == TypeFlavor::FUNCTION) {
				TypePointer *pointer = static_cast<TypePointer *>(type);

				flatten(flattenTo, &pointer->pointerTo);

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
			flatten(flattenTo, &binary->right);

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

			for (auto subExpr : block->exprs)
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

void addDeclaration(Declaration *declaration) {
	if (!declaration->enclosingScope) {
		addDeclarationToBlock(&globalBlock, declaration);
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

	if (literal->unsignedValue > std::numeric_limits<s64>::max()) {
		expr->type = &TYPE_U64;
	}
	else {
		expr->type = &TYPE_S64;
	}
}

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
		assert(right->type->flavor == TypeFlavor::INTEGER);

		if (!boundsCheckImplicitConversion(right->type, static_cast<ExprLiteral *>(left))) {
			assert(false); // @ErrorMessage cannot implicitly cast, value out of range
			*success = false;
		}

		left->type = right->type;
	}
	else if (left->type == &TYPE_SIGNED_INT_LITERAL) {
		assert(right->type->flavor == TypeFlavor::INTEGER);

		if (!(right->flags & TYPE_NUMBER_IS_SIGNED)) {
			assert(false); // @ErrorMessage signed-unsigned mismatch
			*success = false;
		}

		if (!boundsCheckImplicitConversion(right->type, static_cast<ExprLiteral *>(left))) {
			assert(false); // @ErrorMessage cannot implicitly cast, value out of range
			*success = false;
		}
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

void insertImplicitCast(Expr **castFrom, Type *castTo) {
	ExprBinaryOperator *cast = new ExprBinaryOperator;
	cast->flavor = ExprFlavor::BINARY_OPERATOR;
	cast->type = castTo;
	cast->right = *castFrom;
	cast->start = cast->right->start;
	cast->end = cast->left->start;
	cast->flags |= EXPR_CAST_IS_IMPLICIT;
	cast->left = makeTypeLiteral(cast->right->start, cast->right->end, castTo);

	*castFrom = cast;
}

bool inferFlattened(Array<Expr **> flattened, u64 *index) {
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
							assert(typesAreSame(identifier->type, identifier->declaration->initialValue->type));

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

				if (!evaluateToType(&function->returnType)) {
					assert(false); // @ErrorMessage return type must be a type
					return false;
				}

				bool argumentsInferred = true;

				for (auto argument : function->arguments.declarations) {
					assert(argument);

					if (argument->flags & DECLARATION_TYPE_IS_READY) {
						argumentsInferred = false;
						break;
					}

					if (argument->initialValue) {
						if (argument->flags & DECLARATION_VALUE_IS_READY) {
							if (!evaluateToLiteral(&argument->type)) {
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
				
				break;
			}
			case ExprFlavor::TYPE_LITERAL:
			case ExprFlavor::STRING_LITERAL:
			case ExprFlavor::FLOAT_LITERAL:
			case ExprFlavor::INT_LITERAL:
			case ExprFlavor::BREAK:
			case ExprFlavor::CONTINUE:
			case ExprFlavor::BLOCK: {
				assert(false); // Nothing to infer here these shouldn't be in the flattened lists
				break;
			}
			case ExprFlavor::BINARY_OPERATOR: {
				auto binary = static_cast<ExprBinaryOperator *>(expr);

				auto &left = binary->left;
				auto &right = binary->right;

				assert(left); // This is safe since even though auto-casts can have a left of null, they shouldn't be added to the flattened array
				assert(right);

				if (left->type->flavor == right->type->flavor && left->type->flavor == TypeFlavor::AUTO_CAST) {
					assert(false); // @ErrorMessage cannot auto-cast both sides of a binary operator
					return false;
				}

				switch (binary->op) {
					case TokenT::CAST: {

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

											assert(false); // @ErrorMessage cannot compare equality of f32 and f64
											return false;
										}
									}
								}
								case TypeFlavor::FUNCTION: {
									if (!typesAreSame(left->type, right->type)) {
										assert(false); // @ErrorMessage cannot compare different function pointers
										return false;
									}
								}
								case TypeFlavor::INTEGER: {
									if (left->type == right->type) {
										if (left->type == &TYPE_UNSIGNED_INT_LITERAL || left->type == &TYPE_SIGNED_INT_LITERAL) {
											trySolidifyNumericLiteralToDefault(left);
											trySolidifyNumericLiteralToDefault(right);
										}
									}
									else {
										if ((left->type->flags & TYPE_NUMBER_IS_SIGNED) == (right->type->flags & TYPE_NUMBER_IS_SIGNED)) {

										}

										if (left->type == &TYPE_FLOAT_LITERAL || right->type == &TYPE_FLOAT_LITERAL) {
											if (!solidifyOneLiteral(binary)) return false;
										}
										else {
											assert((left->type == &TYPE_F32 && right->type == &TYPE_F64) || (left->type == &TYPE_F64 && right->type == &TYPE_F32));

											assert(false); // @ErrorMessage cannot compare equality of f32 and f64
											return false;
										}
									}
								}
								case TypeFlavor::NULL_: {
									left->type = &TYPE_U64;
									right->type = &TYPE_U64;
								}
								case TypeFlavor::POINTER: {

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
							if (left->type->flavor == TypeFlavor::NULL_) {

							}
							else if (right->type->flavor == TypeFlavor::NULL_) {

							}
							else if (left->type->flavor == TypeFlavor::AUTO_CAST) {

							}
							else if (right->type->flavor == TypeFlavor::AUTO_CAST) {

							}
						}

						expr->type = &TYPE_BOOL;

						break;
					}
					case TokenT::GREATER_EQUAL:
					case TokenT::LESS_EQUAL:
					case TOKEN('>'):
					case TOKEN('<'): {

					}
					case TOKEN('+'):
					case TOKEN('-'): {

					}
					case TOKEN('&'):
					case TOKEN('|'):
					case TOKEN('^'): {
						
					}
					case TOKEN('*'):
					case TOKEN('/'):
					case TOKEN('%'):
					case TokenT::SHIFT_LEFT:
					case TokenT::SHIFT_RIGHT: {

					}
					case TOKEN('='): {

					}
					case TokenT::PLUS_EQUALS:
					case TokenT::MINUS_EQUALS: {

					}
					case TokenT::LOGIC_AND:
					case TokenT::LOGIC_OR: {

					}
					case TokenT::AND_EQUALS:
					case TokenT::OR_EQUALS:
					case TokenT::XOR_EQUALS: {

					}
					case TokenT::TIMES_EQUALS:
					case TokenT::DIVIDE_EQUALS:
					case TokenT::MOD_EQUALS:
					case TokenT::SHIFT_LEFT_EQUALS:
					case TokenT::SHIFT_RIGHT_EQUALS: {

					}
					default:
						assert(false);
						return false;
				}
				
				break;
			}
			case ExprFlavor::FOR: {
				
				break;
			}
			case ExprFlavor::FUNCTION_CALL: {
				
				break;
			}
			case ExprFlavor::IF: {
				
				break;
			}
			case ExprFlavor::RETURN: {
				
				break;
			}
			case ExprFlavor::STRUCT_ACCESS: {
				
				break;
			}
			case ExprFlavor::UNARY_OPERATOR: {
				
				break;
			}
			case ExprFlavor::WHILE: {
				
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
			/*
			InferJob head;
			head.infer.function = declarations.data.function;
			head.type = InferType::FUNCTION_HEAD;

			flatten(head.typeFlattened, &head.infer.function->returnType);

			inferJobs.add(head);
			*/
			InferJob body;
			body.infer.function = declarations.data.function;
			body.type = InferType::FUNCTION_BODY;

			flatten(body.valueFlattened, &body.infer.function->body);

			inferJobs.add(body);
		}
		else if (declarations.count == 1) {
			addDeclaration(declarations.data.declaration);
		}
		else {
			inferJobs.reserve(inferJobs.count + declarations.count); // Make sure we don't do unnecessary allocations

			for (u64 i = 0; i < declarations.count; i++)
				addDeclaration(declarations.data.declarations[i]);
		}

		bool madeProgress;

		do {
			madeProgress = false;

			for (u64 i = 0; i < inferJobs.count; i++) {

				auto job = inferJobs[i];

				switch (job.type) {
					/*
					case InferType::FUNCTION_HEAD: {
						auto function = job.infer.function;

						madeProgress |= inferFlattened(job.typeFlattened, &job.typeFlattenedIndex);

						if (job.typeFlattenedIndex == job.typeFlattened.count) {
							if (!evaluateToType(&function->returnType)) {
								assert(false); // @ErrorMessage return type must be a type
								goto error;
							}

							bool argumentsInferred = true;

							for (auto argument : function->arguments.declarations) {
								if (!argument || !argument->type || argument->type->flavor != ExprFlavor::TYPE_LITERAL) {
									argumentsInferred = false;
									break;
								}
								
								if (argument->initialValue) {
									if (argument->flags & DECLARATION_VALUE_TYPE_CHECK_COMPLETE) {
										if (!evaluateToLiteral(&argument->type)) {
											assert(false); // @ErrorMessage, default argument must be a constant
											goto error;
										}
									}
									else {
										argumentsInferred = false;
										break;
									}
								}
							}

							if (argumentsInferred) {
								madeProgress = true;

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
						}

						break;
					}*/
					case InferType::FUNCTION_BODY: {
						if (!inferFlattened(job.valueFlattened, &job.valueFlattenedIndex)) {
							goto error;
						}

						if (job.valueFlattenedIndex == job.valueFlattened.count) {
							inferJobs.unordered_remove(i--);
						}

						break;
					}
					case InferType::DECLARATION: {
						auto declaration = job.infer.declaration;

						if (declaration->type) {
							if (!inferFlattened(job.typeFlattened, &job.typeFlattenedIndex, &madeProgress)) {
								goto error;
							}
						}

						if (declaration->initialValue) {
							if (!inferFlattened(job.valueFlattened, &job.valueFlattenedIndex, &madeProgress)) {
								goto error;
							}
						}

						if (declaration->type && job.typeFlattenedIndex == job.typeFlattened.count) {
							if (!evaluateToType(&declaration->type)) {
								assert(false); // @ErrorMessage could not evaluate type of declaration
								goto error;
							}

							madeProgress = true;
							declaration->flags |= DECLARATION_TYPE_IS_READY;
						}

						if (declaration->initialValue && job.valueFlattenedIndex == job.valueFlattened.count && (declaration->flags & DECLARATION_IS_CONSTANT)) {
							if (!evaluateToLiteral(&declaration->initialValue)) {
								assert(false); // @ErrorMessage the value of the declaration must be a constant
								goto error;
							}

							// We can't set DECLARATION_VALUE_IS_READY yet because it could be implicitly cast
						}

						if (declaration->type && job.typeFlattenedIndex == job.typeFlattened.count && declaration->initialValue && job.valueFlattenedIndex == job.valueFlattened.count) {
							assert(declaration->initialValue->type);

							madeProgress = true;
							inferJobs.unordered_remove(i--);
							// @Incomplete: Queue to later compile stage

							declaration->flags |= DECLARATION_VALUE_IS_READY;
						}
						else if (declaration->type && job.typeFlattenedIndex == job.typeFlattened.count) {

							madeProgress = true;
							inferJobs.unordered_remove(i--);
							// @Incomplete: Queue to later compile stage

							declaration->flags |= DECLARATION_VALUE_IS_READY;
						}
						else if (declaration->initialValue && job.valueFlattenedIndex == job.valueFlattened.count) {
							assert(declaration->initialValue->type);

							trySolidifyNumericLiteralToDefault(declaration->initialValue);
							
							if (declaration->initialValue->type == &TYPE_NULL) {
								assert(declaration->initialValue->flavor == ExprFlavor::INT_LITERAL);

								assert(false); // @ErrorMessage cannot infer the type from null
								goto error;
							}
							else if (declaration->initialValue->type == &TYPE_VOID) {
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

						break;
					}
				}
			}
		} while (madeProgress);
	}
error:;
}