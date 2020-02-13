#include "Basic.h"
#include "Infer.h"
#include "Array.h"
#include "Parser.h"

enum class InferType {
	FUNCTION_HEAD, 
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

	Array<Expr *> typeFlattened;
	u64 typeFlattenedIndex = 0;

	Array<Expr *> valueFlattened;
	u64 valueFlattenedIndex = 0;
};

extern WorkQueue<DeclarationPack> inferQueue;

static Array<InferJob> inferJobs;


void flatten(Array<Expr *> &flattenTo, Expr *expr) {
	switch (expr->flavor) {
		case ExprFlavor::IDENTIFIER:
		case ExprFlavor::FUNCTION: {
			flattenTo.add(expr);
			break;
		}
		case ExprFlavor::TYPE_LITERAL: {
			ExprLiteral *literal = static_cast<ExprLiteral *>(expr);

			Type *type = literal->typeValue;


			// We don't need to add the type literal itself since we guaranteed know the type, but we do need to add the subcomponents of the type so that we infer those
			if (type->flavor == TypeFlavor::FUNCTION) {
				TypeFunction *function = static_cast<TypeFunction *>(type);

				flatten(flattenTo, function->returnType);

				for (u64 i = 0; i < function->argumentCount; i++) {
					flatten(flattenTo, function->argumentTypes[i]);
				}
			}
			else if (type->flavor == TypeFlavor::FUNCTION) {
				TypePointer *pointer = static_cast<TypePointer *>(type);

				flatten(flattenTo, pointer->pointerTo);
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
			ExprBinaryOperator *binary = static_cast<ExprBinaryOperator *>(expr);

			flatten(flattenTo, binary->left);
			flatten(flattenTo, binary->right);

			flattenTo.add(expr);

			break;
		}
		case ExprFlavor::BLOCK: {
			ExprBlock *block = static_cast<ExprBlock *>(expr);

			for (auto subExpr : block->exprs)
				flatten(flattenTo, subExpr);

			break;
		}
		case ExprFlavor::FOR: {
			ExprLoop *loop = static_cast<ExprLoop *>(loop);

			flatten(flattenTo, loop->forBegin);

			if (loop->forEnd)
				flatten(flattenTo, loop->forEnd);

			flattenTo.add(expr);
			
			if (loop->body)
				flatten(flattenTo, loop->body);

			break;
		}
		case ExprFlavor::FUNCTION_CALL: {
			ExprFunctionCall *call = static_cast<ExprFunctionCall *>(expr);

			for (u64 i = 0; i < call->argumentCount; i++) {
				flatten(flattenTo, call->arguments[i]);
			}

			flatten(flattenTo, call->function);
			flattenTo.add(expr);

			break;
		}
		case ExprFlavor::IF: {
			ExprIf *ifElse = static_cast<ExprIf *>(expr);
			
			flatten(flattenTo, ifElse->condition);
			flattenTo.add(expr);
			
			if (ifElse->ifBody)
				flatten(flattenTo, ifElse->ifBody);

			if (ifElse->elseBody)
				flatten(flattenTo, ifElse->elseBody);

			break;
		}
		case ExprFlavor::RETURN: {
			ExprReturn *return_ = static_cast<ExprReturn *>(expr);

			if (return_->value)
				flatten(flattenTo, return_->value);
			
			flattenTo.add(expr);

			break;
		}
		case ExprFlavor::STRUCT_ACCESS: {
			ExprStructAccess *access = static_cast<ExprStructAccess *>(expr);

			flatten(flattenTo, access->left);
			flattenTo.add(expr);

			break;
		}
		case ExprFlavor::UNARY_OPERATOR: {
			ExprUnaryOperator *unary = static_cast<ExprUnaryOperator *>(expr);

			flatten(flattenTo, unary->value);
			flattenTo.add(expr);

			break;
		}
		case ExprFlavor::WHILE: {
			ExprLoop *loop = static_cast<ExprLoop *>(expr);

			flatten(flattenTo, loop->whileCondition);
			flattenTo.add(expr);

			if (loop->body)
				flatten(flattenTo, loop->body);

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
		flatten(job.typeFlattened, declaration->type);
	}

	if (declaration->initialValue) {
		flatten(job.valueFlattened, declaration->initialValue);
	}

	inferJobs.add(job);
}

void runInfer() {
	bool exit = false;

	while (!exit) {
		DeclarationPack declarations = inferQueue.take();

		if (!declarations.count && !declarations.data.function) {
			exit = true;
		} else if (declarations.count == 0) {
			InferJob head;
			head.infer.function = declarations.data.function;
			head.type = InferType::FUNCTION_HEAD;

			flatten(head.typeFlattened, head.infer.function->returnType);

			inferJobs.add(head);

			InferJob body;
			body.infer.function = declarations.data.function;
			body.type = InferType::FUNCTION_BODY;

			flatten(body.valueFlattened, head.infer.function->body);

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
									type->argumentTypes = new Expr *
								}
							}
						}

						break;
					}
					case InferType::FUNCTION_BODY: {
						break;
					}
					case InferType::DECLARATION: {
						break;
					}
				}
			}
		} while (madeProgress);
	}
#undef EXIT
error:;
}