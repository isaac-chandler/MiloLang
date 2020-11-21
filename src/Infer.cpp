#include "Basic.h"
#include "Infer.h"
#include "Array.h"
#include "Parser.h"
#include "Lexer.h"
#include "IrGenerator.h"
#include "CoffWriter.h"
#include "TypeTable.h"
#include "ArraySet.h"


BucketedArenaAllocator inferArena(1024 * 1024);

#if 1
#define INFER_NEW(T) new (static_cast<T *>(inferArena.allocate(sizeof(T)))) T
#define INFER_NEW_ARRAY(T, C) new (static_cast<T *>(inferArena.allocate((C) * sizeof(T)))) T[C]

#else
#define PARSER_NEW(T) new T
#define PARSER_NEW_ARRAY(T, C) new T[C]
#endif

enum class JobFlavor {
	SIZE,
	DECLARATION_TYPE,
	DECLARATION_VALUE,
	FUNCTION_TYPE,
	FUNCTION_VALUE,
	IMPORTER
};

ExprLiteral *inferMakeTypeLiteral(CodeLocation &start, EndLocation &end, Type *type) {
	ExprLiteral *literal = INFER_NEW(ExprLiteral);
	literal->flavor = ExprFlavor::TYPE_LITERAL;
	literal->start = start;
	literal->end = end;
	literal->typeValue = type;
	literal->type = &TYPE_TYPE;

	return literal;
}

ExprLiteral *createFloatLiteral(CodeLocation start, EndLocation end, Type *type, double value) {
	auto literal = INFER_NEW(ExprLiteral);
	literal->flavor = ExprFlavor::FLOAT_LITERAL;
	literal->type = type;
	literal->floatValue = value;
	literal->start = start;
	literal->end = end;

	return literal;
}

ExprLiteral *createIntLiteral(CodeLocation start, EndLocation end, Type *type, u64 value) {
	auto literal = INFER_NEW(ExprLiteral);
	literal->flavor = ExprFlavor::INT_LITERAL;
	literal->type = type;
	literal->unsignedValue = value;
	literal->start = start;
	literal->end = end;

	return literal;
}

ExprLiteral *createInBoundsIntLiteral(CodeLocation start, EndLocation end, Type *type, u64 value) {
	if (type == &TYPE_BOOL) {
		value = value != 0;
	}
	else if (type == &TYPE_S8) {
		value &= 0xFF;

		if (value & 0x80) {
			value |= 0xFFFF'FFFF'FFFF'FF00;
		}
	}
	else if (type == &TYPE_S16) {
		value &= 0xFFFF;

		if (value & 0x8000) {
			value |= 0xFFFF'FFFF'FFFF'0000;
		}
	}
	else if (type == &TYPE_S32) {
		value &= 0xFFFF'FFFF;

		if (value & 0x8000'0000) {
			value |= 0xFFFF'FFFF'0000'0000;
		}
	}
	else if (type == &TYPE_U8) {
		value &= 0xFF;
	}
	else if (type == &TYPE_U16) {
		value &= 0xFFFF;
	}
	else if (type == &TYPE_U32) {
		value &= 0xFFFF'FFFF;
	}

	return createIntLiteral(start, end, type, value);
}

struct SubJob {
	Array<Type *> *sizeDependencies;

	Array<Array<Expr **>> flatteneds;
	Array<u64> indices;
	u64 flattenedCount = 0;

	JobFlavor flavor;
	String sleepingOnName;

	SubJob(JobFlavor flavor, Array<Type *> *sizeDependencies) : flavor(flavor), sizeDependencies(sizeDependencies) {}
};

bool isDone(SubJob *job) {
	return job->flattenedCount == 0;
}

Expr **getHalt(SubJob *job) {
	return job->flatteneds[job->flattenedCount - 1][job->indices[job->flattenedCount - 1]];
}

void goToSleep(SubJob *job, Array<SubJob *> *sleepingOnMe, String name = String(nullptr, 0ULL)) {
	_ReadWriteBarrier();

	sleepingOnMe->add(job);
}

template<typename T>
void removeJob(T **first, T *job) {
	assert(job->previous || job->next || job == *first);

	if (job->previous) {
		job->previous->next = job->next;
	}
	else {
		*first = job->next;

	}

	if (job->next) {
		job->next->previous = job->previous;
	}

	job->next = nullptr;
}

template<typename T>
void addJob(T **first, T *job) {
	job->next = *first;
	job->previous = nullptr;

	if (job->next) {
		job->next->previous = job;
	}

	assert(job != job->next);
	assert(job != job->previous);

	*first = job;
}

struct DeclarationJob {
	Declaration *declaration;

	SubJob type;
	SubJob value;

	Array<Type *> sizeDependencies;

	DeclarationJob *next = nullptr;
	DeclarationJob *previous = nullptr;

	DeclarationJob() : type(JobFlavor::DECLARATION_TYPE, nullptr),
		value(JobFlavor::DECLARATION_VALUE, &sizeDependencies) {}
};

struct FunctionJob {
	ExprFunction *function;

	SubJob type;
	SubJob value;

	Array<Type *> sizeDependencies;
	FunctionJob *next = nullptr;
	FunctionJob *previous = nullptr;

	FunctionJob() : type(JobFlavor::FUNCTION_TYPE, nullptr),
		value(JobFlavor::FUNCTION_VALUE, &sizeDependencies) {}
};

struct SizeJob : SubJob {
	Type *type;

	u64 sizingIndexInMembers = 0;
	u64 runningSize = 0;

	SizeJob *next = nullptr;
	SizeJob *previous = nullptr;

	SizeJob() : SubJob(JobFlavor::SIZE, nullptr) {}
};

struct ImporterJob : SubJob {
	Importer *importer;

	ImporterJob *next = nullptr;
	ImporterJob *previous = nullptr;

	ImporterJob() : SubJob(JobFlavor::IMPORTER, nullptr) {}
};

DeclarationJob *declarationJobs;
FunctionJob *functionJobs;
SizeJob *sizeJobs;
ImporterJob *importerJobs;

Array<SubJob *> subJobs;
Array<SubJob *> priorityJobs;

inline void addSubJob(SubJob *job, bool highPriority = false) {
#if BUILD_DEBUG
	for (auto x : priorityJobs) {
		assert(x != job);
	}

	for (auto x : subJobs) {
		assert(x != job);
	}
#endif

	if (highPriority) {
		priorityJobs.add(job);
	}
	else {
		subJobs.add(job);
	}
}

bool addDeclaration(Declaration *declaration);

void wakeUpSleepers(Array<SubJob *> *sleepers, bool priority = false, String name = String(nullptr, 0ULL)) {
	if (name.length == 0) {
		for (auto sleeper : *sleepers) {
			addSubJob(sleeper);
		}

		sleepers->clear();
	}
	else {
		for (u64 i = 0; i < sleepers->count; i++) {
			auto sleeper = (*sleepers)[i];

			if (sleeper->sleepingOnName.length == 0 || name == sleeper->sleepingOnName) {
				sleepers->unordered_remove(i--);
				addSubJob(sleeper, priority);
			}
		}
	}
}

void addImporter(Importer *importer);

void addBlock(Block *block) {
	if (block->flags & BLOCK_IS_QUEUED)
		return;

	block->flags |= BLOCK_IS_QUEUED;

	for (auto &declaration : block->declarations) {
		auto success = addDeclaration(declaration);

		assert(success); // Adding a block should never fail, the only failing cases for addDeclaration are if it is added to global scope
	}

	for (auto importer : block->importers) {
		addImporter(importer);
	}

	wakeUpSleepers(&block->sleepingOnMe);
}

SizeJob *allocateSizeJob();
FunctionJob *allocateFunctionJob();

void beginFlatten(SubJob *job, Expr **expr);

void addFunction(ExprFunction *function) {
	if (function->arguments.flags & BLOCK_IS_QUEUED)
		return;

	FunctionJob *job = allocateFunctionJob();

	job->function = function;

	addBlock(&function->arguments);
	addBlock(&function->returns);

	assert(function->body);
	beginFlatten(&job->value, &function->body);

	addSubJob(&job->type, true);

	addSubJob(&job->value);


	++totalFunctions;

	addJob(&functionJobs, job);
}

void addTypeBlock(Expr *expr) {
	auto type = static_cast<ExprLiteral *>(expr)->typeValue;

	if (type->flavor == TypeFlavor::STRUCT) {
		if (!type->sizeJob) {
			auto struct_ = static_cast<TypeStruct *>(type);

			addBlock(&struct_->members);

			SizeJob *job = allocateSizeJob();

			job->type = type;

			type->name = expr->valueOfDeclaration ? expr->valueOfDeclaration->name : "(struct)";
			addStruct(struct_);

			type->sizeJob = job;
			++totalTypesSized;

			addJob(&sizeJobs, job);
			addSubJob(job);
		}
	}
	else if (type->flavor == TypeFlavor::ENUM) {
		if (!type->sizeJob) {
			auto enum_ = static_cast<TypeEnum *>(type);

			addBlock(enum_->values);
			addBlock(&enum_->members);

			SizeJob *job = allocateSizeJob();

			job->type = type;

			type->name = expr->valueOfDeclaration ? expr->valueOfDeclaration->name : "(enum)";
			addStruct(enum_);

			beginFlatten(job, &CAST_FROM_SUBSTRUCT(ExprEnum, struct_, enum_)->integerType);

			type->sizeJob = job;
			++totalTypesSized;

			addJob(&sizeJobs, job);
			addSubJob(job, true);
		}
	}
}

void flatten(Array<Expr **> &flattenTo, Expr **expr) {
	switch ((*expr)->flavor) {
	case ExprFlavor::IDENTIFIER: {
		auto identifier = static_cast<ExprIdentifier *>(*expr);

		if (identifier->type == &TYPE_UNARY_DOT)
			return;

		if (identifier->structAccess) {
			flatten(flattenTo, &identifier->structAccess);
		}

		flattenTo.add(expr);
		break;
	}
	case ExprFlavor::RUN: {
		auto run = static_cast<ExprRun *>(*expr);

		flatten(flattenTo, &run->expr);

		flattenTo.add(expr);
		break;
	}
	case ExprFlavor::SLICE: {
		auto slice = static_cast<ExprSlice *>(*expr);

		flatten(flattenTo, &slice->array);

		if (slice->sliceStart)
			flatten(flattenTo, &slice->sliceStart);
		if (slice->sliceEnd)
			flatten(flattenTo, &slice->sliceEnd);

		flattenTo.add(expr);

		break;
	}
	case ExprFlavor::DEFER: {
		auto defer = static_cast<ExprDefer *>(*expr);

		flatten(flattenTo, &defer->expr);

		break;
	}
	case ExprFlavor::STATIC_IF: {
		auto staticIf = static_cast<ExprIf *>(*expr);

		flatten(flattenTo, &staticIf->condition);

		flattenTo.add(expr);
		break;
	}
	case ExprFlavor::SWITCH: {
		auto switch_ = static_cast<ExprSwitch *>(*expr);

		flatten(flattenTo, &switch_->condition);

		for (auto &case_ : switch_->cases) {
			if (case_.condition) {
				flatten(flattenTo, &case_.condition);
			}
		}

		flattenTo.add(expr);

		for (auto &case_ : switch_->cases) {
			flatten(flattenTo, &case_.block);
		}

		break;
	}
	case ExprFlavor::FUNCTION: {
		auto function = static_cast<ExprFunction *>(*expr);

		addFunction(function);

		flattenTo.add(expr);

		break;
	}
	case ExprFlavor::FUNCTION_PROTOTYPE: {
		auto function = static_cast<ExprFunction *>(*expr);


		addBlock(&function->arguments);
		addBlock(&function->returns);


		flattenTo.add(expr);

		// The function arguments and returns are handled separately since they are declarations of their own so will be type-checked separately
		// The function body is type-checked separately from the head so we don't get circular dependencies with recursive functions
		// And the body's types are irrelevant to the expression using the function

		break;
	}
	case ExprFlavor::COMMA_ASSIGNMENT: {
		auto comma = static_cast<ExprCommaAssignment *>(*expr);

		if (!((*expr)->flags & EXPR_COMMA_ASSIGNMENT_IS_DECLARATION)) {

			for (u64 i = 0; i < comma->exprCount; i++) {
				flatten(flattenTo, &comma->left[i]);
			}
		}

		flatten(flattenTo, &comma->call);

		flattenTo.add(expr);
		break;
	}
	case ExprFlavor::TYPE_LITERAL: {
		addTypeBlock(*expr);

		break;
	}
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

		if (binary->left && !(binary->flags & EXPR_EQUALS_IS_IMPLICIT_SWITCH)) {
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

		addBlock(&block->declarations);

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

		for (u64 i = 0; i < call->arguments.count; i++) {
			flatten(flattenTo, &call->arguments.values[i]);
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

		for (u64 i = 0; i < return_->returns.count; i++)
			flatten(flattenTo, &return_->returns.values[i]);

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
	default:
		assert(false);
	}
}


void beginFlatten(SubJob *job, Expr **expr) {
	PROFILE_FUNC();

	if (job->flattenedCount == job->flatteneds.count) {
		job->flatteneds.add();
		job->indices.add(0);
	}
	else {
		job->flatteneds[job->flattenedCount].clear();
		job->indices[job->flattenedCount] = 0;
	}

	flatten(job->flatteneds[job->flattenedCount], expr);

	job->flattenedCount++;
}


void pushFlatten(SubJob *job) {
	auto expr = getHalt(job);

	job->indices[job->flattenedCount - 1]++;

	beginFlatten(job, expr);
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

bool boundsCheckImplicitConversion(Expr *location, Type *convertTo, ExprLiteral *convertFrom) {
	if (convertTo->flags & TYPE_INTEGER_IS_SIGNED) {
		s64 max = static_cast<s64>((1ULL << (convertTo->size * 8 - 1)) - 1);
		s64 min = -static_cast<s64>(max) - 1;

		if (convertFrom->flags & TYPE_INTEGER_IS_SIGNED) {
			if (convertFrom->signedValue > max) {
				reportError(location, "Error: Integer literal too large for %.*s (max: %" PRIi64 ", given: %" PRIi64 ")", STRING_PRINTF(convertTo->name), max, convertFrom->signedValue);
				return false;
			}

			if (convertFrom->signedValue < min) {
				reportError(location, "Error: Integer literal too small for %.*s (min: %" PRIi64 ", given: %" PRIi64 ")", STRING_PRINTF(convertTo->name), min, convertFrom->signedValue);
				return false;
			}
		}
		else {
			if (convertFrom->unsignedValue > static_cast<u64>(max)) {
				reportError(location, "Error: Integer literal too large for %.*s (max: %" PRIi64 ", given: %" PRIu64 ")", STRING_PRINTF(convertTo->name), max, convertFrom->unsignedValue);
				return false;
			}
		}
	}
	else {
		u64 max = convertTo == &TYPE_U64 ? UINT64_MAX : (1ULL << (convertTo->size * 8)) - 1;

		if ((convertFrom->flags & TYPE_INTEGER_IS_SIGNED) && convertFrom->signedValue < 0) {
			if (convertFrom->signedValue < 0) {
				reportError(location, "Error: Integer literal too small for %.*s (min: 0, given: %" PRIi64 ")", STRING_PRINTF(convertTo->name), convertFrom->signedValue);
				return false;
			}
		}

		if (convertFrom->unsignedValue > max) {
			reportError(location, "Error: Integer literal too large for %.*s (max: %" PRIu64 ", given: %" PRIu64 ")", STRING_PRINTF(convertTo->name), max, convertFrom->unsignedValue);
			return false;
		}
	}

	return true;
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

			if (literal->flags & EXPR_INTEGER_LITERAL_IS_NEGATIVE_ZERO && literal->unsignedValue == 0) {
				literal->floatValue = -0.0;
			}
			else {
				literal->floatValue = static_cast<double>(literal->unsignedValue);
			}

			literal->flavor = ExprFlavor::FLOAT_LITERAL;
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

			if (literal->flags & EXPR_INTEGER_LITERAL_IS_NEGATIVE_ZERO && literal->signedValue == 0) {
				literal->floatValue = -0.0;
			}
			else {
				literal->floatValue = static_cast<double>(literal->signedValue);
			}

			literal->flavor = ExprFlavor::FLOAT_LITERAL;
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

			if (literal->flags & EXPR_INTEGER_LITERAL_IS_NEGATIVE_ZERO && literal->unsignedValue == 0) {
				literal->floatValue = -0.0;
			}
			else {
				literal->floatValue = static_cast<double>(literal->unsignedValue);
			}

			literal->flavor = ExprFlavor::FLOAT_LITERAL;
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

			if (literal->flags & EXPR_INTEGER_LITERAL_IS_NEGATIVE_ZERO && literal->signedValue == 0) {
				literal->floatValue = -0.0;
			}
			else {
				literal->floatValue = static_cast<double>(literal->signedValue);
			}

			literal->flavor = ExprFlavor::FLOAT_LITERAL;
		}

		right->type = left->type;
	}

	return true;
}


SizeJob *allocateSizeJob();
DeclarationJob *allocateDeclarationJob();
FunctionJob *allocateFunctionJob();

bool isAddressable(Expr *expr);

bool isValidCast(Type *to, Type *from, u64 flags) {
	if (from == &TYPE_VOID || to->flavor == TypeFlavor::AUTO_CAST || from->flavor == TypeFlavor::NAMESPACE) {
		return false;
	}

	if (to == &TYPE_VOID || to->flavor == TypeFlavor::AUTO_CAST || to->flavor == TypeFlavor::NAMESPACE) {
		return false;
	}

	if ((flags & EXPR_CAST_IS_BITWISE) && to->size == from->size) {
		return true;
	}

	if (from == TYPE_ANY && to != TYPE_ANY) {
		return true;
	}

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
			return to == from || to == TYPE_ANY;
		}

		return true;
	}

	if (to == TYPE_ANY) {
		return true;
	}

	if (from->flavor == TypeFlavor::BOOL) {
		return to->flavor == TypeFlavor::INTEGER;
	}
	else if (from->flavor == TypeFlavor::TYPE) {
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

void addSizeDependency(Array<Type *> *sizeDependencies, Type *type) {
	if (sizeDependencies && type->size == 0 && !(type->flags & TYPE_IS_INTERNAL)) {
		sizeDependencies->add(type);
	}
}

void checkedRemoveLastSizeDependency(Array<Type *> *sizeDependencies, Type *type) {
	if (sizeDependencies && type->size == 0 && !(type->flags & TYPE_IS_INTERNAL)) {
		if (sizeDependencies->pop() != type) {
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
			*cast = createInBoundsIntLiteral(binary->start, expr->end, castTo, old->unsignedValue);
		}
		else if (castTo->flavor == TypeFlavor::FLOAT) {
			*cast = createFloatLiteral(binary->start, expr->end, castTo,
				(old->type->flags & TYPE_INTEGER_IS_SIGNED) ? static_cast<double>(old->signedValue) : static_cast<double>(old->unsignedValue));
		}
		else if (castTo->flavor == TypeFlavor::BOOL) {
			u64 value = old->unsignedValue;
			if (expr->type->flavor == TypeFlavor::ARRAY && (expr->type->flags & TYPE_ARRAY_IS_FIXED)) {
				auto array = static_cast<ExprArray *>(expr);
				value = array->count;
			}

			*cast = createIntLiteral(binary->start, expr->end, castTo, value ? 1 : 0);
		}
		else if (castTo->flavor == TypeFlavor::POINTER) {
			*cast = createIntLiteral(binary->start, expr->end, castTo, old->unsignedValue);
		}
	}
	else if (expr->flavor == ExprFlavor::FLOAT_LITERAL) {
		auto old = static_cast<ExprLiteral *>(expr);

		if (castTo->flavor == TypeFlavor::INTEGER) {
			u64 value = static_cast<u64>(old->floatValue);
			if (castTo->flags & TYPE_INTEGER_IS_SIGNED) {
				value = static_cast<u64>(static_cast<s64>(old->floatValue));
			}

			*cast = createInBoundsIntLiteral(binary->start, expr->end, castTo, value);
		}
		else if (castTo->flavor == TypeFlavor::FLOAT) {
			*cast = createFloatLiteral(binary->start, expr->end, castTo, old->floatValue);
		}
		else if (castTo->flavor == TypeFlavor::BOOL) {
			*cast = createIntLiteral(binary->start, expr->end, castTo, old->floatValue != 0.0);
		}
	}
	else if (expr->flavor == ExprFlavor::ARRAY) {
		if (castTo == &TYPE_BOOL) {
			auto array = static_cast<ExprArray *>(expr);
			*cast = createIntLiteral(binary->start, expr->end, castTo, array->count != 0 ? 1 : 0);
		}
	}
	else if (expr->flavor == ExprFlavor::STRING_LITERAL) {
		if (castTo == &TYPE_BOOL) {
			auto string = static_cast<ExprStringLiteral *>(expr);
			*cast = createIntLiteral(binary->start, expr->end, castTo, string->string.length != 0 ? 1 : 0);
		}
	}
}

void insertImplicitCast(Array<Type *> *sizeDependencies, Expr **castFrom, Type *castTo) {
	ExprBinaryOperator *cast = INFER_NEW(ExprBinaryOperator);
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

	addSizeDependency(sizeDependencies, castTo);

	assert(isValidCast(castTo, cast->right->type, 0));

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
	case ExprFlavor::TYPE_LITERAL:
	case ExprFlavor::FLOAT_LITERAL:
	case ExprFlavor::INT_LITERAL: {
		ExprLiteral *newLiteral = INFER_NEW(ExprLiteral);
		*newLiteral = *static_cast<ExprLiteral *>(expr);
		newLiteral->start = (*exprPointer)->start;
		newLiteral->end = (*exprPointer)->end;

		*exprPointer = newLiteral;

		break;
	}
	}
}

bool switchCasesAreSame(Expr *a, Expr *b) {
	assert(a->type == b->type);
	assert(a->flavor == b->flavor);

	assert(a->type->size);
	assert(b->type->size);

	auto aLiteral = static_cast<ExprLiteral *>(a);
	auto bLiteral = static_cast<ExprLiteral *>(b);

	switch (a->flavor) {
	case ExprFlavor::INT_LITERAL:
		if (a->type->flags & TYPE_INTEGER_IS_SIGNED)
			aLiteral->unsignedValue == bLiteral->unsignedValue;
	case ExprFlavor::FLOAT_LITERAL:
		if (a->type == &TYPE_F32)
			return static_cast<float>(aLiteral->floatValue) == static_cast<float>(bLiteral->floatValue);
		else
			return aLiteral->floatValue == bLiteral->floatValue;
	case ExprFlavor::TYPE_LITERAL:
		return aLiteral->typeValue == bLiteral->typeValue;
	case ExprFlavor::STRING_LITERAL:
		return static_cast<ExprStringLiteral *>(a)->string == static_cast<ExprStringLiteral *>(b)->string;
	case ExprFlavor::FUNCTION:
		return a == b;
	default:
		assert(false);
		return false;
	}
}


Expr *pushStructAccessDown(ExprIdentifier *accesses, Expr *base, CodeLocation start, EndLocation end) {
	if (!accesses) {
		return base;
	}

	auto result = INFER_NEW(ExprIdentifier);
	auto current = result;

	while (true) {
		*current = *accesses;
		current->start = start;
		current->end = end;

		if (current->structAccess) {
			current->structAccess = INFER_NEW(ExprIdentifier);
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

bool inferIdentifier(SubJob *job, Expr **exprPointer, ExprIdentifier *identifier, bool *yield) {
	*yield = false;

	if ((identifier->declaration->flags & DECLARATION_IMPORTED_BY_USING) && !(identifier->declaration->flags & DECLARATION_IS_CONSTANT)) {
		if (identifier->declaration->initialValue) {
			auto current = static_cast<ExprIdentifier *>(identifier->declaration->initialValue);

			if (job->sizeDependencies) {
				while (current->structAccess) {
					bool onlyConstants;
					addSizeDependency(job->sizeDependencies, getExpressionNamespace(current->structAccess, &onlyConstants, current->structAccess));

					current = static_cast<ExprIdentifier *>(current->structAccess);
					assert(current->flavor == ExprFlavor::IDENTIFIER);
				}
			}

			identifier->structAccess = pushStructAccessDown(static_cast<ExprIdentifier *>(identifier->declaration->initialValue), identifier->structAccess, identifier->start, identifier->end);
		}
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
				addSizeDependency(job->sizeDependencies, identifier->type);
			}
		}
		else {
			goToSleep(job, &identifier->declaration->sleepingOnMyType);

			*yield = true;
			return false;
		}

		if (identifier->declaration->flags & DECLARATION_IS_CONSTANT) {
			if (identifier->declaration->flags & DECLARATION_VALUE_IS_READY) {
				copyLiteral(exprPointer, identifier->declaration->initialValue);
			}
			else {
				goToSleep(job, &identifier->declaration->sleepingOnMyValue);

				*yield = true;
				return false;
			}
		}


	}

	return true;
}

bool inferUnaryDot(SubJob *job, TypeEnum *enum_, Expr **exprPointer, bool *yield) {
	auto identifier = static_cast<ExprIdentifier *>(*exprPointer);
	assert(identifier->flavor == ExprFlavor::IDENTIFIER);

	if (!identifier->declaration) {
		bool yield;
		auto member = findDeclaration(&enum_->members, identifier->name, &yield);

		if (yield) {
			goToSleep(job, &enum_->members.sleepingOnMe, identifier->name);

			return true;
		}

		if (!member) {
			reportError(identifier, "Error: %.*s does not have member %.*s", STRING_PRINTF(enum_->name), STRING_PRINTF(identifier->name));
			return false;
		}
		else if (!(member->flags & DECLARATION_IS_CONSTANT)) {
			reportError(identifier, "Error: Can only access constant members of %.*s from a unary dot", STRING_PRINTF(enum_->name));
			return false;
		}

		identifier->declaration = member;
	}

	assert(identifier->declaration);

	if (!inferIdentifier(job, exprPointer, identifier, yield)) {
		return *yield;
	}

	return true;
}
bool tryUsingConversion(SubJob *job, Type *correct, Expr **exprPointer, bool *yield) {
	*yield = false;
	auto given = *exprPointer;

	if (correct == given->type) {
		return true;
	}

	bool onlyConstants = false;
	auto struct_ = getExpressionNamespace(given, &onlyConstants, nullptr);


	if (!struct_)
		return false;

	for (auto importer : struct_->members.importers) {
		if (!(importer->flags & IMPORTER_IS_COMPLETE)) {
			*yield = true;
			return false;
		}
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

	auto identifier = INFER_NEW(ExprIdentifier);
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

		auto unary = INFER_NEW(ExprUnaryOperator);

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

bool tryAutoCast(SubJob *job, Expr **cast, Type *castTo, bool *yield) {
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

	if (!isValidCast(castTo, autoCast->right->type, autoCast->flags)) {
		return false;
	}

	addSizeDependency(job->sizeDependencies, castTo);
	doConstantCast(cast);

	return true;
}

bool binaryOpForFloat(Expr **exprPointer) {
	auto binary = static_cast<ExprBinaryOperator *>(*exprPointer);
	auto &left = binary->left;
	auto &right = binary->right;

	if (left->type != right->type) {
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

bool evaluateConstantBinary(SubJob *job, Expr **exprPointer, bool *yield) {
	auto binary = static_cast<ExprBinaryOperator *>(*exprPointer);

	auto left = static_cast<ExprLiteral *>(binary->left);
	auto right = static_cast<ExprLiteral *>(binary->right);

	auto stringLeft = static_cast<ExprStringLiteral *>(binary->left);
	auto stringRight = static_cast<ExprStringLiteral *>(binary->right);

	switch (binary->op) {
	case TokenT::CAST: {
		doConstantCast(exprPointer);
		break;
	}
	case TOKEN('['): {
		if (right->flavor != ExprFlavor::INT_LITERAL)
			break;

		if (binary->left->type->flavor == TypeFlavor::ARRAY && 
			(binary->left->type->flags & TYPE_ARRAY_IS_FIXED) && 
			(binary->left->flavor == ExprFlavor::ARRAY || binary->left->flavor == ExprFlavor::INT_LITERAL)) {
			auto arrayType = static_cast<TypeArray *>(binary->left->type);

			if ((right->type->flags & TYPE_INTEGER_IS_SIGNED) && right->signedValue < 0) {
				reportError(binary, "Error: Out of bounds index of array constant, Length: %" PRIu64 ", Index: %" PRIi64, arrayType->count, right->signedValue);
				return false;
			}
			else if (right->unsignedValue >= arrayType->count) {
				reportError(binary, "Error: Out of bounds index of array constant, Length: %" PRIu64 ", Index: %" PRIu64, arrayType->count, right->unsignedValue);
				return false;
			}
			else {
				if (binary->left->flavor == ExprFlavor::ARRAY) {
					auto array = static_cast<ExprArray *>(binary->left);
					// @Speed We can't just look up the index directly since in order to save memory when compiling, 
					// if the array contains the same value repeated to the end, only the first repeat will be stored, and the a null element
					for (u64 i = 0; i <= right->unsignedValue; i++) {
						if (i == right->unsignedValue || array->storage[i + 1] == nullptr) {
							*exprPointer = array->storage[i];
							break;
						}
					}
				}
				else {
					return createIntLiteral(binary->start, binary->end, arrayType->arrayOf, 0);
				}
			}
		}
		else if (binary->left->flavor == ExprFlavor::STRING_LITERAL) {
			auto string = static_cast<ExprStringLiteral *>(binary->left);

			if ((right->type->flags & TYPE_INTEGER_IS_SIGNED) && right->signedValue < 0) {
				reportError(binary, "Error: Out of bounds index of string constant, Length: %" PRIu64 ", Index: %" PRIi64, string->string.length, right->signedValue);
				return false;
			}
			else if (right->unsignedValue >= string->string.length) {
				reportError(binary, "Error: Out of bounds index of array constant, Length: %" PRIu64 ", Index: %" PRIu64, string->string.length, right->unsignedValue);
				return false;
			}
			else {
				return createIntLiteral(binary->start, binary->end, &TYPE_U8, string->string.characters[right->unsignedValue]);
			}
		}

		break;
	}
	case TokenT::NOT_EQUAL: {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->unsignedValue != right->unsignedValue);
		}
		else if (binary->left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->floatValue != right->floatValue);
		}
		else if (binary->left->flavor == ExprFlavor::STRING_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(stringLeft->start, stringRight->end, &TYPE_BOOL, stringLeft->string != stringRight->string);
		}
		break;
	}
	case TokenT::EQUAL: {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->unsignedValue == right->unsignedValue);
		}
		else if (binary->left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->floatValue == right->floatValue);
		}
		else if (binary->left->flavor == ExprFlavor::STRING_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(stringLeft->start, stringRight->end, &TYPE_BOOL, stringLeft->string == stringRight->string);
		}
		break;
	}
	case TokenT::GREATER_EQUAL: {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->signedValue >= right->signedValue);
		}
		else if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->unsignedValue >= right->unsignedValue);
		}
		else if (binary->left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->floatValue >= right->floatValue);
		}
		break;
	}
	case TokenT::LESS_EQUAL: {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->signedValue <= right->signedValue);
		}
		else if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->unsignedValue <= right->unsignedValue);
		}
		else if (binary->left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->floatValue <= right->floatValue);
		}
		break;
	}
	case TOKEN('>'): {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->signedValue > right->signedValue);
		}
		else if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->unsignedValue > right->unsignedValue);
		}
		else if (binary->left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->floatValue > right->floatValue);
		}
		break;
	}
	case TOKEN('<'): {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->signedValue < right->signedValue);
		}
		else if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->unsignedValue < right->unsignedValue);
		}
		else if (binary->left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->floatValue < right->floatValue);
		}
		break;
	}
	case TOKEN('+'): {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			if (left->type->flavor == TypeFlavor::INTEGER) {
				auto literal = createInBoundsIntLiteral(left->start, right->end, left->type, left->signedValue + right->signedValue);

				if (literal->type == &TYPE_SIGNED_INT_LITERAL && literal->signedValue >= 0) {
					literal->type = &TYPE_UNSIGNED_INT_LITERAL;
				}

				*exprPointer = literal;
			}
			else {
				assert(left->type->flavor == TypeFlavor::POINTER);
				
				auto pointerTo = static_cast<TypePointer *>(left->type)->pointerTo;

				if (!pointerTo->size) {
					goToSleep(job, &pointerTo->sleepingOnMe);
					*yield = true;
					return false;
				}

				*exprPointer = createIntLiteral(left->start, right->end, left->type, left->unsignedValue + right->unsignedValue * pointerTo->size);
			}
		}
		else if (binary->left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createFloatLiteral(left->start, right->end, left->type, left->floatValue < right->floatValue);
		}
		break;
	}
	case TOKEN('-'): {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			if (left->type->flavor == TypeFlavor::INTEGER) {
				auto literal = createInBoundsIntLiteral(left->start, right->end, left->type, left->signedValue - right->signedValue);

				if (literal->type == &TYPE_UNSIGNED_INT_LITERAL && left->unsignedValue < right->unsignedValue) {
					literal->type = &TYPE_SIGNED_INT_LITERAL;
				}
				else if (literal->type == &TYPE_SIGNED_INT_LITERAL && literal->signedValue >= 0) {
					literal->type = &TYPE_UNSIGNED_INT_LITERAL;
				}

				*exprPointer = literal;
			}
			else {
				assert(left->type->flavor == TypeFlavor::POINTER);
				
				auto pointerTo = static_cast<TypePointer *>(left->type)->pointerTo;

				if (!pointerTo->size) {
					goToSleep(job, &pointerTo->sleepingOnMe);
					*yield = true;
					return false;
				}

				if (right->type == left->type) {
					*exprPointer = createIntLiteral(left->start, right->end, &TYPE_S64, (left->signedValue - right->signedValue) / pointerTo->size);
				}
				else {
					*exprPointer = createIntLiteral(left->start, right->end, left->type, left->unsignedValue - right->unsignedValue * pointerTo->size);
				}
			}
		}
		else if (binary->left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createFloatLiteral(left->start, right->end, left->type, left->floatValue < right->floatValue);
		}
		break;
	}
	case TOKEN('&'): {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue & right->unsignedValue);
		}
		break;
	}
	case TOKEN('|'): {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue | right->unsignedValue);
		}
		break;
	}
	case TOKEN('^'): {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue ^ right->unsignedValue);
		}
		break;
	}
	case TokenT::SHIFT_LEFT: {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue << right->unsignedValue);
		}
		break;
	}
	case TokenT::SHIFT_RIGHT: {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->signedValue >> right->signedValue);
		}
		else if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue >> right->unsignedValue);
		}
		break;
	}
	case TOKEN('*'): {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			auto literal = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue * right->unsignedValue);

			if (left->type == &TYPE_SIGNED_INT_LITERAL && literal->signedValue >= 0) {
				literal->type = &TYPE_UNSIGNED_INT_LITERAL;
			}

			*exprPointer = literal;
		}
		else if (binary->left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createFloatLiteral(left->start, right->end, left->type, left->floatValue * right->floatValue);
		}
		break;
	}
	case TOKEN('/'): {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
			auto literal = createInBoundsIntLiteral(left->start, right->end, left->type, left->signedValue / right->signedValue);

			if (left->type == &TYPE_SIGNED_INT_LITERAL && literal->signedValue >= 0) {
				literal->type = &TYPE_UNSIGNED_INT_LITERAL;
			}

			*exprPointer = literal;
		}
		else if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue / right->unsignedValue);
		}
		else if (binary->left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createFloatLiteral(left->start, right->end, left->type, left->floatValue * right->floatValue);
		}
		break;
	}
	case TOKEN('%'): {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
			auto literal = createInBoundsIntLiteral(left->start, right->end, left->type, left->signedValue % right->signedValue);

			if (left->type == &TYPE_SIGNED_INT_LITERAL && literal->signedValue >= 0) {
				literal->type = &TYPE_UNSIGNED_INT_LITERAL;
			}

			*exprPointer = literal;
		}
		else if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue % right->unsignedValue);
		}
		else if (binary->left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createFloatLiteral(left->start, right->end, left->type, fmod(left->floatValue, right->floatValue));
		}
		break;
	}
	case TokenT::LOGIC_AND: {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue != 0 && right->unsignedValue != 0);
		}
		break;
	}
	case TokenT::LOGIC_OR: {
		if (binary->left->flavor != binary->right->flavor)
			break;

		if (binary->left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue != 0 || right->unsignedValue != 0);
		}
		break;
	}
	}

	return true;
}

bool binaryOpForInteger(Array<Type *> *sizeDependencies, Expr **exprPointer) {
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

	if (left->type != right->type) {
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
				insertImplicitCast(sizeDependencies, &right, left->type);
			}
			else if (right->type->size > left->type->size) {
				insertImplicitCast(sizeDependencies, &left, right->type);
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

bool assignOpForInteger(Array<Type *> *sizeDependencies, ExprBinaryOperator *binary) {
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
				insertImplicitCast(sizeDependencies, &right, left->type);
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

bool binaryOpForAutoCast(SubJob *job, ExprBinaryOperator *binary, bool *yield) {
	auto &left = binary->left;
	auto &right = binary->right;

	if (left->type == &TYPE_AUTO_CAST) {
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
	else if (right->type == &TYPE_AUTO_CAST) {
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

bool assignOpForAutoCast(SubJob *job, ExprBinaryOperator *binary, bool *yield) {
	auto &right = binary->right;

	if (right->type == &TYPE_AUTO_CAST) {
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
	if ((expr->flavor == ExprFlavor::UNARY_OPERATOR && static_cast<ExprUnaryOperator *>(expr)->op == TokenT::SHIFT_LEFT)) {
		return true;
	}
	else if (expr->flavor == ExprFlavor::BINARY_OPERATOR && static_cast<ExprBinaryOperator *>(expr)->op == TOKEN('[')) {
		auto binary = static_cast<ExprBinaryOperator *>(expr);

		return !(binary->left->type->flags & TYPE_ARRAY_IS_FIXED) || isAssignable(binary->left);
	}
	else if (expr->flavor == ExprFlavor::IDENTIFIER) {
		auto identifier = static_cast<ExprIdentifier *>(expr);
		auto access = identifier->structAccess;

		if (!access) return !(identifier->declaration->flags & (DECLARATION_IS_ARGUMENT | DECLARATION_IS_ITERATOR | DECLARATION_IS_ITERATOR_INDEX));

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
	return ((expr->flavor == ExprFlavor::INT_LITERAL || expr->flavor == ExprFlavor::FLOAT_LITERAL))
		|| expr->flavor == ExprFlavor::TYPE_LITERAL || expr->flavor == ExprFlavor::STRING_LITERAL || expr->flavor == ExprFlavor::FUNCTION || (expr->flavor == ExprFlavor::ARRAY && arrayIsLiteral(static_cast<ExprArray *>(expr))) || expr->flavor == ExprFlavor::STRUCT_DEFAULT;
}

bool defaultValueIsZero(SubJob *job, Type *type, bool *yield) {
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
		return false; // @StringFormat
	}
	case TypeFlavor::ARRAY: {
		if (type->flags & TYPE_ARRAY_IS_FIXED) {
			return defaultValueIsZero(job, static_cast<TypeArray *>(type)->arrayOf, yield);
		}
		else {
			return true;
		}
	}
	case TypeFlavor::ENUM: {
		return type->flags & TYPE_ENUM_IS_FLAGS ? true : false;
	}
	case TypeFlavor::STRUCT: {
		auto struct_ = static_cast<TypeStruct *>(type);

		for (auto member : struct_->members.declarations) {
			if (member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_IMPLICIT_IMPORT)) continue;

			if (member->flags & DECLARATION_IS_UNINITIALIZED) return false;

			if (!(member->flags & DECLARATION_VALUE_IS_READY)) {
				goToSleep(job, &member->sleepingOnMyValue);

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
	default: {
		return false;
	}
	}
}

Expr *createDefaultValue(SubJob *job, Declaration *location, Type *type, bool *shouldYield) {
	*shouldYield = false;
	bool yield;

	if (defaultValueIsZero(job, type, &yield)) {
		return createIntLiteral(location->start, location->end, type, 0);
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
		Expr *literal = INFER_NEW(Expr);
		literal->flavor = ExprFlavor::STRUCT_DEFAULT;
		literal->type = type;

		return literal;
	}
	case TypeFlavor::STRING: {
		ExprStringLiteral *empty = INFER_NEW(ExprStringLiteral);
		empty->flavor = ExprFlavor::STRING_LITERAL;
		empty->string = "";
		empty->type = type;

		return empty;

	}
	case TypeFlavor::ARRAY: {
		ExprArray *defaults = INFER_NEW(ExprArray);
		defaults->flavor = ExprFlavor::ARRAY;
		defaults->type = type;


		if (type->flags & TYPE_ARRAY_IS_FIXED) {
			auto array = static_cast<TypeArray *>(type);
			defaults->count = array->count;


			defaults->storage = INFER_NEW_ARRAY(Expr *, my_min(defaults->count, 2));

			bool yield;

			Expr *value = createDefaultValue(job, location, array->arrayOf, &yield);

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
	case TypeFlavor::ENUM: {
		assert(!(type->flags & TYPE_ENUM_IS_FLAGS)); // The default value for a flags enum is 0
		auto first = static_cast<TypeEnum *>(type)->values->declarations[0];

		if ((first->flags & DECLARATION_VALUE_IS_READY)) {
			return first->initialValue;
		}
		else {
			goToSleep(job, &first->sleepingOnMyValue);

			*shouldYield = true;
			return nullptr;
		}
	}
	case TypeFlavor::TYPE: {
		return inferMakeTypeLiteral(location->start, location->end, &TYPE_VOID);
	}
	default: {
		assert(false);
		return nullptr;
	}
	}
}

bool assignOp(SubJob *job, Expr *location, Type *correct, Expr *&given, bool *yield) {
	*yield = false;
	if (correct != given->type) {
		if (correct->flavor == given->type->flavor) {
			switch (correct->flavor) {
			case TypeFlavor::ARRAY: {
				if (correct != given->type) {
					if (given->type->flags & (TYPE_ARRAY_IS_DYNAMIC | TYPE_ARRAY_IS_FIXED)) {
						if (!(correct->flags & (TYPE_ARRAY_IS_DYNAMIC | TYPE_ARRAY_IS_FIXED))) { // We are converting from [N]T or [..]T to []T
							if (static_cast<TypeArray *>(given->type)->arrayOf == static_cast<TypeArray *>(correct)->arrayOf) {
								insertImplicitCast(job->sizeDependencies, &given, correct);
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
					insertImplicitCast(job->sizeDependencies, &given, correct);
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
				auto a = static_cast<TypeFunction *>(correct);
				auto b = static_cast<TypeFunction *>(given->type);

				if (a->isVarargs != b->isVarargs && a->argumentCount == b->argumentCount && a->returnCount == b->returnCount) {
					// @Incomplete Should this check recursively if function pointers are varargs convertible
					for (u64 i = 0; i < a->argumentCount; i++) {
						if (a->argumentTypes[i] != b->argumentTypes[i]) return false;
						if (a->returnTypes[i] != b->returnTypes[i]) return false;
					}
					insertImplicitCast(job->sizeDependencies, &given, correct);
				}

				if (correct != given->type) {
					reportError(location, "Error: Cannot convert from %.*s to %.*s", STRING_PRINTF(given->type->name), STRING_PRINTF(correct->name));
					return false;
				}
				break;
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
						insertImplicitCast(job->sizeDependencies, &given, correct);
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
						insertImplicitCast(job->sizeDependencies, &given, correct);
					}
					else if (given->type == TYPE_VOID_POINTER) {
						insertImplicitCast(job->sizeDependencies, &given, correct);
					}
					else if (givenPointer->flavor == TypeFlavor::ARRAY && correctPointer->flavor == TypeFlavor::ARRAY &&
						(givenPointer->flags & TYPE_ARRAY_IS_DYNAMIC) && !(correctPointer->flags & (TYPE_ARRAY_IS_FIXED | TYPE_ARRAY_IS_DYNAMIC))) {
						if (static_cast<TypeArray *>(givenPointer)->arrayOf == static_cast<TypeArray *>(correctPointer)->arrayOf) {
							insertImplicitCast(job->sizeDependencies, &given, correct);
						}
					}
					else {
						if (!tryUsingConversion(job, correct, &given, yield)) {
							if (*yield) {
								return false;
							}
						}
						else {
							addSizeDependency(job->sizeDependencies, given->type);
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
				break;
			}
			case TypeFlavor::STRUCT: {
				if (!tryUsingConversion(job, correct, &given, yield)) {
					if (*yield) {
						return false;
					}

					if (correct == TYPE_ANY && given->type != TYPE_ANY) {
						insertImplicitCast(job->sizeDependencies, &given, TYPE_ANY);
						return true;
					}
				}
				else {

					addSizeDependency(job->sizeDependencies, given->type);
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
			if (given->type == &TYPE_AUTO_CAST) {
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

					insertImplicitCast(job->sizeDependencies, &given, correct);
				}
				else if (correct->flavor == TypeFlavor::ENUM && (correct->flags & TYPE_ENUM_IS_FLAGS) &&
					given->type->flavor == TypeFlavor::INTEGER && given->flavor == ExprFlavor::INT_LITERAL && static_cast<ExprLiteral *>(given)->unsignedValue == 0) {
					insertImplicitCast(job->sizeDependencies, &given, correct);
				}
				else if (correct->flavor == TypeFlavor::FLOAT && given->type == &TYPE_SIGNED_INT_LITERAL) {
					auto literal = static_cast<ExprLiteral *>(given);

					literal->flavor = ExprFlavor::FLOAT_LITERAL;
					literal->floatValue = static_cast<double>(literal->signedValue);
					literal->type = correct;
				}
				else if ((given->type == TYPE_VOID_POINTER && correct->flavor == TypeFlavor::FUNCTION) || (given->type->flavor == TypeFlavor::FUNCTION && correct == TYPE_VOID_POINTER)) {
					insertImplicitCast(job->sizeDependencies, &given, correct);
				}
				else if (correct->flavor == TypeFlavor::ENUM && given->type == &TYPE_UNARY_DOT) {
					if (!inferUnaryDot(job, static_cast<TypeEnum *>(correct), &given, yield)) {
						return false;
					}

					if (*yield) {
						return false;
					}
				}

				if (!tryUsingConversion(job, correct, &given, yield)) {
					if (*yield) {
						return false;
					}

					if (correct == TYPE_ANY && given->type != TYPE_ANY) {
						trySolidifyNumericLiteralToDefault(given);
						insertImplicitCast(job->sizeDependencies, &given, TYPE_ANY);
						return true;
					}
				}
				else {

					addSizeDependency(job->sizeDependencies, given->type);
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
	if (expr->flavor == ExprFlavor::BINARY_OPERATOR && static_cast<ExprBinaryOperator *>(expr)->op == TOKEN('[')) {
		auto binary = static_cast<ExprBinaryOperator *>(expr);

		return !(binary->left->type->flags & TYPE_ARRAY_IS_FIXED) || isAddressable(binary->left);
	}
	else if (expr->flavor == ExprFlavor::UNARY_OPERATOR && static_cast<ExprUnaryOperator *>(expr)->op == TokenT::SHIFT_LEFT) {
		return true;
	}
	else if (expr->flavor == ExprFlavor::IDENTIFIER) {
		auto identifier = static_cast<ExprIdentifier *>(expr);

		auto access = identifier->structAccess;

		if (!access) return !(identifier->declaration->flags & (DECLARATION_IS_ARGUMENT | DECLARATION_IS_ITERATOR | DECLARATION_IS_ITERATOR_INDEX));

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

bool inferArguments(SubJob *job, Arguments *arguments, Block *block, const char *message, Expr *callLocation, Expr *functionLocation, bool *yield) {
	*yield = false;

	String functionName = "function";

	if (functionLocation->valueOfDeclaration) {
		functionName = functionLocation->valueOfDeclaration->name;
	}


	if (!arguments->names && arguments->count != block->declarations.count && !(functionLocation->flags & EXPR_FUNCTION_HAS_VARARGS)) {
		if (arguments->count > block->declarations.count) {
			reportError(callLocation, "Error: Too many %ss for %.*s (Expected: %" PRIu64 ", Given: %" PRIu64 ")",
				message, STRING_PRINTF(functionName), block->declarations.count, arguments->count);
			return false;
		}
		else {
			for (u64 i = arguments->count; i < block->declarations.count; i++) {
				if (!block->declarations[i]->initialValue) {
					reportError(callLocation, "Error: Too few %ss for %.*s (Expected: %" PRIu64 ", Given: %" PRIu64 ")",
						message, STRING_PRINTF(functionName), block->declarations.count, arguments->count);
					return false;
				}
			}
		}
	}

#if BUILD_DEBUG
	for (u64 i = 0; i < block->declarations.count; i++) {
		assert(block->declarations[i]->indexInBlock == i);
	}
#endif

	if (arguments->names || arguments->count != block->declarations.count || (functionLocation->flags & EXPR_FUNCTION_HAS_VARARGS)) {
		Expr **sortedArguments = INFER_NEW_ARRAY(Expr *, block->declarations.count){};

		for (u64 i = 0; i < arguments->count; i++) {
			auto argument = block->declarations[i];

			u64 argIndex = i;
			if (arguments->names && arguments->names[i].length) {
				argument = findDeclarationNoYield(block, arguments->names[i]);
			}

			if (!argument) {
				reportError(arguments->values[i], "Error: %.*s does not have a %s called %.*s", STRING_PRINTF(functionName), message, STRING_PRINTF(arguments->names[i]));
				return false;
			}

			argIndex = argument->indexInBlock;
			assert(!(argument->flags & DECLARATION_IMPORTED_BY_USING));

			if (sortedArguments[argIndex]) {
				reportError(arguments->values[i], "Error: %s %.*s was supplied twice", message, STRING_PRINTF(block->declarations[argIndex]->name));
				reportError(sortedArguments[argIndex], "   ..: It was previously given here");
				return false;
			}

			if (argument->flags & DECLARATION_IS_VARARGS) {
				Array<Expr *> varargs;

				auto array = INFER_NEW(ExprArray);
				array->flavor = ExprFlavor::ARRAY;
				array->start = arguments->values[i]->start;

				Type *varargsType = static_cast<TypeArray *>(static_cast<ExprLiteral *>(argument->type)->typeValue)->arrayOf;

				for (; i < arguments->count; i++) {
					if (arguments->names && arguments->names[i].length) {
						break;
					}

					if (!assignOp(job, arguments->values[i], varargsType, arguments->values[i], yield)) {
						return false;
					}

					varargs.add(arguments->values[i]);
					array->end = arguments->values[i]->end;
				}

				array->storage = varargs.storage;
				array->count = varargs.count;

				array->type = getStaticArray(varargsType, varargs.count);
				if (array->type->size == 0 && !array->type->sizeJob) {
					auto sizeJob = allocateSizeJob();
					sizeJob->type = array->type;
					sizeJob->type->sizeJob = sizeJob;

					addJob(&sizeJobs, sizeJob);
					addSubJob(sizeJob);
					++totalTypesSized;
				}


				addSizeDependency(job->sizeDependencies, array->type);

				sortedArguments[argIndex] = array;
			}
			else {
				sortedArguments[argIndex] = arguments->values[i];
			}
		}

		bool failed = false;

		for (u64 i = 0; i < block->declarations.count; i++) {
			if (!sortedArguments[i]) {
				auto argument = block->declarations[i];

				if (argument->initialValue) {
					if (argument->flags & DECLARATION_VALUE_IS_READY) {
						if (!isLiteral(argument->initialValue)) {
							reportError(argument, "Error: Default %ss must be a constant value", message);
							return false;
						}

						sortedArguments[i] = argument->initialValue;
						addSizeDependency(job->sizeDependencies, argument->initialValue->type);
					}
					else {
						*yield = true;
						goToSleep(job, &argument->sleepingOnMyValue);

						return false;
					}
				}
				else if (argument->flags & DECLARATION_IS_VARARGS) {
					auto literal = createIntLiteral(argument->start, argument->end, static_cast<ExprLiteral *>(argument->type)->typeValue, 0);
					sortedArguments[i] = literal;
					addSizeDependency(job->sizeDependencies, literal->type);
				}
				else {
					reportError(callLocation, "Error: Required %s '%.*s' was not given", message, STRING_PRINTF(argument->name));
					reportError(argument, "   ..: Here is the %s", message);
					failed = true;
				}
			}
		}

		if (failed)
			return false;

		arguments->values = sortedArguments;
		arguments->names = nullptr;
		arguments->count = block->declarations.count;
	}

	for (u64 i = 0; i < arguments->count; i++) {
		Type *correct = static_cast<ExprLiteral *>(block->declarations[i]->type)->typeValue;

		if (!assignOp(job, arguments->values[i], correct, arguments->values[i], yield)) { // @Incomplete: Give function call specific error messages instead of general type conversion errors
			return false;
		}
	}

	return true;
}

bool functionIsVoid(TypeFunction *function) {
	return function->returnCount == 1 && function->returnTypes[0] == &TYPE_VOID;
}


bool inferBinary(SubJob *job, Expr **exprPointer, bool *yield) {
	*yield = false;

	auto expr = *exprPointer;
	auto binary = static_cast<ExprBinaryOperator *>(expr);

	auto &left = binary->left;
	auto &right = binary->right;

	if (binary->flags & EXPR_ASSIGN_IS_IMPLICIT_INITIALIZER) {
		auto declaration = static_cast<ExprIdentifier *>(left)->declaration;

		if (!(declaration->flags & DECLARATION_VALUE_IS_READY)) {
			goToSleep(job, &declaration->sleepingOnMyValue);

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

	if (left && right) {
		if (left->type == right->type) {
			if (left->type == &TYPE_AUTO_CAST) {
				reportError(binary, "Error: Cannot infer the type of an expression when both sides are an auto cast");
				return false;
			}
			else if (left->type == &TYPE_UNARY_DOT) {
				reportError(binary, "Error: Cannot infer the type of an expression when both sides are a unary dot");
				return false;
			}
		}
		else if (left->type->flavor == right->type->flavor && left->type->flavor == TypeFlavor::AUTO_CAST) {
			reportError(binary, "Error: Cannot infer the type of an expression when the sides are an auto cast and a unary dot");
			return false;
		}
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
		if (!assignOp(job, left, &TYPE_TYPE, left, yield)) {
			return *yield;
		}

		if (left->flavor != ExprFlavor::TYPE_LITERAL) {
			reportError(left, "Error: Cast target must be a constant");
			return false;
		}


		assert(left->flavor == ExprFlavor::TYPE_LITERAL);

		Type *castTo = static_cast<ExprLiteral *>(left)->typeValue;
		trySolidifyNumericLiteralToDefault(right);

		if (!tryUsingConversion(job, castTo, &right, yield)) {
			if (*yield) {
				return true;
			}

			if (!isValidCast(castTo, right->type, binary->flags)) {
				reportError(binary, "Error: Cannot cast from %.*s to %.*s", STRING_PRINTF(right->type->name), STRING_PRINTF(castTo->name));
				return false;
			}
		}

		expr->type = castTo;

		break;
	}
	case TOKEN('['): {
		trySolidifyNumericLiteralToDefault(right);

		if (right->type->flavor != TypeFlavor::INTEGER) {
			reportError(right, "Error: Array index must be an integer");
			return false;
		}


		if (right->type->size != 8) {
			insertImplicitCast(job->sizeDependencies, &right, right->type->flags & TYPE_INTEGER_IS_SIGNED ? &TYPE_S64 : &TYPE_U64);
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

			addSizeDependency(job->sizeDependencies, left->type);

			expr->type = static_cast<TypeArray *>(left->type)->arrayOf;
		}
		else {
			reportError(binary->left, "Error: Cannot index a %.*s", STRING_PRINTF(left->type->name));
			return false;
		}

		addSizeDependency(job->sizeDependencies, expr->type);

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
						insertImplicitCast(job->sizeDependencies, &right, left->type);
					}
					else if (right->type == TYPE_VOID_POINTER) {
						insertImplicitCast(job->sizeDependencies, &left, right->type);
					}
					else {
						reportError(binary, "Error: Cannot compare %.*s to %.*s", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
						return false;
					}
				}
			}
			case TypeFlavor::INTEGER: {
				if (!binaryOpForInteger(job->sizeDependencies, exprPointer))
					return false;

				break;
			}
			case TypeFlavor::POINTER: {
				if (left->type != right->type) {
					if (left->type == TYPE_VOID_POINTER) {
						insertImplicitCast(job->sizeDependencies, &right, left->type);
					}
					else if (right->type == TYPE_VOID_POINTER) {
						insertImplicitCast(job->sizeDependencies, &left, right->type);
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
				break;
			}
			default:
				assert(false);
			}
		}
		else {
			if (!binaryOpForAutoCast(job, binary, yield)) {
				return *yield;
			}
			else if (!binaryOpForFloatAndIntLiteral(exprPointer)) {
				return false;
			}
			else if (left->type->flavor == TypeFlavor::ENUM && (left->type->flags & TYPE_ENUM_IS_FLAGS) &&
				right->type->flavor == TypeFlavor::INTEGER && right->flavor == ExprFlavor::INT_LITERAL && static_cast<ExprLiteral *>(right)->unsignedValue == 0) {
				insertImplicitCast(job->sizeDependencies, &right, left->type);
			}
			else if (right->type->flavor == TypeFlavor::ENUM && (right->type->flags & TYPE_ENUM_IS_FLAGS) &&
				left->type->flavor == TypeFlavor::INTEGER && left->flavor == ExprFlavor::INT_LITERAL && static_cast<ExprLiteral *>(left)->unsignedValue == 0) {
				insertImplicitCast(job->sizeDependencies, &left, right->type);
			}
			else if (left->type->flavor == TypeFlavor::ENUM && right->type == &TYPE_UNARY_DOT) {
				if (!inferUnaryDot(job, static_cast<TypeEnum *>(left->type), &right, yield)) {
					return false;
				}

				if (*yield) {
					return true;
				}
			}
			else if (right->type->flavor == TypeFlavor::ENUM && left->type == &TYPE_UNARY_DOT) {
				if (!inferUnaryDot(job, static_cast<TypeEnum *>(left->type), &left, yield)) {
					return false;
				}

				if (*yield) {
					return true;
				}
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
				if (!binaryOpForInteger(job->sizeDependencies, exprPointer))
					return false;

				break;
			}
			case TypeFlavor::POINTER: {
				if (left->type != right->type) {
					if (left->type == TYPE_VOID_POINTER) {
						insertImplicitCast(job->sizeDependencies, &right, left->type);
					}
					else if (right->type == TYPE_VOID_POINTER) {
						insertImplicitCast(job->sizeDependencies, &left, right->type);
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
				return *yield;
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
				if (!binaryOpForInteger(job->sizeDependencies, exprPointer))
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

				addSizeDependency(job->sizeDependencies, pointer->pointerTo);

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
				return *yield;
			}
			else if (left->type->flavor == TypeFlavor::POINTER && right->type->flavor == TypeFlavor::INTEGER) {
				trySolidifyNumericLiteralToDefault(right);
				auto pointer = static_cast<TypePointer *>(left->type);

				if (right->type->size != 8) {
					insertImplicitCast(job->sizeDependencies, &right, right->type->flags & TYPE_INTEGER_IS_SIGNED ? &TYPE_S64 : &TYPE_U64);
				}

				addSizeDependency(job->sizeDependencies, pointer->pointerTo);
			}
			else if (!binaryOpForFloatAndIntLiteral(exprPointer)) {
				return false;
			}
			else if (right->type != left->type) {
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
				if (!binaryOpForInteger(job->sizeDependencies, exprPointer))
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
				return *yield;
			}
			else if (left->type->flavor == TypeFlavor::ENUM && (left->type->flags & TYPE_ENUM_IS_FLAGS) &&
				right->type->flavor == TypeFlavor::INTEGER && right->flavor == ExprFlavor::INT_LITERAL && static_cast<ExprLiteral *>(right)->unsignedValue == 0) {
				insertImplicitCast(job->sizeDependencies, &right, left->type);
			}
			else if (right->type->flavor == TypeFlavor::ENUM && (right->type->flags & TYPE_ENUM_IS_FLAGS) &&
				left->type->flavor == TypeFlavor::INTEGER && left->flavor == ExprFlavor::INT_LITERAL && static_cast<ExprLiteral *>(left)->unsignedValue == 0) {
				insertImplicitCast(job->sizeDependencies, &left, right->type);
			}
			else if ((left->type->flavor == TypeFlavor::ENUM && (left->type->flags & TYPE_ENUM_IS_FLAGS)) && right->type == &TYPE_UNARY_DOT) {
				if (!inferUnaryDot(job, static_cast<TypeEnum *>(left->type), &right, yield)) {
					return false;
				}

				if (*yield) {
					return true;
				}
			}
			else if ((right->type->flavor == TypeFlavor::ENUM && (right->type->flags & TYPE_ENUM_IS_FLAGS)) && left->type == &TYPE_UNARY_DOT) {
				if (!inferUnaryDot(job, static_cast<TypeEnum *>(right->type), &left, yield)) {
					return false;
				}

				if (*yield) {
					return true;
				}
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
				if (!binaryOpForInteger(job->sizeDependencies, exprPointer))
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
				return *yield;
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
				if (!binaryOpForInteger(job->sizeDependencies, exprPointer))
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
				return *yield;
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
				if (!assignOpForInteger(job->sizeDependencies, binary))
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

				if (right->type->size != 8) {
					insertImplicitCast(job->sizeDependencies, &right, right->type->flags & TYPE_INTEGER_IS_SIGNED ? &TYPE_S64 : &TYPE_U64);
				}

				addSizeDependency(job->sizeDependencies, pointer->pointerTo);
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
			if (isValidCast(&TYPE_BOOL, left->type, 0)) {
				insertImplicitCast(job->sizeDependencies, &left, &TYPE_BOOL);
			}
			else {
				reportError(binary->left, "Error: Cannot convert %.*s to bool", STRING_PRINTF(left->type->name));
				return false;
			}
		}

		if (right->type != &TYPE_BOOL) {
			if (isValidCast(&TYPE_BOOL, right->type, 0)) {
				insertImplicitCast(job->sizeDependencies, &right, &TYPE_BOOL);
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
				if (!assignOpForInteger(job->sizeDependencies, binary))
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
				insertImplicitCast(job->sizeDependencies, &right, left->type);
			}
			else if ((left->type->flavor == TypeFlavor::ENUM && (left->type->flags & TYPE_ENUM_IS_FLAGS)) && right->type == &TYPE_UNARY_DOT) {
				if (!inferUnaryDot(job, static_cast<TypeEnum *>(left->type), &right, yield)) {
					return false;
				}

				if (*yield) {
					return true;
				}
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
				if (!assignOpForInteger(job->sizeDependencies, binary))
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

		const char *opName = binary->op == TokenT::TIMES_EQUALS ? "multiply" : ((binary->op == TokenT::DIVIDE_EQUALS) ? "divide" : "mod");

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
				if (!assignOpForInteger(job->sizeDependencies, binary))
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
				auto sizeJob = allocateSizeJob();
				sizeJob->type = array;
				sizeJob->type->sizeJob = sizeJob;

				addJob(&sizeJobs, sizeJob);
				addSubJob(sizeJob);
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

	if ((*exprPointer)->flavor == ExprFlavor::BINARY_OPERATOR) {
		if (!evaluateConstantBinary(job, exprPointer, yield)) {
			return *yield;
		}
	}

	return true;
}
bool inferFlattened(SubJob *job) {
	PROFILE_FUNC();
	++totalInfers;

	while (job->flattenedCount) {
		if (job->indices[job->flattenedCount - 1] == job->flatteneds[job->flattenedCount - 1].count) {
			--job->flattenedCount;
			continue;
		}

		auto exprPointer = getHalt(job);
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
					auto member = findDeclaration(&struct_->members, identifier->name, &yield);

					if (yield) {
						goToSleep(job, &struct_->members.sleepingOnMe, identifier->name);

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

					if (!(identifier->declaration->flags & DECLARATION_IS_CONSTANT)) {
						addSizeDependency(job->sizeDependencies, struct_);
					}
				}
				else {
					for (; identifier->resolveFrom; identifier->resolveFrom = identifier->resolveFrom->parentBlock) {
						bool yield;

						if (Declaration *declaration = findDeclaration(identifier->resolveFrom, identifier->name, &yield, identifier->indexInBlock)) {
							if ((declaration->flags & DECLARATION_IS_CONSTANT) && !(declaration->flags & DECLARATION_IMPORTED_BY_USING)) {
								identifier->declaration = declaration;
								break;
							}
							else {
								if ((identifier->flags & EXPR_IDENTIFIER_RESOLVING_ONLY_CONSTANTS) && !(identifier->flags & EXPR_VALUE_NOT_REQUIRED)) {
									reportError(identifier, "Error: Cannot refer to %s from outside, capture is not supported", (declaration->flags & DECLARATION_IS_ARGUMENT) ? "argument" : "variable");
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
							goToSleep(job, &identifier->resolveFrom->sleepingOnMe, identifier->name);

							return true;
						}

						identifier->indexInBlock = identifier->resolveFrom->indexInParent;

						if (identifier->resolveFrom->flags & (BLOCK_IS_RETURNS | BLOCK_IS_STRUCT))
							identifier->flags |= EXPR_IDENTIFIER_RESOLVING_ONLY_CONSTANTS;
					}

					if (!identifier->declaration) {
						if (!identifier->resolveFrom) { // If we have checked all the local scopes and the
							identifier->declaration = findDeclarationNoYield(&globalBlock, identifier->name);

							if (!identifier->declaration) {
								goToSleep(job, &globalBlock.sleepingOnMe, identifier->name);

								return true;
							}
						}
					}

					if (identifier->declaration) {
						if (identifier->enclosingScope && identifier->declaration->enclosingScope != identifier->enclosingScope) {
							assert(identifier->enclosingScope != &globalBlock);

							if (!addImplicitImport(identifier->enclosingScope, identifier->declaration, &identifier->start, &identifier->end)) {
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
			if (!expr->type) {
				auto function = static_cast<ExprFunction *>(expr);

				goToSleep(job, &function->sleepingOnMe);

				return true;
			}

			break;
		}
		case ExprFlavor::FUNCTION_PROTOTYPE: {
			auto function = static_cast<ExprFunction *>(expr);

			for (auto argument : function->arguments.declarations) {
				assert(argument);

				if (!(argument->flags & DECLARATION_TYPE_IS_READY)) {
					goToSleep(job, &argument->sleepingOnMyType);

					return true;
				}
			}


			for (auto return_ : function->returns.declarations) {
				assert(return_);

				if (!(return_->flags & DECLARATION_TYPE_IS_READY)) {
					goToSleep(job, &return_->sleepingOnMyType);

					return true;
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
				if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_IMPLICIT_IMPORT))) {
					if (!(declaration->flags & DECLARATION_TYPE_IS_READY)) {
						goToSleep(job, &declaration->sleepingOnMyType);

						return true;
					}
				}
			}

			// Do two passes over the array because if the first pass added dependencies on some declarations then yielded, it would add them again next time round
			for (auto declaration : block->declarations.declarations) {
				if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_IMPLICIT_IMPORT))) {
					addSizeDependency(job->sizeDependencies, static_cast<ExprLiteral *>(declaration->type)->typeValue);
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
		case ExprFlavor::SLICE: {
			auto slice = static_cast<ExprSlice *>(*exprPointer);

			if (slice->sliceStart) {
				trySolidifyNumericLiteralToDefault(slice->sliceStart);

				if (slice->sliceStart->type->flavor != TypeFlavor::INTEGER) {
					reportError(slice->sliceStart, "Error: Array index must be an integer");
					return false;
				}

				if (slice->sliceStart->type->size != 8) {
					insertImplicitCast(job->sizeDependencies, &slice->sliceStart, slice->sliceStart->type->flags & TYPE_INTEGER_IS_SIGNED ? &TYPE_S64 : &TYPE_U64);
				}
			}


			if (slice->sliceEnd) {
				trySolidifyNumericLiteralToDefault(slice->sliceEnd);

				if (slice->sliceStart->type->flavor != TypeFlavor::INTEGER) {
					reportError(slice->sliceEnd, "Error: Array index must be an integer");
					return false;
				}

				if (slice->sliceStart->type->size != 8) {
					insertImplicitCast(job->sizeDependencies, &slice->sliceEnd, slice->sliceEnd->type->flags & TYPE_INTEGER_IS_SIGNED ? &TYPE_S64 : &TYPE_U64);
				}
			}



			if (slice->array->type->flavor == TypeFlavor::POINTER) {
				TypePointer *pointer = static_cast<TypePointer *>(slice->array->type);

				if (pointer->pointerTo == &TYPE_VOID) {
					reportError(slice, "Error: Cannot slice a void pointer");
					return false;
				}

				if (!slice->sliceEnd) {
					reportError(slice, "Error: Pointer slices must have an end");
					return false;
				}

				assert(!(pointer->pointerTo->flags & TYPE_IS_INTERNAL));

				expr->type = getArray(pointer->pointerTo);
				addSizeDependency(job->sizeDependencies, pointer->pointerTo);
			}
			else if (slice->array->type->flavor == TypeFlavor::STRING) {
				// @StringFormat
				if (slice->sliceEnd) {
					reportError(slice, "Error: String slices cannot have an end");
					return false;
				}

				expr->type = &TYPE_STRING;
			}
			else if (slice->array->type->flavor == TypeFlavor::ARRAY) {
				auto *array = static_cast<TypeArray *>(slice->array->type);

				if ((array->flags & TYPE_ARRAY_IS_FIXED) && !isAddressable(slice->array)) {
					reportError(slice, "Error: Cannot slice an array that has no storage");
					return false;
				}

				if (array->flags & (TYPE_ARRAY_IS_DYNAMIC | TYPE_ARRAY_IS_FIXED)) {
					expr->type = getArray(array->arrayOf);
				}
				else {
					expr->type = array;
				}

				addSizeDependency(job->sizeDependencies, array->arrayOf);
			}
			else {
				reportError(slice->array, "Error: Cannot slice a %.*s", STRING_PRINTF(slice->array->type->name));
				return false;
			}

			addSizeDependency(job->sizeDependencies, expr->type);

			break;

			break;
		}
		case ExprFlavor::SWITCH: {
			auto switch_ = static_cast<ExprSwitch *>(expr);

			if (switch_->flags & EXPR_SWITCH_IS_COMPLETE) {
				if (switch_->condition->type->flavor != TypeFlavor::ENUM || (switch_->condition->type->flags & TYPE_ENUM_IS_FLAGS)) {
					reportError(switch_, "Error: A switch if can only be #complete if it switches on an enum");
					return false;
				}
			}

			for (auto &case_ : switch_->cases) {
				if (case_.condition) {
					assert(case_.condition->flavor == ExprFlavor::BINARY_OPERATOR);

					auto binary = static_cast<ExprBinaryOperator *>(case_.condition);

					if (!isLiteral(binary->right)) {
						reportError(case_.condition, "Error: Switch if case must be a constant value");
						return false;
					}

					for (auto it = &case_ + 1; it != switch_->cases.end(); it++) {
						if (it->condition) {
							assert(it->condition->flavor == ExprFlavor::BINARY_OPERATOR);

							auto other = static_cast<ExprBinaryOperator *>(it->condition);

							if (switchCasesAreSame(binary->right, other->right)) {
								reportError(it->condition, "Error: Duplicate case in switch if");
								reportError(case_.condition, "Error: Here is the previous case");
							}
						}
					}
				}
			}

			if (switch_->flags & EXPR_SWITCH_IS_COMPLETE) {
				assert(switch_->condition->type->flavor == TypeFlavor::ENUM);

				auto enum_ = static_cast<TypeEnum *>(switch_->condition->type);


				for (auto member : enum_->values->declarations) {
					if (!(member->flags & DECLARATION_VALUE_IS_READY)) {
						goToSleep(job, &member->sleepingOnMyValue);
						return true;
					}
				}

				bool failed = false;

				for (auto member : enum_->values->declarations) {
					assert(member->initialValue->flavor == ExprFlavor::INT_LITERAL);

					u64 value = static_cast<ExprLiteral *>(member->initialValue)->unsignedValue;


					bool found = false;
					for (auto case_ : switch_->cases) {
						if (case_.condition) {
							auto literal = static_cast<ExprBinaryOperator *>(case_.condition)->right;

							assert(literal->flavor == ExprFlavor::INT_LITERAL);

							u64 compare = static_cast<ExprLiteral *>(literal)->unsignedValue;

							if (value == compare) {
								found = true;
								break;
							}
						}
					}

					if (!found) {
						failed = true;
						break;
					}
				}

				if (failed) {
					reportError(switch_, "Error: Switch if that was marked as #complete does not handle all values");

					for (auto member : enum_->values->declarations) {
						assert(member->initialValue->flavor == ExprFlavor::INT_LITERAL);

						u64 value = static_cast<ExprLiteral *>(member->initialValue)->unsignedValue;


						bool found = false;
						for (auto case_ : switch_->cases) {
							if (case_.condition) {
								auto literal = static_cast<ExprBinaryOperator *>(case_.condition)->right;

								assert(literal->flavor == ExprFlavor::INT_LITERAL);

								u64 compare = static_cast<ExprLiteral *>(literal)->unsignedValue;

								if (value == compare) {
									found = true;
									break;
								}
							}
						}

						if (!found) {
							reportError(member, "   ..: %.*s is not handled", STRING_PRINTF(member->name));
						}
					}

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
							insertImplicitCast(job->sizeDependencies, &loop->forEnd, loop->forBegin->type);
						}
						else if (loop->forBegin->type->size < loop->forEnd->type->size) {
							insertImplicitCast(job->sizeDependencies, &loop->forBegin, loop->forEnd->type);
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
				}
				else if (loop->forBegin->type->flavor == TypeFlavor::POINTER) {
					if (loop->forBegin->type != loop->forEnd->type) {
						reportError(loop, "Error: Cannot iterate from a %.*s to a %.*s", STRING_PRINTF(loop->forBegin->type->name), STRING_PRINTF(loop->forEnd->type->name));
						return false;
					}

					auto pointer = static_cast<TypePointer *>(loop->forBegin->type);

					if (loop->flags & EXPR_FOR_BY_POINTER) {
						it->type = inferMakeTypeLiteral(it->start, it->end, pointer);
					}
					else {
						if (loop->forBegin->type == TYPE_VOID_POINTER) {
							reportError(loop, "Error: Cannot iterate over a *void by value");
							return false;
						}

						it->type = inferMakeTypeLiteral(it->start, it->end, pointer->pointerTo);
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
					loop->forBegin = createIntLiteral(loop->start, loop->end, loop->forEnd->type, 0);

					it->type = inferMakeTypeLiteral(it->start, it->end, loop->forEnd->type);
				}
				else if (loop->forBegin->type->flavor == TypeFlavor::STRING) {
					if (loop->flags & EXPR_FOR_BY_POINTER) {
						it->type = inferMakeTypeLiteral(it->start, it->end, getPointer(&TYPE_U8));
					}
					else {
						it->type = inferMakeTypeLiteral(it->start, it->end, &TYPE_U8);
					}
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

			it->flags |= DECLARATION_TYPE_IS_READY;
			it_index->flags |= DECLARATION_TYPE_IS_READY;
			wakeUpSleepers(&it->sleepingOnMyType);
			wakeUpSleepers(&it_index->sleepingOnMyType);
			it->sleepingOnMyType.free();
			it_index->sleepingOnMyType.free();

			addSizeDependency(job->sizeDependencies, getDeclarationType(it));

			break;
		}
		case ExprFlavor::COMMA_ASSIGNMENT: {
			auto comma = static_cast<ExprCommaAssignment *>(expr);


			ExprFunction *functionForArgumentNames = nullptr;

			auto call = static_cast<ExprFunctionCall *>(comma->call);

			if (call->function->flavor == ExprFlavor::FUNCTION) {
				functionForArgumentNames = static_cast<ExprFunction *>(call->function);

				for (u64 i = comma->exprCount; i < functionForArgumentNames->returns.declarations.count; i++) {
					auto declaration = functionForArgumentNames->returns.declarations[i];

					if (declaration->flags & DECLARATION_IS_MUST) {
						reportError(comma->call, "Error: Cannot ignore the return value of the function call, it was marked as #must");
						reportError(declaration, "   ..: Here is the function");
						return false;
					}
				}
			}

			auto function = static_cast<TypeFunction *>(static_cast<ExprFunctionCall *>(comma->call)->function->type);

			if (comma->exprCount > function->returnCount || functionIsVoid(function)) {
				if (functionIsVoid(function)) {
					reportError(comma->call, "Error: Cannot assign from a function that returns void");
				}
				else {
					reportError(comma->call, "Error: Assigning to too many variables from function call, assigning to %" PRIu64 " variables, the function returns %" PRIu64, comma->exprCount, function->returnCount);
				}

				if (functionForArgumentNames) {
					reportError(functionForArgumentNames, "   ..: Here is the function");
				}
			}

			if (comma->flags & EXPR_COMMA_ASSIGNMENT_IS_DECLARATION) {
				for (u64 i = 0; i < comma->exprCount; i++) {
					auto identifier = static_cast<ExprIdentifier *>(comma->left[i]);

					auto declaration = identifier->declaration;

					identifier->type = function->returnTypes[i];
					declaration->type = inferMakeTypeLiteral(declaration->start, declaration->end, identifier->type);

					declaration->flags |= DECLARATION_TYPE_IS_READY;
					wakeUpSleepers(&declaration->sleepingOnMyType);
					declaration->sleepingOnMyType.free();
				}
			}
			else {
				for (u64 i = 0; i < comma->exprCount; i++) {
					if (!isAssignable(comma->left[i])) {
						reportError(comma->left[i], "Error: This expression cannot be assigned to");
						return false;
					}

					if (comma->left[i]->type != function->returnTypes[i]) {
						reportError(comma->left[i], "Error: Cannot do a multiple assignment from %.*s to %.*s", STRING_PRINTF(function->returnTypes[i]->name), STRING_PRINTF(comma->left[i]->type->name));

						if (functionForArgumentNames) {
							reportError(functionForArgumentNames->returns.declarations[i], "   ..: Here is the return");
						}

						return false;
					}
				}
			}

			expr->type = &TYPE_VOID;

			break;
		}
		case ExprFlavor::FUNCTION_CALL: {
			auto call = static_cast<ExprFunctionCall *>(expr);

			if (call->function->type->flavor != TypeFlavor::FUNCTION) {
				reportError(call->function, "Error: Cannot call a %.*s", STRING_PRINTF(call->function->type->name));
				return false;
			}

			auto function = static_cast<TypeFunction *>(call->function->type);

			expr->type = function->returnTypes[0];

			addSizeDependency(job->sizeDependencies, expr->type);

			String functionName = call->function->valueOfDeclaration ? call->function->valueOfDeclaration->name : "function";

			ExprFunction *functionForArgumentNames = nullptr;

			if (call->function->flavor == ExprFlavor::FUNCTION) {
				functionForArgumentNames = static_cast<ExprFunction *>(call->function);

				if (call->flags & EXPR_FUNCTION_CALL_IS_STATEMENT_LEVEL) {
					for (auto declaration : functionForArgumentNames->returns.declarations) {
						if (declaration->flags & DECLARATION_IS_MUST) {
							reportError(call, "Error: Cannot ignore the return value of the function call, it was marked as #must");
							reportError(declaration, "   ..: Here is the function");
							return false;
						}
					}
				}
				else if (!(call->flags & EXPR_FUNCTION_CALL_IS_IN_COMMA_ASSIGNMENT)) {
					for (u64 i = 1; i < functionForArgumentNames->returns.declarations.count; i++) {
						auto declaration = functionForArgumentNames->returns.declarations[i];

						if (declaration->flags & DECLARATION_IS_MUST) {
							reportError(call, "Error: Cannot ignore the return value of the function call, it was marked as #must");
							reportError(declaration, "   ..: Here is the function");
							return false;
						}
					}
				}
			}

			bool hasNamedArguments = call->arguments.names != nullptr;


			if (hasNamedArguments && !functionForArgumentNames) {
				reportError(call, "Error: Cannot use named arguments with a non-constant function"); // @Improvement better message
				return false;
			}

			if (functionForArgumentNames) {
				bool yield;

				if (!inferArguments(job, &call->arguments, &functionForArgumentNames->arguments, "argument", call, functionForArgumentNames, &yield)) {
					return yield;
				}
			}
			else {
				if (call->arguments.count != function->argumentCount || function->isVarargs) {

					if (function->isVarargs) {
						if (call->arguments.count < function->argumentCount - 1) {
							reportError(call, "Error: Too few arguments for %.*s (Expected: %" PRIu64 ", Given: %" PRIu64 ")",
								STRING_PRINTF(functionName), function->argumentCount, call->arguments.count);
							return false;
						}
						else if (call->arguments.count == function->argumentCount - 1) {
							auto newArguments = INFER_NEW_ARRAY(Expr *, function->argumentCount);

							for (u64 i = 0; i < call->arguments.count; i++) {
								newArguments[i] = call->arguments.values[i];
							}

							
							auto literal = createIntLiteral(call->start, call->end, function->argumentTypes[function->argumentCount - 1], 0);
							
							newArguments[call->arguments.count] = literal;
							call->arguments.values = newArguments;
							call->arguments.count = function->argumentCount;
						}
						else {
							auto array = INFER_NEW(ExprArray);
							array->flavor = ExprFlavor::ARRAY;
							array->start = call->arguments.values[function->argumentCount - 1]->start;

							Type *varargsType = static_cast<TypeArray *>(function->argumentTypes[function->argumentCount - 1])->arrayOf;

							array->count = call->arguments.count - function->argumentCount + 1;
							array->storage = INFER_NEW_ARRAY(Expr *, array->count);

							for (u64 i = 0; i < array->count; i++) {
								bool yield = false;
								auto &argument = call->arguments.values[function->argumentCount + i - 1];

								if (!assignOp(job, argument, varargsType, argument, &yield)) {
									return yield;
								}

								array->storage[i];
								array->end = argument->end;
							}

							call->arguments.values[function->argumentCount - 1] = array;
							call->arguments.count = function->argumentCount;
						}

					}
					else {
						if (call->arguments.count < function->argumentCount) {
							reportError(call, "Error: Too few arguments for %.*s (Expected: %" PRIu64 ", Given: %" PRIu64 ")",
								STRING_PRINTF(functionName), function->argumentCount, call->arguments.count);
						}
						else if (call->arguments.count > function->argumentCount) {
							reportError(call, "Error: Too many arguments for %.*s (Expected: %" PRIu64 ", Given: %" PRIu64 ")",
								STRING_PRINTF(functionName), function->argumentCount, call->arguments.count);
						}

						return false;
					}

				}

				for (u64 i = 0; i < call->arguments.count; i++) {
					Type *correct = function->argumentTypes[i];

					bool yield;
					if (!assignOp(job, call->arguments.values[i], correct, call->arguments.values[i], &yield)) { // @Incomplete: Give function call specific error messages instead of general type conversion errors
						return yield;
					}
				}
			}

			break;
		}
		case ExprFlavor::IF: {
			auto ifElse = static_cast<ExprIf *>(expr);

			if (ifElse->condition->type != &TYPE_BOOL) {
				if (ifElse->condition->type == &TYPE_AUTO_CAST) {
					bool yield = false;
					if (!tryAutoCast(job, &ifElse->condition, &TYPE_BOOL, &yield)) {
						if (!yield) {
							reportError(ifElse->condition, "Error: Cannot convert %.*s to bool",
								STRING_PRINTF(static_cast<ExprBinaryOperator *>(ifElse->condition)->right->type->name));
							return false;
						}
						else {
							return true;
						}
					}
				}
				else if (isValidCast(&TYPE_BOOL, ifElse->condition->type, 0)) {
					insertImplicitCast(job->sizeDependencies, &ifElse->condition, &TYPE_BOOL);
				}
				else {
					reportError(ifElse->condition, "Error: Cannot convert %.*s to bool", STRING_PRINTF(ifElse->condition->type->name));
					return false;
				}
			}

			break;
		}
		case ExprFlavor::STATIC_IF: {
			auto staticIf = static_cast<ExprIf *>(expr);

			if (staticIf->condition->type != &TYPE_BOOL) {
				if (staticIf->condition->type == &TYPE_AUTO_CAST) {
					bool yield = false;
					if (!tryAutoCast(job, &staticIf->condition, &TYPE_BOOL, &yield)) {
						if (!yield) {
							reportError(staticIf->condition, "Error: Cannot convert %.*s to bool",
								STRING_PRINTF(static_cast<ExprBinaryOperator *>(staticIf->condition)->right->type->name));
							return false;
						}
						else {
							return true;
						}
					}
				}
				else if (isValidCast(&TYPE_BOOL, staticIf->condition->type, 0)) {
					insertImplicitCast(job->sizeDependencies, &staticIf->condition, &TYPE_BOOL);
				}
				else {
					reportError(staticIf->condition, "Error: Cannot convert %.*s to bool", STRING_PRINTF(staticIf->condition->type->name));
					return false;
				}
			}


			if (staticIf->condition->flavor != ExprFlavor::INT_LITERAL || staticIf->condition->type != &TYPE_BOOL) {
				reportError(staticIf->condition, "Error: #if condition must be a constant");
				return false;
			}

			if (job->flavor != JobFlavor::IMPORTER) {
				auto literal = static_cast<ExprLiteral *>(staticIf->condition);

				ExprBlock *block = nullptr;

				if (literal->unsignedValue) {
					if (staticIf->ifBody) {
						assert(staticIf->ifBody->flavor == ExprFlavor::BLOCK);

						block = static_cast<ExprBlock *>(staticIf->ifBody);
					}
				}
				else {
					if (staticIf->elseBody) {
						assert(staticIf->elseBody->flavor == ExprFlavor::BLOCK);

						block = static_cast<ExprBlock *>(staticIf->elseBody);
					}
				}

				if (block) {
					*exprPointer = block;
					pushFlatten(job);
					continue;
				}
			}
			break;
		}
		case ExprFlavor::RETURN: {
			auto return_ = static_cast<ExprReturn *>(expr);

			if (!return_->returnsFrom->type) {
				goToSleep(job, &return_->returnsFrom->sleepingOnMe);

				return true;
			}

			assert(return_->returnsFrom->type->flavor == TypeFlavor::FUNCTION);

			Block *returnTypes = &return_->returnsFrom->returns;

			if (returnTypes->declarations.count == 1 && static_cast<ExprLiteral *>(returnTypes->declarations[0]->type)->typeValue == &TYPE_VOID) {
				if (return_->returns.count == 1 && return_->returns.values[0]->type == &TYPE_VOID) {
					// Returning a function call of void return type is legal
				}
				else if (return_->returns.count) {
					reportError(return_->returns.values[0], "Error: A function with void return type cannot return a value");
					return false;
				}
			}
			else {
				bool yield;

				if (!inferArguments(job, &return_->returns, returnTypes, "return", return_, return_->returnsFrom, &yield)) {
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

							if (literal->signedValue >= 0) {
								literal->type = &TYPE_UNSIGNED_INT_LITERAL;
							}

							if (literal->signedValue == 0) {
								literal->flags ^= EXPR_INTEGER_LITERAL_IS_NEGATIVE_ZERO;
							}

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

							if (literal->unsignedValue != 0)
								literal->type = &TYPE_SIGNED_INT_LITERAL;

							literal->signedValue = -literal->signedValue;


							if (literal->signedValue == 0) {
								literal->flags ^= EXPR_INTEGER_LITERAL_IS_NEGATIVE_ZERO;
							}

							*exprPointer = value;
						}
						else { // @Incomplete: should we allow negation of unsigned numbers, its useful for some bit twiddling but feels weird
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
					else {
						unary->type = value->type;
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

				if (value->flavor == ExprFlavor::INT_LITERAL) {
					*exprPointer = createInBoundsIntLiteral(unary->start, value->end, value->type, ~static_cast<ExprLiteral *>(value)->unsignedValue);
				}

				break;
			}
			case TOKEN('!'): {
				if (value->type != &TYPE_BOOL) {
					if (value->type == &TYPE_AUTO_CAST) {
						bool yield = false;
						if (!tryAutoCast(job, &value, &TYPE_BOOL, &yield)) {
							if (!yield) {
								reportError(value, "Error: Cannot convert %.*s to bool",
									STRING_PRINTF(static_cast<ExprBinaryOperator *>(value)->right->type->name));
								return false;
							}
							else {
								return true;
							}
						}
					}
					else if (isValidCast(&TYPE_BOOL, value->type, 0)) {
						insertImplicitCast(job->sizeDependencies, &value, &TYPE_BOOL);
					}
					else {
						reportError(value, "Error: Cannot convert %.*s to bool", STRING_PRINTF(value->type->name));
						return false;
					}
				}

				unary->type = &TYPE_BOOL;

				if (value->flavor == ExprFlavor::INT_LITERAL) {
					*exprPointer = createInBoundsIntLiteral(unary->start, value->end, &TYPE_BOOL, static_cast<ExprLiteral *>(value)->unsignedValue == 0);
				}

				break;
			}
			case TokenT::SIZE_OF: {
				if (value->type != &TYPE_TYPE) {
					bool yield = false;
					if (!assignOp(job, value, &TYPE_TYPE, value, &yield)) {
						return yield;
					}
				}

				if (value->flavor != ExprFlavor::TYPE_LITERAL) {
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

				if (!type->typeValue->size) {
					goToSleep(job, &type->typeValue->sleepingOnMe);

					return true;
				}

				assert(type->typeValue->size);

				auto literal = createIntLiteral(unary->start, unary->end, &TYPE_UNSIGNED_INT_LITERAL, type->typeValue->size);
				
				
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

				auto literal = inferMakeTypeLiteral(unary->start, unary->end, value->type);

				*exprPointer = literal;

				break;
			}
			case TokenT::TYPE_INFO: {
				if (value->type != &TYPE_TYPE) {
					bool yield = false;
					if (!assignOp(job, value, &TYPE_TYPE, value, &yield)) {
						return yield;
					}
				}

				if (value->flavor == ExprFlavor::TYPE_LITERAL) {
					auto type = static_cast<ExprLiteral *>(value)->typeValue;

					switch (type->flavor) {
					case TypeFlavor::NAMESPACE: {
						reportError(value, "Error: Cannot get type_info for a namespace");
						return false;
					}
					case TypeFlavor::VOID:
					case TypeFlavor::FLOAT:
					case TypeFlavor::BOOL:
					case TypeFlavor::TYPE:
					case TypeFlavor::STRING: {
						if (!TYPE_TYPE_INFO) {
							reportError(unary, "Internal Compiler Error: Type_Info was used before it's declaration was found");
							return false;
						}

						unary->type = getPointer(TYPE_TYPE_INFO);

						break;
					}
					case TypeFlavor::INTEGER: {
						if (!TYPE_TYPE_INFO_INTEGER) {
							reportError(unary, "Internal Compiler Error: Type_Info_Integer was used before it's declaration was found");
							return false;
						}

						unary->type = getPointer(TYPE_TYPE_INFO_INTEGER);

						break;
					}
					case TypeFlavor::POINTER: {
						if (!TYPE_TYPE_INFO_POINTER) {
							reportError(unary, "Internal Compiler Error: Type_Info_Pointer was used before it's declaration was found");
							return false;
						}

						unary->type = getPointer(TYPE_TYPE_INFO_POINTER);

						break;
					}
					case TypeFlavor::FUNCTION: {
						if (!TYPE_TYPE_INFO_FUNCTION) {
							reportError(unary, "Internal Compiler Error: Type_Info_Function was used before it's declaration was found");
							return false;
						}

						unary->type = getPointer(TYPE_TYPE_INFO_FUNCTION);

						break;
					}
					case TypeFlavor::ARRAY: {
						if (!TYPE_TYPE_INFO_ARRAY) {
							reportError(unary, "Internal Compiler Error: Type_Info_Array was used before it's declaration was found");
							return false;
						}

						unary->type = getPointer(TYPE_TYPE_INFO_ARRAY);

						break;
					}
					case TypeFlavor::STRUCT: {
						if (!TYPE_TYPE_INFO_STRUCT) {
							reportError(unary, "Internal Compiler Error: Type_Info_Struct was used before it's declaration was found");
							return false;
						}

						unary->type = getPointer(TYPE_TYPE_INFO_STRUCT);

						break;
					}
					case TypeFlavor::ENUM: {
						if (!TYPE_TYPE_INFO) {
							reportError(unary, "Internal Compiler Error: Type_Info_Enum was used before it's declaration was found");
							return false;
						}

						unary->type = getPointer(TYPE_TYPE_INFO_ENUM);

						break;
					}
					default:
						assert(false);
					}
				}
				else {
					if (!TYPE_TYPE_INFO) {
						reportError(unary, "Internal Compiler Error: Type_Info was used before it's declaration was found");
						return false;
					}

					unary->type = getPointer(TYPE_TYPE_INFO);
				}

				break;
			}
			case TokenT::SHIFT_LEFT: {
				if (value->type->flavor != TypeFlavor::POINTER) {
					reportError(value, "Error: Can only read from a pointer, given a %.*s", STRING_PRINTF(value->type->name));
					return false;
				}

				if (value->type == TYPE_VOID_POINTER) {
					reportError(value, "Error: Cannot read from a *void");
					return false;
				}

				auto pointer = static_cast<TypePointer *>(value->type);

				unary->type = pointer->pointerTo;

				addSizeDependency(job->sizeDependencies, unary->type);

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
						checkedRemoveLastSizeDependency(job->sizeDependencies, value->type);

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

					checkedRemoveLastSizeDependency(job->sizeDependencies, value->type);

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
				if (loop->whileCondition->type == &TYPE_AUTO_CAST) {
					bool yield = false;
					if (!tryAutoCast(job, &loop->whileCondition, &TYPE_BOOL, &yield)) {
						if (!yield) {
							reportError(loop->whileCondition, "Error: Cannot convert %.*s to bool",
								STRING_PRINTF(static_cast<ExprBinaryOperator *>(loop->whileCondition)->right->type->name));
							return false;
						}
						else {
							return true;
						}
					}
				}
				else if (isValidCast(&TYPE_BOOL, loop->whileCondition->type, 0)) {
					insertImplicitCast(job->sizeDependencies, &loop->whileCondition, &TYPE_BOOL);
				}
				else {
					reportError(loop->whileCondition, "Error: Cannot convert %.*s to bool", STRING_PRINTF(loop->whileCondition->type->name));
					return false;
				}
			}

			break;
		}
		case ExprFlavor::ENUM_INCREMENT: {
			auto increment = static_cast<ExprEnumIncrement *>(expr);

			if (increment->previous->flags & DECLARATION_VALUE_IS_READY) {
				auto value = static_cast<ExprLiteral *>(increment->previous->initialValue);

				assert(value->flavor == ExprFlavor::INT_LITERAL);
				assert(value->type->flavor == TypeFlavor::ENUM);

				auto literal = createIntLiteral(increment->start, increment->end,
					value->type->flags & TYPE_INTEGER_IS_SIGNED ? &TYPE_SIGNED_INT_LITERAL : &TYPE_UNSIGNED_INT_LITERAL,
					value->type->flags & TYPE_ENUM_IS_FLAGS ? value->unsignedValue * 2 : value->unsignedValue + 1);

				*exprPointer = literal;
			}
			else {
				goToSleep(job, &increment->previous->sleepingOnMyValue);

				return true;
			}

			break;
		}
		default:
			assert(false);
		}

		++job->indices[job->flattenedCount - 1];
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

static SizeJob *firstFreeSizeJob;
static DeclarationJob *firstFreeDeclarationJob;
static FunctionJob *firstFreeFunctionJob;
static ImporterJob *firstFreeImporterJob;

ImporterJob *allocateImporterJob() {
	if (firstFreeImporterJob) {
		auto result = firstFreeImporterJob;
		firstFreeImporterJob = result->next;

		result->flattenedCount = 0;

		return result;
	}

	return new (inferJobAllocator.allocate(sizeof(ImporterJob))) ImporterJob;
}

SizeJob *allocateSizeJob() {
	if (firstFreeSizeJob) {
		auto result = firstFreeSizeJob;
		firstFreeSizeJob = firstFreeSizeJob->next;

		result->sizingIndexInMembers = 0;
		result->runningSize = 0;

		result->flattenedCount = 0;

		return result;
	}

	return new (inferJobAllocator.allocate(sizeof(SizeJob))) SizeJob;
}

DeclarationJob *allocateDeclarationJob() {
	if (firstFreeDeclarationJob) {
		auto result = firstFreeDeclarationJob;
		firstFreeDeclarationJob = firstFreeDeclarationJob->next;

		result->type.flattenedCount = 0;
		result->value.flattenedCount = 0;
		result->sizeDependencies.clear();

		return result;
	}

	return new (inferJobAllocator.allocate(sizeof(DeclarationJob))) DeclarationJob;
}

FunctionJob *allocateFunctionJob() {
	if (firstFreeFunctionJob) {
		auto result = firstFreeFunctionJob;
		firstFreeFunctionJob = firstFreeFunctionJob->next;

		result->type.flattenedCount = 0;
		result->value.flattenedCount = 0;
		result->sizeDependencies.clear();

		return result;
	}

	return new (inferJobAllocator.allocate(sizeof(FunctionJob))) FunctionJob;
}

void freeJob(SizeJob *job) {
	job->next = firstFreeSizeJob;
	firstFreeSizeJob = job;
}

void freeJob(DeclarationJob *job) {
	job->next = firstFreeDeclarationJob;
	firstFreeDeclarationJob = job;
}


void freeJob(FunctionJob *job) {
	job->next = firstFreeFunctionJob;
	firstFreeFunctionJob = job;
}

void freeJob(ImporterJob *job) {
	job->next = firstFreeImporterJob;
	firstFreeImporterJob = job;
}

bool checkStructDeclaration(Declaration *declaration, Type *&value, String name) {
	if (declaration->name == name) {
		if (!(declaration->flags & DECLARATION_IS_CONSTANT)) {
			reportError(declaration, "Internal Compiler Error: Declaration for %.*s must be a constant", STRING_PRINTF(name));
			return false;
		}
		else if (!declaration->initialValue || declaration->initialValue->flavor != ExprFlavor::TYPE_LITERAL) {
			reportError("Internal Compiler Error: %.s must be a type");
			return false;
		}

		value = static_cast<ExprLiteral *>(declaration->initialValue)->typeValue;
	}

	return true;
}

bool checkFunctionDeclaration(Declaration *declaration, ExprFunction *&value, String name) {
	if (declaration->name == name) {
		if (!(declaration->flags & DECLARATION_IS_CONSTANT)) {
			reportError(declaration, "Internal Compiler Error: Declaration for %.*s must be a constant", name);
			return false;
		}
		else if (!declaration->initialValue || declaration->initialValue->flavor != ExprFlavor::FUNCTION) {
			reportError(declaration, "Internal Compiler Error: %.*s must be a function", name);
			return false;
		}

		value = static_cast<ExprFunction *>(declaration->initialValue);
	}

	return true;
}

bool checkBuiltinDeclaration(Declaration *declaration) {

#define FUNCTION_DECLARATION(value, name)		\
else if (!checkFunctionDeclaration(declaration, value, name)) {	\
	return false;                                               \
}                                             
#define STRUCT_DECLARATION(value, name)		\
else if (!checkStructDeclaration(declaration, value, name)) {	\
	return false;                                               \
}                                             
	if (false);
	FUNCTION_DECLARATION(removeFunction, "__remove")
		FUNCTION_DECLARATION(stringsEqualFunction, "__strings_equal")
		STRUCT_DECLARATION(TYPE_ANY, "any")
		STRUCT_DECLARATION(TYPE_TYPE_INFO, "Type_Info")
		STRUCT_DECLARATION(TYPE_TYPE_INFO_INTEGER, "Type_Info_Integer")
		STRUCT_DECLARATION(TYPE_TYPE_INFO_POINTER, "Type_Info_Pointer")
		STRUCT_DECLARATION(TYPE_TYPE_INFO_FUNCTION, "Type_Info_Function")
		STRUCT_DECLARATION(TYPE_TYPE_INFO_ARRAY, "Type_Info_Array")
		STRUCT_DECLARATION(TYPE_TYPE_INFO_STRUCT, "Type_Info_Struct")
		STRUCT_DECLARATION(TYPE_TYPE_INFO_STRUCT_MEMBER, "Type_Info_Struct_Member")
		STRUCT_DECLARATION(TYPE_TYPE_INFO_ENUM, "Type_Info_Enum")
		STRUCT_DECLARATION(TYPE_TYPE_INFO_ENUM_VALUE, "Type_Info_Enum_Value")

		return true;
#undef STRUCT_DECLARATION
#undef FUNCTION_DECLARATION
}

void addImporter(Importer *importer) {
	PROFILE_FUNC();

	if (!importer->enclosingScope)
		addImporterToBlock(&globalBlock, importer);

	++totalImporters;

	auto job = allocateImporterJob();
	job->importer = importer;

	beginFlatten(job, &importer->import);

	addJob(&importerJobs, job);

	addSubJob(job);
}

bool addDeclaration(Declaration *declaration) {
	PROFILE_FUNC();

	declaration->inferJob = nullptr;

	if (declaration->flags & DECLARATION_IS_IN_COMPOUND)
		return true;
	assert(!(declaration->flags & (DECLARATION_IS_ITERATOR | DECLARATION_IS_ITERATOR_INDEX | DECLARATION_IS_IN_COMPOUND)));

	if (!declaration->enclosingScope) {
		PROFILE_ZONE("Add declaration to global block");

		if (!addDeclarationToBlock(&globalBlock, declaration)) {
			return false;
		}

		if (declaration->start.fileUid == 0 && !checkBuiltinDeclaration(declaration)) {
			return false;
		}

		wakeUpSleepers(&globalBlock.sleepingOnMe, false, declaration->name);
	}

	if ((declaration->flags & DECLARATION_TYPE_IS_READY) && (declaration->flags & DECLARATION_VALUE_IS_READY)) {
		return true;
	}


	// Fast path for declarations that are trivial to infer
	if (declaration->flags & DECLARATION_IS_CONSTANT) {
		if (!declaration->type && declaration->initialValue && declaration->initialValue->flavor == ExprFlavor::INT_LITERAL) {
			declaration->type = inferMakeTypeLiteral(declaration->start, declaration->end, &TYPE_UNSIGNED_INT_LITERAL);
			declaration->flags |= DECLARATION_TYPE_IS_READY | DECLARATION_VALUE_IS_READY;
			wakeUpSleepers(&declaration->sleepingOnMyType);
			wakeUpSleepers(&declaration->sleepingOnMyValue);
			declaration->sleepingOnMyType.free();
			declaration->sleepingOnMyValue.free();
			return true;
		}
		else if (!declaration->type && declaration->initialValue && declaration->initialValue->flavor == ExprFlavor::FUNCTION) {
			addFunction(static_cast<ExprFunction *>(declaration->initialValue));

			return true;
		}
	}
	else if (declaration->enclosingScope) {
		if (!declaration->type && declaration->initialValue && declaration->initialValue->flavor == ExprFlavor::INT_LITERAL) {
			trySolidifyNumericLiteralToDefault(declaration->initialValue);
			declaration->type = inferMakeTypeLiteral(declaration->start, declaration->end, declaration->initialValue->type);
			declaration->flags |= DECLARATION_TYPE_IS_READY | DECLARATION_VALUE_IS_READY;
			wakeUpSleepers(&declaration->sleepingOnMyType);
			wakeUpSleepers(&declaration->sleepingOnMyValue);
			declaration->sleepingOnMyType.free();
			declaration->sleepingOnMyValue.free();
			return true;
		}
		else if (declaration->type && declaration->type->flavor == ExprFlavor::TYPE_LITERAL && !declaration->initialValue && (declaration->flags & (DECLARATION_IS_ARGUMENT | DECLARATION_IS_UNINITIALIZED | DECLARATION_IS_RETURN))) {
			declaration->flags |= DECLARATION_TYPE_IS_READY;

			addTypeBlock(declaration->type);

			wakeUpSleepers(&declaration->sleepingOnMyType);
			declaration->sleepingOnMyType.free();
			return true;
		}
		else if (!declaration->type && declaration->initialValue && declaration->initialValue->flavor == ExprFlavor::FUNCTION) {
			addFunction(static_cast<ExprFunction *>(declaration->initialValue));

			return true;
		}
	}

	++totalDeclarations;

	DeclarationJob *job = allocateDeclarationJob();
	job->declaration = declaration;
	job->declaration->inferJob = job;

	if (declaration->type) {
		beginFlatten(&job->type, &declaration->type);
		addSubJob(&job->type, true);
	}

	if (declaration->initialValue) {
		beginFlatten(&job->value, &declaration->initialValue);
		addSubJob(&job->value);
	}

	addJob(&declarationJobs, job);

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

		for (auto return_ : function->returns.declarations) {
			if (!(return_->flags & DECLARATION_TYPE_IS_READY)) {
				return return_;
			}
			else if (return_->initialValue && !(return_->flags & DECLARATION_VALUE_IS_READY)) {
				return return_;
			}
		}
	}

	assert(false); // There should be no other cases where we ha
	return nullptr;
}

static s64 findLoop(Array<Declaration *> &loop, Declaration *declaration) {
	if (!declaration->inferJob) {
		return -1;
	}

	loop.add(declaration);

	auto job = declaration->inferJob;

	if (!isDone(&job->type)) {
		auto dependency = getDependency(*getHalt(&job->type));

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

	if (!isDone(&job->value)) {
		auto dependency = getDependency(*getHalt(&job->value));

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

	if (!isDone(&job->type) && getDependency(*getHalt(&job->type)) == next) {
		*haltedOn = *getHalt(&job->type);
		*typeDependence = true;
	}
	else if (!isDone(&job->value) && getDependency(*getHalt(&job->value)) == next) {
		*haltedOn = *getHalt(&job->value);
		*typeDependence = false;
	}
	else {
		assert(false);
	}
}

Array<FunctionJob *> functionWaitingOnSize;
Array<DeclarationJob *> declarationWaitingOnSize;

void addImporter(Importer *importer);

bool inferImporter(ImporterJob *job) {
	PROFILE_FUNC();

	auto importer = job->importer;

	Block *block = nullptr;
	bool onlyConstants = false;
	Expr *structAccess = nullptr;

	if (importer->import->flavor == ExprFlavor::STATIC_IF) {
		auto staticIf = static_cast<ExprIf *>(importer->import);

		assert(staticIf->condition);
		assert(staticIf->condition->flavor == ExprFlavor::INT_LITERAL);
		assert(staticIf->condition->type == &TYPE_BOOL);

		auto literal = static_cast<ExprLiteral *>(staticIf->condition);

		if (literal->unsignedValue) {
			if (staticIf->ifBody) {
				assert(staticIf->ifBody->flavor == ExprFlavor::BLOCK);

				block = &static_cast<ExprBlock *>(staticIf->ifBody)->declarations;
			}
		}
		else {
			if (staticIf->elseBody) {
				assert(staticIf->elseBody->flavor == ExprFlavor::BLOCK);

				block = &static_cast<ExprBlock *>(staticIf->elseBody)->declarations;
			}
		}
	}
	else {
		block = &getExpressionNamespace(importer->import, &onlyConstants, importer->import)->members;

		onlyConstants |= (importer->flags & IMPORTER_IS_CONSTANT) != 0;

		if (!block) {
			return false;
		}

		if (importer->import->flavor != ExprFlavor::TYPE_LITERAL) {
			auto identifier = static_cast<ExprIdentifier *>(importer->import);

			while (identifier) {
				if (identifier->flavor != ExprFlavor::IDENTIFIER) {
					reportError(importer->import, "Error: You can only 'using' an identifier, a struct access of an identifier or a type");
					return false;
				}

				identifier = static_cast<ExprIdentifier *>(identifier->structAccess);
			}

			structAccess = pushStructAccessDown(static_cast<ExprIdentifier *>(importer->import), importer->structAccess, importer->import->start, importer->import->end);
		}
	}

	if (block) {
		for (auto member : block->importers) {
			if (!(member->flags & IMPORTER_IS_IMPORTED)) {
				auto import = INFER_NEW(Importer);
				import->flags = member->flags;

				if (onlyConstants) {
					import->flags |= IMPORTER_IS_CONSTANT;
				}

				import->flags |= IMPORTER_IS_IMPORTED;
				import->flags &= ~IMPORTER_IS_COMPLETE;

				import->import = member->import;
				import->structAccess = structAccess;

				addImporterToBlock(importer->enclosingScope, import, importer->indexInBlock);

				addImporter(import);
			}
		}

		if (importer->import->flavor == ExprFlavor::STATIC_IF && importer->enclosingScope == &globalBlock) {
			for (auto member : block->declarations) {
				if (!(member->flags & (DECLARATION_IS_IMPLICIT_IMPORT | DECLARATION_IMPORTED_BY_USING))) {
					if (checkForRedeclaration(importer->enclosingScope, member, importer->import)) {
						if (!onlyConstants || (member->flags & DECLARATION_IS_CONSTANT)) {
							addDeclarationToBlockUnchecked(importer->enclosingScope, member, importer->indexInBlock);

							wakeUpSleepers(&importer->enclosingScope->sleepingOnMe, false, member->name);

							if (!addDeclaration(member)) {
								return false;
							}
						}
					}
					else {
						return false;
					}
				}
			}
		}
		else {
			for (auto member : block->declarations) {
				if (!(member->flags & (DECLARATION_IS_IMPLICIT_IMPORT | DECLARATION_IMPORTED_BY_USING))) {
					if (checkForRedeclaration(importer->enclosingScope, member, importer->import)) {
						if (!onlyConstants || (member->flags & DECLARATION_IS_CONSTANT)) {
							auto import = INFER_NEW(Declaration);
							import->start = member->start;
							import->end = member->end;
							import->name = member->name;
							import->flags = member->flags;

							if (member->flags & DECLARATION_IS_CONSTANT) {
								import->type = member->type;
								import->initialValue = member->initialValue;
							}
							else {
								import->import = member;
								import->initialValue = structAccess;
							}

							import->flags |= DECLARATION_IMPORTED_BY_USING;

							addDeclarationToBlockUnchecked(importer->enclosingScope, import, importer->indexInBlock);

							assert(import->name.length);
							wakeUpSleepers(&importer->enclosingScope->sleepingOnMe, true, import->name);

							if (member->flags & DECLARATION_IS_CONSTANT) {
								if (!addDeclaration(import)) {
									return false;
								}
							}
						}
					}
					else {
						return false;
					}
				}
			}
		}
	}

	wakeUpSleepers(&importer->enclosingScope->sleepingOnMe);
	importer->flags |= IMPORTER_IS_COMPLETE;

	removeJob(&importerJobs, job);
	freeJob(job);

	return true;
}


bool inferDeclarationType(DeclarationJob *job) {
	PROFILE_FUNC();
	auto declaration = job->declaration;

	if (!(declaration->flags & DECLARATION_TYPE_IS_READY) && declaration->type) {
		bool yield = false;
		if (!assignOp(&job->value, declaration->type, &TYPE_TYPE, declaration->type, &yield)) {
			if (yield)
				return true;

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
			if (declaration->flags & DECLARATION_IS_RETURN) {
				if (declaration->enclosingScope->declarations.count != 1) {
					reportError(declaration->type, "Error: Functions with multiple return values cannot return void");
					return false;
				}
			}
			else {
				reportError(declaration->type, "Error: Declaration cannot have type void");
				return false;
			}
		}

		if (static_cast<ExprLiteral *>(declaration->type)->typeValue->flavor == TypeFlavor::NAMESPACE) {
			reportError(declaration->type, "Error: Declaration type cannot be a namespace");
			return false;
		}

		declaration->flags |= DECLARATION_TYPE_IS_READY;
		wakeUpSleepers(&declaration->sleepingOnMyType);
		declaration->sleepingOnMyType.free();
	}

	if (declaration->type && !declaration->initialValue) {
		if ((declaration->flags & DECLARATION_IS_EXPLICIT_DEFAULT) || !(declaration->flags & (DECLARATION_IS_UNINITIALIZED | DECLARATION_IS_ITERATOR | DECLARATION_IS_ITERATOR_INDEX | DECLARATION_IS_ARGUMENT | DECLARATION_IS_RETURN))) {
			auto type = static_cast<ExprLiteral *>(declaration->type)->typeValue;

			bool yield;
			declaration->initialValue = createDefaultValue(&job->type, declaration, type, &yield);

			if (yield) {
				return true;
			}

			if (!declaration->initialValue) {
				return false;
			}

			declaration->initialValue->valueOfDeclaration = declaration;

			declaration->initialValue->start = declaration->start;
			declaration->initialValue->end = declaration->end;
		}

		removeJob(&declarationJobs, job);

		declaration->flags |= DECLARATION_VALUE_IS_READY;
		wakeUpSleepers(&declaration->sleepingOnMyValue);
		declaration->sleepingOnMyValue.free();


		if (!(declaration->flags & DECLARATION_IS_CONSTANT) && declaration->enclosingScope == &globalBlock) {
			declarationWaitingOnSize.add(job);
		}
		else {
			freeJob(job);
		}
	}

	return true;
}

bool inferDeclarationValue(DeclarationJob *job) {
	PROFILE_FUNC();

	auto declaration = job->declaration;

	if (declaration->type && declaration->initialValue) {
		if (declaration->flags & DECLARATION_VALUE_IS_READY) return true;

		if (!(declaration->flags & DECLARATION_TYPE_IS_READY)) {
			goToSleep(&job->value, &declaration->sleepingOnMyType);
			return true;
		}

		assert(declaration->initialValue->type);

		Type *correct = static_cast<ExprLiteral *>(declaration->type)->typeValue;

		if (declaration->flags & DECLARATION_IS_ENUM_VALUE) {
			assert(correct->flavor == TypeFlavor::ENUM);

			if (declaration->initialValue->type == &TYPE_SIGNED_INT_LITERAL || declaration->initialValue->type == &TYPE_UNSIGNED_INT_LITERAL) {
				assert(declaration->initialValue->flavor == ExprFlavor::INT_LITERAL);

				if (!static_cast<TypeEnum *>(correct)->integerType) {
					goToSleep(&job->type, &correct->sleepingOnMe);

					return true;
				}

				if (!boundsCheckImplicitConversion(declaration->initialValue, static_cast<TypeEnum *>(correct)->integerType, static_cast<ExprLiteral *>(declaration->initialValue))) {
					reportError(CAST_FROM_SUBSTRUCT(ExprEnum, struct_, static_cast<TypeEnum *>(correct))->integerType, "   ..: Here is the enum type declaration");

					return false;
				}

				declaration->initialValue->type = correct;
			}
		}

		bool yield;
		if (!assignOp(&job->value, declaration->initialValue, correct, declaration->initialValue, &yield)) {
			return yield;
		}

		if ((declaration->flags & DECLARATION_IS_CONSTANT) || declaration->enclosingScope == &globalBlock ||
			(declaration->enclosingScope->flags & (BLOCK_IS_STRUCT | BLOCK_IS_ARGUMENTS | BLOCK_IS_RETURNS))) {
			if (!isLiteral(declaration->initialValue)) {
				reportError(declaration->type, "Error: Declaration value must be a constant");
				return false;
			}
		}

		removeJob(&declarationJobs, job);

		declaration->flags |= DECLARATION_VALUE_IS_READY;
		wakeUpSleepers(&declaration->sleepingOnMyValue);
		declaration->sleepingOnMyValue.free();

		if (!(declaration->flags & DECLARATION_IS_CONSTANT) && declaration->enclosingScope == &globalBlock) {
			declarationWaitingOnSize.add(job);
		}
		else {
			freeJob(job);
		}
	}
	else if (declaration->initialValue) {
		if (isDone(&job->value)) {
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

			if ((declaration->flags & DECLARATION_IS_CONSTANT) || declaration->enclosingScope == &globalBlock ||
				(declaration->enclosingScope->flags & (BLOCK_IS_STRUCT | BLOCK_IS_ARGUMENTS | BLOCK_IS_RETURNS))) {
				if (!isLiteral(declaration->initialValue)) {
					reportError(declaration->type, "Error: Declaration value must be a constant");
					return false;
				}
			}

			removeJob(&declarationJobs, job);

			declaration->flags |= DECLARATION_VALUE_IS_READY;
			declaration->flags |= DECLARATION_TYPE_IS_READY;
			wakeUpSleepers(&declaration->sleepingOnMyType);
			wakeUpSleepers(&declaration->sleepingOnMyValue);
			declaration->sleepingOnMyType.free();
			declaration->sleepingOnMyValue.free();


			if (!(declaration->flags & DECLARATION_IS_CONSTANT) && declaration->enclosingScope == &globalBlock) {
				declarationWaitingOnSize.add(job);
			}
			else {
				freeJob(job);
			}
		}
	}

	return true;
}

bool inferFunctionType(FunctionJob *job) {
	PROFILE_FUNC();

	auto function = job->function;

	for (auto argument : function->arguments.declarations) {
		assert(argument);

		if (!(argument->flags & DECLARATION_TYPE_IS_READY)) {
			goToSleep(&job->type, &argument->sleepingOnMyType);

			return true;
		}
	}


	for (auto return_ : function->returns.declarations) {
		assert(return_);

		if (!(return_->flags & DECLARATION_TYPE_IS_READY)) {
			goToSleep(&job->type, &return_->sleepingOnMyType);

			return true;
		}
		else if (static_cast<ExprLiteral *>(return_->type)->typeValue == &TYPE_VOID && function->returns.declarations.count != 1) {
			reportError(return_, "Error: Functions with multiple return values cannot return a void value");
			return false;
		}
	}

	function->type = getFunctionType(function);

	wakeUpSleepers(&function->sleepingOnMe);

	if (function->valueOfDeclaration && !function->valueOfDeclaration->type && !function->valueOfDeclaration->inferJob) {
		function->valueOfDeclaration->type = inferMakeTypeLiteral(function->start, function->end, function->type);
		function->valueOfDeclaration->flags |= DECLARATION_TYPE_IS_READY | DECLARATION_VALUE_IS_READY;
		wakeUpSleepers(&function->valueOfDeclaration->sleepingOnMyType);
		wakeUpSleepers(&function->valueOfDeclaration->sleepingOnMyValue);
		function->valueOfDeclaration->sleepingOnMyType.free();
		function->valueOfDeclaration->sleepingOnMyValue.free();
	}

	for (auto argument : function->arguments.declarations) {
		addSizeDependency(&job->sizeDependencies, static_cast<ExprLiteral *>(argument->type)->typeValue);
	}

	for (auto return_ : function->returns.declarations) {
		addSizeDependency(&job->sizeDependencies, static_cast<ExprLiteral *>(return_->type)->typeValue);
	}

	return true;
}

bool inferFunctionValue(FunctionJob *job) {
	PROFILE_FUNC();

	auto function = job->function;

	if (!function->type) {
		goToSleep(&job->value, &function->sleepingOnMe);
		return true;
	}

	if (function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
		if (function->body->flavor != ExprFlavor::STRING_LITERAL) {
			reportError(function->body, "Error: External function library must be a constant string");
			return false;
		}

		libraries.add(static_cast<ExprStringLiteral *>(function->body)->string);

		if (!function->valueOfDeclaration) {
			reportError(function, "Error: External functions must be named");
			return false;
		}

		CoffJob coff;
		coff.flavor = CoffJobFlavor::FUNCTION;
		coff.function = function;

		coffWriterQueue.add(coff);

		removeJob(&functionJobs, job);
		function->sleepingOnMe.free();
		freeJob(job);
	}
	else {
		assert(job->function->body);

		removeJob(&functionJobs, job);
		function->sleepingOnMe.free();
		functionWaitingOnSize.add(job);
	}

	return true;
}


bool inferSize(SizeJob *job) {
	PROFILE_FUNC();

	auto type = job->type;

	if (type->flavor == TypeFlavor::STRUCT) {
		totalSizes++;

		auto struct_ = static_cast<TypeStruct *>(type);

		while (job->sizingIndexInMembers < struct_->members.declarations.count) {
			auto member = struct_->members.declarations[job->sizingIndexInMembers];

			if (!(member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING))) {
				if (!(member->flags & DECLARATION_TYPE_IS_READY)) {
					goToSleep(job, &member->sleepingOnMyType);

					return true;
				}

				auto memberType = static_cast<ExprLiteral *>(member->type)->typeValue;

				if (!memberType->size) {
					goToSleep(job, &memberType->sleepingOnMe);

					return true;
				}

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

			wakeUpSleepers(&struct_->sleepingOnMe);

			removeJob(&sizeJobs, job);
			type->sleepingOnMe.free();
			freeJob(job);
		}
	}
	else if (type->flavor == TypeFlavor::ARRAY) {
		auto array = static_cast<TypeArray *>(type);

		assert(array->flags & TYPE_ARRAY_IS_FIXED);

		if (array->arrayOf->size) {
			array->size = array->arrayOf->size * array->count;
			array->alignment = array->arrayOf->alignment;

			wakeUpSleepers(&array->sleepingOnMe);
			removeJob(&sizeJobs, job);
			type->sleepingOnMe.free();
			freeJob(job);
		}
		else {
			goToSleep(job, &array->arrayOf->sleepingOnMe);
			return true;
		}
	}
	else if (type->flavor == TypeFlavor::ENUM) {
		auto struct_ = static_cast<TypeEnum *>(type);

		auto enum_ = CAST_FROM_SUBSTRUCT(ExprEnum, struct_, struct_);

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

		wakeUpSleepers(&struct_->sleepingOnMe, true);

		removeJob(&sizeJobs, job);
		type->sleepingOnMe.free();
		freeJob(job);
	}
	else {
		assert(false);
	}

	return true;
}

void createBasicDeclarations() {
	Declaration *targetWindows = INFER_NEW(Declaration);
	targetWindows->enclosingScope = nullptr;
	targetWindows->start = {};
	targetWindows->end = {};
	targetWindows->name = "TARGET_WINDOWS";
	targetWindows->type = inferMakeTypeLiteral(targetWindows->start, targetWindows->end, &TYPE_BOOL);

	auto literal = createIntLiteral(targetWindows->start, targetWindows->end, &TYPE_BOOL, BUILD_WINDOWS);

	targetWindows->initialValue = literal;
	targetWindows->flags |= DECLARATION_TYPE_IS_READY | DECLARATION_VALUE_IS_READY | DECLARATION_IS_CONSTANT;

	addDeclarationToBlock(&globalBlock, targetWindows);
}

bool doJob(SubJob *job) {
	if (!inferFlattened(job)) {
		return false;
	}
	else if (isDone(job)) {
		if (job->flavor == JobFlavor::DECLARATION_TYPE) {
			if (!inferDeclarationType(CAST_FROM_SUBSTRUCT(DeclarationJob, type, job)))
				return false;
		}
		else if (job->flavor == JobFlavor::DECLARATION_VALUE) {
			if (!inferDeclarationValue(CAST_FROM_SUBSTRUCT(DeclarationJob, value, job)))
				return false;
		}
		else if (job->flavor == JobFlavor::FUNCTION_TYPE) {
			if (!inferFunctionType(CAST_FROM_SUBSTRUCT(FunctionJob, type, job)))
				return false;
		}
		else if (job->flavor == JobFlavor::FUNCTION_VALUE) {
			if (!inferFunctionValue(CAST_FROM_SUBSTRUCT(FunctionJob, value, job)))
				return false;
		}
		else if (job->flavor == JobFlavor::SIZE) {
			if (!inferSize(static_cast<SizeJob *>(job))) {
				return false;
			}
		}
		else if (job->flavor == JobFlavor::IMPORTER) {
			if (!inferImporter(static_cast<ImporterJob *>(job)));
		}
	}

	return true;
}

void runInfer() {
	PROFILE_FUNC();


	createBasicDeclarations();

	while (true) {
		auto job = inferQueue.take();

		if (!job.declaration)
			break;

		if (job.isImporter) {
			addImporter(job.importer);
		}
		else {
			if (!addDeclaration(job.declaration))
				goto error;
		}

		while (subJobs.count || priorityJobs.count) {
			auto job = priorityJobs.count ? priorityJobs.pop() : subJobs.pop();

			if (!doJob(job)) {
				goto error;
			}
		}

		{
			PROFILE_ZONE("Check size dependencies");

			for (u64 i = 0; i < functionWaitingOnSize.count; i++) {
				auto job = functionWaitingOnSize[i];

				for (u64 j = 0; j < job->sizeDependencies.count; j++) {
					auto depend = job->sizeDependencies[j];

					if (depend->size) {
						job->sizeDependencies.unordered_remove(j--);
					}
				}

				if (job->sizeDependencies.count == 0) {
					functionWaitingOnSize.unordered_remove(i--);

					irGeneratorQueue.add(job->function);

					freeJob(job);
				}
			}

			for (u64 i = 0; i < declarationWaitingOnSize.count; i++) {
				auto job = declarationWaitingOnSize[i];

				for (u64 j = 0; j < job->sizeDependencies.count; j++) {
					auto depend = job->sizeDependencies[j];

					if (depend->size) {
						job->sizeDependencies.unordered_remove(j--);
					}
				}

				if (job->sizeDependencies.count == 0) {
					declarationWaitingOnSize.unordered_remove(i--);

					CoffJob coffJob;
					coffJob.declaration = job->declaration;
					coffJob.flavor = CoffJobFlavor::GLOBAL_DECLARATION;
					coffWriterQueue.add(coffJob);

					freeJob(job);
				}
			}
		}
	}

	if ((sizeJobs || functionJobs || declarationJobs) && !hadError) { // We didn't complete type inference, check for undeclared identifiers or circular dependencies! If we already had an error don't spam more
		// Check for undeclared identifiers

		for (auto job = functionJobs; job; job = job->next) {
			if (!isDone(&job->type)) {
				auto haltedOn = *getHalt(&job->type);

				if (checkForUndeclaredIdentifier(haltedOn)) {
					goto error;
				}
			}

			if (!isDone(&job->value)) {
				auto haltedOn = *getHalt(&job->value);

				if (checkForUndeclaredIdentifier(haltedOn)) {
					goto error;
				}
			}
		}

		for (auto job = declarationJobs; job; job = job->next) {
			if (!isDone(&job->type)) {
				auto haltedOn = *getHalt(&job->type);

				if (checkForUndeclaredIdentifier(haltedOn)) {
					goto error;
				}
			}

			if (!isDone(&job->value)) {
				auto haltedOn = *getHalt(&job->value);

				if (checkForUndeclaredIdentifier(haltedOn)) {
					goto error;
				}
			}
		}

		for (auto job = importerJobs; job; job = job->next) {
			if (!isDone(job)) {
				auto haltedOn = *getHalt(job);

				if (checkForUndeclaredIdentifier(haltedOn)) {
					goto error;
				}
			}
		}

		for (auto job = sizeJobs; job; job = job->next) {
			if (!isDone(job)) {
				auto haltedOn = *getHalt(job);

				if (checkForUndeclaredIdentifier(haltedOn)) {
					goto error;
				}
			}
		}

		// Check for circular dependencies

		Array<Declaration *> loop;

		for (auto job = declarationJobs; job; job = job->next) {
			loop.clear();

			s64 loopIndex = findLoop(loop, job->declaration);

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

		for (auto job = sizeJobs; job; job = job->next) {
			auto type = job->type;

			if (type->flavor == TypeFlavor::STRUCT) {
				auto haltedOn = static_cast<TypeStruct *>(type)->members.declarations[job->sizingIndexInMembers];

				reportError(haltedOn, "%.*s sizing halted here", STRING_PRINTF(type->name));
			}
			else {
				reportError("%.*s sizing halted", STRING_PRINTF(type->name));
			}
		}

		for (auto job = functionJobs; job; job = job->next) {
			String name = (job->function->valueOfDeclaration ? job->function->valueOfDeclaration->name : "(function)");

			if (!isDone(&job->type)) {
				auto haltedOn = *getHalt(&job->type);

				reportError(haltedOn, "%.*s type halted here", STRING_PRINTF(name));
			}

			if (!isDone(&job->value)) {
				auto haltedOn = *getHalt(&job->value);

				reportError(haltedOn, "%.*s value halted here", STRING_PRINTF(name));

				if (haltedOn->flavor == ExprFlavor::IDENTIFIER) {
					auto identifier = static_cast<ExprIdentifier *>(haltedOn);
				}
			}

			if (isDone(&job->type) && isDone(&job->value)) {
				reportError(job->function, "Function halted after completing infer");

				for (auto declaration : job->function->arguments.declarations) {
					if (declaration->sleepingOnMyType.count) {
						reportError(declaration, "Declaration type has sleepers, flags: %llx", declaration->flags);
					}
				}

				for (auto declaration : job->function->arguments.declarations) {
					if (declaration->sleepingOnMyValue.count) {
						reportError(declaration, "Declaration value has sleepers, flags: %llx", declaration->flags);
					}
				}

				for (auto declaration : job->function->returns.declarations) {
					if (declaration->sleepingOnMyType.count) {
						reportError(declaration, "Declaration type has sleepers, flags: %llx", declaration->flags);
					}
				}

				for (auto declaration : job->function->returns.declarations) {
					if (declaration->sleepingOnMyValue.count) {
						reportError(declaration, "Declaration value has sleepers, flags: %llx", declaration->flags);
					}
				}
			}
		}

		for (auto job = declarationJobs; job; job = job->next) {
			String name = job->declaration->name;

			if (!isDone(&job->type)) {
				auto haltedOn = *getHalt(&job->type);

				reportError(haltedOn, "%.*s type halted here", STRING_PRINTF(name));
			}

			if (!isDone(&job->value)) {
				auto haltedOn = *getHalt(&job->value);

				reportError(haltedOn, "%.*s value halted here", STRING_PRINTF(name));

				if (haltedOn->flavor == ExprFlavor::IDENTIFIER) {
					auto identifier = static_cast<ExprIdentifier *>(haltedOn);
				}
			}

			if (isDone(&job->type) && isDone(&job->value)) {
				reportError(job->declaration, "Declaration halted after completing infer");
			}
		}

		goto error;
	}

	if (functionWaitingOnSize.count || declarationWaitingOnSize.count) {
		reportError("Error: waiting on size @Incomplete");
		goto error;
	}

	startLlvm.notify_one();

	irGeneratorQueue.add(nullptr);

	return;
error:;
	assert(hadError);

	startLlvm.notify_one();

	irGeneratorQueue.add(nullptr);
}