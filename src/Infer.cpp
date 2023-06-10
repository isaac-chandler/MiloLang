#include "Basic.h"
#include "Infer.h"
#include "Array.h"
#include "Parser.h"
#include "Lexer.h"
#include "IrGenerator.h"
#include "IrRunner.h"
#include "CoffWriter.h"
#include "TypeTable.h"
#include "ArraySet.h"
#include "Error.h"
#include "Polymorph.h"

BucketedArenaAllocator inferArena(1024 * 1024);

#if 1
#define INFER_NEW(T) new (static_cast<T *>(inferArena.allocate(sizeof(T)))) T
#define INFER_NEW_ARRAY(T, C) new (static_cast<T *>(inferArena.allocate((C) * sizeof(T)))) T[C]

#else
#define PARSER_NEW(T) new T
#define PARSER_NEW_ARRAY(T, C) new T[C]
#endif

enum class JobFlavor : u8 {
	SIZE,
	DECLARATION_TYPE,
	DECLARATION_VALUE,
	FUNCTION_HEADER,
	FUNCTION_BODY,
	IMPORTER, 
	RUN
};

ExprLiteral *inferMakeTypeLiteral(CodeLocation start, EndLocation end, Type *type) {
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
	if (value != 0) { // @Hack so we don't crash when doing an implicit cast from a 0 to enum_flags that hasn't had its underlying type inferred
		auto underlying = type;

		if (type->flavor == TypeFlavor::ENUM) {
			underlying = static_cast<TypeEnum *>(type)->integerType;
		}

		if (underlying == &TYPE_BOOL) {
			value = value != 0;
		}
		else if (underlying->flavor == TypeFlavor::INTEGER && (underlying->flags & TYPE_INTEGER_IS_SIGNED)) {
			u64 sign = 1ULL << (underlying->size * 8 - 1);
			u64 mask = (sign - 1) | sign;
			value &= mask;

			if ((underlying->flags & TYPE_INTEGER_IS_SIGNED) && (value & sign)) {
				value |= ~mask;
			}
		}
	}

	return createIntLiteral(start, end, type, value);
}

struct SubJob {
	Array<Type *> *sizeDependencies;

	Array<Array<Expr **>> flatteneds;
	Array<u32> indices;
	u32 flattenedCount = 0;
	u32 sleepCount = 0;
	String sleepingOnName;

	JobFlavor flavor;

#if BUILD_DEBUG
	const char *sleepReason;
#endif

	SubJob(JobFlavor flavor, Array<Type *> *sizeDependencies) : flavor(flavor), sizeDependencies(sizeDependencies) {}
};

bool isDone(SubJob *job) {
	return job->flattenedCount == 0;
}

Expr **getHalt(SubJob *job) {
	return job->flatteneds[job->flattenedCount - 1][job->indices[job->flattenedCount - 1]];
}

void goToSleep(SubJob *job, Array<SubJob *> *sleepingOnMe, const char *reason, String name) {
	assert(job->sleepCount == 0 || job->sleepingOnName == name);
	
	job->sleepingOnName = name;

	job->sleepCount++;
	
	sleepingOnMe->add(job);

#ifdef BUILD_DEBUG
	job->sleepReason = reason;
#endif
}

void goToSleep(SubJob *job, Array<SubJob *> *sleepingOnMe, const char *reason) {
	assert(job->sleepCount == 0 || job->sleepingOnName == "");

	job->sleepingOnName = { nullptr, 0u };

	job->sleepCount++;

	sleepingOnMe->add(job);

#ifdef BUILD_DEBUG
	job->sleepReason = reason;
#endif
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

	SubJob header;
	SubJob body;

	Array<Type *> sizeDependencies;
	FunctionJob *next = nullptr;
	FunctionJob *previous = nullptr;

	FunctionJob() : header(JobFlavor::FUNCTION_HEADER, nullptr),
		body(JobFlavor::FUNCTION_BODY, &sizeDependencies) {}
};

struct SizeJob : SubJob {
	Type *type;

	u32 sizingIndexInMembers = 0;
	u32 runningSize = 0;

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

struct RunJob : SubJob {
	Array<ExprFunction *> checkingFunctions;
	Array<u32> checkingFunctionIndices;

	Array<Type *> generatingTypeInfos;

	ExprRun *run;

	RunJob *next = nullptr;
	RunJob *previous = nullptr;

	RunJob() : SubJob(JobFlavor::RUN, nullptr) {}
};

DeclarationJob *declarationJobs;
FunctionJob *functionJobs;
SizeJob *sizeJobs;
RunJob *runJobs;
ImporterJob *importerJobs;

Array<SubJob *> subJobs;

inline void addSubJob(SubJob *job) {
#if BUILD_DEBUG
	for (auto x : subJobs) {
		assert(x != job);
	}
#endif
	
	subJobs.add(job);
}

struct OverloadWaitingForLock {
	Declaration *overloadSet;
	SubJob *waiting;
};

Array<OverloadWaitingForLock> overloadsWaitingForLock;

void forceAddDeclaration(Declaration *declaration);
bool addDeclaration(Declaration *declaration, Module *members);

void wakeUpSleepers(Array<SubJob *> *sleepers, String name = String(nullptr, 0u)) {

	if (name.length == 0) {
		PROFILE_ZONE("Unnamed wakeUpSleepers");
		for (auto sleeper : *sleepers) {
			if (--sleeper->sleepCount == 0)
				addSubJob(sleeper);
		}

		sleepers->clear();
	}
	else {
		PROFILE_ZONE("Named wakeUpSleepers");
		for (u32 i = 0; i < sleepers->count; i++) {
			auto sleeper = (*sleepers)[i];

			if (sleeper->sleepingOnName.length == 0 || name == sleeper->sleepingOnName) {
				if (--sleeper->sleepCount == 0) {
					sleepers->unordered_remove(i--);
					addSubJob(sleeper);
				}
			}
		}
	}
}

void addImporter(Importer *importer, struct Module *module);

void addBlock(Block *block) {
	if (block->queued)
		return;

	block->queued = true;

	for (auto &declaration : block->declarations) {
		auto success = addDeclaration(declaration, nullptr);

		assert(success); // Adding a block should never fail, the only failing cases for addDeclaration are if it is added to global scope
	}

	for (auto importer : block->importers) {
		addImporter(importer, nullptr);
	}

	wakeUpSleepers(&block->sleepingOnMe);
}

SizeJob *allocateSizeJob();
RunJob *allocateRunJob();
FunctionJob *allocateFunctionJob();

void beginFlatten(SubJob *job, Expr **expr);

void addFunction(ExprFunction *function) {
	if (function->arguments.queued)
		return;

	FunctionJob *job = allocateFunctionJob();

	job->function = function;

	if (!(function->flags & EXPR_FUNCTION_IS_POLYMORPHIC)) {
		addBlock(&function->constants);
		addBlock(&function->arguments);
		addBlock(&function->returns);
	}

	addSubJob(&job->header);


	++totalFunctions;

	addJob(&functionJobs, job);
}

void addSizeJobIfNeeded(Type *type) {
	if (!type->sizeJob && !type->size) {
		SizeJob *job = allocateSizeJob();

		job->type = type;

		type->sizeJob = job;
		++totalSizes;

		addJob(&sizeJobs, job);
		addSubJob(job);
	}
}

void addTypeBlock(Type *type, Declaration *valueOfDeclaration) {
	if (type->flavor == TypeFlavor::STRUCT || type->flavor == TypeFlavor::ENUM) {
		auto struct_ = static_cast<TypeStruct *>(type);

		if (!struct_->constants.queued) {
			if (valueOfDeclaration && (valueOfDeclaration->flags & DECLARATION_IS_CONSTANT)) {
				type->name = valueOfDeclaration->name;
				struct_->enclosingScope = valueOfDeclaration->enclosingScope;
			}
			else if (type != &TYPE_CONTEXT) { // The context is passed into addTypeBlock but does not have a declaration. It is already given a name so don't rename to anonymous
				if (type->flavor == TypeFlavor::STRUCT)
					type->name = (type->flags & TYPE_STRUCT_IS_UNION) ? "union" : "struct";
				else
					type->name = (type->flags & TYPE_ENUM_IS_FLAGS) ? "enum_flags" : "enum";
				struct_->enclosingScope = &runtimeModule->members;
				type->flags |= TYPE_IS_ANONYMOUS;
			}
			

			if (struct_->flags & TYPE_IS_POLYMORPHIC) {
				addBlock(&struct_->constants);
			} 
			else {
				addSizeJobIfNeeded(type);
				addBlock(&struct_->members);

				addStruct(struct_);
			}
		}
	}
	else if (type->flavor == TypeFlavor::MODULE) {
		auto struct_ = static_cast<TypeStruct *>(type);
		addBlock(&struct_->members);
	}
}


void addRunJob(ExprRun *run);

void flatten(Array<Expr **> &flattenTo, Expr **expr) {
	switch ((*expr)->flavor) {
	case ExprFlavor::IDENTIFIER: {
		auto identifier = static_cast<ExprIdentifier *>(*expr);

		if (identifier->type == &TYPE_UNARY_DOT)
			return;

		assert(!identifier->resolveFrom || identifier->resolveFrom->flavor != BlockFlavor::ARGUMENTS || !(CAST_FROM_SUBSTRUCT(ExprFunction, arguments, identifier->resolveFrom)->flags & EXPR_FUNCTION_IS_POLYMORPHIC));

		if (identifier->structAccess) {
			flatten(flattenTo, &identifier->structAccess);
		}

		flattenTo.add(expr);
		break;
	}
	case ExprFlavor::ENTRY_POINT:
	case ExprFlavor::CONTEXT:
	case ExprFlavor::ADD_CONTEXT:
	case ExprFlavor::BREAK:
	case ExprFlavor::CONTINUE:
	case ExprFlavor::REMOVE:
	case ExprFlavor::ENUM_INCREMENT: {
		flattenTo.add(expr);
		break;
	}
	case ExprFlavor::IMPORT:
	case ExprFlavor::LOAD: {
		auto load = static_cast<ExprLoad *>(*expr);

		flatten(flattenTo, &load->file);

		flattenTo.add(expr);

		break;
	}
	case ExprFlavor::RUN: {
		auto run = static_cast<ExprRun *>(*expr);

		addRunJob(run);

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
	case ExprFlavor::ARRAY_LITERAL: {
		auto literal = static_cast<ExprArrayLiteral *>(*expr);

		// Unary . array literals have their type set by the enclosing expr
		// and then will be pushed
		if (literal->type != &TYPE_ARRAY_LITERAL) {
			if (literal->typeValue) {
				flatten(flattenTo, &literal->typeValue);
			}

			for (u32 i = 0; i < literal->count; i++) {
				flatten(flattenTo, &literal->values[i]);
			}

			flattenTo.add(expr);
		}

		break;
	}
	case ExprFlavor::STRUCT_LITERAL: {
		auto literal = static_cast<ExprStructLiteral *>(*expr);

		// Literal type is null for explicitly typed struct literals, 
		// unary . struct literals have their type set by the enclosing expr
		// and then will be pushed
		if (literal->type != &TYPE_STRUCT_LITERAL) {
			if (literal->typeValue) {
				flatten(flattenTo, &literal->typeValue);
			}

			for (u32 i = 0; i < literal->initializers.count; i++) {
				flatten(flattenTo, &literal->initializers.values[i]);
			}

			flattenTo.add(expr);
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

		if (!(comma->flags & EXPR_COMMA_ASSIGNMENT_IS_DECLARATION)) {

			for (u64 i = 0; i < comma->exprCount; i++) {
				flatten(flattenTo, &comma->left[i]);
			}
		}

		flatten(flattenTo, &comma->call);

		flattenTo.add(expr);
		break;
	}
	case ExprFlavor::TYPE_LITERAL: {
		auto literal = static_cast<ExprLiteral *>(*expr);

		if (literal->typeValue != &TYPE_CONTEXT)
			addTypeBlock(literal->typeValue, literal->valueOfDeclaration);
		
		if (literal->typeValue->flags & TYPE_IS_POLYMORPHIC)
			flattenTo.add(expr);

		break;
	}
	case ExprFlavor::STRING_LITERAL:
	case ExprFlavor::FLOAT_LITERAL:
	case ExprFlavor::INT_LITERAL:
		break;
	case ExprFlavor::PUSH_CONTEXT: {
		ExprBinaryOperator *pushContext = static_cast<ExprBinaryOperator *>(*expr);

		flatten(flattenTo, &pushContext->left);
		flattenTo.add(expr);
		flatten(flattenTo, &pushContext->right);
		break;
	}
	case ExprFlavor::BINARY_OPERATOR: {
		ExprBinaryOperator *binary = static_cast<ExprBinaryOperator *>(*expr);

		if (binary->left)
			flatten(flattenTo, &binary->left);

		if (binary->right) {
			flatten(flattenTo, &binary->right);
		}

		if (binary->type == &TYPE_AUTO_CAST) {
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


void pushFlatten(SubJob *job, Expr *newExpr) {
	auto expr = getHalt(job);
	*expr = newExpr;

	beginFlatten(job, expr);
}

void trySolidifyNumericLiteralToDefault(Expr *expr) {
	expr->type = getTypeForExpr(expr);
}

bool boundsCheckImplicitConversion(Expr *location, Type *convertTo, ExprLiteral *convertFrom, bool silentCheck = false) {
	if (convertTo->flags & TYPE_INTEGER_IS_SIGNED) {
		s64 max = static_cast<s64>((1ULL << (convertTo->size * 8 - 1)) - 1);
		s64 min = -static_cast<s64>(max) - 1;

		if (convertFrom->type->flags & TYPE_INTEGER_IS_SIGNED) {
			if (convertFrom->signedValue > max) {
				if (!silentCheck) 
					reportError(location, "Error: Integer literal too large for %.*s (max: %" PRIi64 ", given: %" PRIi64 ")", STRING_PRINTF(convertTo->name), max, convertFrom->signedValue);
				return false;
			}

			if (convertFrom->signedValue < min) {
				if (!silentCheck)
					reportError(location, "Error: Integer literal too small for %.*s (min: %" PRIi64 ", given: %" PRIi64 ")", STRING_PRINTF(convertTo->name), min, convertFrom->signedValue);
				return false;
			}
		}
		else {
			if (convertFrom->unsignedValue > static_cast<u64>(max)) {
				if (!silentCheck)
					reportError(location, "Error: Integer literal too large for %.*s (max: %" PRIi64 ", given: %" PRIu64 ")", STRING_PRINTF(convertTo->name), max, convertFrom->unsignedValue);
				return false;
			}
		}
	}
	else {
		u64 max = convertTo == &TYPE_U64 ? UINT64_MAX : (1ULL << (convertTo->size * 8)) - 1;

		if ((convertFrom->type->flags & TYPE_INTEGER_IS_SIGNED) && convertFrom->signedValue < 0) {
			if (convertFrom->signedValue < 0) {
				if (!silentCheck)
					reportError(location, "Error: Integer literal too small for %.*s (min: 0, given: %" PRIi64 ")", STRING_PRINTF(convertTo->name), convertFrom->signedValue);
				return false;
			}
		}

		if (convertFrom->unsignedValue > max) {
			if (!silentCheck)
				reportError(location, "Error: Integer literal too large for %.*s (max: %" PRIu64 ", given: %" PRIu64 ")", STRING_PRINTF(convertTo->name), max, convertFrom->unsignedValue);
			return false;
		}
	}

	return true;
}


bool solidifyOneLiteral(ExprBinaryOperator *binary) {
	auto left = binary->left;
	auto right = binary->right;

	if (left->type == &TYPE_FLOAT_LITERAL && right->type->flavor == TypeFlavor::FLOAT) {
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
			literal->floatValue = static_cast<double>(literal->unsignedValue);

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


			literal->floatValue = static_cast<double>(literal->signedValue);
			literal->flavor = ExprFlavor::FLOAT_LITERAL;
		}

		left->type = right->type;
	}
	else if (right->type == &TYPE_FLOAT_LITERAL && left->type->flavor == TypeFlavor::FLOAT) {
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

			literal->floatValue = static_cast<double>(literal->unsignedValue);
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

			literal->floatValue = static_cast<double>(literal->signedValue);

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

bool isValidCast(Type *to, Type *from) {
	if (from == &TYPE_VOID || from->flavor == TypeFlavor::AUTO_CAST || from->flavor == TypeFlavor::MODULE) {
		return false;
	}

	if (to == &TYPE_VOID || to->flavor == TypeFlavor::AUTO_CAST || to->flavor == TypeFlavor::MODULE) {
		return false;
	}
	if (from == TYPE_ANY && to != TYPE_ANY) {
		return true;
	}

	if (to->flavor == from->flavor) {
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
		return to->flavor == TypeFlavor::BOOL || (to->flavor == TypeFlavor::INTEGER && to->size == 8) || (from == TYPE_VOID_POINTER && to->flavor == TypeFlavor::FUNCTION);
	}
	else if (from->flavor == TypeFlavor::STRING) {
		return to->flavor == TypeFlavor::BOOL || to == TYPE_U8_ARRAY;
	}
	else if (from->flavor == TypeFlavor::STRUCT) {
		return false;
	}
	else if (from->flavor == TypeFlavor::ARRAY) {
		// Casts between array types are handled above
		return to->flavor == TypeFlavor::BOOL || (to == &TYPE_STRING && from == TYPE_U8_ARRAY);
	}
	else {
		assert(false); // Invalid code path
		return false;
	}
}

bool isValidCast(SubJob *job, Type *to, Type *from, u64 flags, bool *yield) {
	*yield = false;
	if (from == &TYPE_VOID || from->flavor == TypeFlavor::AUTO_CAST || from->flavor == TypeFlavor::MODULE) {
		return false;
	}

	if (to == &TYPE_VOID || to->flavor == TypeFlavor::AUTO_CAST || to->flavor == TypeFlavor::MODULE) {
		return false;
	}

	if (flags & EXPR_CAST_IS_BITWISE) {
		if (!to->size) {
			goToSleep(job, &to->sleepingOnMe, "Bit cast waiting on dest size");
			*yield = true;
			return false;
		}

		if (!from->size) {
			goToSleep(job, &to->sleepingOnMe, "Bit cast waiting on source size");
			*yield = true;
			return false;
		}

		return to->size == from->size;
	}

	return isValidCast(to, from);
}

void addSizeDependency(Array<Type *> *sizeDependencies, Type *type) {
	if (sizeDependencies && type->size == 0 && !(type->flags & TYPE_IS_INTERNAL)) {
		sizeDependencies->add(type);
	}
}

void copyLiteral(Expr **exprPointer, Expr *expr) {
	switch (expr->flavor) {
	case ExprFlavor::FUNCTION: // Functions are unique
	case ExprFlavor::IMPORT: // No reason to duplicate imports
	case ExprFlavor::STRING_LITERAL: // Don't duplicate string literals this will bloat the binary
	case ExprFlavor::IDENTIFIER: { // An identifier can be on a constant if it is an overload set
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
	case ExprFlavor::ARRAY_LITERAL: {
		auto literal = static_cast<ExprArrayLiteral *>(expr);

		if (literal->type == &TYPE_ARRAY_LITERAL) {
			auto newLiteral = INFER_NEW(ExprArrayLiteral);
			*newLiteral = *static_cast<ExprArrayLiteral *>(expr);

			newLiteral->values = INFER_NEW_ARRAY(Expr *, literal->count);

			for (u32 i = 0; i < literal->count; i++) {
				newLiteral->values[i] = literal->values[i];

				copyLiteral(&newLiteral->values[i], literal->values[i]);
			}

			newLiteral->start = (*exprPointer)->start;
			newLiteral->end = (*exprPointer)->end;

			*exprPointer = newLiteral;
		}
		else {
			*exprPointer = literal;
		}

		break;
	}
	case ExprFlavor::STRUCT_LITERAL: {
		auto literal = static_cast<ExprStructLiteral *>(expr);

		if (literal->type == &TYPE_STRUCT_LITERAL) {
			auto newLiteral = INFER_NEW(ExprStructLiteral);
			*newLiteral = *static_cast<ExprStructLiteral *>(expr);

			newLiteral->initializers.values = INFER_NEW_ARRAY(Expr *, literal->initializers.count);

			for (u32 i = 0; i < literal->initializers.count; i++) {
				newLiteral->initializers.values[i] = literal->initializers.values[i];

				copyLiteral(&newLiteral->initializers.values[i], literal->initializers.values[i]);
			}

			newLiteral->start = (*exprPointer)->start;
			newLiteral->end = (*exprPointer)->end;

			*exprPointer = newLiteral;
		}
		else {
			*exprPointer = literal;
		}

		break;
	}
	default:
		assert(false);
	}
}

void doConstantCast(Expr **cast) {
	auto binary = static_cast<ExprBinaryOperator *>(*cast);
	auto castTo = static_cast<ExprLiteral *>(binary->left)->typeValue;
	auto expr = binary->right;

	if (expr->type == castTo) {
		*cast = expr;
	}

	if (binary->flags & EXPR_CAST_IS_BITWISE) {
		return; // @Incomplete
	}

	if (expr->flavor == ExprFlavor::INT_LITERAL) {
		auto old = static_cast<ExprLiteral *>(expr);

		if (castTo->flavor == TypeFlavor::INTEGER || castTo->flavor == TypeFlavor::ENUM || castTo == &TYPE_BOOL) {
			*cast = createInBoundsIntLiteral(binary->start, expr->end, castTo, old->unsignedValue);
		}
		else if (castTo->flavor == TypeFlavor::FLOAT) {
			*cast = createFloatLiteral(binary->start, expr->end, castTo,
				(old->type->flags & TYPE_INTEGER_IS_SIGNED) ? static_cast<double>(old->signedValue) : static_cast<double>(old->unsignedValue));
		}
		else if (castTo->flavor == TypeFlavor::POINTER || castTo->flavor == TypeFlavor::FUNCTION) {
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
	else if (expr->flavor == ExprFlavor::ARRAY_LITERAL) {
		if (castTo == &TYPE_BOOL) {
			*cast = createIntLiteral(binary->start, expr->end, castTo, static_cast<TypeArray *>(expr->type)->count != 0 ? 1 : 0);
		}
		else if (castTo->flavor == TypeFlavor::ARRAY) {
			assert(!(castTo->flags & (TYPE_ARRAY_IS_FIXED | TYPE_ARRAY_IS_DYNAMIC)));

			assert(static_cast<TypeArray *>(castTo)->arrayOf == static_cast<TypeArray *>(expr->type)->arrayOf);

			copyLiteral(cast, expr);
			(*cast)->type = castTo;
			(*cast)->start = binary->start;
		}
	}
	else if (expr->flavor == ExprFlavor::STRING_LITERAL) {
		if (castTo == &TYPE_BOOL) {
			auto string = static_cast<ExprStringLiteral *>(expr);
			*cast = createIntLiteral(binary->start, expr->end, castTo, string->string.length != 0 ? 1 : 0);
		}
	}
	else if (expr->flavor == ExprFlavor::FUNCTION) {
		if (castTo == &TYPE_BOOL) {
			*cast = createIntLiteral(binary->start, expr->end, castTo, 1);
		}
	}
}

void markDeclarationsAsPointedTo(Expr *expr) {
	while (true) {
		if (expr->flavor == ExprFlavor::IDENTIFIER) {
			auto identifier = static_cast<ExprIdentifier *>(expr);
			if (identifier->structAccess) {
				// If a struct access is by pointer, we are not pointing to any new declarations
				if (identifier->structAccess->type->flavor == TypeFlavor::POINTER) {
					break;
				}

				expr = identifier->structAccess;
			}
			else {
				identifier->declaration->flags |= DECLARATION_IS_POINTED_TO;
				break;
			}
		}
		else if (expr->flavor == ExprFlavor::BINARY_OPERATOR) {
			auto binary = static_cast<ExprBinaryOperator *>(expr);

			if (binary->op != TOKEN('[')) {
				break;
			}

			if (!(binary->left->type->flags & TYPE_ARRAY_IS_FIXED)) {
				break;
			}

			expr = binary->left;
		}
		else {
			break;
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

	if (cast->right->flags & EXPR_IS_SPREAD) cast->flags |= EXPR_IS_SPREAD;


	if (((*castFrom)->type->flags & TYPE_ARRAY_IS_FIXED) && castTo->flavor == TypeFlavor::ARRAY && !(castTo->flags & TYPE_ARRAY_IS_FIXED)) {
		markDeclarationsAsPointedTo(*castFrom);
	}

	*castFrom = cast;

	addSizeDependency(sizeDependencies, castTo);



	assert(isValidCast(castTo, cast->right->type));
	doConstantCast(castFrom);
}

// Passing null for location means that an error shouldn't be reported, and null should be returned if the expression doesn't have a namespace
TypeStruct *getExpressionNamespace(Expr *expr, bool *onlyConstants, Expr *location) {
	if (expr->type->flavor == TypeFlavor::STRUCT || expr->type->flavor == TypeFlavor::ARRAY || expr->type == &TYPE_STRING) {
		*onlyConstants = false;
		return static_cast<TypeStruct *>(expr->type);
	}
	else if (expr->type->flavor == TypeFlavor::POINTER) {
		auto type = static_cast<TypePointer *>(expr->type)->pointerTo;

		if (type->flavor == TypeFlavor::STRUCT || type->flavor == TypeFlavor::ARRAY || type == &TYPE_STRING) {
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

		if (type->flags & TYPE_IS_POLYMORPHIC) {
			if (location)
				reportError(location, "Error: Cannot access the fields of a polymorphic struct");
			return nullptr;
		}

		if (type->flavor == TypeFlavor::STRUCT || type->flavor == TypeFlavor::ARRAY || type->flavor == TypeFlavor::MODULE || type->flavor == TypeFlavor::ENUM) {
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

bool switchCasesAreSame(Expr *a, Expr *b) {
	auto aLiteral = static_cast<ExprLiteral *>(a);
	auto bLiteral = static_cast<ExprLiteral *>(b);

	switch (a->flavor) {
	case ExprFlavor::INT_LITERAL:
		return aLiteral->unsignedValue == bLiteral->unsignedValue;
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
	case ExprFlavor::ARRAY_LITERAL: {
		auto arrayA = static_cast<ExprArrayLiteral *>(a);
		auto arrayB = static_cast<ExprArrayLiteral *>(b);

		if (arrayA->count != arrayB->count)
			return false;

		if (arrayA->typeValue && arrayB->typeValue && !switchCasesAreSame(arrayA->typeValue, arrayB->typeValue))
			return false;

		for (u32 i = 0; i < arrayA->count; i++) {
			if (!switchCasesAreSame(arrayA->values[i], arrayB->values[i]))
				return false;
		}

		return true;
	}
	case ExprFlavor::STRUCT_LITERAL: {
		auto structA = static_cast<ExprStructLiteral *>(a);
		auto structB = static_cast<ExprStructLiteral *>(b);

		if (structA->initializers.count != structB->initializers.count)
			return false;

		if ((structA->flags & EXPR_STRUCT_LITERAL_UNSPECIFIED_MEMBERS_UNINITIALZIED) != (structB->flags & EXPR_STRUCT_LITERAL_UNSPECIFIED_MEMBERS_UNINITIALZIED))
			return false;

		if (structA->typeValue && structB->typeValue && !switchCasesAreSame(structA->typeValue, structB->typeValue))
			return false;

		if (!structA->initializers.declarations != !structB->initializers.declarations)
			return false;

		for (u32 i = 0; i < structA->initializers.count; i++) {
			if (structA->initializers.declarations && structA->initializers.declarations[i] != structB->initializers.declarations[i])
				return false;
			else if (structA->initializers.names && structA->initializers.names[i] != structB->initializers.names[i])
				return false;

			if (!structA->initializers.values[i] != !structB->initializers.values[i])
				return false;

			if (!switchCasesAreSame(structA->initializers.values[i], structB->initializers.values[i]))
				return false;
		}

		return true;
	}
	default:
		return false;
	}
}

bool inferUnaryDot(SubJob *job, TypeEnum *enum_, Expr **exprPointer, bool *yield, bool silentCheck = false) {
	bool invert = false;
	*yield = false;

	Expr **expr = exprPointer;

	while ((*expr)->flavor == ExprFlavor::UNARY_OPERATOR) {
		auto unary = static_cast<ExprUnaryOperator *>(*expr);

		assert(unary->op == TOKEN('~'));

		expr = &unary->value;

		invert = !invert;
	}

	auto identifier = static_cast<ExprIdentifier *>(*expr);
	assert(identifier->flavor == ExprFlavor::IDENTIFIER);

	auto member = findDeclarationNoYield(&enum_->members, identifier->name);

	if (!member) {
		if (!silentCheck)
			reportError(identifier, "Error: %.*s does not have member %.*s", STRING_PRINTF(enum_->name), STRING_PRINTF(identifier->name));
		return false;
	}
	assert(member->flags & DECLARATION_IS_CONSTANT); // All enum members should be constants

	if (!silentCheck) {
		if (!(member->flags & DECLARATION_VALUE_IS_READY)) {
			*yield = true;
			goToSleep(job, &member->sleepingOnMyValue, "Unary dot waiting for enum value");
			return false;
		}

		if (expr != exprPointer) {
			if (!(enum_->flags & TYPE_ENUM_IS_FLAGS)) {
				reportError(*exprPointer, "Error: Cannot invert an enum");
				return false;
			}

			assert(member->initialValue->flavor == ExprFlavor::INT_LITERAL);

			u64 value = static_cast<ExprLiteral *>(member->initialValue)->unsignedValue;

			*exprPointer = createInBoundsIntLiteral((*exprPointer)->start, (*expr)->end, enum_, invert ? ~value : value);
		}
		else {
			copyLiteral(exprPointer, member->initialValue);
		}
	}

	return true;
}

bool usingConversionIsValid(SubJob *job, Type *correct, Expr *given, bool *yield) {
	assert(correct != given->type);

	bool onlyConstants = false;
	auto struct_ = getExpressionNamespace(given, &onlyConstants, nullptr);

	if (!struct_)
		return false;

	if (struct_->members.importers.count) {
		goToSleep(job, &struct_->members.sleepingOnMe, "Using conversion waiting on importer");
		job->sleepCount += struct_->members.importers.count - 1;
		*yield = true;
		return false;
	}

	for (auto member : struct_->members.declarations) {
		if (member->flags & DECLARATION_MARKED_AS_USING) {
			if (!(member->flags & DECLARATION_TYPE_IS_READY)) {
				goToSleep(job, &member->sleepingOnMyType, "Using conversion waiting on declaration type");
				*yield = true;
				return false;
			}

			auto import = member;

			if (import->flags & DECLARATION_IMPORTED_BY_USING) {
				import = import->import;
			}

			if (getDeclarationType(import) == correct) {
				return true;
			}
			else if (given->type->flavor == TypeFlavor::POINTER && !(member->flags & DECLARATION_IS_CONSTANT) && getPointer(getDeclarationType(import)) == correct) {
				return true;
			}
		}
	}

	return false;
}

bool tryUsingConversion(SubJob *job, Type *correct, Expr **exprPointer) {
	auto given = *exprPointer;

	if (correct == given->type) return false;

	bool onlyConstants = false;
	auto struct_ = getExpressionNamespace(given, &onlyConstants, nullptr);


	if (!struct_)
		return false;

	if (struct_->members.importers.count) {
		goToSleep(job, &struct_->members.sleepingOnMe, "Using conversion waiting on importer");
		job->sleepCount += struct_->members.importers.count - 1;
		return true;
	}

	Declaration *found = nullptr;
	Type *type;
	for (auto member : struct_->members.declarations) {
		if (member->flags & DECLARATION_MARKED_AS_USING) {

			auto import = member;

			if (import->flags & DECLARATION_IMPORTED_BY_USING) {
				import = import->import;
			}

			if (!(import->flags & DECLARATION_TYPE_IS_READY)) {
				goToSleep(job, &member->sleepingOnMyType, "Using conversion waiting on declaration type");
				return true;
			}

			type = getDeclarationType(import);

			if (type == correct) {
				found = member;
				break;
			}
			else if (given->type->flavor == TypeFlavor::POINTER && !(member->flags & DECLARATION_IS_CONSTANT) && getPointer(type) == correct) {
				found = member;
				break;
			}
		}
	}

	if (!found)
		return false;

	auto identifier = INFER_NEW(ExprIdentifier);
	identifier->start = given->start;
	identifier->end = given->end;
	identifier->flavor = ExprFlavor::IDENTIFIER;
	identifier->name = found->name;
	identifier->resolveFrom = nullptr;
	identifier->enclosingScope = nullptr;
	identifier->structAccess = given;
	identifier->serial = 0;
	identifier->declaration = found;

	Expr *expr = identifier;

	if (type != correct) {
		assert(given->type->flavor == TypeFlavor::POINTER && !(found->flags & DECLARATION_IS_CONSTANT) && getPointer(type) == correct);

		auto unary = INFER_NEW(ExprUnaryOperator);

		unary->start = given->start;
		unary->end = given->end;
		unary->flavor = ExprFlavor::UNARY_OPERATOR;
		unary->op = TOKEN('*');
		unary->value = expr;
		unary->type = correct;

		expr = unary;
	}

	*exprPointer = expr;
	beginFlatten(job, exprPointer);

	addSubJob(job);

	return true;
}

bool tryAutoCast(SubJob *job, Expr **cast, Type *castTo, bool *yield) {
	*yield = false;
	auto castExpr = *cast;
	assert(castExpr->type == &TYPE_AUTO_CAST);
	assert(castExpr->flavor == ExprFlavor::BINARY_OPERATOR);

	auto autoCast = static_cast<ExprBinaryOperator *>(castExpr);
	assert(autoCast->op == TokenT::CAST);

	auto &castFrom = autoCast->right->type;

	assert(!autoCast->left);

	trySolidifyNumericLiteralToDefault(autoCast->right);


	assert(!(castTo->flags & TYPE_IS_INTERNAL));

	autoCast->type = castTo;
	autoCast->left = inferMakeTypeLiteral(autoCast->start, autoCast->end, castTo);

	if (autoCast->right->flags & EXPR_FUNCTION_IS_INSTRINSIC) {
		reportError(autoCast, "Error: Cannot cast an intrinsic function");
		return false;
	}

	if (castTo == castFrom)
		return true;

	if (tryUsingConversion(job, castTo, &autoCast->right)) {
		*yield = true;
		return false;
	} else {
		if (!isValidCast(job, castTo, autoCast->right->type, autoCast->flags, yield)) {
			
			if (!*yield) reportError(autoCast, "Error: Cannot cast from %.*s to %.*s", STRING_PRINTF(autoCast->right->type->name), STRING_PRINTF(castTo->name));
			return false;
		}

		if ((castFrom->flags & TYPE_ARRAY_IS_FIXED) && castTo->flavor == TypeFlavor::ARRAY && !(castTo->flags & TYPE_ARRAY_IS_FIXED)) {
			markDeclarationsAsPointedTo(autoCast->right);
		}

		addSizeDependency(job->sizeDependencies, castTo);
		doConstantCast(cast);
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

		if (left->type->flavor == TypeFlavor::ARRAY && 
			(left->type->flags & TYPE_ARRAY_IS_FIXED)) {
			auto arrayType = static_cast<TypeArray *>(left->type);

			if ((right->type->flags & TYPE_INTEGER_IS_SIGNED) && right->signedValue < 0) {
				reportError(binary, "Error: Out of bounds index of array, Length: %" PRIu64 ", Index: %" PRIi64, arrayType->count, right->signedValue);
				return false;
			}
			else if (right->unsignedValue >= arrayType->count) {
				reportError(binary, "Error: Out of bounds index of array, Length: %" PRIu64 ", Index: %" PRIu64, arrayType->count, right->unsignedValue);
				return false;
			}
			else {
				if (left->flavor == ExprFlavor::ARRAY_LITERAL) {
					auto array = static_cast<ExprArrayLiteral *>(binary->left);

					if (right->unsignedValue >= array->count) {
						assert(right->unsignedValue < arrayType->count);
						*exprPointer = array->values[array->count - 1];
					}
					else {
						*exprPointer = array->values[right->unsignedValue];
					}
				}
				else if (left->flavor == ExprFlavor::INT_LITERAL) {
					*exprPointer = createIntLiteral(binary->start, binary->end, arrayType->arrayOf, 0);
				}
			}
		}
		else if (left->type == &TYPE_STRING) {
			if (left->flavor == ExprFlavor::STRING_LITERAL) {
				if ((right->type->flags & TYPE_INTEGER_IS_SIGNED) && right->signedValue < 0) {
					reportError(binary, "Error: Out of bounds index of string constant, Length: %" PRIu64 ", Index: %" PRIi64, stringLeft->string.length, right->signedValue);
					return false;
				}
				else if (right->unsignedValue >= stringLeft->string.length) {
					reportError(binary, "Error: Out of bounds index of string constant, Length: %" PRIu64 ", Index: %" PRIu64, stringLeft->string.length, right->unsignedValue);
					return false;
				}
				else {
					*exprPointer = createIntLiteral(binary->start, binary->end, &TYPE_U8, stringLeft->string.characters[right->unsignedValue]);
				}
			}
			else if (left->flavor == ExprFlavor::INT_LITERAL) {
				reportError(binary, "Error: Out of bounds index of string constant, Length: 0, Index: %" PRIu64, right->unsignedValue);
				return false;
			}
		}

		break;
	}
	case TokenT::NOT_EQUAL: {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->unsignedValue != right->unsignedValue);
		}
		else if (left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->floatValue != right->floatValue);
		}
		else if (left->flavor == ExprFlavor::STRING_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(stringLeft->start, stringRight->end, &TYPE_BOOL, stringLeft->string != stringRight->string);
		}
		else if (left->flavor == ExprFlavor::TYPE_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->typeValue != right->typeValue);
		}
		break;
	}
	case TokenT::EQUAL: {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->unsignedValue == right->unsignedValue);
		}
		else if (left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->floatValue == right->floatValue);
		}
		else if (left->flavor == ExprFlavor::STRING_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(stringLeft->start, stringRight->end, &TYPE_BOOL, stringLeft->string == stringRight->string);
		}
		else if (left->flavor == ExprFlavor::TYPE_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->typeValue == right->typeValue);
		}
		break;
	}
	case TokenT::GREATER_EQUAL: {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->signedValue >= right->signedValue);
		}
		else if (left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->unsignedValue >= right->unsignedValue);
		}
		else if (left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->floatValue >= right->floatValue);
		}
		break;
	}
	case TokenT::LESS_EQUAL: {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->signedValue <= right->signedValue);
		}
		else if (left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->unsignedValue <= right->unsignedValue);
		}
		else if (left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->floatValue <= right->floatValue);
		}
		break;
	}
	case TOKEN('>'): {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->signedValue > right->signedValue);
		}
		else if (left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->unsignedValue > right->unsignedValue);
		}
		else if (left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->floatValue > right->floatValue);
		}
		break;
	}
	case TOKEN('<'): {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->signedValue < right->signedValue);
		}
		else if (left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->unsignedValue < right->unsignedValue);
		}
		else if (left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, &TYPE_BOOL, left->floatValue < right->floatValue);
		}
		break;
	}
	case TOKEN('+'): {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL) {
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
					goToSleep(job, &pointerTo->sleepingOnMe, "Constexpr + pointer math size not ready");
					*yield = true;
					return false;
				}

				*exprPointer = createIntLiteral(left->start, right->end, left->type, left->unsignedValue + right->unsignedValue * pointerTo->size);
			}
		}
		else if (left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createFloatLiteral(left->start, right->end, left->type, left->floatValue < right->floatValue);
		}
		break;
	}
	case TOKEN('-'): {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL) {
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
					goToSleep(job, &pointerTo->sleepingOnMe, "Constexpr - pointer size not ready");
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
		else if (left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createFloatLiteral(left->start, right->end, left->type, left->floatValue < right->floatValue);
		}
		break;
	}
	case TOKEN('&'): {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue & right->unsignedValue);
		}
		break;
	}
	case TOKEN('|'): {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue | right->unsignedValue);
		}
		break;
	}
	case TOKEN('^'): {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue ^ right->unsignedValue);
		}
		break;
	}
	case TokenT::SHIFT_LEFT: {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue << right->unsignedValue);
		}
		break;
	}
	case TokenT::SHIFT_RIGHT: {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->signedValue >> right->signedValue);
		}
		else if (left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue >> right->unsignedValue);
		}
		break;
	}
	case TOKEN('*'): {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL) {
			auto literal = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue * right->unsignedValue);

			if (left->type == &TYPE_SIGNED_INT_LITERAL && literal->signedValue >= 0) {
				literal->type = &TYPE_UNSIGNED_INT_LITERAL;
			}

			*exprPointer = literal;
		}
		else if (left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createFloatLiteral(left->start, right->end, left->type, left->floatValue * right->floatValue);
		}
		break;
	}
	case TOKEN('/'): {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
			auto literal = createInBoundsIntLiteral(left->start, right->end, left->type, left->signedValue / right->signedValue);

			if (left->type == &TYPE_SIGNED_INT_LITERAL && literal->signedValue >= 0) {
				literal->type = &TYPE_UNSIGNED_INT_LITERAL;
			}

			*exprPointer = literal;
		}
		else if (left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue / right->unsignedValue);
		}
		else if (left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createFloatLiteral(left->start, right->end, left->type, left->floatValue / right->floatValue);
		}
		break;
	}
	case TOKEN('%'): {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL && (left->type->flags & TYPE_INTEGER_IS_SIGNED)) {
			auto literal = createInBoundsIntLiteral(left->start, right->end, left->type, left->signedValue % right->signedValue);

			if (left->type == &TYPE_SIGNED_INT_LITERAL && literal->signedValue >= 0) {
				literal->type = &TYPE_UNSIGNED_INT_LITERAL;
			}

			*exprPointer = literal;
		}
		else if (left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue % right->unsignedValue);
		}
		else if (left->flavor == ExprFlavor::FLOAT_LITERAL) {
			*exprPointer = createFloatLiteral(left->start, right->end, left->type, fmod(left->floatValue, right->floatValue));
		}
		break;
	}
	case TokenT::LOGIC_AND: {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue != 0 && right->unsignedValue != 0);
		}
		break;
	}
	case TokenT::LOGIC_OR: {
		if (left->flavor != right->flavor)
			break;

		if (left->flavor == ExprFlavor::INT_LITERAL) {
			*exprPointer = createInBoundsIntLiteral(left->start, right->end, left->type, left->unsignedValue != 0 || right->unsignedValue != 0);
		}
		break;
	}
	}

	return true;
}

bool defaultValueIsZero(SubJob *job, Type *type, bool *yield) {
	*yield = false;

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

		if (struct_->flags & TYPE_STRUCT_IS_UNION) {
			bool zero = false;

			for (auto member : struct_->members.declarations) {
				if (member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING)) 
					continue;

				if (member->flags & DECLARATION_IS_UNINITIALIZED) 
					continue;

				if (!(member->flags & DECLARATION_VALUE_IS_READY)) {
					goToSleep(job, &member->sleepingOnMyValue, "Struct default value is zero member value not ready");

					*yield = true;
					continue;
				}

				assert(member->initialValue);

				auto initialValue = member->initialValue;

				if ((initialValue->flavor == ExprFlavor::INT_LITERAL || initialValue->flavor == ExprFlavor::FLOAT_LITERAL)
					&& static_cast<ExprLiteral *>(initialValue)->unsignedValue == 0) { // Check against unsignedValue even for float literals so if the user explicitly 
																		   // sets the default value to -0, they will actually get -0, not 0
				}
				else {
					return false;
				}
			}

			if (*yield)
				return false;

			return zero;
		}
		else {
			for (auto member : struct_->members.declarations) {
				if (member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING)) continue;

				if (member->flags & DECLARATION_IS_UNINITIALIZED)
					return false;

				if (!(member->flags & DECLARATION_VALUE_IS_READY)) {
					goToSleep(job, &member->sleepingOnMyValue, "Struct default value is zero member value not ready");

					*yield = true;
					continue;
				}

				assert(member->initialValue);

				auto initialValue = member->initialValue;

				if ((initialValue->flavor == ExprFlavor::INT_LITERAL || initialValue->flavor == ExprFlavor::FLOAT_LITERAL)
					&& static_cast<ExprLiteral *>(initialValue)->unsignedValue == 0) { // Check against unsignedValue even for float literals so if the user explicitly 
																							   // sets the default value to -0, they will actually get -0, not 0
				}
				else {
					return false;
				}
			}

			if (*yield)
				return false;

			return true;
		}
	}
	default: {
		return false;
	}
	}
}

Expr *getDefaultValue(SubJob *job, Type *type, bool *shouldYield);

static bool contextIsLocked;

Expr *createDefaultValue(SubJob *job, Type *type, bool *shouldYield) {
	*shouldYield = false;

	if (type == &TYPE_CONTEXT && !contextIsLocked) {
		goToSleep(job, &TYPE_CONTEXT.sleepingOnMe, "Context default value waiting for context lock");
		*shouldYield = true;
		return nullptr;
	}

	if (defaultValueIsZero(job, type, shouldYield)) {
		return createIntLiteral({}, {}, type, 0);
	}

	if (*shouldYield)
		return nullptr;


	switch (type->flavor) {
	case TypeFlavor::BOOL:
	case TypeFlavor::FLOAT:
	case TypeFlavor::FUNCTION:
	case TypeFlavor::INTEGER:
	case TypeFlavor::POINTER:
	case TypeFlavor::STRING: {
		assert(false); // This should be handled by the defaultValueIsZero check
		return nullptr;
	}
	case TypeFlavor::STRUCT: {
		u32 initializerCount = 0;

		auto block = &static_cast<TypeStruct *>(type)->members;

		for (auto declaration : block->declarations) {
			if (declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_UNINITIALIZED))
				continue;

			if (!(declaration->flags & DECLARATION_VALUE_IS_READY)) {
				*shouldYield = true;
				goToSleep(job, &declaration->sleepingOnMyValue, "Struct default value waiting for member value");
			}

			initializerCount++;
		}

		if (*shouldYield)
			return nullptr;

		auto literal = INFER_NEW(ExprStructLiteral);

		literal->flavor = ExprFlavor::STRUCT_LITERAL;
		literal->type = type;

		literal->typeValue = nullptr;

		literal->initializers.names = nullptr;
		literal->initializers.count = 0;
		literal->initializers.values = INFER_NEW_ARRAY(Expr *, initializerCount);
		literal->initializers.declarations = INFER_NEW_ARRAY(Declaration *, initializerCount);

		for (auto declaration : block->declarations) {
			if (declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_UNINITIALIZED))
				continue;

			literal->initializers.declarations[literal->initializers.count] = declaration;
			literal->initializers.values[literal->initializers.count] = declaration->initialValue;

			literal->initializers.count++;
		}

		assert(literal->initializers.count == initializerCount);

		return literal;
	}
	case TypeFlavor::ARRAY: {
		auto array = static_cast<TypeArray *>(type);
			
		Expr *value = getDefaultValue(job, array->arrayOf, shouldYield);

		if (*shouldYield) {
			return nullptr;
		}

		if (!value) {
			return nullptr;
		}

		auto defaults = INFER_NEW(ExprArrayLiteral);
		defaults->flavor = ExprFlavor::ARRAY_LITERAL;
		defaults->type = type;
		defaults->typeValue = nullptr;

		assert(type->flags &TYPE_ARRAY_IS_FIXED);

		defaults->count = 1;


		defaults->values = INFER_NEW(Expr *);

		defaults->values[0] = value;

		return defaults;
	}
	case TypeFlavor::ENUM: {
		assert(!(type->flags & TYPE_ENUM_IS_FLAGS)); // The default value for a flags enum is 0
		auto first = static_cast<TypeEnum *>(type)->members.declarations[0];

		if ((first->flags & DECLARATION_VALUE_IS_READY)) {
			return first->initialValue;
		}
		else {
			goToSleep(job, &first->sleepingOnMyValue, "Enum default value first value not ready");

			*shouldYield = true;
			return nullptr;
		}
	}
	case TypeFlavor::TYPE: {
		return inferMakeTypeLiteral({}, {}, &TYPE_VOID);
	}
	default: {
		assert(false);
		return nullptr;
	}
	}
}

Expr *getDefaultValue(SubJob *job, Type *type, bool *shouldYield) {
	*shouldYield = false;

	if (!type->defaultValue) {
		type->defaultValue = createDefaultValue(job, type, shouldYield);
	}

	return type->defaultValue;
}

struct OverloadIterator {
	Declaration *overload;
	u32 serial;

	Declaration *next() {
		while (overload) {
			Declaration *result = overload;
			overload = overload->nextOverload;

			if (result->flags & DECLARATION_IMPORTED_BY_USING) {
				if (result->enclosingScope->flavor == BlockFlavor::IMPERATIVE && result->serial >= serial) {
					continue;
				}

				result = result->import;
			}

			return result;
		}

		return nullptr;
	}

	operator bool() {
		return overload != nullptr;
	}
};

OverloadIterator overloads(ExprOverloadSet *overload) {
	return { overload->currentOverload, overload->identifier->serial };
}

bool checkOverloadSet(SubJob *job, Declaration *firstOverload, bool *yield) {
	PROFILE_FUNC();
	bool sleep = false;

	*yield = false;

	for (auto overload = firstOverload; overload; overload = overload->nextOverload) {
		auto declaration = overload;

		if (declaration->flags & DECLARATION_IMPORTED_BY_USING) {
			declaration = declaration->import;
		}

		assert(declaration->flags & DECLARATION_IS_CONSTANT);

		if (!(declaration->flags & DECLARATION_TYPE_IS_READY)) {
			goToSleep(job, &declaration->sleepingOnMyType, "Waiting for type of overloaded function");
			sleep = true;

			if (!declaration->inferJob)
				forceAddDeclaration(declaration);
		}
		else {
			auto type = getDeclarationType(declaration);

			if (type == &TYPE_POLYMORPHIC_FUNCTION) {
				reportError(declaration, "Error: Polymorphic functions cannot be overloaded");
				return false;
			}

			if (type->flavor != TypeFlavor::FUNCTION) {
				reportError(declaration, "Error: Only functions can be overloaded");
				return false;
			}
		}

		if (!(declaration->flags & DECLARATION_VALUE_IS_READY)) {
			goToSleep(job, &declaration->sleepingOnMyValue, "Waiting for value of overloaded function");
			sleep = true;

			if (!declaration->inferJob)
				forceAddDeclaration(declaration);
		}
		else {
			assert(declaration->initialValue);

			auto initialValue = declaration->initialValue;

			if (initialValue->flavor != ExprFlavor::FUNCTION) {
				reportError(declaration, "Error: Only functions can be overloaded");
				return false;
			}

			if (initialValue->flags & EXPR_FUNCTION_IS_C_CALL) {
				reportError(declaration, "Error: #c_call functions cannot be overloaded");
				return false;
			}
		}
	}

	if (sleep) {
		*yield = true;
		return false;
	}

	if (firstOverload->enclosingScope->module && !(firstOverload->flags & DECLARATION_OVERLOADS_LOCKED)) {
		job->sleepCount = 1;

#if BUILD_DEBUG
		job->sleepReason = "Waiting for overload set to be locked";
#endif

		overloadsWaitingForLock.add({ firstOverload, job });

		*yield = true;
		return false;
	}

	return true;
}

bool declarationIsOverloadSet(SubJob *job, Declaration *declaration, bool *yield) {
	*yield = false;

	if (!(declaration->flags & DECLARATION_IS_CONSTANT))
		return false;

	// Struct polymorph parameters are marked as both constant and argument
	// They cannot be overloaded
	if (declaration->flags & DECLARATION_IS_ARGUMENT)
		return false;

	if (declaration->nextOverload)
		return true;

	if (declaration->flags & DECLARATION_IMPORTED_BY_USING)
		declaration = declaration->import;

	if (!(declaration->flags & DECLARATION_VALUE_IS_READY)) {
		goToSleep(job, &declaration->sleepingOnMyValue, "Identifier sleeping to see if constant is potentially overloaded function");

		if (!declaration->inferJob)
			forceAddDeclaration(declaration);

		*yield = true;
		return false;
	}

	assert(declaration->initialValue);

	auto initialValue = declaration->initialValue;

	if (initialValue->flavor == ExprFlavor::FUNCTION && !(initialValue->flags & EXPR_FUNCTION_IS_C_CALL) && initialValue->type != &TYPE_POLYMORPHIC_FUNCTION)
		return true;

	return false;
}

bool nextOverloadSet(SubJob *job, ExprOverloadSet *overload, bool *yield) {
	*yield = false;

	if (overload->identifier->structAccess)
		return false;

	if (overload->block->module) {
		overload->block = overload->identifier->declaration->enclosingScope;
		overload->currentOverload = overload->identifier->declaration;
		return false;
	}

	while (overload->block->parentBlock) {
		overload->currentOverload = findDeclaration(overload->block->parentBlock, overload->identifier->name, yield, overload->identifier->serial);
		if (!overload->currentOverload) {
			if (*yield) {
				return false;
			}
			else {
				overload->block = overload->block->parentBlock;
				continue;
			}
		}

		if (!declarationIsOverloadSet(job, overload->currentOverload, yield)) {
			if (*yield) {
				return false;
			}
			else {
				overload->block = overload->block->parentBlock;
				continue;
			}
		}

		overload->block = overload->block->parentBlock;
		return true;
	}

	overload->currentOverload = findDeclarationNoYield(&overload->identifier->module->members, overload->identifier->name);

	if (!overload->currentOverload) {
		overload->currentOverload = overload->identifier->declaration;
		overload->block = overload->identifier->declaration->enclosingScope;
		
		return false;
	}

	if (!declarationIsOverloadSet(job, overload->currentOverload, yield)) {
		if (*yield) {
			return false;
		}
		else {
			overload->currentOverload = overload->identifier->declaration;
			overload->block = overload->identifier->declaration->enclosingScope;

			return false;
		}
	}

	overload->block = &overload->identifier->module->members;

	return true;
}

void collectAllOverloads(ArraySet<Declaration *> *overloadList, ExprOverloadSet *overload) {
	assert(overload->currentOverload == overload->identifier->declaration);
	assert(overload->block == overload->identifier->declaration->enclosingScope);

	bool yield;
	do {
		auto it = overloads(overload);

		while (auto current = it.next()) {
			overloadList->add(current);
		}
	} while (nextOverloadSet(nullptr, overload, &yield));

	assert(!yield);
}

bool inferOverloadSetForNonCall(SubJob *job, Type *correct, Expr *&given, bool *yield, bool silentCheck = false) {
	PROFILE_FUNC();
	assert(given->flavor == ExprFlavor::OVERLOAD_SET);
	auto overload = static_cast<ExprOverloadSet *>(given);

	Declaration *found = nullptr;

	do {
		auto it = overloads(overload);
		bool reported = false;
		
		if (!checkOverloadSet(job, it.overload, yield))
			return false;

		while (auto current = it.next()) {
			if (getDeclarationType(current) == correct) {
				if (!silentCheck && found) {
					if (!reported) {
						reportError(overload, "Error: Multiple overloads of %.*s match the type %.*s", STRING_PRINTF(overload->identifier->name), STRING_PRINTF(correct->name));
						reportError(found, "");
						reported = true;
					}

					reportError(current, "");
				}

				found = current;
			}
		}

		if (reported)
			return false;

		if (found)
			break;
	} while (nextOverloadSet(job, overload, yield));

	if (*yield)
		return false;

	if (found) {
		if (!silentCheck)
			given = found->initialValue;

		return true;
	}
	else {
		if (!silentCheck) {
			ArraySet<Declaration *> overloads;
			collectAllOverloads(&overloads, overload);

			if (overloads.size() == 1) {
				reportError(given, "Error: Could not convert from %.*s to  %.*s", STRING_PRINTF(getDeclarationType(overloads[0])->name), STRING_PRINTF(correct->name));
			}
			else {
				reportError(given, "Error: No overloads of %.*s match the type %.*s", STRING_PRINTF(overload->identifier->name), STRING_PRINTF(correct->name));

				for (auto overload : overloads) {
					reportError(overload, "");
				}
			}
		}

		return false;
	}
}

bool inferPolymorphicFunctionForNonCall(Type *correct, ExprFunction *given) {
	return false;
}

bool inferUnaryDotArrayLiteral(SubJob *job, Type *correct, Expr *&given, bool *yield) {
	*yield = false;
	auto array = static_cast<TypeArray *>(correct);

	if (array->flags & TYPE_ARRAY_IS_DYNAMIC) {
		reportError(given, "Error: Cannot have an array literal for a dynamic array");
		return false;
	}
	if (given->flavor == ExprFlavor::INT_LITERAL) {
		assert(!(array->flags & TYPE_ARRAY_IS_FIXED));
		assert(static_cast<ExprLiteral *>(given)->unsignedValue == 0);

		addSizeDependency(job->sizeDependencies, correct);

		given->type = correct;
		return true;
	}

	auto literal = static_cast<ExprArrayLiteral *>(given);

	if (array->flags & TYPE_ARRAY_IS_FIXED) {
		if (literal->count != array->count) {
			reportError(literal, "Error: Incorrect number of values for array literal (Wanted %u, Given %u)", array->count, literal->count);
			return false;
		}
	}

	addSizeJobIfNeeded(correct);
	addSizeDependency(job->sizeDependencies, correct);

	literal->type = correct;

	*yield = true;
	beginFlatten(job, &given);
	subJobs.add(job);

	return false;
}

bool inferUnaryDotStructLiteral(SubJob *job, Type *correct, Expr *&given) {
	auto literal = static_cast<ExprStructLiteral *>(given);

	assert(correct->flavor == TypeFlavor::ARRAY || correct->flavor == TypeFlavor::STRING || correct->flavor == TypeFlavor::STRUCT);

	if (correct->flags & TYPE_ARRAY_IS_FIXED) {
		reportError(given, "Error: Cannot have a struct literal for a fixed array");
		return false;
	}


	literal->type = correct;

	beginFlatten(job, &given);
	subJobs.add(job);

	return true;
}

bool checkExpressionIsRuntimeValid(Expr *expr) {
	if (!expr)
		return true;

	if (expr->type == &TYPE_VOID) {
		reportError(expr, "Error: Cannot operate on a value of type void");
		return false;
	}
	else if (expr->type->flavor == TypeFlavor::MODULE) {
		reportError(expr, "Error: Cannot operate on a module");
		return false;
	}
	else if (expr->flags & EXPR_FUNCTION_IS_INSTRINSIC) {
		reportError(expr, "Error: Cannot operate on an intrinsic function");
		return false;
	}
	else if (expr->type->flags & TYPE_IS_POLYMORPHIC) {
		reportError(expr, "Error: Cannot operate on a polymorphic type");
		return false;
	}

	return true;
}

bool assignOp(SubJob *job, Expr *location, Type *correct, Expr *&given, bool *yield) {
	*yield = false;

	if (!checkExpressionIsRuntimeValid(given))
		return false;

	if (correct != given->type) {
		if (correct->flavor == given->type->flavor) {
			switch (correct->flavor) {
			case TypeFlavor::BOOL:
			case TypeFlavor::STRING:
			case TypeFlavor::TYPE: {
				break;
			}
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
			case TypeFlavor::FLOAT: {
				if (given->type == &TYPE_FLOAT_LITERAL) {
					given->type = correct;
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
						if (tryUsingConversion(job, correct, &given)) {
							*yield = true;
							return false;
						}
						else {
							reportError(location, "Error: Cannot convert from %.*s to %.*s", STRING_PRINTF(given->type->name), STRING_PRINTF(correct->name));
							return false;
						}
					}
				}

				break;
			}
			case TypeFlavor::STRUCT: {
				if (tryUsingConversion(job, correct, &given)) {
					*yield = true;
					return false;
				}
				else {
					if (correct == TYPE_ANY && given->type != TYPE_ANY) {
						insertImplicitCast(job->sizeDependencies, &given, TYPE_ANY);
						return true;
					}

					if (correct != given->type) {
						reportError(location, "Error: Cannot convert from %.*s to %.*s", STRING_PRINTF(given->type->name), STRING_PRINTF(correct->name));
						return false;
					}
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
						reportError(location, "Error: Cannot cast from %.*s to %.*s", STRING_PRINTF(static_cast<ExprBinaryOperator *>(given)->right->type->name), 
							STRING_PRINTF(correct->name));
					return false;
				}
			}
			else if (correct->flavor == TypeFlavor::FLOAT && given->type == &TYPE_UNSIGNED_INT_LITERAL) {
				auto literal = static_cast<ExprLiteral *>(given);

				literal->flavor = ExprFlavor::FLOAT_LITERAL;
				literal->floatValue = static_cast<double>(literal->unsignedValue);
				literal->type = correct;
			}
			else if (correct->flavor == TypeFlavor::ENUM && (correct->flags & TYPE_ENUM_IS_FLAGS) &&
				given->type->flavor == TypeFlavor::INTEGER && given->flavor == ExprFlavor::INT_LITERAL && static_cast<ExprLiteral *>(given)->unsignedValue == 0) {
				insertImplicitCast(job->sizeDependencies, &given, correct);
			}
			else if (correct->flavor == TypeFlavor::FUNCTION && given->type == &TYPE_OVERLOAD_SET) {
				if (!inferOverloadSetForNonCall(job, correct, given, yield)) {
					return false;
				}
			}
			else if (correct == TYPE_U8_POINTER && given->flavor == ExprFlavor::STRING_LITERAL) {
				auto identifier = INFER_NEW(ExprIdentifier);
				identifier->start = given->start;
				identifier->end = given->end;
				identifier->flavor = ExprFlavor::IDENTIFIER;
				identifier->name = "data";
				identifier->resolveFrom = nullptr;
				identifier->enclosingScope = nullptr;
				identifier->structAccess = given;
				identifier->serial = 0;
				identifier->declaration = TYPE_STRING.members.declarations[0];
				identifier->type = correct;

				given = identifier;
			}
			else if (correct->flavor == TypeFlavor::FLOAT && given->type == &TYPE_SIGNED_INT_LITERAL) {
				auto literal = static_cast<ExprLiteral *>(given);

				literal->flavor = ExprFlavor::FLOAT_LITERAL;
				literal->floatValue = static_cast<double>(literal->signedValue);
				literal->type = correct;
			}
			else if (correct == TYPE_VOID_POINTER && given->type->flavor == TypeFlavor::FUNCTION) {
				insertImplicitCast(job->sizeDependencies, &given, correct);
			}
			else if (given->type == TYPE_VOID_POINTER && correct->flavor == TypeFlavor::FUNCTION) {
				insertImplicitCast(job->sizeDependencies, &given, correct);
			}
			else if (correct->flavor == TypeFlavor::ENUM && given->type == &TYPE_UNARY_DOT) {
				if (!inferUnaryDot(job, static_cast<TypeEnum *>(correct), &given, yield)) {
					return false;
				}
			}
			else if (given->type == &TYPE_ARRAY_LITERAL && correct->flavor == TypeFlavor::ARRAY) {
				return inferUnaryDotArrayLiteral(job, correct, given, yield);
			}
			else if (given->type == &TYPE_STRUCT_LITERAL && (correct->flavor == TypeFlavor::ARRAY || correct->flavor == TypeFlavor::STRING || correct->flavor == TypeFlavor::STRUCT)) {
				*yield = inferUnaryDotStructLiteral(job, correct, given);
				return false;
			}


			if (correct != given->type) {
				if (tryUsingConversion(job, correct, &given)) {
					*yield = true;
					return false;
				} else {
					if (correct == TYPE_ANY && given->type != TYPE_ANY && given->type->flavor != TypeFlavor::AUTO_CAST) {
						trySolidifyNumericLiteralToDefault(given);
						insertImplicitCast(job->sizeDependencies, &given, TYPE_ANY);
						return true;
					}

					reportError(location, "Error: Cannot convert from %.*s to %.*s", STRING_PRINTF(given->type->name), STRING_PRINTF(correct->name));
					return false;
				}
			}
		}
	}

	return true;
}

namespace ConversionCost {
	enum Conversion : u8 {
		EXACT_MATCH,
		EXTEND_INT_TO_INT8, // Used for int literals that fit in s8/u8
		EXTEND_INT_TO_INT16,
		EXTEND_INT_TO_INT32,
		EXTEND_INT_TO_INT64,
		INT_LITERAL_TO_FLOAT,  // Used when an int literal converts to float (normally int->float is illegal)
		CONVERSION,
		ANY,

		MAX_VALID,

		CANNOT_CONVERT
	};
};

typedef ConversionCost::Conversion Conversion;

Conversion getIntExtendCost(Type *type) {
	switch (type->size) {
	case 1:  return ConversionCost::EXTEND_INT_TO_INT8;
	case 2:  return ConversionCost::EXTEND_INT_TO_INT16;
	case 4:  return ConversionCost::EXTEND_INT_TO_INT32;
	case 8:  return ConversionCost::EXTEND_INT_TO_INT64;
	default: assert(false); return ConversionCost::CANNOT_CONVERT;
	}
}

Conversion getConversionCost(SubJob *job, Type *correct, Expr *given, bool *yield) {
	*yield = false;
	if (correct == given->type)
		return ConversionCost::EXACT_MATCH;

	if (correct->flavor == given->type->flavor) {
		switch (correct->flavor) {
		case TypeFlavor::BOOL:
		case TypeFlavor::STRING:
		case TypeFlavor::TYPE: {
			assert(false); //bool, type and string should be handled by correct == given->type
			return ConversionCost::CANNOT_CONVERT;
		}
		case TypeFlavor::ARRAY: {
			// Array conversions must be [N] -> [] or [..] to []
			if (!(given->type->flags & (TYPE_ARRAY_IS_DYNAMIC | TYPE_ARRAY_IS_FIXED)))
				return ConversionCost::CANNOT_CONVERT;

			if (correct->flags & (TYPE_ARRAY_IS_DYNAMIC | TYPE_ARRAY_IS_FIXED))
				return ConversionCost::CANNOT_CONVERT;

			if (static_cast<TypeArray *>(given->type)->arrayOf != static_cast<TypeArray *>(correct)->arrayOf)
				return ConversionCost::CANNOT_CONVERT;
			

			return ConversionCost::CONVERSION;
		}
		case TypeFlavor::FLOAT: {
			if (given->type == &TYPE_FLOAT_LITERAL)
				return ConversionCost::EXACT_MATCH;
			else
				return ConversionCost::CANNOT_CONVERT;
		}
		case TypeFlavor::FUNCTION: {
			// Allow convsersions like (int..) -> void to ([]int) -> void

			auto a = static_cast<TypeFunction *>(correct);
			auto b = static_cast<TypeFunction *>(given->type);

			if (a->isVarargs == b->isVarargs || a->argumentCount != b->argumentCount || a->returnCount != b->returnCount)
				return ConversionCost::CANNOT_CONVERT;

			if ((a->flags & TYPE_FUNCTION_IS_C_CALL) != (b->flags & TYPE_FUNCTION_IS_C_CALL))
				return ConversionCost::CANNOT_CONVERT;

			for (u64 i = 0; i < a->argumentCount; i++) {
				if (a->argumentTypes[i] != b->argumentTypes[i]) return ConversionCost::CANNOT_CONVERT;
				if (a->returnTypes[i] != b->returnTypes[i]) return ConversionCost::CANNOT_CONVERT;
			}

			return ConversionCost::CONVERSION;
		}
		case TypeFlavor::INTEGER: {
			if ((correct->flags & TYPE_INTEGER_IS_SIGNED) == (given->type->flags & TYPE_INTEGER_IS_SIGNED)) {
				if (given->type == &TYPE_UNSIGNED_INT_LITERAL || given->type == &TYPE_SIGNED_INT_LITERAL) {
					if (!boundsCheckImplicitConversion(nullptr, correct, static_cast<ExprLiteral *>(given)))
						return ConversionCost::CANNOT_CONVERT;
				}
				else if (correct->size < given->type->size) {
					return ConversionCost::CANNOT_CONVERT;
				}
			}
			else {
				if (given->type != &TYPE_UNSIGNED_INT_LITERAL)
					return ConversionCost::CANNOT_CONVERT;

				if (!boundsCheckImplicitConversion(nullptr, correct, static_cast<ExprLiteral *>(given)))
					return ConversionCost::CANNOT_CONVERT;
			}

			return getIntExtendCost(correct);
		}
		case TypeFlavor::POINTER: {
			if (correct == TYPE_VOID_POINTER)
				return ConversionCost::CONVERSION;

			if (given->type == TYPE_VOID_POINTER)
				return ConversionCost::CONVERSION;

			auto givenPointer = static_cast<TypePointer *>(given->type)->pointerTo;
			auto correctPointer = static_cast<TypePointer *>(correct)->pointerTo;
				
			if (givenPointer->flavor == TypeFlavor::ARRAY && correctPointer->flavor == TypeFlavor::ARRAY &&
				(givenPointer->flags & TYPE_ARRAY_IS_DYNAMIC) && !(correctPointer->flags & (TYPE_ARRAY_IS_FIXED | TYPE_ARRAY_IS_DYNAMIC))) {
				if (static_cast<TypeArray *>(givenPointer)->arrayOf == static_cast<TypeArray *>(correctPointer)->arrayOf) {
					return ConversionCost::CONVERSION;
				}
			}
				
			if (usingConversionIsValid(job, correct, given, yield))
				return ConversionCost::CONVERSION;

			return ConversionCost::CANNOT_CONVERT;
		}
		case TypeFlavor::STRUCT: {
			if (usingConversionIsValid(job, correct, given, yield))
				return ConversionCost::CONVERSION;
			else if (*yield)
				return ConversionCost::CANNOT_CONVERT;

			if (correct == TYPE_ANY)
				return ConversionCost::ANY;

			return ConversionCost::CANNOT_CONVERT;
		}
		case TypeFlavor::ENUM: {
			return ConversionCost::CANNOT_CONVERT; // Legal case handled by correct == given->type
		}
		default:
			assert(false);
			return ConversionCost::CANNOT_CONVERT;
		}
	}
	else {
		if (given->type == &TYPE_AUTO_CAST) {
			auto right = static_cast<ExprBinaryOperator *>(given)->right;

			if (right->type == correct)
				return ConversionCost::EXACT_MATCH;

			if (usingConversionIsValid(job, correct, right, yield))
				return ConversionCost::EXACT_MATCH;
			else if (*yield)
				return ConversionCost::CANNOT_CONVERT;


			if (isValidCast(correct, getTypeForExpr(right)))
				return ConversionCost::EXACT_MATCH;
			else
				return ConversionCost::CANNOT_CONVERT;
		}
		
		if (correct->flavor == TypeFlavor::FLOAT && (given->type == &TYPE_UNSIGNED_INT_LITERAL || given->type == &TYPE_SIGNED_INT_LITERAL)) {
			return ConversionCost::INT_LITERAL_TO_FLOAT;
		}

		if (correct->flavor == TypeFlavor::ENUM && (correct->flags & TYPE_ENUM_IS_FLAGS) &&
			given->type->flavor == TypeFlavor::INTEGER && given->flavor == ExprFlavor::INT_LITERAL && static_cast<ExprLiteral *>(given)->unsignedValue == 0) {
			return ConversionCost::CONVERSION;
		}

		if (correct->flavor == TypeFlavor::FUNCTION && given->type == &TYPE_OVERLOAD_SET) {
			if (!inferOverloadSetForNonCall(job, correct, given, yield, true)) {
				return ConversionCost::CANNOT_CONVERT;
			}

			return ConversionCost::EXACT_MATCH;
		}

		if (correct->flavor == TypeFlavor::POINTER && static_cast<TypePointer *>(correct)->pointerTo == &TYPE_U8 && given->flavor == ExprFlavor::STRING_LITERAL) {
			return ConversionCost::CONVERSION;
		}
		
		if (given->type->flavor == TypeFlavor::FUNCTION && correct == TYPE_VOID_POINTER) {
			return ConversionCost::CONVERSION;
		}

		if (correct->flavor == TypeFlavor::FUNCTION && given->type == TYPE_VOID_POINTER) {
			return ConversionCost::CONVERSION;
		}
		
		if (correct->flavor == TypeFlavor::ENUM && given->type == &TYPE_UNARY_DOT) {
			if (!inferUnaryDot(job, static_cast<TypeEnum *>(correct), &given, yield, true)) {
				return ConversionCost::CANNOT_CONVERT;
			}

			return ConversionCost::EXACT_MATCH;
		}

		if (correct->flavor == TypeFlavor::ARRAY && given->type == &TYPE_ARRAY_LITERAL) {
			auto literal = static_cast<ExprArrayLiteral *>(given);

			auto array = static_cast<TypeArray *>(correct);

			if (array->flags & TYPE_ARRAY_IS_DYNAMIC) {
				return ConversionCost::CANNOT_CONVERT;
			}

			if (array->flags & TYPE_ARRAY_IS_FIXED) {
				if (literal->count != array->count) {
					return ConversionCost::CANNOT_CONVERT;
				}

				return ConversionCost::EXACT_MATCH;
			}
			else {
				return ConversionCost::CONVERSION;
			}
		}


		if ((correct->flavor == TypeFlavor::ARRAY || correct->flavor == TypeFlavor::STRING || correct->flavor == TypeFlavor::STRUCT) && given->type == &TYPE_STRUCT_LITERAL) {
			if (correct->flags & TYPE_ARRAY_IS_FIXED)
				return ConversionCost::CANNOT_CONVERT;

			auto block = &static_cast<TypeStruct *>(correct)->members;

			for (auto importer : block->importers) {
				if (importer->import->flavor == ExprFlavor::STATIC_IF) {
					goToSleep(job, &block->sleepingOnMe, "Struct literal sleeping on static if");
					*yield = true;
					return ConversionCost::CANNOT_CONVERT;
				}
			}

			auto literal = static_cast<ExprStructLiteral *>(given);

			if (literal->initializers.names) {
				for (u32 i = 0; i < literal->initializers.count; i++) {
					if (literal->initializers.names[i].length && !findDeclarationNoYield(block, literal->initializers.names[i]))
						return ConversionCost::CANNOT_CONVERT;
				}
			}

			return ConversionCost::EXACT_MATCH;
		}

		if (usingConversionIsValid(job, correct, given, yield))
			return ConversionCost::CONVERSION;
		else if (*yield)
				return ConversionCost::CANNOT_CONVERT;

		if (correct == TYPE_ANY && given->type->flavor != TypeFlavor::AUTO_CAST)
			return ConversionCost::ANY;

		return ConversionCost::CANNOT_CONVERT;
	}
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
	else if (expr->flavor == ExprFlavor::CONTEXT) {
		return true;
	}
	else {
		return false;
	}
}

bool sortArguments(SubJob *job, Arguments *arguments, Block *block, const char *message, Expr *callLocation, Expr *functionLocation) {
	String functionName = "function";

	if (functionLocation->valueOfDeclaration) {
		functionName = functionLocation->valueOfDeclaration->name;
	}


	if (!arguments->names && arguments->count != block->declarations.count && !(functionLocation->flags & EXPR_FUNCTION_HAS_VARARGS)) {
		if (arguments->count > block->declarations.count) {
			reportError(callLocation, "Error: Too many %ss for %.*s (Expected: %" PRIu32 ", Given: %" PRIu32 ")",
				message, STRING_PRINTF(functionName), block->declarations.count, arguments->count);
			return false;
		}
		else {
			for (u32 i = arguments->count; i < block->declarations.count; i++) {
				if (!block->declarations[i]->initialValue && !(block->declarations[i]->flags & DECLARATION_IS_EXPLICIT_DEFAULT)) {
					reportError(callLocation, "Error: Too few %ss for %.*s (Expected: %" PRIu32 ", Given: %" PRIu32 ")",
						message, STRING_PRINTF(functionName), block->declarations.count, arguments->count);
					return false;
				}
			}
		}
	}

#if BUILD_DEBUG
	for (u32 i = 0; i < block->declarations.count; i++) {
		assert(block->declarations[i]->serial == i);
	}
#endif

	bool hadNamed = false;

	if (arguments->names || arguments->count != block->declarations.count || (functionLocation->flags & EXPR_FUNCTION_HAS_VARARGS)) {
		Expr **sortedArguments = INFER_NEW_ARRAY(Expr *, block->declarations.count) {};

		for (u32 i = 0; i < arguments->count; i++) {
			Declaration *argument;

			u32 argIndex = i;
			if (arguments->names && arguments->names[i].length) {
				hadNamed = true;
				argument = findDeclarationNoYield(block, arguments->names[i]);

				if (!argument) {
					reportError(arguments->values[i], "Error: %.*s does not have a %s called %.*s", STRING_PRINTF(functionName), message, STRING_PRINTF(arguments->names[i]));
					return false;
				}
			}
			else if (hadNamed) {
				reportError(arguments->values[i], "Error: Cannot have unnamed arguments after named arguments");
				return false;
			}
			else {
				argument = block->declarations[i];
			}

			argIndex = argument->serial;
			assert(!(argument->flags & DECLARATION_IMPORTED_BY_USING));

			if (sortedArguments[argIndex]) {
				reportError(arguments->values[i], "Error: %s %.*s was supplied twice", message, STRING_PRINTF(block->declarations[argIndex]->name));
				reportError(sortedArguments[argIndex], "   ..: It was previously given here");
				return false;
			}

			if ((argument->flags & DECLARATION_IS_VARARGS) && !(arguments->values[i]->flags & EXPR_IS_SPREAD)) {
				Array<Expr *> varargs;

				auto array = INFER_NEW(ExprArrayLiteral);
				array->flavor = ExprFlavor::ARRAY_LITERAL;
				array->start = arguments->values[i]->start;
				array->flags |= EXPR_IS_SPREAD;
				array->typeValue = nullptr;

				for (; i < arguments->count; i++) {
					if (arguments->names && arguments->names[i].length) {
						i--;
						break;
					}

					varargs.add(arguments->values[i]);
					array->end = arguments->values[i]->end;
				}

				

				array->values = varargs.storage;
				array->count = varargs.count;
				sortedArguments[argIndex] = array;

				if (argument->flags & DECLARATION_TYPE_POLYMORPHIC)  {
					array->type = getStaticArray(arguments->values[i]->type, array->count);

					addSizeJobIfNeeded(array->type);

					addSizeDependency(job->sizeDependencies, array->type);

					beginFlatten(job, &sortedArguments[argIndex]);
				}
				else {
					array->type = &TYPE_ARRAY_LITERAL;
				}

			}
			else {
				if (!(argument->flags & DECLARATION_IS_VARARGS) && (arguments->values[i]->flags & EXPR_IS_SPREAD)) {
					reportError(arguments->values[i], "Error: Cannot spread into a non varargs argument");
					return false;
				}
				sortedArguments[argIndex] = arguments->values[i];
			}
		}

		bool failed = false;

		for (u32 i = 0; i < block->declarations.count; i++) {
			if (!sortedArguments[i]) {
				auto argument = block->declarations[i];

				if (argument->initialValue || (argument->flags & DECLARATION_IS_EXPLICIT_DEFAULT)) {
					// The actual value will be filled in during inferArguments or doPolymorphMatching
				}
				else if (argument->flags & DECLARATION_IS_VARARGS) {
					if (argument->flags & DECLARATION_TYPE_POLYMORPHIC) {
						reportError(callLocation, "Error: Varargs argument '%.*s' of polymorphic type must have at least one value", STRING_PRINTF(argument->name));
						reportError(argument, "   ..: Here is the %s", message);
						failed = true;
					}

					auto literal = createIntLiteral(argument->start, argument->end, &TYPE_ARRAY_LITERAL, 0);
					literal->flags |= EXPR_IS_SPREAD;
					sortedArguments[i] = literal;
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

	return true;
}

bool inferArguments(SubJob *job, Arguments *arguments, Block *block, bool *yield) {
	*yield = false;

	for (u32 i = 0; i < arguments->count; i++) {
		auto argument = block->declarations[i];

		if (!arguments->values[i]) {
			assert(argument->initialValue);

			if (argument->flags & DECLARATION_VALUE_IS_READY) {
				assert((argument->flags & DECLARATION_IS_RUN_RETURN) || isLiteral(argument->initialValue));

				arguments->values[i] = argument->initialValue;
				addSizeDependency(job->sizeDependencies, argument->initialValue->type);
			}
			else {
				*yield = true;
				goToSleep(job, &argument->sleepingOnMyValue, "Default argument value not ready");
				return false;
			}
		}

		Type *correct = getDeclarationType(argument);

		if (!assignOp(job, arguments->values[i], correct, arguments->values[i], yield)) { // @Incomplete: Give function call specific error messages instead of general type conversion errors
			return false;
		}
	}

	return true;
}

bool checkArgumentsForOverload(SubJob *job, Arguments *arguments, Block *block, ExprFunction *function, u32 *conversions, bool *yield) {
	*yield = false;

#if BUILD_DEBUG
	for (u32 i = 0; i < block->declarations.count; i++) {
		assert(block->declarations[i]->serial == i);
	}
#endif

	auto sortedArguments = new bool[block->declarations.count]{};

	at_exit{
		delete[] sortedArguments;
	};

	for (u32 i = 0; i < arguments->count; i++) {
		Declaration *argument;

		u32 argIndex = i;
		if (arguments->names && arguments->names[i].length) {
			argument = findDeclarationNoYield(block, arguments->names[i]);
		}
		else {
			if (i >= block->declarations.count) {
				return false;
			}

			argument = block->declarations[i];
		}

		if (!argument) {
			return false;
		}

		argIndex = argument->serial;
		assert(!(argument->flags & DECLARATION_IMPORTED_BY_USING));

		if ((argument->flags & DECLARATION_IS_VARARGS) && !(arguments->values[i]->flags & EXPR_IS_SPREAD)) {
			Type *varargsType = static_cast<TypeArray *>(getDeclarationType(argument))->arrayOf;

			for (; i < arguments->count; i++) {
				if (arguments->names && arguments->names[i].length) {
					i--; // i will be incremented in the outer loop
					break;
				}

				auto cost = getConversionCost(job, varargsType, arguments->values[i], yield);

				if (cost == ConversionCost::CANNOT_CONVERT) {
					return false;
				}

				conversions[cost]++;
			}

			sortedArguments[argIndex] = true;
		}
		else {
			if (!(argument->flags & DECLARATION_IS_VARARGS) && (arguments->values[i]->flags & EXPR_IS_SPREAD)) {
				return false;
			}

			auto cost = getConversionCost(job, getDeclarationType(argument), arguments->values[i], yield);

			if (cost == ConversionCost::CANNOT_CONVERT) {
				return false;
			}

			conversions[cost]++;

			sortedArguments[argIndex] = true;
		}
	}

	for (u32 i = 0; i < block->declarations.count; i++) {
		if (sortedArguments[i])
			continue;

		auto argument = block->declarations[i];

		if (!argument->initialValue && !(argument->flags & (DECLARATION_IS_VARARGS | DECLARATION_IS_EXPLICIT_DEFAULT))) {
			return false;
		}
	}

	return true;
}

bool matchPolymorphArgument(Expr *polymorphExpression, Type *type, Expr *location, bool *yield, bool silent) {
	if (polymorphExpression->flavor == ExprFlavor::IDENTIFIER && (polymorphExpression->flags & EXPR_IDENTIER_DEFINES_POLYMORPH_VARIABLE)) {
		auto identifier = static_cast<ExprIdentifier *>(polymorphExpression);

		assert(identifier->declaration);
		assert(identifier->declaration->flags & DECLARATION_IS_CONSTANT);
		assert(identifier->declaration->enclosingScope->flavor == BlockFlavor::CONSTANTS);

		if (!silent) {
			assert(identifier->declaration->initialValue && identifier->declaration->initialValue->flavor == ExprFlavor::TYPE_LITERAL);
			static_cast<ExprLiteral *>(identifier->declaration->initialValue)->typeValue = type;
		}
		//reportInfo("%.*s Polymorph type matched %.*s", STRING_PRINTF(identifier->name), STRING_PRINTF(type->name));
		return true;
	}
	else if (polymorphExpression->flavor == ExprFlavor::UNARY_OPERATOR) {
		auto unary = static_cast<ExprUnaryOperator *>(polymorphExpression);

		if (unary->op == TOKEN('*')) {
			if (type->flavor != TypeFlavor::POINTER) {
				if (!silent) {
					reportError(location, "Error: Could not match polymorph pattern, %.*s is not a pointer", STRING_PRINTF(type->name));
					reportError(polymorphExpression, "   ..: Here is the polymorph argument");
				}
				return false;
			}

			return matchPolymorphArgument(unary->value, static_cast<TypePointer *>(type)->pointerTo, location, yield, silent);
		}

	}
	else if (polymorphExpression->flavor == ExprFlavor::BINARY_OPERATOR) {
		auto binary = static_cast<ExprBinaryOperator *>(polymorphExpression);

		if (binary->op == TokenT::ARRAY_TYPE) {
			if (type->flavor != TypeFlavor::ARRAY) {
				if (!silent) {
					reportError(location, "Error: Could not match polymorph pattern, %.*s is not an array", STRING_PRINTF(type->name));
					reportError(polymorphExpression, "   ..: Here is the polymorph argument");
				}
				return false;
			}

			auto array = static_cast<TypeArray *>(type);

			if (binary->left) {
				if (!(array->flags & TYPE_ARRAY_IS_FIXED)) {
					if (!silent) {
						reportError(location, "Error: Could not match polymorph pattern, %.*s is not a fixed array", STRING_PRINTF(type->name));
						reportError(polymorphExpression, "   ..: Here is the polymorph argument");
					}
					return false;
				}
			}
			else if (binary->flags & EXPR_ARRAY_IS_DYNAMIC) {
				if (!(array->flags & TYPE_ARRAY_IS_DYNAMIC)) {
					if (!silent) {
						reportError(location, "Error: Could not match polymorph pattern, %.*s is not a dynamic array", STRING_PRINTF(type->name));
						reportError(polymorphExpression, "   ..: Here is the polymorph argument");
					}
					return false;
				}
			}

			return matchPolymorphArgument(binary->right, array->arrayOf, location, yield, silent);
		}
	}
	else if (polymorphExpression->flavor == ExprFlavor::FUNCTION_PROTOTYPE) {
		auto function = static_cast<ExprFunction *>(polymorphExpression);

		if (type->flavor != TypeFlavor::FUNCTION) {
			if (!silent) {
				reportError(location, "Error: Could not match polymorph pattern, %.*s is not a function", STRING_PRINTF(type->name));
				reportError(polymorphExpression, "   ..: Here is the polymorph argument");
			}
			return false;
		}

		auto functionType = static_cast<TypeFunction *>(type);

		if (function->arguments.declarations.count != functionType->argumentCount) {
			if (!silent) {
				reportError(location, "Error: Could not match polymorph pattern, function argument counts mismatched for %.*s (%u arguments)",
					STRING_PRINTF(functionType->name), functionType->argumentCount);
				reportError(polymorphExpression, "   ..: Wanted %u arguments", function->arguments.declarations.count);
			}
			return false;
		}

		if (function->returns.declarations.count != functionType->returnCount) {
			if (!silent) {
				reportError(location, "Error: Could not match polymorph pattern, function return counts mismatched for %.*s (%u returns)",
					STRING_PRINTF(functionType->name), functionType->returnCount);
				reportError(polymorphExpression, "   ..: Wanted %u returns", function->returns.declarations.count);
			}
			return false;
		}

		if ((function->flags & EXPR_FUNCTION_HAS_VARARGS) && !functionType->isVarargs) {
			if (!silent) {
				reportError(location, "Error: Could not match polymorph pattern, wanted a varargs function but given %.*s",
					STRING_PRINTF(functionType->name));
				reportError(polymorphExpression, "   ..: Here is the polymorph argument");
			}
			return false;
		}

		if (((function->flags & EXPR_FUNCTION_IS_C_CALL) != 0) != ((functionType->flags & TYPE_FUNCTION_IS_C_CALL) != 0)) {
			if (!silent) {
				reportError(location, "Error: Could not match polymorph pattern #c_call specifiers do not match for function type %.*s",
					STRING_PRINTF(functionType->name));
				reportError(polymorphExpression, "   ..: Here is the polymorph argument");
			}
			return false;
		}

		for (auto argument : function->arguments.declarations) {
			if (argument->flags & DECLARATION_TYPE_POLYMORPHIC) {
				assert(argument->type);

				if (!matchPolymorphArgument(argument->type, functionType->argumentTypes[argument->serial], location, yield, silent))
					return false;
			}
		}

		for (auto return_ : function->returns.declarations) {
			if (return_->flags & DECLARATION_TYPE_POLYMORPHIC) {
				assert(return_->type);

				if (!matchPolymorphArgument(return_->type, functionType->returnTypes[return_->serial], location, yield, silent))
					return false;
			}
		}

		return true;
	}
	else if (polymorphExpression->flavor == ExprFlavor::FUNCTION_CALL) {
		auto call = static_cast<ExprFunctionCall *>(polymorphExpression);

		if (type->flags & TYPE_IS_POLYMORPHIC) {
			if (!silent) {
				reportError(location, "Error: Could not match polymorph pattern, given argument %.*s is a polymorphic struct", STRING_PRINTF(type->name));
				reportError(polymorphExpression, "   ..: Here is is the polymorph argument");
			}
			return false;
		}
		if (type->flavor != TypeFlavor::STRUCT) {
			if (!silent) {
				reportError(location, "Error: Could not match polymorph pattern, wanted a polymorphic struct but given a %.*s", STRING_PRINTF(type->name));
				reportError(polymorphExpression, "   ..: Here is is the polymorph argument");
			}
			return false;
		}

		auto struct_ = static_cast<TypeStruct *>(type);

		if (call->arguments.count > struct_->constants.declarations.count) {
			if (!silent) {
				reportError(location, "Error: Could not match polymorph pattern, pattern has %" PRIu64 " arguments and given struct has %" PRIu64, call->arguments.count, 
					struct_->constants.declarations.count);
				reportError(polymorphExpression, "   ..: Here is is the polymorph argument");
			}
			return false;
		}

		for (u32 i = 0; i < call->arguments.count; i++) {
			auto declaration = struct_->constants.declarations[i];

			if (call->arguments.names && call->arguments.names[i]) {
				declaration = findDeclarationNoYield(&struct_->constants, call->arguments.names[i]);

				if (!declaration) {
					if (!silent) {
						reportError(location, "Error: Could not match polymorph pattern, expected a struct argument called %.*s but given struct %.*s does not have it",
							STRING_PRINTF(call->arguments.names[i]), STRING_PRINTF(struct_->name));
						reportError(polymorphExpression, "   ..: Here is the polymorph argument");
					}
					return false;
				}
			}

			assert(declaration->flags & DECLARATION_VALUE_IS_READY);
			assert(declaration->initialValue);

			if (declaration->initialValue->flavor != ExprFlavor::TYPE_LITERAL) {
				// @Incomplete pattern matching for polymorphic structs (requires handling non-type values) i.e. Static_List(u8, 16)
				//                                                                                                               ^
				if (!silent) {
					reportError(location, "Error: Could not match polymorph pattern, polymorph variables can currently only be a constant type but got a %.*s instead",
						STRING_PRINTF(declaration->initialValue->type->name));
					reportError(polymorphExpression, "   ..: Here is the polymorph argument");
					reportError(polymorphExpression, "   ..: Consider making the argument a wildcard polymorph instead");
				}
				return false;
			}

			if (!matchPolymorphArgument(call->arguments.values[i], static_cast<ExprLiteral *>(declaration->initialValue)->typeValue, location, yield, silent)) {
				return false;
			}
		}

		return true;
	}

	if (!silent)
		reportError(polymorphExpression, "Error: Could not match polymorphic argument pattern");
	return false;
}


bool doPolymorphMatchingForCall(Arguments *arguments, ExprFunction *function, bool *yield) {
	for (u32 i = 0; i < arguments->count; i++) {
		if (!arguments->values[i])
			continue;

		Declaration *argument = function->arguments.declarations[i];
		assert(!(argument->flags & DECLARATION_IMPORTED_BY_USING));

		auto argumentType = arguments->values[i]->type;

		
		auto argumentExpr = arguments->values[i];

		assert(!(argument->flags & DECLARATION_IS_VARARGS) || (argumentExpr->flags & EXPR_IS_SPREAD));

		if (argument->flags & DECLARATION_TYPE_POLYMORPHIC) {
			auto typeExpr = argument->type;
			assert(typeExpr);
			if (!matchPolymorphArgument(typeExpr, getTypeForExpr(argumentExpr), argumentExpr, yield, false))
				return false;
		}

		if (argument->flags & DECLARATION_VALUE_POLYMORPHIC) {
			for (auto constant : function->constants.declarations) {
				if (constant->name == argument->name) {
					constant->initialValue = argumentExpr;
				}
			}
		}
		
	}

	return true;
}

bool constantsBlockMatches(Block *original, Block *polymorph) {
	assert(original->declarations.count == polymorph->declarations.count);

	for (u64 i = 0; i < original->declarations.count; i++) {
		auto originalDeclaration  = original->declarations[i];
		auto polymorphDeclaration = polymorph->declarations[i];

		assert(originalDeclaration->name == polymorphDeclaration->name);
		
		if (!switchCasesAreSame(originalDeclaration->initialValue, polymorphDeclaration->initialValue)) {
			return false;
		}
	}

	return true;
}

bool constantsBlockMatches(Block *original, Arguments *polymorph) {
	assert(original->declarations.count == polymorph->count);

	for (u64 i = 0; i < original->declarations.count; i++) {
		if (!switchCasesAreSame(original->declarations[i]->initialValue, polymorph->values[i])) {
			return false;
		}
	}

	return true;
}

bool functionIsVoid(TypeFunction *function) {
	return function->returnCount == 1 && function->returnTypes[0] == &TYPE_VOID;
}

bool coerceToBool(SubJob *job, Expr **given, bool *yield) {
	*yield = false;
	auto &expr = *given;

	if (expr->type == &TYPE_BOOL)
		return true;

	if (expr->type == &TYPE_AUTO_CAST) {
		if (!tryAutoCast(job, given, &TYPE_BOOL, yield))
			return false;
	}
	else if (isValidCast(&TYPE_BOOL, expr->type)) {
		insertImplicitCast(job->sizeDependencies, given, &TYPE_BOOL);
	}
	else {
		reportError(expr, "Error: Cannot convert %.*s to bool", STRING_PRINTF(expr->type->name));
		return false;
	}

	return true;
}

bool coerceToConstantType(SubJob *job, Expr **given, const char *message, bool *yield) {
	*yield = false;
	auto &expr = *given;

	if (expr->type == &TYPE_AUTO_CAST) {
		if (!tryAutoCast(job, given, &TYPE_TYPE, yield))
			return false;
	}

	if (expr->type != &TYPE_TYPE) {
		reportError(expr, "Error: %s type must be a type but got a %.*s", message, STRING_PRINTF(expr->type->name));
		return false;
	}

	if (expr->flavor != ExprFlavor::TYPE_LITERAL) {
		reportError(expr, "Error: %s type must be a constant", message);
		return false;
	}

	return true;
}

const char *binaryOpName(TokenT op) {
	switch (op) {
	case TokenT::ARRAY_TYPE:
		return "make an array from";
	case TOKEN('['):
		return "index";
	case TokenT::CAST:
		return "cast";
	case TokenT::LOGIC_AND:
	case TOKEN('&'):
	case TokenT::AND_EQUALS:
		return "and";
	case TokenT::LOGIC_OR:
	case TOKEN('|'):
	case TokenT::OR_EQUALS:
		return "or";
	case TokenT::EQUAL:
	case TokenT::NOT_EQUAL:
	case TokenT::GREATER_EQUAL:
	case TokenT::LESS_EQUAL:
	case TOKEN('>'):
	case TOKEN('<'):
		return "compare";
	case TOKEN('+'):
	case TokenT::PLUS_EQUALS:
		return "add";
	case TOKEN('-'):
	case TokenT::MINUS_EQUALS:
		return "subtract";
	case TOKEN('^'):
	case TokenT::XOR_EQUALS:
		return "xor";
	case TokenT::SHIFT_LEFT:
	case TokenT::SHIFT_RIGHT:
	case TokenT::SHIFT_LEFT_EQUALS:
	case TokenT::SHIFT_RIGHT_EQUALS:
		return "shift";
	case TOKEN('*'):
	case TokenT::TIMES_EQUALS:
		return "multiply";
	case TOKEN('/'):
	case TokenT::DIVIDE_EQUALS:
		return "divide";
	case TOKEN('%'):
	case TokenT::MOD_EQUALS:
		return "mod";
	case TOKEN('='):
		return "assign";
	default:
		assert(false);
		return "<unknown binary operator>";
	}
}

bool inferBinary(SubJob *job, Expr **exprPointer, bool *yield) {
	*yield = false;

	auto expr = *exprPointer;
	auto binary = static_cast<ExprBinaryOperator *>(expr);

	auto &left = binary->left;
	auto &right = binary->right;

	if (!checkExpressionIsRuntimeValid(left) || !checkExpressionIsRuntimeValid(right))
		return false;

	if (binary->flags & EXPR_ASSIGN_IS_IMPLICIT_INITIALIZER) {
		auto declaration = static_cast<ExprIdentifier *>(left)->declaration;

		if (!(declaration->flags & DECLARATION_VALUE_IS_READY)) {
			goToSleep(job, &declaration->sleepingOnMyValue, "Implicit initializer value not ready");

			*yield = true;
			return true;
		}
		else {
			binary->right = declaration->initialValue;
			assert(binary->left->type == binary->right->type);
			return true;
		}
	}

	switch (binary->op) {
	case TokenT::ARRAY_TYPE: {
		if (!coerceToConstantType(job, &right, "Array element", yield))
			return *yield;

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

			if (size->unsignedValue > UINT32_MAX) {
				reportError(right, "Error: Array size is too large, the max size is %u (given: %" PRIu64 ")", UINT32_MAX, size->unsignedValue);
				return false;
			}

			array = getStaticArray(type->typeValue, static_cast<u32>(size->unsignedValue));

			addSizeJobIfNeeded(array);
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
	case TokenT::CAST: {
		if (!coerceToConstantType(job, &left, "Cast", yield)) {
			return *yield;
		}

		assert(left->flavor == ExprFlavor::TYPE_LITERAL);

		Type *castTo = static_cast<ExprLiteral *>(left)->typeValue;
		trySolidifyNumericLiteralToDefault(right);

		if (!(binary->flags & EXPR_CAST_IS_BITWISE) && tryUsingConversion(job, castTo, &right)) {
			*yield = true;
			return true;
		}
		else {
			if (!isValidCast(job, castTo, right->type, binary->flags, yield)) {
				if (*yield) return true;
				reportError(binary, "Error: Cannot cast from %.*s to %.*s", STRING_PRINTF(right->type->name), STRING_PRINTF(castTo->name));
				return false;
			}

			expr->type = castTo;

			if ((right->type->flags & TYPE_ARRAY_IS_FIXED) && castTo->flavor == TypeFlavor::ARRAY && !(castTo->flags & TYPE_ARRAY_IS_FIXED)) {
				markDeclarationsAsPointedTo(right);
			}
		}
		break;
	}
	case TokenT::LOGIC_AND:
	case TokenT::LOGIC_OR: {
		if (!coerceToBool(job, &binary->left, yield))
			return *yield;

		if (!coerceToBool(job, &binary->right, yield))
			return *yield;

		expr->type = &TYPE_BOOL;

		break;
	}
	case TokenT::EQUAL:
	case TokenT::NOT_EQUAL:
	case TokenT::GREATER_EQUAL:
	case TokenT::LESS_EQUAL:
	case TOKEN('>'):
	case TOKEN('<'):
	case TOKEN('+'):
	case TOKEN('-'):
	case TOKEN('&'):
	case TOKEN('|'):
	case TOKEN('^'):
	case TokenT::SHIFT_LEFT:
	case TokenT::SHIFT_RIGHT:
	case TOKEN('*'):
	case TOKEN('/'):
	case TOKEN('%'): {
		if (left->type->flavor == TypeFlavor::AUTO_CAST && right->type->flavor == TypeFlavor::AUTO_CAST) {
			reportError(binary, "Error: Cannot infer the type of an expression when the sides are %.*s and %.*s", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
			return false;
		}

		if (left->type->flavor == TypeFlavor::AUTO_CAST && getTypeForExpr(right) != right->type) {
			insertImplicitCast(job->sizeDependencies, &right, getTypeForExpr(right));
		} else if (right->type->flavor == TypeFlavor::AUTO_CAST && getTypeForExpr(left) != left->type) {
			insertImplicitCast(job->sizeDependencies, &left, getTypeForExpr(left));
		}

		bool handled = left->type == right->type;

		if (binary->op == TOKEN('+')) {
			if (left->type->flavor == TypeFlavor::POINTER && right->type->flavor == TypeFlavor::POINTER) {
				reportError(binary, "Error: Cannot add pointers %.*s and %.*s", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
				return false;
			}
			else if (left->type->flavor == TypeFlavor::POINTER && right->type->flavor == TypeFlavor::INTEGER) {
				if (right->type->size != 8) {
					insertImplicitCast(job->sizeDependencies, &right, right->type->flags & TYPE_INTEGER_IS_SIGNED ? &TYPE_S64 : &TYPE_U64);
				}

				binary->type = left->type;

				break;
			}
			else if (left->type->flavor == TypeFlavor::INTEGER && right->type->flavor == TypeFlavor::POINTER) {
				if (left->type->size != 8) {
					insertImplicitCast(job->sizeDependencies, &left, left->type->flags & TYPE_INTEGER_IS_SIGNED ? &TYPE_S64 : &TYPE_U64);
				}

				binary->type = right->type;

				break;
			}
		}
		else if (binary->op == TOKEN('-')) {
			// Prevent implicit conversion from between pointer types or *void-function when doing pointer subtraction
			if ((left->type->flavor == TypeFlavor::POINTER || left->type->flavor == TypeFlavor::FUNCTION) && (right->type->flavor == TypeFlavor::POINTER || right->type->flavor == TypeFlavor::FUNCTION)) {

				if (left->type != right->type || left->type->flavor == TypeFlavor::FUNCTION) {
					reportError(binary, "Error: Cannot subtract %.*s and %.*s", STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
					return false;
				}
				else {
					binary->type = &TYPE_S64;
					break;
				}
			}
			else if (left->type->flavor == TypeFlavor::POINTER && right->type->flavor == TypeFlavor::INTEGER) {
				if (right->type->size != 8) {
					insertImplicitCast(job->sizeDependencies, &right, right->type->flags & TYPE_INTEGER_IS_SIGNED ? &TYPE_S64 : &TYPE_U64);
				}

				binary->type = left->type;

				break;
			}
		}

		if (left->type != right->type) {
			if (getConversionCost(job, left->type, right, yield) == ConversionCost::CANNOT_CONVERT) {
				if (*yield)
					return true;

				if (getConversionCost(job, right->type, left, yield) == ConversionCost::CANNOT_CONVERT) {
					if (*yield)
						return true;

					reportError(binary, "Error: Cannot %s %.*s and %.*s", binaryOpName(binary->op), STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
					return false;
				}
				else {
					if (!assignOp(job, binary, right->type, left, yield))
						return *yield;
				}
			}
			else {
				if (!assignOp(job, binary, left->type, right, yield))
					return *yield;
			}
		}

		switch (binary->op) {
		case TokenT::EQUAL:
		case TokenT::NOT_EQUAL: {
			if (left->type->flavor == TypeFlavor::ARRAY || left->type->flavor == TypeFlavor::STRUCT) {
				reportError(binary, "Error: Cannot compare %.*s's", STRING_PRINTF(left->type->name));
				return false;
			}

			expr->type = &TYPE_BOOL;
			break;
		}
		case TokenT::GREATER_EQUAL:
		case TokenT::LESS_EQUAL:
		case TOKEN('>'):
		case TOKEN('<'): {
			if (left->type->flavor != TypeFlavor::INTEGER && left->type->flavor != TypeFlavor::FLOAT) {
				reportError(binary, "Error: Cannot compare %.*s's", STRING_PRINTF(left->type->name));
				return false;
			}

			expr->type = &TYPE_BOOL;
			break;
		}
		case TOKEN('*'):
		case TOKEN('/'):
		case TOKEN('%'):
		case TOKEN('+'):
		case TOKEN('-'): {
			if (left->type->flavor != TypeFlavor::INTEGER && left->type->flavor != TypeFlavor::FLOAT) {
				reportError(binary, "Error: Cannot %s %.*s's", binaryOpName(binary->op), STRING_PRINTF(left->type->name));
				return false;
			}

			expr->type = left->type;
			break;
		}
		case TOKEN('&'):
		case TOKEN('|'):
		case TOKEN('^'): {
			if (left->type->flavor == TypeFlavor::BOOL || left->type->flavor == TypeFlavor::INTEGER) {
				// Valid
			}
			else if (left->type->flavor == TypeFlavor::ENUM) {
				if (!(left->type->flags & TYPE_ENUM_IS_FLAGS)) {
					reportError(binary, "Error: Cannot %s %.*s's, they are an enum. This operation can only be performed on enum_flags", binaryOpName(binary->op), STRING_PRINTF(left->type->name));
					return false;

				}
			}
			else {
				reportError(binary, "Error: Cannot %s %.*s's", binaryOpName(binary->op), STRING_PRINTF(left->type->name));
				return false;
			}

			expr->type = left->type;
			break;
		}
		case TokenT::SHIFT_LEFT:
		case TokenT::SHIFT_RIGHT: {
			if (left->type->flavor != TypeFlavor::INTEGER) {
				reportError(binary, "Error: Cannot shift %.*s's", STRING_PRINTF(left->type->name));
				return false;
			}
			
			expr->type = left->type;
			break;
		}
		default:
			assert(false);
		}


		break;
	}
	case TOKEN('='):
	case TokenT::PLUS_EQUALS:
	case TokenT::MINUS_EQUALS:
	case TokenT::AND_EQUALS:
	case TokenT::OR_EQUALS:
	case TokenT::XOR_EQUALS:
	case TokenT::SHIFT_LEFT_EQUALS:
	case TokenT::SHIFT_RIGHT_EQUALS:
	case TokenT::TIMES_EQUALS:
	case TokenT::DIVIDE_EQUALS:
	case TokenT::MOD_EQUALS: {
		if (!isAddressable(left)) {
			// @Incomplete: better error messages here
			//  - If we were assigning to a constant
			//  - If we were assigning to an r-value pointer

			reportError(binary, "Error: Left side of binary is not assignable");
			return false;
		}


		if (left->type->flavor == TypeFlavor::POINTER && (binary->op == TokenT::PLUS_EQUALS || binary->op == TokenT::MINUS_EQUALS)) {
			if (right->type->flavor == TypeFlavor::INTEGER) {
				trySolidifyNumericLiteralToDefault(right);
				if (right->type->size != 8) {
					insertImplicitCast(job->sizeDependencies, &right, right->type->flags & TYPE_INTEGER_IS_SIGNED ? &TYPE_S64 : &TYPE_U64);
				}
			}
			else {
				reportError(binary, "Error: Cannot %s a *.*s and a %.*s", binaryOpName(binary->op), STRING_PRINTF(left->type->name), STRING_PRINTF(right->type->name));
				return false;
			}
			break;
		}


		if (!assignOp(job, binary, left->type, right, yield)) {
			return *yield;
		}

		switch (binary->op) {
		case TokenT::AND_EQUALS:
		case TokenT::OR_EQUALS:
		case TokenT::XOR_EQUALS: {
			if (left->type->flavor == TypeFlavor::BOOL || left->type->flavor == TypeFlavor::INTEGER) {
				// Valid
			}
			else if (left->type->flavor == TypeFlavor::ENUM) {
				if (!(left->type->flags & TYPE_ENUM_IS_FLAGS)) {
					reportError(binary, "Error: Cannot %s %.*s's, they are an enum. This operation can only be performed on enum_flags", binaryOpName(binary->op), STRING_PRINTF(left->type->name));
					return false;

				}
			}
			else {
				reportError(binary, "Error: Cannot %s %.*s's", binaryOpName(binary->op), STRING_PRINTF(left->type->name));
				return false;
			}

			break;
		}
		case TokenT::SHIFT_LEFT_EQUALS:
		case TokenT::SHIFT_RIGHT_EQUALS: {
			if (left->type->flavor != TypeFlavor::INTEGER) {
				reportError(binary, "Error: Cannot %s %.*s's", binaryOpName(binary->op), STRING_PRINTF(left->type->name));
				return false;
			}
			break;
		}
		case TokenT::PLUS_EQUALS:
		case TokenT::MINUS_EQUALS:
		case TokenT::TIMES_EQUALS:
		case TokenT::DIVIDE_EQUALS:
		case TokenT::MOD_EQUALS: {
			if (left->type->flavor != TypeFlavor::INTEGER && left->type->flavor != TypeFlavor::FLOAT) {
				reportError(binary, "Error: Cannot %s %.*s's", binaryOpName(binary->op), STRING_PRINTF(left->type->name));
				return false;
			}
			break;
		}
		}

		break;
	}
	default:
		assert(false);
	}

	
	if ((*exprPointer)->flavor == ExprFlavor::BINARY_OPERATOR)
		if (!evaluateConstantBinary(job, exprPointer, yield))
			return *yield;

	return true;
}

void addRunJob(ExprRun *run) {
	++totalRuns;

	run->runJob = allocateRunJob();
	run->runJob->run = run;

	beginFlatten(run->runJob, &run->function);

	auto function = static_cast<ExprFunction *>(run->function);
	if (!(function->flags & EXPR_FUNCTION_RUN_CHECKED)) {
		run->runJob->checkingFunctions.add(function);
		run->runJob->checkingFunctionIndices.add(0);
	}
	addJob(&runJobs, run->runJob);
	subJobs.add(run->runJob);
}

static Declaration *entryPoint;
static Array<SubJob *> waitingOnEntryPoint;


bool inferFlattened(SubJob *job) {
	PROFILE_FUNC();
	++totalFlattenedInfers;

	while (job->flattenedCount) {
		if (job->indices[job->flattenedCount - 1] == job->flatteneds[job->flattenedCount - 1].count) {
			--job->flattenedCount;
			continue;
		}

		u32 layer = job->flattenedCount - 1;

		auto exprPointer = getHalt(job);
		auto expr = *exprPointer;

		++totalInferIterations;

		switch (expr->flavor) {
		case ExprFlavor::CONTEXT: {
			addSizeDependency(job->sizeDependencies, &TYPE_CONTEXT);

			break;
		}
		case ExprFlavor::ENTRY_POINT: {
			if (!entryPoint) {
				goToSleep(job, &waitingOnEntryPoint, "Waiting for entry point");
				return true;
			}

			if (!(entryPoint->flags & DECLARATION_VALUE_IS_READY)) {
				goToSleep(job, &entryPoint->sleepingOnMyValue, "Waiting for entry point value");
				return true;
			}

			*exprPointer = entryPoint->initialValue;
			break;
		}
		case ExprFlavor::PUSH_CONTEXT: {
			auto pushContext = static_cast<ExprBinaryOperator *>(expr);

			bool yield;
			if (!assignOp(job, pushContext->left, &TYPE_CONTEXT, pushContext->left, &yield)) {
				return yield;
			}

			break;
		}
		case ExprFlavor::IDENTIFIER: {

			auto identifier = static_cast<ExprIdentifier *>(expr);

			if (!identifier->declaration) {
				if (identifier->structAccess) {
					if (identifier->structAccess->flavor == ExprFlavor::IMPORT) {
						auto import = static_cast<ExprLoad *>(identifier->structAccess);

						identifier->declaration = findDeclarationNoYield(&import->module->members, identifier->name);

						if (!identifier->declaration) {
							goToSleep(job, &identifier->module->members.sleepingOnMe, "Identifier not found in module by namespace", identifier->name);

							return true;
						}

						identifier->structAccess = nullptr;
					}
					else {
						bool onlyConstants;
						auto struct_ = getExpressionNamespace(identifier->structAccess, &onlyConstants, identifier);

						if (!struct_) {
							return false;
						}

						bool yield;
						auto member = findDeclaration(&struct_->members, identifier->name, &yield);

						if (yield) { // Check for importers even if we did find the member in case the import adds more overloads
							goToSleep(job, &struct_->members.sleepingOnMe, "Struct access scope has importers", identifier->name);

							return true;
						}

						if (!member) {
							if (struct_ == &TYPE_CONTEXT && !contextIsLocked) {
								goToSleep(job, &TYPE_CONTEXT.members.sleepingOnMe, "Struct access waiting for potential #add_context");
								return true;
							}

							auto member = findDeclarationNoYield(&struct_->constants, identifier->name);

							if (!member) {
								reportError(identifier, "Error: %.*s does not have member %.*s", STRING_PRINTF(struct_->name), STRING_PRINTF(identifier->name));
							}
							return false;
						}
						else if (onlyConstants & !(member->flags & DECLARATION_IS_CONSTANT)) {
							reportError(identifier, "Error: Can only access constant members of %.*s from it's prototype", STRING_PRINTF(struct_->name));
							return false;
						}
						identifier->declaration = member;

						// @Incomplete: Constant evaluate accesses of struct literals

						if (!(identifier->declaration->flags & DECLARATION_IS_CONSTANT)) {
							addSizeDependency(job->sizeDependencies, struct_);
						}
					}
				}
				else {
					for (; identifier->resolveFrom; identifier->resolveFrom = identifier->resolveFrom->parentBlock) {
						bool yield;

						if (Declaration *declaration = findDeclaration(identifier->resolveFrom, identifier->name, &yield, identifier->serial)) {
							// Don't resolve while loop iterator variable during normal identifier lookup
							// this
							if (declaration->flags & DECLARATION_IS_ITERATOR) {
								auto loop = CAST_FROM_SUBSTRUCT(ExprLoop, iteratorBlock, declaration->enclosingScope);
								assert(loop->flavor == ExprFlavor::WHILE || loop->flavor == ExprFlavor::FOR);

								if (loop->flavor == ExprFlavor::WHILE)
									continue;
							}

							if (declaration->flags & DECLARATION_IS_CONSTANT) {
								identifier->declaration = declaration;
								break;
							}

							if (identifier->flags & EXPR_IDENTIFIER_RESOLVING_ONLY_CONSTANTS) {
								reportError(identifier, "Error: Cannot refer to variable from outside function or struct, capture is not supported");
								return false;
							}

							if (declaration->enclosingScope->flavor != BlockFlavor::IMPERATIVE || declaration->serial < identifier->serial) {
								identifier->declaration = declaration;
								break;
							}

							reportError(identifier, "Error: Cannot refer to variable '%.*s' before it was declared", STRING_PRINTF(identifier->name));
							reportError(declaration, "   ..: Here is the location of the declaration");
							return false;
						}
						else if (yield) {
							goToSleep(job, &identifier->resolveFrom->sleepingOnMe, "Identifier scope has importers", identifier->name);

							return true;
						}

						if (identifier->resolveFrom->flavor != BlockFlavor::IMPERATIVE)
							identifier->flags |= EXPR_IDENTIFIER_RESOLVING_ONLY_CONSTANTS;
					}

					if (!identifier->declaration) {
						if (!identifier->resolveFrom) { // If we have checked all the local scopes and the
							identifier->declaration = findDeclarationNoYield(&identifier->module->members, identifier->name);

							if (!identifier->declaration) {
								goToSleep(job, &identifier->module->members.sleepingOnMe, "Identifier not found in module", identifier->name);

								return true;
							}
						}
					}

					if (identifier->enclosingScope && identifier->declaration && identifier->declaration->enclosingScope != identifier->enclosingScope) {
						addImplicitImport(identifier->enclosingScope, identifier);
					}
				}
			}

			if (!identifier->declaration)
				return true;

			auto unimportedDeclaration = identifier->declaration;


			if ((identifier->declaration->flags & DECLARATION_IMPORTED_BY_USING)) {
				if (!identifier->declaration->initialValue) {
					identifier->declaration = identifier->declaration->import;
				}
				else {
					assert(identifier->declaration->initialValue->flavor == ExprFlavor::IDENTIFIER);

					auto accesses = static_cast<ExprIdentifier *>(identifier->declaration->initialValue);
					auto result = INFER_NEW(ExprIdentifier);
					auto current = result;

					while (true) {
						*current = *accesses;

						bool onlyConstants;
						addSizeDependency(job->sizeDependencies, getExpressionNamespace(current, &onlyConstants, current));

						current->start = identifier->start;
						current->end = identifier->end;

						if (!current->structAccess)
							break;


						current->structAccess = INFER_NEW(ExprIdentifier);
						current = static_cast<ExprIdentifier *>(current->structAccess);

						assert(accesses->structAccess->flavor == ExprFlavor::IDENTIFIER);
						accesses = static_cast<ExprIdentifier *>(accesses->structAccess);
					}

					current->structAccess = identifier->structAccess;
					identifier->structAccess = result;
					identifier->declaration = identifier->declaration->import;

					pushFlatten(job, identifier);
					continue;
				}
			}

			bool yield;
			if (declarationIsOverloadSet(job, unimportedDeclaration, &yield)) {
				auto overloadSet = INFER_NEW(ExprOverloadSet);
				overloadSet->flavor = ExprFlavor::OVERLOAD_SET;
				overloadSet->start = identifier->start;
				overloadSet->end = identifier->end;
				overloadSet->identifier = identifier;
				overloadSet->currentOverload = identifier->declaration;
				overloadSet->block = identifier->declaration->enclosingScope;

				overloadSet->type = &TYPE_OVERLOAD_SET;

				*exprPointer = overloadSet;
				break;
			}
			else if (yield) {
				return true;
			}
			

			if (unimportedDeclaration->flags & DECLARATION_IMPORTED_BY_USING) {
				if (unimportedDeclaration->enclosingScope->flavor == BlockFlavor::IMPERATIVE && unimportedDeclaration->serial >= identifier->serial) {
					reportError(identifier, "Error: Cannot refer to variable '%.*s' before it was declared", STRING_PRINTF(identifier->name));
					reportError(unimportedDeclaration, "   ..: Here is the location of the declaration");
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
				goToSleep(job, &identifier->declaration->sleepingOnMyType, "Identifier type not ready");

				if (!identifier->declaration->inferJob)
					forceAddDeclaration(identifier->declaration);

				return true;
			}

			if (identifier->declaration->flags & DECLARATION_IS_CONSTANT) {
				if (identifier->declaration->flags & DECLARATION_VALUE_IS_READY) {
					assert(identifier->declaration->flags & DECLARATION_TYPE_IS_READY);
					copyLiteral(exprPointer, identifier->declaration->initialValue);


				}
				else {
					goToSleep(job, &identifier->declaration->sleepingOnMyValue, "Identifier value not ready");

					if (!identifier->declaration->inferJob)
						forceAddDeclaration(identifier->declaration);

					return true;
				}
			}
			else if (identifier->structAccess) {
				if (identifier->structAccess->type->flags & TYPE_ARRAY_IS_FIXED) {
					if (!isAddressable(identifier->structAccess)) {
						reportError(identifier, "Error: Cannot get the data pointer of a fixed array that has no storage");
						return false;
					}

					markDeclarationsAsPointedTo(identifier->structAccess);

					auto address = INFER_NEW(ExprUnaryOperator);
					address->flavor = ExprFlavor::UNARY_OPERATOR;
					address->op = TOKEN('*');
					address->start = identifier->start;
					address->end = identifier->end;
					address->value = identifier->structAccess;
					address->type = getPointer(identifier->structAccess->type);

					*exprPointer = address;

					insertImplicitCast(job->sizeDependencies, exprPointer, identifier->type);
				}
				else if (identifier->structAccess->type->flavor == TypeFlavor::POINTER &&
					(static_cast<TypePointer *>(identifier->structAccess->type)->pointerTo->flags & TYPE_ARRAY_IS_FIXED)) {
					auto cast = INFER_NEW(ExprBinaryOperator);
					cast->flavor = ExprFlavor::BINARY_OPERATOR;
					cast->op = TokenT::CAST;
					cast->start = identifier->start;
					cast->end = identifier->end;
					cast->right = identifier->structAccess;
					cast->type = identifier->type;
					cast->left = inferMakeTypeLiteral(cast->start, cast->end, cast->type);

					*exprPointer = cast;
				}
			}
			break;
		}
		case ExprFlavor::FUNCTION: {
			if (!expr->type) {
				auto function = static_cast<ExprFunction *>(expr);

				goToSleep(job, &function->sleepingOnInfer, "Function header not ready");

				return true;
			}

			break;
		}
		case ExprFlavor::ARRAY_LITERAL: {
			auto literal = static_cast<ExprArrayLiteral *>(expr);

			if (!literal->type) {
				assert(literal->typeValue);
				bool yield;
				if (literal->typeValue->type == &TYPE_AUTO_CAST) {
					bool yield = false;
					if (!tryAutoCast(job, &literal->typeValue, &TYPE_TYPE, &yield)) {
						if (!yield) {
							reportError(literal->typeValue, "Error: Cannot convert %.*s to type",
								STRING_PRINTF(static_cast<ExprBinaryOperator *>(literal->typeValue)->right->type->name));
							return false;
						}
						else {
							return true;
						}
					}
				}

				if (literal->typeValue->type->flavor != TypeFlavor::TYPE) {
					reportError(literal->typeValue, "Error: Array element type must be a type");
					return false;
				}

				if (literal->typeValue->flavor != ExprFlavor::TYPE_LITERAL) {
					reportError(literal->typeValue, "Error: Array element type must be a constant");
					return false;
				}

				auto type = static_cast<ExprLiteral *>(literal->typeValue)->typeValue;

				if (type == &TYPE_VOID) {
					reportError(literal->typeValue, "Error: Cannot have an array of void elements");
					return false;
				}
				else if (type->flavor == TypeFlavor::MODULE) {
					reportError(literal->typeValue, "Error: Cannot have an array of namespaces");
					return false;
				}

				literal->type = getStaticArray(type, literal->count);

				addSizeJobIfNeeded(literal->type);
				addSizeDependency(job->sizeDependencies, literal->type);
			}

			assert(literal->type != &TYPE_ARRAY_LITERAL);
			assert(literal->type && literal->type->flavor == TypeFlavor::ARRAY);

			auto array = static_cast<TypeArray *>(literal->type);

			for (u32 i = 0; i < literal->count; i++) {
				bool yield;
				if (!assignOp(job, literal->values[i], array->arrayOf, literal->values[i], &yield)) {
					return yield;
				}
			}
			
			break;
		}
		case ExprFlavor::STRUCT_LITERAL: {
			auto literal = static_cast<ExprStructLiteral *>(expr);

			if (!literal->type) {
				assert(literal->typeValue);
				if (literal->typeValue->type == &TYPE_AUTO_CAST) {
					bool yield = false;
					if (!tryAutoCast(job, &literal->typeValue, &TYPE_TYPE, &yield)) {
						if (!yield) {
							reportError(literal->typeValue, "Error: Cannot convert %.*s to type",
								STRING_PRINTF(static_cast<ExprBinaryOperator *>(literal->typeValue)->right->type->name));
							return false;
						}
						else {
							return true;
						}
					}
				}

				if (literal->typeValue->type->flavor != TypeFlavor::TYPE) {
					reportError(literal->typeValue, "Error: Struct literal type must be a type");
					return false;
				}

				if (literal->typeValue->flavor != ExprFlavor::TYPE_LITERAL) {
					reportError(literal->typeValue, "Error: Struct literal type must be a constant");
					return false;
				}

				auto type = static_cast<ExprLiteral *>(literal->typeValue)->typeValue;

				if (type->flavor != TypeFlavor::ARRAY && type->flavor != TypeFlavor::STRING && type->flavor != TypeFlavor::STRUCT) {
					reportError(literal->typeValue, "Error: Can only have a struct literal for a struct-like type, not a %.*s", STRING_PRINTF(type->name));
					return false;
				}

				if (type->flags & TYPE_ARRAY_IS_FIXED) {
					reportError(literal->typeValue, "Error: Cannot have a struct literal for a fixed array");
					return false;
				}

				literal->type = type;
				addSizeDependency(job->sizeDependencies, literal->type);
			}

			assert(literal->type != &TYPE_STRUCT_LITERAL);

			auto block = &static_cast<TypeStruct *>(literal->type)->members;

			

			for (auto importer : block->importers) {
				if (importer->import->flavor == ExprFlavor::STATIC_IF) {
					goToSleep(job, &block->sleepingOnMe, "Struct literal sleeping on importer");
					return true;
				}
			}

			if (!literal->initializers.declarations) {
				u32 memberCount = 0;

				for (auto member : block->declarations) {
					if (member->flags & (DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_CONSTANT))
						continue;

					memberCount++;
				}

				u32 wantedInitializerCount = (literal->type->flags & TYPE_STRUCT_IS_UNION ? 1 : memberCount);

				if (literal->initializers.count > wantedInitializerCount) {
					if ((literal->type->flags & TYPE_STRUCT_IS_UNION)) {
						reportError(literal, "Error: Can only initialize one value of union '%.*s'", STRING_PRINTF(literal->type->name),
							STRING_PRINTF(literal->type->name), memberCount, literal->initializers.count);
					}
					else {
						reportError(literal, "Error: Too many initializer values for %.*s, (Wanted: %u, Given: %u)",
							STRING_PRINTF(literal->type->name), memberCount, literal->initializers.count);
					}
					return false;
				}

				Array<Expr *> initializers;
				Array<Declaration *> members;

				Expr **sortedInitializers = nullptr;

				bool needsExtraInitializers = literal->initializers.count != wantedInitializerCount &&
					!(literal->flags & EXPR_STRUCT_LITERAL_UNSPECIFIED_MEMBERS_UNINITIALZIED);

				if (literal->initializers.names || needsExtraInitializers) {
					sortedInitializers = new Expr * [memberCount] {};
				}

				for (u32 i = 0; i < literal->initializers.count; i++) {
					Declaration *member;
					u32 memberIndex = i;

					auto initializer = literal->initializers.values[i];

					if (literal->initializers.names && literal->initializers.names[i].length) {
						member = findDeclarationNoYield(block, literal->initializers.names[i]);

						if (!member) {
							reportError(initializer, "Error: %.*s does not have member %.*s", STRING_PRINTF(literal->type->name), STRING_PRINTF(literal->initializers.names[i]));
							return false;
						}

						if (member->flags & DECLARATION_IS_CONSTANT) {
							reportError(initializer, "Error: Cannot initialize a constant member");
							return false;
						}

						// @Incomplete Allow this in cases where the 'using'ed variable wouldn't cause a dereference of a potentially uninitialized pointer
						if (member->flags & DECLARATION_IMPORTED_BY_USING) {
							reportError(initializer, "Error: Cannot initialize a member that is added by a using");
							return false;
						}

						memberIndex = getDeclarationIndex(block, member, DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_CONSTANT);
					}
					else {
						member = getDeclarationByIndex(block, i, DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_CONSTANT);
					}

					if (!(member->flags & DECLARATION_TYPE_IS_READY)) {
						delete[] sortedInitializers;
						members.free();

						goToSleep(job, &member->sleepingOnMyType, "Struct initializer waiting on member type");
						return true;
					}

					members.add(member);

					if (sortedInitializers) {
						if (sortedInitializers[memberIndex]) {
							reportError(initializer, "Error: Cannot initialize %.*s multiple times", STRING_PRINTF(member->name));
							reportError(sortedInitializers[memberIndex], "   ..: Here is the previous initialization");
							return false;
						}
						else {
							sortedInitializers[memberIndex] = initializer;
						}
					}
				}

				if (needsExtraInitializers) {
					initializers.reserve(memberCount);

					for (u32 i = 0; i < literal->initializers.count; i++) {
						initializers.add(literal->initializers.values[i]);
					}

					for (u32 i = 0; i < memberCount; i++) {
						if (sortedInitializers[i])
							continue;

						// This could be done in the
						auto member = getDeclarationByIndex(block, i, DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_CONSTANT);

						if (member->flags & DECLARATION_IS_UNINITIALIZED)
							continue;

						if (!(member->flags & DECLARATION_TYPE_IS_READY)) {
							delete[] sortedInitializers;
							initializers.free();
							members.free();

							goToSleep(job, &member->sleepingOnMyType, "Struct initializer waiting on member type");
							return true;
						}

						if (!(member->flags & DECLARATION_VALUE_IS_READY)) {
							delete[] sortedInitializers;
							initializers.free();
							members.free();

							goToSleep(job, &member->sleepingOnMyValue, "Struct initializer waiting on member default");
							return true;
						}

						initializers.add(member->initialValue);
						members.add(member);
					}

					literal->initializers.count = initializers.count;
					literal->initializers.values = initializers.storage;
				}

				literal->initializers.declarations = members.storage;

				delete[] sortedInitializers;
			}

			for (u32 i = 0; i < literal->initializers.count; i++) {
				bool yield;
				if (!assignOp(job, literal->initializers.values[i], getDeclarationType(literal->initializers.declarations[i]), literal->initializers.values[i], &yield)) {
					return yield;
				}
			}


			break;
		}
		case ExprFlavor::RUN: {
			auto run = static_cast<ExprRun *>(expr);

			auto function = static_cast<ExprFunction *>(run->function);

			if (run->returnValue) {
				*exprPointer = run->returnValue;
				break;
			}

			assert(run->runJob);
			if (run->runJob) {
				goToSleep(job, &run->sleepingOnMe, "Run not completed");
				return true;
			}

			return true;
		}
		case ExprFlavor::IMPORT: {
			auto import = static_cast<ExprLoad *>(expr);

			if (import->file->type != &TYPE_STRING) {
				reportError(import->file, "Error: Module to import must be a string");
				return false;
			}
			else if (import->file->flavor != ExprFlavor::STRING_LITERAL) {
				reportError(import->file, "Error: Module to import must be a constant");
				return false;
			}

			auto name = static_cast<ExprStringLiteral *>(import->file)->string;

			auto module = getModule(name);

			if (module == import->module) {
				reportError(import, "Error: A module cannot import itself");
				return false;
			}

			if (!module)
				return false;

			import->module = module;

			break;
		}
		case ExprFlavor::LOAD: {
			auto load = static_cast<ExprLoad *>(expr);

			if (load->module) {
				if (load->file->type != &TYPE_STRING) {
					reportError(load->file, "Error: File to load must be a string");
					return false;
				}
				else if (load->file->flavor != ExprFlavor::STRING_LITERAL) {
					reportError(load->file, "Error: File to load must be a constant");
					return false;
				}

				auto name = static_cast<ExprStringLiteral *>(load->file)->string;

				loadNewFile(name, load->module);

				load->module = nullptr;
			}
			
			break;
		}
		case ExprFlavor::FUNCTION_PROTOTYPE: {
			auto function = static_cast<ExprFunction *>(expr);

			bool sleep = false;

			for (auto argument : function->arguments.declarations) {
				assert(argument);

				if (!(argument->flags & DECLARATION_TYPE_IS_READY)) {
					goToSleep(job, &argument->sleepingOnMyType, "Function argument type not ready");

					sleep = true;
				}
			}


			for (auto return_ : function->returns.declarations) {
				assert(return_);

				if (!(return_->flags & DECLARATION_TYPE_IS_READY)) {
					goToSleep(job, &return_->sleepingOnMyType, "Function return type not ready");

					sleep = true;
				}
			}

			if (sleep)
				return true;

			auto type = getFunctionType(function);

			*exprPointer = inferMakeTypeLiteral(function->start, function->end, type);
			(*exprPointer)->valueOfDeclaration = expr->valueOfDeclaration;

			break;
		}
		case ExprFlavor::TYPE_LITERAL: {
			auto type = static_cast<ExprLiteral *>(expr)->typeValue;

			if (type->flavor == TypeFlavor::STRUCT) {
				auto struct_ = static_cast<TypeStruct *>(type);

				bool sleep = false;

				for (auto declaration : struct_->constants.declarations) {
					if (!(declaration->flags & DECLARATION_TYPE_IS_READY)) {
						goToSleep(job, &declaration->sleepingOnMyType, "Polymorphic struct argument type not ready");

						sleep = true;
					}
				}

				if (sleep)
					return true;
			}

			break;
		}
		case ExprFlavor::STRING_LITERAL:
		case ExprFlavor::FLOAT_LITERAL:
		case ExprFlavor::INT_LITERAL: {
			break;
		}
		case ExprFlavor::BLOCK: {
			auto block = static_cast<ExprBlock *>(expr);

			bool sleep = false;

			for (auto declaration : block->declarations.declarations) {
				if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING))) {
					if (!(declaration->flags & DECLARATION_TYPE_IS_READY)) {
						goToSleep(job, &declaration->sleepingOnMyType, "Block declaration type not ready");

						sleep = true;
					}
				}
			}

			if (sleep)
				return true;

			// Do two passes over the array because if the first pass added dependencies on some declarations then yielded, it would add them again next time round
			for (auto declaration : block->declarations.declarations) {
				if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING))) {
					addSizeDependency(job->sizeDependencies, getDeclarationType(declaration));
				}
			}

			break;
		}
		case ExprFlavor::BREAK:
		case ExprFlavor::CONTINUE:
		case ExprFlavor::REMOVE: {
			auto continue_ = static_cast<ExprBreakOrContinue *>(expr);

				// Don't pass through arguments blocks since we can't break from an inner function to an outer one
			for (Block *block = continue_->enclosingScope; block && block->flavor == BlockFlavor::IMPERATIVE; block = block->parentBlock) {
				if (!block->loop)
					continue;
				if (!continue_->label || findDeclarationNoYield(block, continue_->label)) {

					continue_->refersTo = CAST_FROM_SUBSTRUCT(ExprLoop, iteratorBlock, block);

					if (continue_->flavor == ExprFlavor::BREAK) {
						continue_->refersTo->flags |= EXPR_LOOP_HAS_BREAK;
					}
					break;
				}
			}

			if (!continue_->refersTo) {
				if (continue_->label)
					reportError(continue_, "Error: Could not resolve loop with iterator %.*s", STRING_PRINTF(continue_->label));
				else
					reportError(continue_, "Error: Cannot have a %s outside of a loop", continue_->flavor == ExprFlavor::CONTINUE ? "continue" :
						(continue_->flavor == ExprFlavor::REMOVE ? "remove" : "break"));
				return false;
			}

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

				if (!isAddressable(continue_->refersTo->forBegin)) {
					reportError(continue_->refersTo->forBegin, "Error: Cannot remove from a temporary value");

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

				if (slice->sliceEnd->type->flavor != TypeFlavor::INTEGER) {
					reportError(slice->sliceEnd, "Error: Array index must be an integer");
					return false;
				}

				if (slice->sliceEnd->type->size != 8) {
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
		}
		case ExprFlavor::SWITCH: {
			auto switch_ = static_cast<ExprSwitch *>(expr);

			trySolidifyNumericLiteralToDefault(switch_->condition);

			// Currently switch statements work with exactly the same types as ==, 
			// which means you can do some weird but fine stuff like switching on bools
			// but also some highly error prone stuff like switching on floats and enum_flags
			switch (switch_->condition->type->flavor) {
			case TypeFlavor::INTEGER:
			case TypeFlavor::FLOAT:
			case TypeFlavor::POINTER:
			case TypeFlavor::BOOL:
			case TypeFlavor::FUNCTION:
			case TypeFlavor::TYPE:
			case TypeFlavor::STRING:
				break;
			case TypeFlavor::ENUM:
				if (switch_->flags & EXPR_SWITCH_IS_COMPLETE) {
					if (switch_->condition->type->flags & TYPE_ENUM_IS_FLAGS) {
						reportError(switch_, "Error: A switch if can only be #complete if it switches on an enum");
						return false;
					}
				}
				break;
			case TypeFlavor::ARRAY:
			case TypeFlavor::STRUCT:
			case TypeFlavor::MODULE:
			case TypeFlavor::AUTO_CAST:
			case TypeFlavor::VOID:
				reportError(switch_->condition, "Error: Cannot switch on %ss", STRING_PRINTF(switch_->condition->type->name));
				return false;
			default:
				assert(false);
			}

			for (auto &case_ : switch_->cases) {
				if (!case_.condition)
					continue;

				bool yield = false;
				if (!assignOp(job, case_.condition, switch_->condition->type, case_.condition, &yield)) {
					return yield;
				}
			}

			// @Speed switch statements could have a large number of cases so we need a non n^2 version of this
			for (auto &case_ : switch_->cases) {
				if (!case_.condition)
					continue;

				if (!isLiteral(case_.condition)) {
					reportError(case_.condition, "Error: Switch if case must be a constant value");
					return false;
				}

				for (auto it = &case_ + 1; it < switch_->cases.end(); it++) {
					if (!it->condition)
						continue;
					if (switchCasesAreSame(case_.condition, it->condition)) {
						reportError(it->condition, "Error: Duplicate case in switch if");
						reportError(case_.condition, "Error: Here is the previous case");
						return false;
					}
				}
			}

			if (switch_->flags & EXPR_SWITCH_IS_COMPLETE) {
				assert(switch_->condition->type->flavor == TypeFlavor::ENUM);

				auto enum_ = static_cast<TypeEnum *>(switch_->condition->type);

				bool sleep = false;

				Array<Declaration *> uncheckedMembers;

				for (auto member : enum_->members.declarations) {
					if (!(member->flags & DECLARATION_IS_ENUM_VALUE))
						continue;

					if (!(member->flags & DECLARATION_VALUE_IS_READY)) {
						goToSleep(job, &member->sleepingOnMyValue, "Complete switch enum value not ready");
						sleep = true;
						continue;
					}
					assert(member->initialValue->flavor == ExprFlavor::INT_LITERAL);

					u64 value = static_cast<ExprLiteral *>(member->initialValue)->unsignedValue;

					bool found = false;
					for (auto case_ : switch_->cases) {
						if (case_.condition) {
							assert(case_.condition->flavor == ExprFlavor::INT_LITERAL);

							u64 compare = static_cast<ExprLiteral *>(case_.condition)->unsignedValue;

							if (value == compare) {
								found = true;
								break;
							}
						}
					}

					if (!found) {
						uncheckedMembers.add(member);
						break;
					}
				}

				if (sleep) {
					uncheckedMembers.free();
					return true;
				}

				if (uncheckedMembers.count) {
					reportError(switch_, "Error: Switch if that was marked as #complete does not handle all values");

					for (auto member : uncheckedMembers) {
						reportError(member, "   ..: %.*s is not handled", STRING_PRINTF(member->name));
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
					reportError(loop, "Error: Cannot infer types of loop range when both are auto casted");
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

						if (loop->forBegin->type->flags & TYPE_ARRAY_IS_FIXED) {
							markDeclarationsAsPointedTo(loop->forBegin);
						}
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

				for (u32 i = comma->exprCount; i < functionForArgumentNames->returns.declarations.count; i++) {
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

					if (i >= 1) {
						declaration->flags |= DECLARATION_IS_POINTED_TO;
					}

					wakeUpSleepers(&declaration->sleepingOnMyType);
					declaration->sleepingOnMyType.free();
				}
			}
			else {
				for (u32 i = 0; i < comma->exprCount; i++) {
					if (!isAddressable(comma->left[i])) {
						reportError(comma->left[i], "Error: This expression cannot be assigned to");
						return false;
					}

					if (i >= 1) {
						markDeclarationsAsPointedTo(comma->left[i]);
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

			if (call->function->type == &TYPE_OVERLOAD_SET) {
				assert(call->function->flavor == ExprFlavor::OVERLOAD_SET);
				auto overload = static_cast<ExprOverloadSet *>(call->function);

				bool yield;
				do {

					if (!checkOverloadSet(job, overload->currentOverload, &yield))
						return yield;

					struct OverloadMatchCost {
						Declaration *declaration;
						u32 argumentCount[ConversionCost::MAX_VALID];
					};

					Array<OverloadMatchCost> matches;
					at_exit{
						matches.free();
					};

					auto it = overloads(overload);

					while (Declaration *declaration = it.next()) {
						assert(declaration->initialValue->flavor == ExprFlavor::FUNCTION);

						OverloadMatchCost conversions { declaration };

						auto function = static_cast<ExprFunction *>(declaration->initialValue);

						assert(function->type != &TYPE_POLYMORPHIC_FUNCTION);

						if (!checkArgumentsForOverload(job, &call->arguments, &function->arguments, function, conversions.argumentCount, &yield)) {
							if (yield)
								return true;
							else
								continue;
						}

						if (matches.count) {
							for (u32 i = 0; i < ConversionCost::MAX_VALID; i++) {
								if (conversions.argumentCount[i] < matches[0].argumentCount[i]) {
									goto skip;
								}
								else if (conversions.argumentCount[i] > matches[0].argumentCount[i]) {
									matches.clear();
									break;
								}
							}

							matches.add(conversions);
						skip:;
						}
						else {
							matches.add(conversions);
						}
					}

					if (matches.count > 1) {
						reportError(call, "Error: Multiple overloads of match the given arguments");

						for (auto match : matches) {
							reportError(match.declaration, "");
						}

						return false;
					}
					else if (matches.count == 1) {
						call->function = matches[0].declaration->initialValue;
						break;
					}
				} while (nextOverloadSet(job, overload, &yield));

				if (yield)
					return true;

				// We didn't find any matching overloads so do error reporting
				if (call->function->flavor == ExprFlavor::OVERLOAD_SET) {
					ArraySet<Declaration *> overloads;

					collectAllOverloads(&overloads, overload);

					if (overloads.size() == 1) {
						// If there is only one overload go to the normal call handling code to report a specific error
						call->function = overloads[0]->initialValue;
					}
					else {
						reportError(call, "Error: No overloads match the type given arguments");

						for (auto overload : overloads) {
							reportError(overload, "");
						}

						return false;
					}
				}
			}

			String functionName = call->function->valueOfDeclaration ? call->function->valueOfDeclaration->name : "function";

			Block *argumentNames = nullptr;
			Block *returnNames = nullptr;

			if (call->function->flavor == ExprFlavor::FUNCTION) {
				argumentNames = &static_cast<ExprFunction *>(call->function)->arguments;
				returnNames = &static_cast<ExprFunction *>(call->function)->returns;
			}
			else if (call->function->type == &TYPE_TYPE) {
				if (call->function->flavor != ExprFlavor::TYPE_LITERAL) {
					reportError(call, "Error: Cannot create a polymorph of a non-constant type");
					return false;
				}
				auto type = static_cast<ExprLiteral *>(call->function)->typeValue;

				if (type->flavor != TypeFlavor::STRUCT) {
					reportError(call, "Error: Can only create a polymorph of a struct type");
					return false;
				}

				if (!(type->flags & TYPE_IS_POLYMORPHIC)) {
					reportError(call, "Error: Cannot polymorph a non-polymorphic struct");
					return false;
				}

				argumentNames = &static_cast<TypeStruct *>(type)->constants;
			}

			if (call->progress.phase == InferProgress::Phase::NONE) {
				if (call->flags & EXPR_FUNCTION_CALL_IS_STATEMENT_LEVEL) {
					if (returnNames) {
						for (auto declaration : returnNames->declarations) {
							if (declaration->flags & DECLARATION_IS_MUST) {
								reportError(call, "Error: Cannot ignore the return value of the function call, it was marked as #must");
								reportError(declaration, "   ..: Here is the function");
								return false;
							}
						}
					}

					if (call->function->type == &TYPE_TYPE) {
						reportError(call, "Error: Cannot have a naked struct polymorph at statement level");
						return false;
					}
				}
				else if (call->flags & EXPR_FUNCTION_CALL_IS_IN_COMMA_ASSIGNMENT) {
					if (call->function->type == &TYPE_TYPE) {
						reportError(call, "Error: Cannot assign a struct polymorph to multiple values");
						return false;
					}
				}
				else {
					if (returnNames) {
						for (u32 i = 1; i < returnNames->declarations.count; i++) {
							auto declaration = returnNames->declarations[i];

							if (declaration->flags & DECLARATION_IS_MUST) {
								reportError(call, "Error: Cannot ignore the return value of the function call, it was marked as #must");
								reportError(declaration, "   ..: Here is the function");
								return false;
							}
						}
					}
				}

				if (!argumentNames && call->arguments.names) {
					reportError(call, "Error: Cannot use named arguments with a non-constant function"); // @Improvement better message
					return false;
				}

				if (call->function->type->flavor == TypeFlavor::FUNCTION && !(call->function->type->flags & TYPE_FUNCTION_IS_C_CALL) && !(call->flags & EXPR_CONTEXT_AVAILABLE)) {
					reportError(call, "Error: Can only call #c_call functions without an active context. Use push_context to add a context");
					return false;
				}

				if (argumentNames) {
					if (!sortArguments(job, &call->arguments, argumentNames, "argument", call, call->function)) {
						return false;
					}
				}

				call->progress.phase = InferProgress::Phase::ARGUMENTS_SORTED;
			}

			if (call->function->type == &TYPE_POLYMORPHIC_FUNCTION) {
				assert(call->function->flavor == ExprFlavor::FUNCTION);

				auto function = static_cast<ExprFunction *>(call->function);

				bool yield = false;
				if (!doPolymorphMatchingForCall(&call->arguments, function, &yield))
					return yield;

				u32 nonPolymorphicCount = 0;
				for (u32 i = 0; i < function->arguments.declarations.count; i++) {
					if (!(function->arguments.declarations[i]->flags & DECLARATION_VALUE_POLYMORPHIC)) {
						call->arguments.values[nonPolymorphicCount++] = call->arguments.values[i];
					}
				}
				call->arguments.count = nonPolymorphicCount;

#if BUILD_DEBUG
				for (auto constant : function->constants.declarations) {
					assert(constant->initialValue);
				}
#endif

				ExprFunction *existingPolymorph = nullptr;

				for (auto polymorph : function->polymorphs) {
					if (constantsBlockMatches(&function->constants, &polymorph->constants)) {
						existingPolymorph = polymorph;
						break;
					}
				}

				if (!existingPolymorph) {
					auto newFunction = polymorphFunction(function);
					function->polymorphs.add(newFunction);

					addFunction(newFunction);

					call->function = newFunction;

				}
				else {
					call->function = existingPolymorph;
				}

				beginFlatten(job, &call->function);
				continue;
			}

			if (call->function->type->flavor != TypeFlavor::FUNCTION && call->function->type != &TYPE_TYPE) {
				reportError(call->function, "Error: Cannot call a %.*s", STRING_PRINTF(call->function->type->name));
				return false;
			}


			if (argumentNames) {
				bool yield;

				if (!inferArguments(job, &call->arguments, argumentNames, &yield)) {
					return yield;
				}

				if (call->function->type == &TYPE_TYPE) {
					auto struct_ = static_cast<TypeStruct *>(static_cast<ExprLiteral *>(call->function)->typeValue);

					TypeStruct *existingPolymorph = nullptr;

					for (auto polymorph : struct_->polymorphs) {
						if (constantsBlockMatches(&polymorph->constants, &call->arguments)) {
							existingPolymorph = polymorph;
							break;
						}
					}

					if (!existingPolymorph) {
						existingPolymorph = polymorphStruct(struct_, &call->arguments);
						struct_->polymorphs.add(existingPolymorph);
						// @Incomplete for now polymorphed structs will have the name of the struct they were polymorphed from
						addTypeBlock(existingPolymorph, call->function->valueOfDeclaration);
					}


					*exprPointer = inferMakeTypeLiteral(call->start, call->end, existingPolymorph);
				}
				else {
					auto function = static_cast<TypeFunction *>(call->function->type);
					expr->type = function->returnTypes[0];
					addSizeDependency(job->sizeDependencies, expr->type);
				}
			}
			else {
				assert(call->function->type->flavor == TypeFlavor::FUNCTION);
				auto function = static_cast<TypeFunction *>(call->function->type);
				if (call->arguments.count != function->argumentCount || function->isVarargs) {

					if (function->isVarargs) {
						if (call->arguments.count < function->argumentCount - 1) {
							reportError(call, "Error: Too few arguments for %.*s (Expected: %" PRIu32 ", Given: %" PRIu32 ")",
								STRING_PRINTF(functionName), function->argumentCount, call->arguments.count);
							return false;
						}
						else if (call->arguments.count == function->argumentCount - 1) {
							auto newArguments = INFER_NEW_ARRAY(Expr *, function->argumentCount);

							for (u64 i = 0; i < call->arguments.count; i++) {
								newArguments[i] = call->arguments.values[i];
							}

							
							auto literal = createIntLiteral(call->start, call->end, function->argumentTypes[function->argumentCount - 1], 0);
							literal->flags |= EXPR_IS_SPREAD;
							newArguments[call->arguments.count] = literal;
							call->arguments.values = newArguments;
							call->arguments.count = function->argumentCount;
						}
						else {
							auto array = INFER_NEW(ExprArrayLiteral);
							array->flavor = ExprFlavor::ARRAY_LITERAL;
							array->start = call->arguments.values[function->argumentCount - 1]->start;

							Type *varargsType = static_cast<TypeArray *>(function->argumentTypes[function->argumentCount - 1])->arrayOf;

							array->count = call->arguments.count - function->argumentCount + 1;
							array->values = INFER_NEW_ARRAY(Expr *, array->count);

							array->type = getStaticArray(varargsType, array->count);

							addSizeJobIfNeeded(array->type);

							for (u64 i = 0; i < array->count; i++) {
								bool yield = false;
								auto &argument = call->arguments.values[function->argumentCount + i - 1];

								if (!assignOp(job, argument, varargsType, argument, &yield)) {
									return yield;
								}

								array->values[i];
								array->end = argument->end;
							}

							array->flags |= EXPR_IS_SPREAD;

							call->arguments.values[function->argumentCount - 1] = array;
							call->arguments.count = function->argumentCount;
						}

					}
					else {
						if (call->arguments.count < function->argumentCount) {
							reportError(call, "Error: Too few arguments for %.*s (Expected: %" PRIu32 ", Given: %" PRIu32 ")",
								STRING_PRINTF(functionName), function->argumentCount, call->arguments.count);
						}
						else if (call->arguments.count > function->argumentCount) {
							reportError(call, "Error: Too many arguments for %.*s (Expected: %" PRIu32 ", Given: %" PRIu32 ")",
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

				expr->type = function->returnTypes[0];
				addSizeDependency(job->sizeDependencies, expr->type);
			}

			break;
		}
		case ExprFlavor::IF: {
			auto ifElse = static_cast<ExprIf *>(expr);

			bool yield;
			if (!coerceToBool(job, &ifElse->condition, &yield)) {
				return yield;
			}

			break;
		}
		case ExprFlavor::STATIC_IF: {
			auto staticIf = static_cast<ExprIf *>(expr);

			bool yield;
			if (!coerceToBool(job, &staticIf->condition, &yield))
				return yield;


			if (staticIf->condition->flavor != ExprFlavor::INT_LITERAL) {
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
					pushFlatten(job, block);
				}
			}
			break;
		}
		case ExprFlavor::RETURN: {
			auto return_ = static_cast<ExprReturn *>(expr);

			if (!return_->returnsFrom->type) {
				goToSleep(job, &return_->returnsFrom->sleepingOnInfer, "Return function header not ready");

				return true;
			}

			assert(return_->returnsFrom->type->flavor == TypeFlavor::FUNCTION);

			Block *returnTypes = &return_->returnsFrom->returns;

			assert(returnTypes->declarations.count >= 1);

			if (getDeclarationType(returnTypes->declarations[0]) == &TYPE_VOID) {
				if (returnTypes->declarations[0]->initialValue) {
					assert(returnTypes->declarations[0]->flags & DECLARATION_IS_RUN_RETURN);
					*exprPointer = returnTypes->declarations[0]->initialValue;

					assert(return_->returnsFrom->body->flavor == ExprFlavor::BLOCK);
					auto block = static_cast<ExprBlock *>(return_->returnsFrom->body);
					assert(block->exprs[block->exprs.count - 1] == returnTypes->declarations[0]->initialValue);
					block->exprs.add(return_);
				} else if (return_->returns.count) {
					reportError(return_->returns.values[0], "Error: A function with void return type cannot return a value");
					return false;
				}
			}
			else {
				if (return_->progress.phase == InferProgress::Phase::NONE) {
					if (!sortArguments(job, &return_->returns, returnTypes, "return", return_, return_->returnsFrom)) {
						return false;
					}

					return_->progress.phase = InferProgress::Phase::ARGUMENTS_SORTED;
				}

				bool yield;

				if (!inferArguments(job, &return_->returns, returnTypes, &yield)) {
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
				if (value->type == &TYPE_UNARY_DOT) {
					unary->type = value->type;
				}
				else if (value->type->flavor == TypeFlavor::INTEGER) {
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
				bool yield;
				if (!coerceToBool(job, &unary->value, &yield))
					return yield;

				unary->type = &TYPE_BOOL;

				if (value->flavor == ExprFlavor::INT_LITERAL) {
					*exprPointer = createInBoundsIntLiteral(unary->start, value->end, &TYPE_BOOL, static_cast<ExprLiteral *>(value)->unsignedValue == 0);
				}

				break;
			}
			case TokenT::SIZE_OF: {
				bool yield;
				if (!coerceToConstantType(job, &unary->value, "size_of", &yield))
					return yield;

				auto type = static_cast<ExprLiteral *>(value);

				if (type->typeValue == &TYPE_VOID) {
					reportError(value, "Error: Cannot take the size of void");
					return false;
				}

				if (type->typeValue->flavor == TypeFlavor::MODULE) {
					reportError(value, "Error: Cannot take the size of a module");
					return false;
				}

				if (type->typeValue->flags & TYPE_IS_POLYMORPHIC) {
					reportError(value, "Error: Cannot take the size of a polymorphic type");
					return false;
				}

				if (!type->typeValue->size) {
					goToSleep(job, &type->typeValue->sleepingOnMe, "Sizeof size not ready");

					return true;
				}

				assert(type->typeValue->size);

				auto literal = createIntLiteral(unary->start, unary->end, &TYPE_UNSIGNED_INT_LITERAL, type->typeValue->size);
				
				
				*exprPointer = literal;

				break;
			}
			case TokenT::ALIGN_OF: {
				bool yield;
				if (!coerceToConstantType(job, &unary->value, "align_of", &yield))
					return yield;

				auto type = static_cast<ExprLiteral *>(value);

				if (type->typeValue == &TYPE_VOID) {
					reportError(value, "Error: Cannot get the alignment of void");
					return false;
				}

				if (type->typeValue->flavor == TypeFlavor::MODULE) {
					reportError(value, "Error: Cannot take the align of a module");
					return false;
				}

				if (type->typeValue->flags & TYPE_IS_POLYMORPHIC) {
					reportError(value, "Error: Cannot take the align of a polymorphic type");
					return false;
				}

				if (!type->typeValue->size) {
					goToSleep(job, &type->typeValue->sleepingOnMe, "Alignof size not ready");

					return true;
				}

				assert(type->typeValue->size);
				assert(type->typeValue->alignment);

				auto literal = createIntLiteral(unary->start, unary->end, &TYPE_UNSIGNED_INT_LITERAL, type->typeValue->alignment);

				*exprPointer = literal;

				break;
			}
			case TokenT::TYPE_OF: {
				trySolidifyNumericLiteralToDefault(value);
				
				if (value->type == &TYPE_AUTO_CAST) {
					reportError(value, "Error: Cannot take the type of an auto casted value");
					return false;
				}
				else if (value->type == &TYPE_UNARY_DOT) {
					reportError(value, "Error: Cannot take the type of an unary dot value");
					return false;
				}
				else if (value->type == &TYPE_OVERLOAD_SET) {
					reportError(value, "Error: Cannot take the type of an overload set");
					return false;
				}
				else if (value->type == &TYPE_POLYMORPHIC_FUNCTION) {
					reportError(value, "Error: Cannot take the type of a polymorphic function");
					return false;
				}
				else if (value->type->flavor == TypeFlavor::MODULE) {
					reportError(value, "Error: Cannot take the type of a module");
					return false;
				}

				auto literal = inferMakeTypeLiteral(unary->start, unary->end, value->type);

				*exprPointer = literal;

				break;
			}
			case TokenT::IS_CONSTANT: {

				*exprPointer = createIntLiteral(unary->start, unary->end, &TYPE_BOOL, isLiteral(value));

				break;
			}
			case TokenT::TYPE_INFO: {
				if (value->type != &TYPE_TYPE) {
					bool yield = false;
					if (!assignOp(job, value, &TYPE_TYPE, value, &yield)) {
						return yield;
					}
				}

				Type *typeInfoType = nullptr;

				if (value->flavor == ExprFlavor::TYPE_LITERAL) {
					auto type = static_cast<ExprLiteral *>(value)->typeValue;
					
					switch (type->flavor) {
					case TypeFlavor::MODULE: {
						reportError(value, "Error: Cannot get type_info for a module");
						return false;
					}
					case TypeFlavor::VOID:
					case TypeFlavor::FLOAT:
					case TypeFlavor::BOOL:
					case TypeFlavor::TYPE:
					case TypeFlavor::STRING:
						typeInfoType = TYPE_TYPE_INFO;
						break;
					case TypeFlavor::INTEGER:
						typeInfoType = TYPE_TYPE_INFO_INTEGER;
						break;
					case TypeFlavor::POINTER:
						typeInfoType = TYPE_TYPE_INFO_POINTER;
						break;
					case TypeFlavor::FUNCTION:
						typeInfoType = TYPE_TYPE_INFO_FUNCTION;
						break;
					case TypeFlavor::ARRAY:
						typeInfoType = TYPE_TYPE_INFO_ARRAY;
						break;
					case TypeFlavor::STRUCT:
						if (type->flags & TYPE_IS_POLYMORPHIC) {
							reportError(unary, "Error: Cannot get type_info for a polymorphic struct");
							return false;
						}
						typeInfoType = TYPE_TYPE_INFO_STRUCT;
						break;
					case TypeFlavor::ENUM:
						typeInfoType = TYPE_TYPE_INFO_ENUM;
						break;
					default:
						assert(false);
					}
				}
				else {
					typeInfoType = TYPE_TYPE_INFO;
				}


				if (!typeInfoType) {
					reportError(unary, "Internal Compiler Error: Type_Info was used before it's declaration was found (this may be caused by modifying the runtime module)");
					return false;
				}
				else {
					unary->type = getPointer(typeInfoType);
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
				if (value->type == &TYPE_AUTO_CAST) {
					bool yield = false;
					if (!tryAutoCast(job, &value, &TYPE_TYPE, &yield)) {
						if (!yield) {
							reportError(value, "Error: Cannot convert %.*s to type",
								STRING_PRINTF(static_cast<ExprBinaryOperator *>(value)->right->type->name));
							return false;
						}
						else {
							return true;
						}
					}
				}

				if (value->flavor == ExprFlavor::TYPE_LITERAL) {
					auto literal = static_cast<ExprLiteral *>(value);

					if (literal->typeValue->flavor == TypeFlavor::MODULE) {
						reportError(unary, "Error: Cannot take a pointer to a module");
						return false;
					}
					if (literal->typeValue->flags & TYPE_IS_POLYMORPHIC) {
						reportError(unary, "Error: Cannot take a pointer to a polymorphic struct");
						return false;
					}

					*exprPointer = inferMakeTypeLiteral(unary->start, value->end, getPointer(literal->typeValue));
					(*exprPointer)->valueOfDeclaration = expr->valueOfDeclaration;
				}
				else {
					if (!isAddressable(value)) {
						reportError(value, "Error: Cannot take an address to something that has no storage");
						return false;
					}

					auto pointer = getPointer(value->type);

					unary->type = pointer;
					markDeclarationsAsPointedTo(unary->value);

				}

				break;
			}
			}

			break;
		}
		case ExprFlavor::WHILE: {
			auto loop = static_cast<ExprLoop *>(expr);

			bool yield;
			if (!coerceToBool(job, &loop->whileCondition, &yield))
				return yield;

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
				goToSleep(job, &increment->previous->sleepingOnMyValue, "Enum previous value not ready");

				return true;
			}

			break;
		}
		default:
			assert(false);
		}

		++job->indices[layer];
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
static RunJob *firstFreeRunJob;
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

RunJob *allocateRunJob() {
	if (firstFreeRunJob) {
		auto result = firstFreeRunJob;
		firstFreeRunJob = firstFreeRunJob->next;

		result->checkingFunctionIndices.clear();
		result->checkingFunctions.clear();
		result->generatingTypeInfos.clear();

		return result;
	}

	return new (inferJobAllocator.allocate(sizeof(RunJob))) RunJob;
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

		result->header.flattenedCount = 0;
		result->body.flattenedCount = 0;
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

void freeJob(RunJob *job) {
	job->next = firstFreeRunJob;
	firstFreeRunJob = job;
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
			reportError(declaration, "Internal Compiler Error: %.s must be a type");
			return false;
		}

		value = static_cast<ExprLiteral *>(declaration->initialValue)->typeValue;

		if (value->flavor != TypeFlavor::STRUCT) {
			reportError(declaration, "Internal Compiler Error: %.s must be a struct");
			return false;
		}

		if (name == "Type_Info_Struct") {
			auto member = findDeclarationNoYield(&static_cast<TypeStruct *>(value)->members, "Member");
			if (!member) {
				reportError(declaration, "Internal Compiler Error: Could not find decalration for Type_Info_Struct.Member");
				return false;
			}

			if (!(member->flags & DECLARATION_IS_CONSTANT)) {
				reportError(member, "Internal Compiler Error: Declaration for Type_Info_Struct.Member must be a constant", STRING_PRINTF(name));
				return false;
			}
			else if (!member->initialValue || member->initialValue->flavor != ExprFlavor::TYPE_LITERAL) {
				reportError(declaration, "Internal Compiler Error: Type_Info_Struct.Member must be a type");
				return false;
			}

			TYPE_TYPE_INFO_STRUCT_MEMBER = static_cast<ExprLiteral *>(member->initialValue)->typeValue;
		}
		else if (name == "Type_Info_Enum") {
			auto member = findDeclarationNoYield(&static_cast<TypeStruct *>(value)->members, "Value");
			if (!member) {
				reportError(declaration, "Internal Compiler Error: Could not find decalration for Type_Info_Enum.Value");
				return false;
			}

			if (!(member->flags & DECLARATION_IS_CONSTANT)) {
				reportError(member, "Internal Compiler Error: Declaration for Type_Info_Enum.Value must be a constant", STRING_PRINTF(name));
				return false;
			}
			else if (!member->initialValue || member->initialValue->flavor != ExprFlavor::TYPE_LITERAL) {
				reportError(declaration, "Internal Compiler Error: Type_Info_Enum.Value must be a type");
				return false;
			}

			TYPE_TYPE_INFO_ENUM_VALUE = static_cast<ExprLiteral *>(member->initialValue)->typeValue;
		}
	}

	return true;
}

bool checkFunctionDeclaration(Declaration *declaration, ExprFunction *&value, String name) {
	if (declaration->name == name) {
		if (value) {
			reportError(declaration, "Internal Compiler Error: Cannot declare %.*s more than once", STRING_PRINTF(name));
		}

		if (!(declaration->flags & DECLARATION_IS_CONSTANT)) {
			reportError(declaration, "Internal Compiler Error: Declaration for %.*s must be a constant", STRING_PRINTF(name));
			return false;
		}
		else if (!declaration->initialValue || declaration->initialValue->flavor != ExprFlavor::FUNCTION) {
			reportError(declaration, "Internal Compiler Error: %.*s must be a function", STRING_PRINTF(name));
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
		FUNCTION_DECLARATION(programStart, PROGRAM_START)
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

void addImporter(Importer *importer, Module *module) {
	PROFILE_FUNC();

	if (!importer->enclosingScope) {
		addImporterToBlock(&module->members, importer, 0);
	}

	assert(importer->enclosingScope);


	++totalImporters;

	auto job = allocateImporterJob();
	job->importer = importer;

	beginFlatten(job, &importer->import);

	addJob(&importerJobs, job);

	addSubJob(job);
}

bool importDeclarationIntoModule(Block *module, Declaration *declaration, Expr *using_) {
	assert(!(declaration->flags & DECLARATION_IS_MODULE_SCOPE));
	Declaration *potentialOverloadSet;

	if (!checkForRedeclaration(module, declaration, &potentialOverloadSet, using_))
		return false;

	auto import = INFER_NEW(Declaration);
	import->start = declaration->start;
	import->end = declaration->end;
	import->name = declaration->name;
	import->flags = declaration->flags;
	import->flags &= ~DECLARATION_OVERLOADS_LOCKED;

	if (declaration->flags & DECLARATION_IMPORTED_BY_USING) {
		import->import = declaration->import;
	}
	else {
		import->import = declaration;
	}
	import->initialValue = nullptr;

	import->flags |= DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_MODULE_SCOPE;

	addDeclarationToBlockUnchecked(module, import, potentialOverloadSet, 0);

	wakeUpSleepers(&module->sleepingOnMe, declaration->name);

	return true;
}

bool maybeAddDeclarationToImports(Module *module, Declaration *declaration) {
	assert(module->members.module);

	if (declaration->flags & DECLARATION_IS_MODULE_SCOPE)
		return true;

	for (auto import : module->imports) {
		if (!importDeclarationIntoModule(import->enclosingScope, declaration, import->import))
			return false;
	}

	return true;
}

void forceAddDeclaration(Declaration *declaration) {
	PROFILE_FUNC();

	if (declaration->flags & (DECLARATION_IS_ITERATOR_INDEX | DECLARATION_IS_IN_COMPOUND | DECLARATION_IS_ITERATOR))
		return;

	if ((declaration->flags & DECLARATION_TYPE_IS_READY) && (declaration->flags & DECLARATION_VALUE_IS_READY)) {
		return;
	}

	// Fast path for declarations that are trivial to infer
	if (declaration->flags & DECLARATION_IS_CONSTANT) {
		if (!declaration->type && declaration->initialValue && declaration->initialValue->type == &TYPE_UNSIGNED_INT_LITERAL) {
			declaration->type = inferMakeTypeLiteral(declaration->start, declaration->end, &TYPE_UNSIGNED_INT_LITERAL);
			declaration->flags |= DECLARATION_TYPE_IS_READY | DECLARATION_VALUE_IS_READY;
			wakeUpSleepers(&declaration->sleepingOnMyType);
			wakeUpSleepers(&declaration->sleepingOnMyValue);
			declaration->sleepingOnMyType.free();
			declaration->sleepingOnMyValue.free();
			return;
		}
		else if (!declaration->type && declaration->initialValue && declaration->initialValue->flavor == ExprFlavor::FUNCTION) {
			addFunction(static_cast<ExprFunction *>(declaration->initialValue));

			return;
		}
	}
	else if (declaration->enclosingScope && declaration->enclosingScope->flavor != BlockFlavor::GLOBAL) {

		if (!declaration->type && declaration->initialValue && declaration->initialValue->flavor == ExprFlavor::INT_LITERAL) {
			trySolidifyNumericLiteralToDefault(declaration->initialValue);
			declaration->type = inferMakeTypeLiteral(declaration->start, declaration->end, declaration->initialValue->type);
			declaration->flags |= DECLARATION_TYPE_IS_READY | DECLARATION_VALUE_IS_READY;
			wakeUpSleepers(&declaration->sleepingOnMyType);
			wakeUpSleepers(&declaration->sleepingOnMyValue);
			declaration->sleepingOnMyType.free();
			declaration->sleepingOnMyValue.free();
			return;
		}
		/* TODO This fast path doesn't check struct polymorphism
		else if (declaration->type && declaration->type->flavor == ExprFlavor::TYPE_LITERAL && !declaration->initialValue && (declaration->flags & (DECLARATION_IS_ARGUMENT | DECLARATION_IS_UNINITIALIZED | DECLARATION_IS_RETURN))) {
			declaration->flags |= DECLARATION_TYPE_IS_READY;

			auto type = static_cast<ExprLiteral *>(declaration->type);

			if (type->typeValue != &TYPE_CONTEXT)
				addTypeBlock(type->typeValue, declaration);

			wakeUpSleepers(&declaration->sleepingOnMyType);
			declaration->sleepingOnMyType.free();
			return;
		}*/
		else if (!declaration->type && declaration->initialValue && declaration->initialValue->flavor == ExprFlavor::FUNCTION) {
			addFunction(static_cast<ExprFunction *>(declaration->initialValue));

			return;
		}
	}

	++totalInferredDeclarations;


	DeclarationJob *job = allocateDeclarationJob();
	job->declaration = declaration;
	job->declaration->inferJob = job;

	if (declaration->type) {
		beginFlatten(&job->type, &declaration->type);
		addSubJob(&job->type);
	}

	if (declaration->initialValue) {
		beginFlatten(&job->value, &declaration->initialValue);
		addSubJob(&job->value);
	}

	addJob(&declarationJobs, job);

	return;
}

bool addDeclaration(Declaration *declaration, Module *module) {
	PROFILE_FUNC();

	++totalDeclarations;

	if (declaration->flags & DECLARATION_IS_IN_COMPOUND)
		return true;
	assert(!(declaration->flags & (DECLARATION_IS_ITERATOR | DECLARATION_IS_ITERATOR_INDEX | DECLARATION_IS_IN_COMPOUND)));

	if (!declaration->enclosingScope) {
		PROFILE_ZONE("Add declaration to global block");

		if (!addDeclarationToBlock(&module->members, declaration, 0)) {
			return false;
		}

		if (declaration->name == "main") {
			if (entryPoint) {
				reportError(declaration, "Error: Cannot define multiple entry points");
				reportError(entryPoint, "   ..: Here is the previous declaration");
				return false;
			}

			if (!(declaration->flags & DECLARATION_IS_CONSTANT)) {
				reportError(declaration, "Error: The entry point must be a constant declaration");
				return false;
			}

			entryPoint = declaration;
			wakeUpSleepers(&waitingOnEntryPoint);
		}

		wakeUpSleepers(&module->members.sleepingOnMe, declaration->name);

		if (!maybeAddDeclarationToImports(module, declaration))
			return false;
	}

	assert(declaration->enclosingScope);

	if (module == runtimeModule && !checkBuiltinDeclaration(declaration)) {
		return false;
	}
	
	if (noDce || declaration->enclosingScope->flavor != BlockFlavor::GLOBAL || declaration->enclosingScope == &mainModule->members || declaration->enclosingScope == &runtimeModule->members)
		forceAddDeclaration(declaration);

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

		for (u32 i = 0; i < loop.count; i++) {
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

		for (u32 i = 0; i < loop.count; i++) {
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

bool inferImporter(SubJob *subJob) {
	PROFILE_FUNC();
	auto job = static_cast<ImporterJob *>(subJob);

	++totalInferImporters;

	auto importer = job->importer;

	Block *block = nullptr;
	Expr *structAccess = nullptr;

	bool onlyConstants = false;

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
	else if (importer->import->flavor == ExprFlavor::IMPORT) { 
		// @Hack @Cleanup There is no way to tell the difference between a normal import and
		// std :: #import "Standard"; using std
		// since the constant gets subsituted into the Importer struct, 
		// causing the using to become a #import
		// To fix this when import is parsed other than at the top level, 
		// it is flagged as an expression
		// :ImportExprFlagging
		if (importer->import->flags & EXPR_IMPORT_IS_EXPR) {
			reportError(importer->import, "Error: Cannot using an import");
			return false;
		}

		auto import = static_cast<ExprLoad *>(importer->import);

		for (auto block : import->module->imports) {
			if (block->enclosingScope == importer->enclosingScope) {
				goto done;
			}
		}

		import->module->imports.add(importer);


		for (auto member : import->module->members.declarations) {
			do {
				if (member->flags & DECLARATION_IS_MODULE_SCOPE)
					continue;

				if (!importDeclarationIntoModule(importer->enclosingScope, member, importer->import))
					return false;
			} while (member = member->nextOverload);
		}
	done:;
	}
	else if (importer->import->flavor != ExprFlavor::LOAD) {

		block = &getExpressionNamespace(importer->import, &onlyConstants, importer->import)->members;

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
		}
	}

	if (block) {
		if (importer->import->flavor == ExprFlavor::STATIC_IF) {
			assert(!onlyConstants);
			assert(!structAccess);

			for (auto member : block->importers) {
				addImporterToBlock(importer->enclosingScope, member, member->serial);

				addImporter(member, nullptr);
			}

			block->importers.clear();

			for (auto member : block->declarations) {
				Declaration *potentialOverloadSet;
				if (!checkForRedeclaration(importer->enclosingScope, member, &potentialOverloadSet, importer->import))
					return false;
				
				addDeclarationToBlockUnchecked(importer->enclosingScope, member, potentialOverloadSet, member->serial);

				if (importer->enclosingScope->flavor == BlockFlavor::STRUCT) {
					if (!(member->flags & DECLARATION_IS_CONSTANT)) {
						// Do an insertion sort by declaration serial since struct members must be ordered to preserve memory layout
						// @Speed create space big enough for all the inserted members then just sort the inserted members
						u32 index = importer->enclosingScope->declarations.count - 1;

						while (index > 0 && member->serial < importer->enclosingScope->declarations[index - 1]->serial) {
							importer->enclosingScope->declarations[index] = importer->enclosingScope->declarations[index - 1];

							index--;
						}

						importer->enclosingScope->declarations[index] = member;
					}
				}

				wakeUpSleepers(&importer->enclosingScope->sleepingOnMe, member->name);

				do {
					if (importer->enclosingScope->module) {
						auto module = CAST_FROM_SUBSTRUCT(Module, members, importer->enclosingScope);
						if (!maybeAddDeclarationToImports(module, member))
							return false;
					}

					if (!addDeclaration(member, nullptr))
						return false;
				} while (member = member->nextOverload);
			}

			block->declarations.clear();

			auto exprBlock = CAST_FROM_SUBSTRUCT(ExprBlock, declarations, block);

			if (importer->enclosingScope->module) {
				for (auto expr : exprBlock->exprs) {
					assert(expr->flavor == ExprFlavor::RUN);
					addRunJob(static_cast<ExprRun *>(expr));
				}

				exprBlock->exprs.clear();
			}
		}
		else if (importer->import->flavor != ExprFlavor::LOAD) {
			for (u32 i = 0; i < block->importers.count; i++) {
				goToSleep(job, &block->sleepingOnMe, "Importer waiting for importer");
			}

			if (block->importers.count)
				return true;



			for (auto member : block->declarations) {
				if (block->flavor == BlockFlavor::ENUM && !(member->flags & DECLARATION_IS_ENUM_VALUE))
					continue;

				if (onlyConstants && !(member->flags & DECLARATION_IS_CONSTANT))
					continue;

				Declaration *potentialOverloadSet;

				if (!checkForRedeclaration(importer->enclosingScope, member, &potentialOverloadSet, importer->import))
					return false;

				auto oldMember = member;

				do {
					auto import = INFER_NEW(Declaration);
					import->start = member->start;
					import->end = member->end;
					import->name = member->name;
					import->flags = member->flags;
					import->flags &= ~DECLARATION_OVERLOADS_LOCKED;

					if (importer->moduleScope)
						import->flags |= DECLARATION_IS_MODULE_SCOPE;


					if (member->flags & DECLARATION_IS_CONSTANT) {
						if (member->flags & DECLARATION_IMPORTED_BY_USING) {
							import->import = member->import;
						}
						else {
							import->import = member;
						}
						import->initialValue = nullptr;
					}
					else {
						import->import = member;
						import->initialValue = importer->import;
					}

					import->flags |= DECLARATION_IMPORTED_BY_USING;

					addDeclarationToBlockUnchecked(importer->enclosingScope, import, potentialOverloadSet, importer->serial);

					assert(import->name.length);

					if (importer->enclosingScope->module) {
						auto module = CAST_FROM_SUBSTRUCT(Module, members, importer->enclosingScope);
						if (!maybeAddDeclarationToImports(module, import))
							return false;
					}
				} while (member = member->nextOverload);

				wakeUpSleepers(&importer->enclosingScope->sleepingOnMe, oldMember->name);
			}
		}
	}

	if (!importer->enclosingScope->module)
		wakeUpSleepers(&importer->enclosingScope->sleepingOnMe);

	importer->enclosingScope->importers.unordered_remove(importer);

	removeJob(&importerJobs, job);
	freeJob(job);

	return true;
}


bool inferDeclarationType(SubJob *subJob) {
	PROFILE_FUNC();
	auto job = CAST_FROM_SUBSTRUCT(DeclarationJob, type, subJob);

	++totalInferDeclarationTypes;

	auto declaration = job->declaration;

	if (!(declaration->flags & DECLARATION_TYPE_IS_READY) && declaration->type) {
		bool yield = false;
		if (!coerceToConstantType(subJob, &declaration->type, "Declaration type", &yield)) {
			return yield;
		}

		declaration->flags |= DECLARATION_TYPE_IS_READY;
		auto type = getDeclarationType(declaration);

		if (type == &TYPE_VOID) {
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

		if (type->flavor == TypeFlavor::MODULE) {
			reportError(declaration->type, "Error: Declaration type cannot be a module");
			return false;
		}

		if (type->flags & TYPE_IS_POLYMORPHIC) {
			reportError(declaration->type, "Error: Declaration type cannot be polymorphic");
			return false;
		}

		wakeUpSleepers(&declaration->sleepingOnMyType);
		declaration->sleepingOnMyType.free();
	}

	if (declaration->type && !declaration->initialValue) {
		if ((declaration->flags & DECLARATION_IS_EXPLICIT_DEFAULT) || !(declaration->flags & (DECLARATION_IS_UNINITIALIZED | DECLARATION_IS_ITERATOR | DECLARATION_IS_ITERATOR_INDEX | DECLARATION_IS_ARGUMENT | DECLARATION_IS_RETURN))) {
			auto type = getDeclarationType(declaration);

			bool yield;
			declaration->initialValue = getDefaultValue(&job->type, type, &yield);

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


		if (!(declaration->flags & DECLARATION_IS_CONSTANT) && declaration->enclosingScope->flavor == BlockFlavor::GLOBAL) {
			declarationWaitingOnSize.add(job);
		}
		else {
			freeJob(job);
		}
	}

	return true;
}

bool inferDeclarationValue(SubJob *subJob) {
	PROFILE_FUNC();
	auto job = CAST_FROM_SUBSTRUCT(DeclarationJob, value, subJob);

	++totalInferDeclarationValues;

	auto declaration = job->declaration;

	assert(declaration->initialValue);

	if (declaration->type && declaration->initialValue) {
		if (declaration->flags & DECLARATION_VALUE_IS_READY) return true;

		if (!(declaration->flags & DECLARATION_TYPE_IS_READY)) {
			goToSleep(&job->value, &declaration->sleepingOnMyType, "Declaration type not ready");
			return true;
		}

		assert(declaration->initialValue->type);

		Type *correct = getDeclarationType(declaration);

		if (declaration->flags & DECLARATION_IS_ENUM_VALUE) {
			assert(correct->flavor == TypeFlavor::ENUM);

			if (declaration->initialValue->type == &TYPE_SIGNED_INT_LITERAL || declaration->initialValue->type == &TYPE_UNSIGNED_INT_LITERAL) {
				assert(declaration->initialValue->flavor == ExprFlavor::INT_LITERAL);

				if (!static_cast<TypeEnum *>(correct)->integerType) {
					goToSleep(&job->value, &correct->sleepingOnMe, "Enum base type not ready");

					return true;
				}

				if (!boundsCheckImplicitConversion(declaration->initialValue, static_cast<TypeEnum *>(correct)->integerType, static_cast<ExprLiteral *>(declaration->initialValue))) {
					reportError(CAST_FROM_SUBSTRUCT(ExprEnum, struct_, static_cast<TypeEnum *>(correct)), "   ..: Here is the enum declaration");

					return false;
				}

				declaration->initialValue->type = correct;
			}
		}

		bool yield;
		if (!assignOp(&job->value, declaration->initialValue, correct, declaration->initialValue, &yield)) {
			return yield;
		}

		if ((declaration->flags & DECLARATION_IS_CONSTANT) )
		if ((declaration->flags & DECLARATION_IS_CONSTANT) || declaration->enclosingScope->flavor != BlockFlavor::IMPERATIVE) {
			if (!isLiteral(declaration->initialValue)) {
				reportError(declaration->type, "Error: Declaration value must be a constant");
				return false;
			}
		}

		removeJob(&declarationJobs, job);

		declaration->flags |= DECLARATION_VALUE_IS_READY;
		wakeUpSleepers(&declaration->sleepingOnMyValue);
		declaration->sleepingOnMyValue.free();

		if (!(declaration->flags & DECLARATION_IS_CONSTANT) && declaration->enclosingScope->flavor == BlockFlavor::GLOBAL) {
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

			if (declaration->initialValue->type == &TYPE_VOID && !(declaration->flags & DECLARATION_IS_RUN_RETURN)) {
				reportError(declaration, "Error: Declaration cannot have type void");
				return false;
			}
			else if (declaration->initialValue->type == &TYPE_AUTO_CAST) {
				reportError(declaration, "Error: Cannot infer the type of a declaration if the value is an auto cast");
				return false;
			}
			else if (!(declaration->flags & DECLARATION_IS_CONSTANT) && declaration->initialValue->type == &TYPE_ARRAY_LITERAL) {
				reportError(declaration, "Error: Cannot infer the type of a declaration if the value is an implicit array literal");
				return false;
			}
			else if (!(declaration->flags & DECLARATION_IS_CONSTANT) && declaration->initialValue->type == &TYPE_STRUCT_LITERAL) {
				reportError(declaration, "Error: Cannot infer the type of a declaration if the value is an implicit struct literal");
				return false;
			}
			else if (!(declaration->flags & DECLARATION_IS_CONSTANT) && declaration->initialValue->type->flavor == TypeFlavor::MODULE) {
				reportError(declaration, "Error: Modules may not be assigned to a non-constant declaration");
				return false;
			}
			else if (!(declaration->flags & DECLARATION_IS_CONSTANT) && (declaration->initialValue->type->flags & TYPE_IS_POLYMORPHIC)) {
				reportError(declaration, "Error: Polymorphic types may not be assigned to a non-constant declaration");
				return false;
			}

			if ((declaration->flags & DECLARATION_IS_CONSTANT) || declaration->enclosingScope->flavor != BlockFlavor::IMPERATIVE) {
				if ((declaration->flags & DECLARATION_IS_CONSTANT) && declaration->initialValue->type == &TYPE_OVERLOAD_SET) {

				}
				else if (!(declaration->flags & DECLARATION_IS_RUN_RETURN) && !isLiteral(declaration->initialValue)) {
					reportError(declaration, "Error: Declaration value must be a constant");
					return false;
				}
			}

			if (!(declaration->flags & DECLARATION_IS_CONSTANT) && declaration->initialValue->type == &TYPE_OVERLOAD_SET) {
				assert(declaration->initialValue->flavor == ExprFlavor::OVERLOAD_SET);
				auto overload = static_cast<ExprOverloadSet *>(declaration->initialValue);

				bool yield;

				if (!checkOverloadSet(&job->value, overload->currentOverload, &yield)) {
					return yield;
				}

				if (overload->currentOverload->nextOverload) {
					reportError(declaration, "Error: Cannot infer which overload to use for declaration value");
					return false;
				}
				else {
					auto overloadDecl = overload->currentOverload;

					if (overloadDecl->flags & DECLARATION_IMPORTED_BY_USING) {
						overloadDecl = overloadDecl->import;
					}

					assert(overloadDecl->initialValue && overloadDecl->initialValue->flavor == ExprFlavor::FUNCTION);
					declaration->initialValue = overloadDecl->initialValue;
				}
			}

			if (!(declaration->flags & DECLARATION_IS_CONSTANT) && declaration->initialValue->type == &TYPE_POLYMORPHIC_FUNCTION) {
				reportError(declaration, "Error: A polymorphic function cannot be assigned to a variable");
				return false;
			}

			if (!(declaration->flags & DECLARATION_IS_CONSTANT) && (declaration->initialValue->flags & EXPR_FUNCTION_IS_INSTRINSIC)) {
				reportError(declaration, "Error: An intrinsic function cannot be assigned to a variable");
				return false;
			}

			declaration->type = inferMakeTypeLiteral(declaration->start, declaration->end, declaration->initialValue->type);

			removeJob(&declarationJobs, job);

			declaration->flags |= DECLARATION_VALUE_IS_READY;
			declaration->flags |= DECLARATION_TYPE_IS_READY;
			wakeUpSleepers(&declaration->sleepingOnMyType);
			wakeUpSleepers(&declaration->sleepingOnMyValue);
			declaration->sleepingOnMyType.free();
			declaration->sleepingOnMyValue.free();


			if (!(declaration->flags & DECLARATION_IS_CONSTANT) && declaration->enclosingScope->flavor == BlockFlavor::GLOBAL) {
				declarationWaitingOnSize.add(job);
			}
			else {
				freeJob(job);
			}
		}
	}

	return true;
}

bool checkGuaranteedReturn(Expr *expr) {
	switch (expr->flavor) {
	case ExprFlavor::RETURN:
		return true;
	case ExprFlavor::BLOCK: {
		for (auto statement : static_cast<ExprBlock *>(expr)->exprs) {
			if (checkGuaranteedReturn(statement))
				return true;
		}

		return false;
	}
	case ExprFlavor::IF: {
		auto if_ = static_cast<ExprIf *>(expr);

		if (if_->condition->flavor == ExprFlavor::INT_LITERAL) {
			if (static_cast<ExprLiteral *>(if_->condition)->unsignedValue) {
				return if_->ifBody && checkGuaranteedReturn(if_->ifBody);
			}
			else {
				return if_->elseBody && checkGuaranteedReturn(if_->elseBody);
			}
		}
		else {
			return if_->ifBody && if_->elseBody && checkGuaranteedReturn(if_->ifBody) && checkGuaranteedReturn(if_->elseBody);
		}
	}
	case ExprFlavor::SWITCH: {
		auto switch_ = static_cast<ExprSwitch *>(expr);

		bool hasElse = false;
	
		for (u32 i = 0; i < switch_->cases.count; i++) {
			auto &case_ = switch_->cases[i];

			if (!case_.condition)
				hasElse = true;

			if (!checkGuaranteedReturn(case_.block) && !(case_.fallsThrough && i + 1 != switch_->cases.count)) {
				return false;
			}
		}

		return hasElse;
	}
	case ExprFlavor::PUSH_CONTEXT: {
		return checkGuaranteedReturn(static_cast<ExprBinaryOperator *>(expr)->right);
	}
	case ExprFlavor::WHILE: {
		// In the trivial case of a while true loop that contains no break statements
		// don't require a return after the loop

		if (expr->flags & EXPR_LOOP_HAS_BREAK)
			return false;

		auto loopCondition = static_cast<ExprLoop *>(expr)->whileCondition;

		return loopCondition->flavor == ExprFlavor::INT_LITERAL && static_cast<ExprLiteral *>(loopCondition)->unsignedValue;
	}
	default:
		return false;
	}
}

bool inferFunctionHeader(SubJob *subJob) {
	PROFILE_FUNC();
	auto job = CAST_FROM_SUBSTRUCT(FunctionJob, header, subJob);

	++totalInferFunctionHeaders;

	auto function = job->function;

	if (function->flags & EXPR_FUNCTION_IS_INSTRINSIC) {
		if (!function->valueOfDeclaration || !function->valueOfDeclaration->name) {
			reportError(function, "Error: Instrinsic functions cannot be anonymous");
			return false;
		}
	}

	if (function->flags & EXPR_FUNCTION_IS_POLYMORPHIC) {
		function->type = &TYPE_POLYMORPHIC_FUNCTION;
	}
	else {
		bool sleep = false;

		for (auto argument : function->arguments.declarations) {
			assert(argument);

			if (!(argument->flags & DECLARATION_TYPE_IS_READY)) {
				goToSleep(&job->header, &argument->sleepingOnMyType, "Function argument type not ready");

				sleep = true;
			}
		}

		for (auto return_ : function->returns.declarations) {
			assert(return_);

			if (!(return_->flags & DECLARATION_TYPE_IS_READY)) {
				goToSleep(&job->header, &return_->sleepingOnMyType, "Funtion return type not ready");

				sleep = true;
			}
			else if (getDeclarationType(return_) == &TYPE_VOID && function->returns.declarations.count != 1) {
				reportError(return_, "Error: Functions with multiple return values cannot return a void value");
				return false;
			}
		}


		if (sleep)
			return true;

		function->type = getFunctionType(function);
	}

	wakeUpSleepers(&function->sleepingOnInfer);

	if (function->valueOfDeclaration && !function->valueOfDeclaration->type && !function->valueOfDeclaration->inferJob) {
		function->valueOfDeclaration->type = inferMakeTypeLiteral(function->start, function->end, function->type);
		function->valueOfDeclaration->flags |= DECLARATION_TYPE_IS_READY | DECLARATION_VALUE_IS_READY;
		wakeUpSleepers(&function->valueOfDeclaration->sleepingOnMyType);
		wakeUpSleepers(&function->valueOfDeclaration->sleepingOnMyValue);
		function->valueOfDeclaration->sleepingOnMyType.free();
		function->valueOfDeclaration->sleepingOnMyValue.free();
	}

	if (!(function->flags & EXPR_FUNCTION_IS_POLYMORPHIC) && function->body) {
		beginFlatten(&job->body, &function->body);

		addSubJob(&job->body);
	}
	else {
		removeJob(&functionJobs, job);
		function->sleepingOnInfer.free();
		freeJob(job);
	}

	if (!(function->flags & EXPR_FUNCTION_IS_POLYMORPHIC)) {
		for (auto argument : function->arguments.declarations) {
			addSizeDependency(&job->sizeDependencies, getDeclarationType(argument));
		}

		for (auto return_ : function->returns.declarations) {
			addSizeDependency(&job->sizeDependencies, getDeclarationType(return_));
		}

		if (!(function->flags & EXPR_FUNCTION_IS_C_CALL)) {
			addSizeDependency(&job->sizeDependencies, &TYPE_CONTEXT);
		}
	}

	return true;
}

bool inferFunctionBody(SubJob *subJob) {
	PROFILE_FUNC();

	++totalInferFunctionBodies;

	auto job = CAST_FROM_SUBSTRUCT(FunctionJob, body, subJob);
	auto function = job->function;

	if (!function->type) {
		goToSleep(&job->body, &function->sleepingOnInfer, "Function header not ready");
		return true;
	}

	if (function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
		if (function->body->flavor != ExprFlavor::STRING_LITERAL) {
			reportError(function->body, "Error: External function library must be a constant string");
			return false;
		}

		libraries.add({ static_cast<ExprStringLiteral *>(function->body)->string, nullptr });

		if (!function->valueOfDeclaration) {
			reportError(function, "Error: External functions must be named");
			return false;
		}

		function->flags |= EXPR_FUNCTION_RUN_READY;
		wakeUpSleepers(&function->sleepingOnIr);

		CoffJob coff;
		coff.flavor = CoffJobFlavor::FUNCTION;
		coff.function = function;

		coffWriterQueue.add(coff);

		removeJob(&functionJobs, job);
		function->sleepingOnInfer.free();
		freeJob(job);
	}
	else {
		assert(job->function->body);

		if (function->flags & EXPR_FUNCTION_IS_COMPILER) {
			if (!function->valueOfDeclaration) {
				reportError(function, "Error: Compiler functions must be named");
				return false;
			}
		}

		if (!checkGuaranteedReturn(function->body)) {
			bool needsReturn = false;

			if (getDeclarationType(function->returns.declarations[0]) != &TYPE_VOID) {
				for (auto declaration : function->returns.declarations) {
					if (!declaration->initialValue)
						needsReturn = true;
				}
			}


			if (needsReturn) {
				reportError(job->function, "Error: Not all control paths return a value");
				return false;	
			}
			else {
				assert(function->body->flavor == ExprFlavor::BLOCK);

				auto return_ = INFER_NEW(ExprReturn);
				return_->flavor = ExprFlavor::RETURN;
				return_->start = function->start;
				return_->end = function->end;
				return_->returnsFrom = function;
				return_->returns.count = 0;

				auto body = static_cast<ExprBlock *>(function->body);
				beginFlatten(&job->body, &body->exprs.add(return_));

				subJobs.add(&job->body);
				return true;
			}
		}


		removeJob(&functionJobs, job);
		function->sleepingOnInfer.free();
		functionWaitingOnSize.add(job);
	}

	return true;
}


bool inferSize(SubJob *subJob) {
	PROFILE_FUNC();
	auto job = static_cast<SizeJob *>(subJob);

	auto type = job->type;

	if (type->flavor == TypeFlavor::STRUCT) {
		++totalInferStructSizes;

		auto struct_ = static_cast<TypeStruct *>(type);

		for (auto importer : struct_->members.importers) {
			if (importer->import->flavor == ExprFlavor::STATIC_IF) {
				goToSleep(job, &struct_->members.sleepingOnMe, "Struct sizing #if not complete");
				return true;
			}
		}

		s64 sleeping = -1;
		while (job->sizingIndexInMembers < struct_->members.declarations.count) {
			auto member = struct_->members.declarations[job->sizingIndexInMembers];


			if (!(member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING))) {
				if (!(member->flags & DECLARATION_TYPE_IS_READY)) {
					goToSleep(job, &member->sleepingOnMyType, "Struct sizing member type not ready");

					if (sleeping == -1) sleeping = job->sizingIndexInMembers;
				}
				else {
					auto memberType = getDeclarationType(member);

					if (!memberType->size) {
						goToSleep(job, &memberType->sleepingOnMe, "Struct sizing member size not ready");

						if (sleeping == -1) sleeping = job->sizingIndexInMembers;
					}

					if (sleeping == -1) {

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
				}
			}

			job->sizingIndexInMembers++;
		}

		if (sleeping != -1) {
			job->sizingIndexInMembers = static_cast<u32>(sleeping);
			return true;
		}

		if (job->sizingIndexInMembers == struct_->members.declarations.count) {
			if (job->runningSize == 0) { // Empty structs are allowed, but we can't have size 0 types
				struct_->alignment = 1;
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
		++totalInferArraySizes;

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
			goToSleep(job, &array->arrayOf->sleepingOnMe, "Array sizing element type not sized");
			return true;
		}
	}
	else if (type->flavor == TypeFlavor::ENUM) {
		++totalInferEnumSizes;

		auto struct_ = static_cast<TypeEnum *>(type);

		auto enum_ = CAST_FROM_SUBSTRUCT(ExprEnum, struct_, struct_);

		assert(enum_->struct_.members.declarations.count >= ENUM_SPECIAL_MEMBER_COUNT);
		auto integerType = enum_->struct_.members.declarations[enum_->struct_.members.declarations.count - 1];
		assert(integerType->name == "integer");
		

		if (!(integerType->flags & DECLARATION_TYPE_IS_READY)) {
			goToSleep(job, &integerType->sleepingOnMyType, "Enum waiting for type type");
			return true;
		}

		if (!(integerType->flags & DECLARATION_VALUE_IS_READY)) {
			goToSleep(job, &integerType->sleepingOnMyType, "Enum waiting for type value");
			return true;
		}

		bool yield;
		if (!coerceToConstantType(job, &integerType->initialValue, "enum type", &yield)) {
			assert(!yield);
			return false;
		}

		auto type = static_cast<ExprLiteral *>(integerType->initialValue)->typeValue;

		if (type->flavor != TypeFlavor::INTEGER) {
			reportError(integerType, "Error: enum type must be an integer");
			return false;
		}
		else if (type->flags & TYPE_INTEGER_IS_SIGNED) {
			reportError(integerType, "Error: enums cannot have a signed type");
			return false;
		}


		enum_->struct_.integerType = type;
		enum_->struct_.alignment = enum_->struct_.integerType->alignment;
		enum_->struct_.size = enum_->struct_.integerType->size;
		enum_->struct_.flags |= enum_->struct_.integerType->flags & TYPE_INTEGER_IS_SIGNED;

		wakeUpSleepers(&struct_->sleepingOnMe);

		removeJob(&sizeJobs, job);
		type->sleepingOnMe.free();
		freeJob(job);
	}
	else {
		assert(false);
	}

	return true;
}

void createTypeInfo(Type *type) {
	assert(!type->runtimeTypeInfo);

	switch (type->flavor) {
	case TypeFlavor::BOOL:
	case TypeFlavor::FLOAT:
	case TypeFlavor::STRING:
	case TypeFlavor::TYPE:
	case TypeFlavor::VOID:
		type->runtimeTypeInfo = INFER_NEW(Type_Info);
		break;
	case TypeFlavor::ARRAY:
		type->runtimeTypeInfo = INFER_NEW(Type_Info_Array);
		break;
	case TypeFlavor::ENUM:
		type->runtimeTypeInfo = INFER_NEW(Type_Info_Enum);
		break;
	case TypeFlavor::FUNCTION:
		type->runtimeTypeInfo = INFER_NEW(Type_Info_Function);
		break;
	case TypeFlavor::INTEGER:
		type->runtimeTypeInfo = INFER_NEW(Type_Info_Integer);
		break;
	case TypeFlavor::POINTER:
		type->runtimeTypeInfo = INFER_NEW(Type_Info_Pointer);
		break;
	case TypeFlavor::STRUCT:
		type->runtimeTypeInfo = INFER_NEW(Type_Info_Struct);
		break;
	default:
		assert(false);
	}
}

bool pushFunctionToRunCheck(RunJob *job, ExprFunction *function) {
	if (function->flags & EXPR_FUNCTION_RUN_CHECKED)
		return false;

	for (auto pending : job->checkingFunctions) {
		if (pending == function) {
			return false;
		}
	}

	job->checkingFunctions.add(function);
	job->checkingFunctionIndices.add(0);
	return true;
}

bool findTypeInfoRecurse(RunJob *job, ArraySet<Type *> *types, Type *type);

bool findTypeInfoInExprRecurse(RunJob *job, ArraySet<Type *> *types, Expr *expr) {
	switch (expr->flavor) {
	case ExprFlavor::ARRAY_LITERAL: {
		auto array = static_cast<ExprArrayLiteral *>(expr);

		for (u64 i = 0; i < array->count; i++) {
			if (!findTypeInfoInExprRecurse(job, types, array->values[i]))
				return false;
		}

		return true;
	}
	case ExprFlavor::STRUCT_LITERAL: {
		auto struct_ = static_cast<ExprStructLiteral *>(expr);

		for (u64 i = 0; i < struct_->initializers.count; i++) {
			if (!findTypeInfoInExprRecurse(job, types, struct_->initializers.values[i]))
				return false;
		}

		return true;
	}
	case ExprFlavor::TYPE_LITERAL: {
		return findTypeInfoRecurse(job, types, static_cast<ExprLiteral *>(expr)->typeValue);
	}
	case ExprFlavor::FUNCTION: {
		if (pushFunctionToRunCheck(job, static_cast<ExprFunction *>(expr))) {
			subJobs.add(job);
			return false;
		}

		return true;
	}
	default:
		return true;
	}
}

bool findTypeInfoRecurse(RunJob *job, ArraySet<Type *> *types, Type *type) {
	if (type->runtimeTypeInfo) {
		return true;
	}

	if (type->flavor == TypeFlavor::MODULE) { // @Incomplete Output type info for namespaces
		return true;
	}

	if (!type->size) {
		goToSleep(job, &type->sleepingOnMe, "Find type info type not sized");
		return false;
	}

	if (!types->add(type))
		return true;

	switch (type->flavor) {
	case TypeFlavor::BOOL:
	case TypeFlavor::FLOAT:
	case TypeFlavor::INTEGER:
	case TypeFlavor::STRING:
	case TypeFlavor::TYPE:
	case TypeFlavor::VOID:
		return true;
	case TypeFlavor::ARRAY: {
		auto array = static_cast<TypeArray *>(type);

		return findTypeInfoRecurse(job, types, array->arrayOf);
	}
	case TypeFlavor::ENUM: {
		auto enum_ = static_cast<TypeEnum *>(type);

		for (auto member : enum_->members.declarations) {
			if (!(member->flags & DECLARATION_IS_ENUM_VALUE)) 
				continue;
			if (!(member->flags & DECLARATION_VALUE_IS_READY)) {
				goToSleep(job, &member->sleepingOnMyValue, "Find type info enum value not ready");
				return false;
			}
		}

		return findTypeInfoRecurse(job, types, enum_->integerType);
	}
	case TypeFlavor::POINTER: {
		auto pointer = static_cast<TypePointer *>(type);

		return findTypeInfoRecurse(job, types, pointer->pointerTo);
	}
	case TypeFlavor::FUNCTION: {
		auto function = static_cast<TypeFunction *>(type);

		for (u64 i = 0; i < function->argumentCount; i++) {
			if (!findTypeInfoRecurse(job, types, function->argumentTypes[i]))
				return false;
		}
		
		for (u64 i = 0; i < function->returnCount; i++) {
			if (!findTypeInfoRecurse(job, types, function->returnTypes[i]))
				return false;
		}

		return true;
	}
	case TypeFlavor::STRUCT: {
		auto struct_ = static_cast<TypeStruct *>(type);

		for (auto member : struct_->members.declarations) {
			if (member->flags & (DECLARATION_IMPORTED_BY_USING)) continue;

			if (!(member->flags & DECLARATION_TYPE_IS_READY)) {
				goToSleep(job, &member->sleepingOnMyType, "Find type info struct member type not ready");
				return false;
			}

			auto memberType = getDeclarationType(member);

			if (memberType == &TYPE_UNSIGNED_INT_LITERAL || memberType == &TYPE_SIGNED_INT_LITERAL || memberType == &TYPE_FLOAT_LITERAL) {
				assert(member->initialValue);
				memberType = getTypeForExpr(member->initialValue);
			}

			if (!findTypeInfoRecurse(job, types, memberType))
				return false;

			if (member->flags & DECLARATION_IS_UNINITIALIZED) continue;

			if (!(member->flags & DECLARATION_VALUE_IS_READY)) {
				goToSleep(job, &member->sleepingOnMyValue, "Find type info struct member value not ready");
				return false;
			}

			if (!findTypeInfoInExprRecurse(job, types, member->initialValue))
				return false;
		}

		return true;
	}
	default:
		assert(false);
		return false;
	}
}

void fillTypeInfo(Type *type) {
	assert(type->runtimeTypeInfo);

	auto typeInfo = type->runtimeTypeInfo;

	typeInfo->size = type->size;
	typeInfo->alignment = type->alignment;
	typeInfo->name = { (u8 *) type->name.characters, type->name.length };

	switch (type->flavor) {
	case TypeFlavor::BOOL: {
		typeInfo->tag = Type_Info::Tag::BOOL;
		break;
	}
	case TypeFlavor::FLOAT: {
		typeInfo->tag = Type_Info::Tag::FLOAT;
		break;
	}
	case TypeFlavor::STRING: {
		typeInfo->tag = Type_Info::Tag::STRING;
		break;
	}
	case TypeFlavor::TYPE: {
		typeInfo->tag = Type_Info::Tag::TYPE;
		break;
	}
	case TypeFlavor::VOID: {
		typeInfo->tag = Type_Info::Tag::VOID;
		break;
	}
	case TypeFlavor::ARRAY: {
		typeInfo->tag = Type_Info::Tag::ARRAY;

		auto arrayInfo = static_cast<Type_Info_Array *>(typeInfo);
		auto array = static_cast<TypeArray *>(type);

		assert(array->arrayOf->runtimeTypeInfo);
		arrayInfo->element_type = array->arrayOf->runtimeTypeInfo;

		if (array->flags & TYPE_ARRAY_IS_DYNAMIC) {
			arrayInfo->flavor = Type_Info_Array::Flavor::DYNMAIC;
			arrayInfo->count = 0;
		}
		else if (array->flags & TYPE_ARRAY_IS_FIXED) {
			arrayInfo->flavor = Type_Info_Array::Flavor::FIXED;
			arrayInfo->count = array->count;
		}
		else {
			arrayInfo->flavor = Type_Info_Array::Flavor::NORMAL;
			arrayInfo->count = 0;
		}
		
		break;
	}
	case TypeFlavor::ENUM: {
		typeInfo->tag = Type_Info::Tag::ENUM;

		auto enumInfo = static_cast<Type_Info_Enum *>(typeInfo);
		auto enum_ = static_cast<TypeEnum *>(type);

		assert(enum_->integerType->runtimeTypeInfo);
		enumInfo->base_type = static_cast<Type_Info_Integer *>(enum_->integerType->runtimeTypeInfo);

		enumInfo->is_flags = enum_->flags & TYPE_ENUM_IS_FLAGS ? true : false;

		auto values = &enum_->members;

		enumInfo->values.count = values->declarations.count - ENUM_SPECIAL_MEMBER_COUNT;
		enumInfo->values.data = INFER_NEW_ARRAY(Type_Info_Enum::Value, enumInfo->values.count);

		for (u32 i = 0; i + ENUM_SPECIAL_MEMBER_COUNT < values->declarations.count; i++) {
			enumInfo->values.data[i].name = { (u8 *)values->declarations[i]->name.characters, values->declarations[i]->name.length };
			enumInfo->values.data[i].value = static_cast<ExprLiteral *>(values->declarations[i]->initialValue)->unsignedValue;
		}

		break;
	}
	case TypeFlavor::FUNCTION: {
		typeInfo->tag = Type_Info::Tag::FUNCTION;

		auto functionInfo = static_cast<Type_Info_Function *>(typeInfo);
		auto function = static_cast<TypeFunction *>(type);

		functionInfo->arguments.count = function->argumentCount;
		functionInfo->arguments.data = INFER_NEW_ARRAY(Type_Info *, function->argumentCount);

		for (u64 i = 0; i < function->argumentCount; i++) {
			assert(function->argumentTypes[i]->runtimeTypeInfo);
			functionInfo->arguments.data[i] = function->argumentTypes[i]->runtimeTypeInfo;
		}

		functionInfo->returns.count = function->returnCount;
		functionInfo->returns.data = INFER_NEW_ARRAY(Type_Info *, function->returnCount);

		for (u64 i = 0; i < function->returnCount; i++) {
			assert(function->returnTypes[i]->runtimeTypeInfo);
			functionInfo->returns.data[i] = function->returnTypes[i]->runtimeTypeInfo;
		}

		functionInfo->c_call = function->flags & TYPE_FUNCTION_IS_C_CALL ? true : false;
		functionInfo->varargs = function->isVarargs;

		break;
	}
	case TypeFlavor::INTEGER: {
		typeInfo->tag = Type_Info::Tag::INTEGER;

		auto integerInfo = static_cast<Type_Info_Integer *>(typeInfo);

		integerInfo->signed_ = type->flags & TYPE_INTEGER_IS_SIGNED ? true : false;

		break;
	}
	case TypeFlavor::POINTER: {
		typeInfo->tag = Type_Info::Tag::POINTER;

		auto pointerInfo = static_cast<Type_Info_Pointer *>(typeInfo);
		auto pointer = static_cast<TypePointer *>(type);

		assert(pointer->pointerTo->runtimeTypeInfo);
		pointerInfo->value_type = pointer->pointerTo->runtimeTypeInfo;

		break;
	}
	case TypeFlavor::STRUCT: {
		typeInfo->tag = Type_Info::Tag::STRUCT;

		auto structInfo = static_cast<Type_Info_Struct *>(typeInfo);
		auto struct_ = static_cast<TypeStruct *>(type);

		structInfo->flags = 0;
		if (struct_->flags & TYPE_STRUCT_IS_UNION) structInfo->flags |= Type_Info_Struct::Flags::UNION;
		if (struct_->flags & TYPE_STRUCT_IS_PACKED) structInfo->flags |= Type_Info_Struct::Flags::PACKED;

		u32 count = 0;

		for (auto member : struct_->members.declarations) {
			if (member->flags & DECLARATION_IMPORTED_BY_USING) continue;

			++count;
		}

		structInfo->members.count = count;
		structInfo->members.data = INFER_NEW_ARRAY(Type_Info_Struct::Member, count);

		count = 0;

		for (auto member : struct_->members.declarations) {
			if (member->flags & DECLARATION_IMPORTED_BY_USING) continue;

			auto &memberInfo = structInfo->members.data[count++];

			memberInfo.name = { (u8 *)member->name.characters, member->name.length };
			memberInfo.offset = (member->flags & DECLARATION_IS_CONSTANT) ? 0 : member->physicalStorage;

			auto memberType = getDeclarationType(member);

			if (member->initialValue) {
				memberType = getTypeForExpr(member->initialValue);
			}
			assert(memberType->runtimeTypeInfo);
			memberInfo.member_type = memberType->runtimeTypeInfo;

			if (member->initialValue && (member->initialValue->flavor != ExprFlavor::TYPE_LITERAL || static_cast<ExprLiteral *>(member->initialValue)->typeValue != &TYPE_MODULE)) { 
				// @Incomplete: Export info for namespaces
				memberInfo.initial_value = inferArena.allocate(memberType->size);
				createRuntimeValue(member->initialValue, memberInfo.initial_value);
			}
			
			memberInfo.flags = 0;

			if (member->flags & DECLARATION_IS_UNINITIALIZED) memberInfo.flags |= Type_Info_Struct::Member::Flags::UNINITIALIZED;
			if (member->flags & DECLARATION_IS_CONSTANT) memberInfo.flags |= Type_Info_Struct::Member::Flags::CONSTANT;
			if (member->flags & DECLARATION_MARKED_AS_USING) memberInfo.flags |= Type_Info_Struct::Member::Flags::USING;
			
		}

		break;
	}
	}
}

void createAllTypeInfos(ArraySet<Type *> types) {
	for (auto needed : types) {
		createTypeInfo(needed);
	}

	for (auto needed : types) {
		fillTypeInfo(needed);
	}
}

bool ensureTypeInfos(RunJob *job, Type *type) {
	PROFILE_FUNC();
	ArraySet<Type *> types;

	if (!findTypeInfoRecurse(job, &types, type)) {
		types.free();
		return false;
	}

	createAllTypeInfos(types);

	types.free();

	return true;
}

bool ensureTypeInfos(RunJob *job, Expr *expr) {
	PROFILE_FUNC();
	ArraySet<Type *> types;

	if (!findTypeInfoInExprRecurse(job, &types, expr)) {
		types.free();
		return false;
	}

	createAllTypeInfos(types);

	types.free();

	return true;
}

bool returnTypeIsLegalForRun(Expr *run, Type *type) {
	if (type->flags & TYPE_FUNCTION_IS_C_CALL) {
		reportError(run, "Error: #run statements cannot return a #c_call function");
		return false;
	}
	else if (type->flags & TYPE_ARRAY_IS_DYNAMIC) {
		reportError(run, "Error: #run statements cannot return a dynamic array");
		return false;
	}
	else if (type->flavor == TypeFlavor::ARRAY && !(type->flags & TYPE_ARRAY_IS_FIXED)) {
		reportError(run, "Error: #run statements cannot return an array view @Incomplete");
		return false;
	}
	else if (type->flags & TYPE_STRUCT_IS_UNION) {
		reportError(run, "Error: #run statements cannot return a union");
		return false;
	}
	else if (type->flavor == TypeFlavor::ARRAY) {
		return returnTypeIsLegalForRun(run, static_cast<TypeArray *>(type)->arrayOf);
	}
	else if (type->flavor == TypeFlavor::STRUCT) {
		auto struct_ = static_cast<TypeStruct *>(type);

		for (auto declaration : struct_->members.declarations) {
			if (declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING))
				continue;

			if (!returnTypeIsLegalForRun(run, getDeclarationType(declaration))) {
				reportError(declaration, "   ..: Which is part of struct %.*s", STRING_PRINTF(type->name));
				return false;
			}
		}
	}
	
	return true;
}

bool inferRun(SubJob *subJob) {
	PROFILE_FUNC();

	auto job = static_cast<RunJob *>(subJob);
	auto run = job->run;
	++totalInferRuns;

	auto function = static_cast<TypeFunction *>(run->function->type);

	if (!contextIsLocked) {
		goToSleep(job, &TYPE_CONTEXT.sleepingOnMe, "#run directive waiting for context to be locked");
		return true;
	}

	bool yield;

	getDefaultValue(job, &TYPE_CONTEXT, &yield);

	if (yield)
		return true;

	if (!ensureTypeInfos(job, TYPE_CONTEXT.defaultValue))
		return true;


	assert(function->argumentCount == 0);
	assert(function->returnCount == 1);
	assert(!(function->flags & TYPE_FUNCTION_IS_C_CALL));

	auto returnType = function->returnTypes[0];

	while (job->checkingFunctions.count) {
		auto &index = job->checkingFunctionIndices[job->checkingFunctionIndices.count - 1];
		auto &function = job->checkingFunctions[job->checkingFunctions.count - 1];
		auto &ir = function->state.ir;

		if (function->flags & EXPR_FUNCTION_RUN_CHECKED) {
			job->checkingFunctions.pop();
			job->checkingFunctionIndices.pop();
		}
		else {
			if (!(function->flags & EXPR_FUNCTION_RUN_READY)) {
				goToSleep(job, &function->sleepingOnIr, "Run function ir not ready");
				return true;
			}

			if (function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
				assert(!function->loadedFunctionPointer);
				assert(function->body->flavor == ExprFlavor::STRING_LITERAL);

				auto functionLibrary = static_cast<ExprStringLiteral *>(function->body)->string;

				Library *lib = nullptr;

				for (auto &library : libraries) {
					if (library.name == functionLibrary) {
						lib = &library;
					}
				}

				assert(lib);

				if (!lib->handle) {
#if BUILD_WINDOWS
					if (lib->name == "c") {
						if (buildOptions.c_runtime_library & Build_Options::C_Runtime_Library::DEBUG) {
							lib->handle = LoadLibraryA("ucrtbased");
						}
						else {
							lib->handle = LoadLibraryA("ucrtbase");

						}
					} 
					else
#endif
						lib->handle = LoadLibraryA(toCString(functionLibrary)); // @Leak
					
					if (!lib->handle) {
						reportError(function->body, "Error: Failed to load library %.*s", STRING_PRINTF(functionLibrary));
						return false;
					}
				}

				function->loadedFunctionPointer = GetProcAddress(lib->handle, toCString(function->valueOfDeclaration->name)); // @Leak
				if (!function->loadedFunctionPointer) {
					reportError(function->valueOfDeclaration, 
						"Error: Could not find function %.*s in library %.*s", STRING_PRINTF(function->valueOfDeclaration->name), STRING_PRINTF(functionLibrary));
					return false;
				}

				function->flags |= EXPR_FUNCTION_RUN_CHECKED;
			}
			else {
				if (function->flags & EXPR_FUNCTION_IS_C_CALL) {
					if (!function->loadedFunctionPointer) {
						function->loadedFunctionPointer = createCCallFunction(function);
					}
				}

				while (index < ir.count) {
					auto &op = ir[index];

					if (op.op == IrOp::TYPE) {
						if (!ensureTypeInfos(job, static_cast<Type *>(op.data)))
							return true;

					}
					else if (op.op == IrOp::ARRAY_LITERAL) {
						if (!ensureTypeInfos(job, static_cast<ExprArrayLiteral *>(op.data)))
							return true;

					}
					else if (op.op == IrOp::STRUCT_LITERAL) {
						if (!ensureTypeInfos(job, static_cast<ExprStructLiteral *>(op.data)))
							return true;

					}
					else if (op.op == IrOp::FUNCTION) {
						if (!(op.function->flags & EXPR_FUNCTION_RUN_CHECKED)) {
							if (pushFunctionToRunCheck(job, op.function)) {
								break;
							}
						}
					}
					else if (op.op == IrOp::ADDRESS_OF_GLOBAL) {
						auto declaration = static_cast<Declaration *>(op.data);
						if (!declaration->runtimeValue) {
							if (!(declaration->flags & DECLARATION_VALUE_IS_READY)) {
								goToSleep(job, &declaration->sleepingOnMyValue, "Address of global declaration value not ready");
								return true;
							}

							if (!ensureTypeInfos(job, declaration->initialValue)) {
								return true;
							}

							declaration->runtimeValue = malloc(getDeclarationType(declaration)->size);

							createRuntimeValue(declaration->initialValue, declaration->runtimeValue);
						}
					}

					index++;
				}

				if (index == ir.count) {
					job->checkingFunctions.pop()->flags |= EXPR_FUNCTION_RUN_CHECKED;
					job->checkingFunctionIndices.pop();
				}
			}
		}
	}

	if (!returnTypeIsLegalForRun(run, function->returnTypes[0]))
		return false;

	VMState state;
	initVMState(&state); // @Speed, keep this around for multiple run directives
	run->returnValue = runFunctionRoot(&state, run);
	deinitVMState(&state);

	if (hadError)
		return false;

	if (getDeclarationType(static_cast<ExprFunction *>(run->function)->returns.declarations[0]) == &TYPE_VOID) {
		assert(!run->returnValue);
		run->returnValue = run;
		run->type = &TYPE_VOID;
	}
	else {
		assert(run->returnValue);
	}

	wakeUpSleepers(&run->sleepingOnMe);
	removeJob(&runJobs, job);
	freeJob(job);

	return true;
}

void createBasicDeclarations() {
	Declaration *targetWindows = INFER_NEW(Declaration);
	targetWindows->enclosingScope = nullptr;
	targetWindows->start = {};
	targetWindows->end = {};
	targetWindows->name = "TARGET_WINDOWS";

	auto literal = createIntLiteral(targetWindows->start, targetWindows->end, &TYPE_BOOL, BUILD_WINDOWS);

	targetWindows->initialValue = literal;
	targetWindows->type = nullptr;
	targetWindows->flags |= DECLARATION_IS_CONSTANT;

	addDeclaration(targetWindows, runtimeModule);
}

bool doJob(SubJob *job) {
	if (!inferFlattened(job)) {
		return false;
	}
	else if (isDone(job)) {
		constexpr bool (*table[])(SubJob *) = {
			inferSize, 
			inferDeclarationType, 
			inferDeclarationValue, 
			inferFunctionHeader, 
			inferFunctionBody, 
			inferImporter, 
			inferRun
		};

		if (!table[(u8) job->flavor](job))
			return false;
	}

	return true;
}

bool doSubJobs(u64 *irGenerationPending) {
	while (subJobs.count) {
		SubJob *job;

		if (SHUFFLE_JOBS) {
			u32 i = rand() % subJobs.count;
			job = subJobs[i];
			subJobs.unordered_remove(i);
		}
		else {
			job = subJobs.pop();
		}

		if (!doJob(job)) {
			return false;
		}
	}

	{
		PROFILE_ZONE("Check size dependencies");

		for (u32 i = 0; i < functionWaitingOnSize.count; i++) {
			auto job = functionWaitingOnSize[i];

			for (u32 j = 0; j < job->sizeDependencies.count; j++) {
				auto depend = job->sizeDependencies[j];

				if (depend->size) {
					job->sizeDependencies.unordered_remove(j--);
				}
			}

			if (job->sizeDependencies.count == 0) {
				functionWaitingOnSize.unordered_remove(i--);

				irGeneratorQueue.add(job->function);
				(*irGenerationPending)++;

				freeJob(job);
			}
		}

		for (u32 i = 0; i < declarationWaitingOnSize.count; i++) {
			auto job = declarationWaitingOnSize[i];

			for (u32 j = 0; j < job->sizeDependencies.count; j++) {
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

	return true;
}

bool addContext(ExprAddContext *addContext) {
	if (contextIsLocked) {
		reportError(addContext, "Error: Cannot add more members to the context after the context has been used");
		return false;
	}

	if (!addDeclarationToBlock(&TYPE_CONTEXT.members, addContext->declaration, TYPE_CONTEXT.members.declarations.count)) {
		return false;
	}

	bool success = addDeclaration(addContext->declaration, runtimeModule);
	assert(success);

	if (addContext->using_) {
		addImporterToBlock(&TYPE_CONTEXT.members, addContext->using_, addContext->declaration->serial);
		addImporter(addContext->using_, runtimeModule);
	}

	wakeUpSleepers(&TYPE_CONTEXT.members.sleepingOnMe, addContext->declaration->name);

	return true;
}

void runInfer(String inputFile) {
	PROFILE_FUNC();

	u64 irGenerationPending = 0;

	createBasicDeclarations();

	while (true) {
		inferInput.add(inferQueue.take());

		while (inferInput.count) {
			auto job = inferInput.pop();

			if (job.type == InferJobType::DONE) {
				goto outer;
			}

			if (job.type == InferJobType::IMPORTER) {
				addImporter(job.importer, job.module);
			}
			else if (job.type == InferJobType::GLOBAL_DECLARATION) {
				if (job.declaration)
					if (!addDeclaration(job.declaration, job.module))
						goto error;
			}
			else if (job.type == InferJobType::LOAD_COMPLETE) {
				if (job.fileUid == 0) {
					auto name = INFER_NEW(ExprStringLiteral);
					name->flavor = ExprFlavor::STRING_LITERAL;
					name->start = {};
					name->end = {};
					name->string = inputFile;
					name->type = &TYPE_STRING;

					auto load = INFER_NEW(ExprLoad);
					load->flavor = ExprFlavor::LOAD;
					load->start = {};
					load->end = {};
					load->file = name;
					load->module = mainModule;

					auto import = INFER_NEW(Importer);
					import->import = load;

					inferInput.add(InferQueueJob(import, mainModule));
				}

				loadsPending--;
			}
			else if (job.type == InferJobType::EXPR) {
				if (job.expr->flavor == ExprFlavor::RUN) {
					addRunJob(static_cast<ExprRun *>(job.expr));
				}
				else if (job.expr->flavor == ExprFlavor::FUNCTION) {
					assert(irGenerationPending > 0);
					irGenerationPending--;

					auto function = static_cast<ExprFunction *>(job.expr);

					wakeUpSleepers(&function->sleepingOnIr);
				}
				else if (job.expr->flavor == ExprFlavor::ADD_CONTEXT) {
					if (!addContext(static_cast<ExprAddContext *>(job.expr))) {
						goto error;
					}
				}
				else {
					assert(false);
				}
			}
			else {
				assert(false);

				
			}

			if (!doSubJobs(&irGenerationPending))
				goto error;
		}

		while (loadsPending == 0 && irGenerationPending == 0) {
			if (!contextIsLocked) {
				contextIsLocked = true;

				addSizeJobIfNeeded(&TYPE_CONTEXT);
				addStruct(&TYPE_CONTEXT);

				// Wake up any job that is still looking for a member in the context so it can report a member not found error
				wakeUpSleepers(&TYPE_CONTEXT.members.sleepingOnMe);
				if (!doSubJobs(&irGenerationPending))
					goto error;
			} 
			else if (overloadsWaitingForLock.count) {
				for (auto waiting : overloadsWaitingForLock) {
					waiting.overloadSet->flags |= DECLARATION_OVERLOADS_LOCKED;
					waiting.waiting->sleepCount = 0;

					subJobs.add(waiting.waiting);
				}

				overloadsWaitingForLock.clear();
				if (!doSubJobs(&irGenerationPending))
					goto error;
			}
			else {
				goto outer;
			}
		}
	}
outer:

	if ((sizeJobs || functionJobs || declarationJobs || runJobs || importerJobs) && !hadError) { // We didn't complete type inference, check for undeclared identifiers or circular dependencies! If we already had an error don't spam more
		// Check for undeclared identifiers

		/*for (auto job = functionJobs; job; job = job->next) {
			if (!isDone(&job->header)) {
				auto haltedOn = *getHalt(&job->header);

				if (checkForUndeclaredIdentifier(haltedOn)) {
					goto error;
				}
			}

			if (!isDone(&job->body)) {
				auto haltedOn = *getHalt(&job->body);

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

		for (auto job = runJobs; job; job = job->next) {
			if (!isDone(job)) {
				auto haltedOn = *getHalt(job);

				if (checkForUndeclaredIdentifier(haltedOn)) {
					goto error;
				}
			}
		}*/

		// Check for circular dependencies

		/*Array<Declaration *> loop;

		for (auto job = declarationJobs; job; job = job->next) {
			loop.clear();

			s64 loopIndex = findLoop(loop, job->declaration);

			if (loopIndex == -1) continue;

			reportError(loop[static_cast<u32>(loopIndex)], "Error: There were circular dependencies");

			if (loopIndex + 1 == loop.count) {
				Expr *haltedOn;
				bool typeDependece;


				auto declaration = loop[static_cast<u32>(loopIndex)];

				getHaltStatus(declaration, declaration, &haltedOn, &typeDependece);

				reportError(haltedOn, "   ..: The %s of %.*s depends on itself", typeDependece ? "type" : "value", STRING_PRINTF(declaration->name));
			}
			else {
				for (u32 i = static_cast<u32>(loopIndex); i < loop.count; i++) {
					Expr *haltedOn;
					bool typeDependece;

					auto declaration = loop[i];
					auto next = loop[i + 1 == loop.count ? static_cast<u32>(loopIndex) : i + 1];

					getHaltStatus(declaration, next, &haltedOn, &typeDependece);

					reportError(haltedOn, "   ..: The %s of %.*s depends on %.*s", typeDependece ? "type" : "value", STRING_PRINTF(declaration->name), STRING_PRINTF(next->name));
				}
			}

			goto error;
		}*/

		reportError("Internal Compiler Error: Inference got stuck but no undeclared identifiers or circular dependencies were detected");

		for (auto job = sizeJobs; job; job = job->next) {
			auto type = job->type;

			if (type->flavor == TypeFlavor::STRUCT) {
				auto haltedOn = static_cast<TypeStruct *>(type)->members.declarations[job->sizingIndexInMembers];

				reportError(haltedOn, "%.*s sizing halted here", STRING_PRINTF(type->name));
			}
			else {
				reportError("%.*s sizing halted", STRING_PRINTF(type->name));
			}

#if BUILD_DEBUG
			if (job->sleepCount) {
				reportError("%s", job->sleepReason);
			}
#endif
		}

		for (auto job = runJobs; job; job = job->next) {
			reportError(job->run, "Run halted");

#if BUILD_DEBUG
			if (job->sleepCount) {
				reportError("%s", job->sleepReason);
			}
#endif
		}

		for (auto job = functionJobs; job; job = job->next) {
			String name = (job->function->valueOfDeclaration ? job->function->valueOfDeclaration->name : "(function)");

			if (!isDone(&job->header)) {
				auto haltedOn = *getHalt(&job->header);

				reportError(haltedOn, "%.*s header halted here", STRING_PRINTF(name));
			}

			if (!isDone(&job->body)) {
				auto haltedOn = *getHalt(&job->body);

				reportError(haltedOn, "%.*s body halted here", STRING_PRINTF(name));
			}

			if (isDone(&job->header) && isDone(&job->body)) {
				reportError(job->function, "Function halted after completing infer");

				for (auto declaration : job->function->arguments.declarations) {
					if (declaration->sleepingOnMyType.count) {
						reportError(declaration, "Argument type has sleepers, flags: %llx", declaration->flags);
					}
				}

				for (auto declaration : job->function->arguments.declarations) {
					if (declaration->sleepingOnMyValue.count) {
						reportError(declaration, "Argument value has sleepers, flags: %llx", declaration->flags);
					}
				}

				for (auto declaration : job->function->returns.declarations) {
					if (declaration->sleepingOnMyType.count) {
						reportError(declaration, "Return type has sleepers, flags: %llx", declaration->flags);
					}
				}

				for (auto declaration : job->function->returns.declarations) {
					if (declaration->sleepingOnMyValue.count) {
						reportError(declaration, "Return value has sleepers, flags: %llx", declaration->flags);
					}
				}
			}

#if BUILD_DEBUG
			if (job->header.sleepCount) {
				reportError("Type: %s", job->header.sleepReason);
			}

			if (job->body.sleepCount) {
				reportError("Value: %s", job->body.sleepReason);
			}
#endif
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

#if BUILD_DEBUG
			if (job->type.sleepCount) {
				reportError("Type: %s", job->type.sleepReason);
			}

			if (job->value.sleepCount) {
				reportError("Value: %s", job->value.sleepReason);
			}
#endif
		}

		for (auto job = importerJobs; job; job = job->next) {
			if (isDone(job)) {
				reportError(job->importer->import, "Importer halted after completing infer");
			}
			else {
				auto haltedOn = *getHalt(job);

				reportError(haltedOn, "Importer halted here");
			}

#if BUILD_DEBUG
			if (job->sleepCount) {
				reportError("%s", job->sleepReason);
			}
#endif
		}

		for (auto job : declarationWaitingOnSize) {
			for (auto depend : job->sizeDependencies) {
				reportError(job->declaration, "Error: Declaration waiting on size of %.*s", STRING_PRINTF(depend->name));
			}
		}

		for (auto job : functionWaitingOnSize) {
			for (auto depend : job->sizeDependencies) {
				reportError(job->function, "Error: Function waiting on size of %.*s", STRING_PRINTF(depend->name));
			}
		}

		goto error;
	}

	if (printDiagnostics) {
		reportInfo("Infer memory used: %ukb", inferArena.totalSize / 1024);
		reportInfo("Type table memory used: %ukb", typeArena.totalSize / 1024);
	}

	if (!hadError) {
		if (!entryPoint) {
			reportError("Error: No entry point was declared");
			goto error;
		}

		if (!programStart) {
			reportError("Error: No declaration was found for the program start");
			goto error;
		}

		assert(entryPoint->flags & DECLARATION_TYPE_IS_READY);
		assert(entryPoint->flags & DECLARATION_VALUE_IS_READY);
	}

	

	irGeneratorQueue.add(nullptr);
	parserQueue.add(nullptr);

	return;
error:;
	assert(hadError);

	parserQueue.add(nullptr);
	irGeneratorQueue.add(nullptr);
}