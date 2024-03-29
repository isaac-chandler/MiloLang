#include "Basic.h"
#include "Polymorph.h"
#include "BucketedArenaAllocator.h"

BucketedArenaAllocator polymorphArena(1024 * 1024);

#if 1
#define POLYMORPH_NEW(T) new (static_cast<T *>(polymorphArena.allocate(sizeof(T)))) T
#define POLYMORPH_NEW_ARRAY(T, C) new (static_cast<T *>(polymorphArena.allocate((C) * sizeof(T)))) T[C]

#else
#define PARSER_NEW(T) new T
#define PARSER_NEW_ARRAY(T, C) new T[C]
#endif

struct CopyingBlock {
	Block *dest;
	Block *src;
};

struct CopyingFunction {
	ExprFunction *dest;
	ExprFunction *src;
};

Array<CopyingBlock> copyingBlocks;
Array<CopyingFunction> copyingFunctions;

#define create(type) auto dest = POLYMORPH_NEW(type)
#define copy(field) (dest->field = src->field)
#define copy_location() (copy(start), copy(end))
#define copy_flags(mask) (dest->flags = src->flags & (mask))
#define copy_expr(field) (dest->field = copyExpr(src->field))

Block *copyBlockReference(Block *src) {
	if (!src)
		return nullptr;

	for (auto block : copyingBlocks) {
		if (block.src == src)
			return block.dest;
	}

	return src;
}

Declaration *copyDeclarationReference(Declaration *src) {
	if (!src)
		return nullptr;

	for (auto block : copyingBlocks) {
		if (block.src != src->enclosingScope) continue;

		if (src->name) {
			auto declaration = findDeclarationNoYield(block.dest, src->name);
			assert(declaration);
			return declaration;
		}
		else {
			for (auto declaration : block.dest->declarations) {
				if (declaration->serial == src->serial) {
					return declaration;
				}
			}

			assert(false);
			return nullptr;
		}
	}

	return src;
}

ExprFunction *copyFunctionReference(ExprFunction *src) {
	if (!src)
		return nullptr;

	for (auto function : copyingFunctions) {
		if (function.src == src)
			return function.dest;
	}

	return src;
}

void copyExprInfo(Expr *dest, Expr *src) {
	copy_location();
	copy(flavor);
	copy_flags(~(EXPR_HAS_STORAGE | EXPR_FUNCTION_RUN_CHECKED | EXPR_FUNCTION_RUN_READY));
	copy(type);
	dest->valueOfDeclaration = copyDeclarationReference(src->valueOfDeclaration);
}

void copyBlockInfo(Block *dest, Block *src) {
	copy(serial);
	dest->parentBlock = copyBlockReference(src->parentBlock);
	copy(flavor);
	copy(module);
	copy(loop);
}


void copyBlock(Block *dest, Block *src);

TypeStruct *copyStruct(TypeStruct *srcType) {
	auto destType = POLYMORPH_NEW(TypeStruct);

	destType->size = srcType->size;
	destType->alignment = srcType->alignment;
	destType->name = srcType->name;
	destType->hash = 0;
	destType->flags = srcType->flags;
	destType->flavor = srcType->flavor;

	copyingBlocks.add({ &destType->constants, &srcType->constants });
	copyBlock(&destType->constants, &srcType->constants);
	copyingBlocks.add({ &destType->members, &srcType->members });
	copyBlock(&destType->members, &srcType->members);
	copyingBlocks.pop();
	copyingBlocks.pop();

	return destType;
}

Expr *copyExpr(Expr *srcExpr) {

#define c(type)\
	auto src = static_cast<type *>(srcExpr);\
	create(type);\
	copyExprInfo(dest, src);

	if (!srcExpr)
		return nullptr;

	switch (srcExpr->flavor) {
	case ExprFlavor::PUSH_CONTEXT:
	case ExprFlavor::BINARY_OPERATOR: {
		c(ExprBinaryOperator);

		copy_expr(left);
		copy_expr(right);
		copy(op);
		return dest;
	}
	case ExprFlavor::BLOCK: {
		c(ExprBlock);
		copyingBlocks.add({ &dest->declarations, &src->declarations });

		copyBlock(&dest->declarations, &src->declarations);


		dest->exprs.reserve(src->exprs.count);

		for (auto expr : src->exprs) {
			dest->exprs.add(copyExpr(expr));
		}

		copyingBlocks.pop();
		return dest;
	}
	case ExprFlavor::BREAK:
	case ExprFlavor::CONTINUE:
	case ExprFlavor::REMOVE: {
		c(ExprBreakOrContinue);
		copy(label);
		copy(refersTo);
		dest->enclosingScope = copyBlockReference(src->enclosingScope);

		return dest;
	}
	case ExprFlavor::COMMA_ASSIGNMENT: {
		c(ExprCommaAssignment);
		dest->left = POLYMORPH_NEW_ARRAY(Expr *, src->exprCount);

		for (u32 i = 0; i < src->exprCount; i++) {
			copy_expr(left[i]);
		}

		copy_expr(call);
		copy(exprCount);

		return dest;
	}
	case ExprFlavor::DEFER: {
		c(ExprDefer);
		copy_expr(expr);
		dest->enclosingScope = copyBlockReference(src->enclosingScope);

		return dest;
	}
	case ExprFlavor::ENUM: {
		c(ExprEnum);

		copy(struct_.size);
		copy(struct_.alignment);
		copy(struct_.name);
		dest->struct_.hash = 0;
		copy(struct_.flags);
		copy(struct_.flavor);
		copyingBlocks.add({ &dest->struct_.members, &src->struct_.members });
		copyBlock(&dest->struct_.members, &src->struct_.members);
		copy(struct_.integerType);

		copyingBlocks.pop();

		return dest;
	}
	case ExprFlavor::ENUM_INCREMENT: {
		c(ExprEnumIncrement);

		dest->previous = copyDeclarationReference(src->previous);

		return dest;
	}
	case ExprFlavor::FLOAT_LITERAL: {
		c(ExprLiteral);

		copy(floatValue);

		return dest;
	}
	case ExprFlavor::ARRAY_LITERAL: {
		c(ExprArrayLiteral);

		copy_expr(typeValue);
		copy(count);
		dest->values = POLYMORPH_NEW_ARRAY(Expr *, src->count);

		for (u32 i = 0; i < src->count; i++) {
			copy_expr(values[i]);
		}

		return dest;
	}
	case ExprFlavor::STRUCT_LITERAL: {
		c(ExprStructLiteral);

		copy_expr(typeValue);
		copy(initializers.names);
		copy(initializers.count);

		dest->initializers.values = POLYMORPH_NEW_ARRAY(Expr *, src->initializers.count);

		for (u32 i = 0; i < src->initializers.count; i++) {
			copy_expr(initializers.values[i]);
		}

		return dest;
	}
	case ExprFlavor::FOR: {
		c(ExprLoop);

		copy_expr(forBegin);
		copy_expr(forEnd);

		copyingBlocks.add({ &dest->iteratorBlock, &src->iteratorBlock });
		copyBlock(&dest->iteratorBlock, &src->iteratorBlock);

		copy_expr(body);
		
		copyingBlocks.pop();

		copy_expr(completedBody);

		return dest;
	}
	case ExprFlavor::FUNCTION:
	case ExprFlavor::FUNCTION_PROTOTYPE: {
		c(ExprFunction);
		copyingFunctions.add({ dest, src });

		{
			copyingBlocks.add({ &dest->constants, &src->constants });
			copyingBlocks.add({ &dest->arguments, &src->arguments });
			copyBlock(&dest->constants, &src->constants);
			{
				copyBlock(&dest->arguments, &src->arguments);

				{
					copyingBlocks.add({ &dest->returns, &src->returns });
					copyBlock(&dest->returns, &src->returns);
					copyingBlocks.pop();
				}

				copy_expr(body);

				copyingBlocks.pop();
			}

			copyingBlocks.pop();
		}

		copyingFunctions.pop();

		return dest;
	}
	case ExprFlavor::FUNCTION_CALL: {
		c(ExprFunctionCall);
		copy_expr(function);

		copy(arguments.names);
		copy(arguments.count);

		dest->arguments.values = POLYMORPH_NEW_ARRAY(Expr *, src->arguments.count);

		for (u32 i = 0; i < src->arguments.count; i++) {
			copy_expr(arguments.values[i]);
		}
		return dest;
	}
	case ExprFlavor::IDENTIFIER: {
		c(ExprIdentifier);
		copy(name);
		copy(serial);
		dest->resolveFrom = copyBlockReference(src->resolveFrom);
		dest->enclosingScope = copyBlockReference(src->enclosingScope);
		copy_expr(structAccess);
		copy(module);
		dest->declaration = copyDeclarationReference(src->declaration);

		return dest;
	}
	case ExprFlavor::IF: {
		c(ExprIf);
		copy_expr(condition);
		copy_expr(ifBody);
		copy_expr(elseBody);

		return dest;
	}
	case ExprFlavor::STATIC_IF: {
		assert(copyingBlocks.count);
		auto enclosingScope = copyingBlocks[copyingBlocks.count - 1];

		for (u32 i = 0; i < enclosingScope.dest->importers.count; i++) {
			if (enclosingScope.src->importers[i]->import == srcExpr) {
				return enclosingScope.dest->importers[i]->import;
			}
		}

		c(ExprIf);
		copy_expr(condition);
		copy_expr(ifBody);
		copy_expr(elseBody);

		return dest;
	}
	case ExprFlavor::IMPORT:
	case ExprFlavor::LOAD: {
		c(ExprLoad);
		copy_expr(file);
		copy(module);

		return dest;
	}
	case ExprFlavor::INT_LITERAL: {
		c(ExprLiteral);
		copy(unsignedValue);

		return dest;
	}
	case ExprFlavor::RETURN: {
		c(ExprReturn);
		dest->returnsFrom = copyFunctionReference(src->returnsFrom);

		copy(returns.names);
		copy(returns.count);

		dest->returns.values = POLYMORPH_NEW_ARRAY(Expr *, src->returns.count);

		for (u32 i = 0; i < src->returns.count; i++) {
			copy_expr(returns.values[i]);
		}
		return dest;
	}
	case ExprFlavor::RUN: {
		c(ExprRun);
		copy(module);
		copy_expr(function);

		return dest;
	}
	case ExprFlavor::SLICE: {
		c(ExprSlice);
		copy_expr(array);
		copy_expr(sliceStart);
		copy_expr(sliceEnd);

		return dest;
	}
	case ExprFlavor::STRING_LITERAL: {
		c(ExprStringLiteral);
		copy(string);

		return dest;
	}
	case ExprFlavor::SWITCH: {
		c(ExprSwitch);

		copy_expr(condition);
		dest->cases.reserve(src->cases.count);

		for (auto case_ : src->cases) {
			ExprSwitch::Case destCase = case_;
			destCase.condition = copyExpr(case_.condition);
			destCase.block = copyExpr(case_.block);

			dest->cases.add(destCase);
		}

		return dest;
	}
	case ExprFlavor::TYPE_LITERAL: {
		c(ExprLiteral);

		auto type = src->typeValue;

		if (type->flavor == TypeFlavor::STRUCT) {
			dest->typeValue = copyStruct(static_cast<TypeStruct *>(type));
		}
		else {
			copy(typeValue);
		}

		return dest;
	}
	case ExprFlavor::UNARY_OPERATOR: {
		c(ExprUnaryOperator);

		copy_expr(value);
		copy(op);

		return dest;
	}
	case ExprFlavor::WHILE: {
		c(ExprLoop);

		copy_expr(whileCondition);

		copyingBlocks.add({ &dest->iteratorBlock, &src->iteratorBlock });
		copyBlock(&dest->iteratorBlock, &src->iteratorBlock);

		copy_expr(body);

		copyingBlocks.pop();

		copy_expr(completedBody);

		return dest;
	}
	case ExprFlavor::INIT_IMPERATIVE_DECLARATION:
	case ExprFlavor::CONTEXT: {
		c(Expr);

		return dest;
	}
	default:
	case ExprFlavor::INFER_SET_DECLARATION_TYPE: // Should only ever exist in a flattened array
	case ExprFlavor::ADD_CONTEXT:  // This should only ever exist at the top level
	case ExprFlavor::OVERLOAD_SET: // This should never exist in something that is yet to be inferred
		assert(false);
		return nullptr;
	}

#undef c
}

void copyDeclaration(Declaration *dest, Declaration *src) {	
	copy_location();
	copy(name);
	copy(type_);
	copy_flags(~(DECLARATION_HAS_STORAGE | DECLARATION_OVERLOADS_LOCKED | DECLARATION_VALUE_IS_READY));

	if ((src->flags & DECLARATION_IS_CONSTANT) && (src->flags & DECLARATION_VALUE_POLYMORPHIC)) {
		copy_expr(typeExpr);
		copy(initialValue);
	}
	else if ((src->flags & DECLARATION_IS_CONSTANT) && (src->flags & DECLARATION_TYPE_POLYMORPHIC)) {
		copy(typeExpr);

		auto srcLiteral = static_cast<ExprLiteral *>(src->initialValue);
		auto destLiteral = POLYMORPH_NEW(ExprLiteral);
		copyExprInfo(destLiteral, srcLiteral);
		destLiteral->typeValue = srcLiteral->typeValue;
		dest->initialValue = destLiteral;
	}
	else {
		copy_expr(typeExpr);
		copy_expr(initialValue);
	}
}

Importer *copyImporter(Importer *src) {
	create(Importer);

	copy(moduleScope);
	copy_expr(import);

	return dest;
}

void copyBlock(Block *dest, Block *src) {
	copyBlockInfo(dest, src);

	for (auto declaration : src->declarations) {
		auto destDeclaration = POLYMORPH_NEW(Declaration);

		addDeclarationToBlockUnchecked(dest, destDeclaration, nullptr, declaration->serial, nullptr);

		copyDeclaration(destDeclaration, declaration);

		auto overload = declaration;
		while (overload = overload->nextOverload) {
			auto destOverload = POLYMORPH_NEW(Declaration);

			addDeclarationToBlockUnchecked(dest, destOverload, destDeclaration, overload->serial, nullptr);

			copyDeclaration(destOverload, overload);
		}
	}

	for (auto importer : src->importers) {
		auto destImporter = copyImporter(importer);

		addImporterToBlock(dest, destImporter, importer->serial);
	}
}

ExprFunction *polymorphFunction(ExprFunction *src) {
	PROFILE_FUNC();
	assert(!copyingBlocks.count);
	assert(src->flavor == ExprFlavor::FUNCTION);

	ExprFunction *dest = static_cast<ExprFunction *>(copyExpr(src));

	// Remove all arguments that have been baked during this polymorph
	u32 nonPolymorphicCount = 0;
	for (u32 i = 0; i < dest->arguments.declarations.count; i++) {
		auto argument = dest->arguments.declarations[i];

		if (argument->flags & DECLARATION_VALUE_POLYMORPHIC) {
			if (dest->arguments.table) {
				// @Hack Since we can't remove declarations from the block hash table, just set their name to the empty string so that
				// they can't be found and won't shadow the constants block delcarations
				// In reality this should never matter since the hash table is only created if there are a large number of declartions in the
				// block and there will probably never be a function with this many arguments
				argument->name = nullptr;
			}
		}
		else {
			argument->serial = nonPolymorphicCount;
			dest->arguments.declarations[nonPolymorphicCount++] = argument;
		}
	}
	dest->arguments.declarations.count = nonPolymorphicCount;

	dest->flags &= ~EXPR_FUNCTION_IS_POLYMORPHIC;
	dest->type = nullptr;

	return dest;
}

TypeStruct *polymorphStruct(TypeStruct *struct_, Arguments *arguments) {
	PROFILE_FUNC();
	assert(!copyingBlocks.count);
	assert(arguments->count == struct_->constants.declarations.count);

	TypeStruct *dest = copyStruct(struct_);

	for (u32 i = 0; i < dest->constants.declarations.count; i++) {
		dest->constants.declarations[i]->initialValue = arguments->values[i];
		dest->constants.declarations[i]->flags |= DECLARATION_VALUE_IS_READY;

		addDeclarationToBlockUnchecked(&dest->members, dest->constants.declarations[i], nullptr, dest->constants.declarations[i]->serial, nullptr);
	}

	dest->flags &= ~TYPE_IS_POLYMORPHIC;

	return dest;
}