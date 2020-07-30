#include "Basic.h"

#include "Parser.h"
#include "Lexer.h"
#include "Infer.h"
#include "TypeTable.h"

BucketedArenaAllocator parserArena(1024 * 1024);

#if 1
#define PARSER_NEW(T) new (static_cast<T *>(assert(std::this_thread::get_id() == mainThread), parserArena.allocate(sizeof(T)))) T
#define PARSER_NEW_ARRAY(T, C) new (static_cast<T *>(assert(std::this_thread::get_id() == mainThread), parserArena.allocate((C) * sizeof(T)))) T[C]

#else
#define PARSER_NEW(T) new T
#define PARSER_NEW_ARRAY(T, C) new T[C]
#endif

static Block *currentBlock = nullptr;

static bool addDeclarationToCurrentBlock(Declaration *declaration) {
	return addDeclarationToBlock(currentBlock, declaration);
}

struct BinaryOperator {
	TokenT tokens[5];
	bool rightAssociative;
};

static BinaryOperator binaryOpPrecedences[] = {
	{{TokenT::LOGIC_OR, TokenT::LOGIC_AND}},
	{{TokenT::EQUAL, TokenT::NOT_EQUAL}},
	{{TOKEN('|'), TOKEN('^'), TOKEN('&')}},
	{{TOKEN('>'), TokenT::LESS_EQUAL, TOKEN('<'), TokenT::GREATER_EQUAL}},
	{{TokenT::SHIFT_LEFT, TokenT::SHIFT_RIGHT}},
	{{TOKEN('+'), TOKEN('-')}},
	{{TOKEN('*'), TOKEN('/'), TOKEN('%')}},
};

static u64 getTokenPrecedence(TokenT token) {
	for (u64 i = 0; i < ARRAY_COUNT(binaryOpPrecedences); i++) {
		for (TokenT t : binaryOpPrecedences[i].tokens) {
			if (t == TOKEN(0)) break;

			if (t == token) return i + 1;
		}
	}

	return 0;
}

static void insertBlock(Block *block) {
	if (currentBlock) {
		block->indexInParent = currentBlock->currentIndex++;
	}
	else {
		block->indexInParent = 0;
	}
	block->parentBlock = currentBlock;
}

static void pushBlock(Block *block) {
	insertBlock(block);

	currentBlock = block;
}

static void popBlock(Block *block) { // This only takes the parameter to make sure we are always popping the block we think we are in debug
	assert(currentBlock == block);

	currentBlock = currentBlock->parentBlock;
}

static bool expectAndConsume(LexerFile *lexer, TokenT type) {
	if (lexer->token.type == type) {
		lexer->advance();
		return true;
	}

	return false;
}

static bool expectAndConsume(LexerFile *lexer, char c) {
	return expectAndConsume(lexer, TOKEN(c));
}

static bool matches(TokenT *a, TokenT *b, u64 length) {
	return memcmp(a, b, sizeof(TokenT) * length) == 0;
}

Declaration *parseDeclaration(LexerFile *lexer);
Expr *parseExpr(LexerFile *lexer);
ExprBlock *parseBlock(LexerFile *lexer, ExprBlock *block = nullptr);

ExprLiteral *parserMakeTypeLiteral(CodeLocation &start, EndLocation &end, Type *type) {
	ExprLiteral *literal = PARSER_NEW(ExprLiteral);
	literal->flavor = ExprFlavor::TYPE_LITERAL;
	literal->start = start;
	literal->end = end;
	literal->typeValue = type;
	literal->type = &TYPE_TYPE;

	return literal;
}


ExprIdentifier *makeIdentifier(CodeLocation &start, EndLocation &end, Declaration *declaration) {
	ExprIdentifier *identifier = PARSER_NEW(ExprIdentifier);
	identifier->flavor = ExprFlavor::IDENTIFIER;
	identifier->start = start;
	identifier->end = end;
	identifier->declaration = declaration;
	identifier->name = declaration->name;
	identifier->resolveFrom = currentBlock;
	identifier->enclosingScope = currentBlock;
	identifier->indexInBlock = 0;
	identifier->structAccess = nullptr;

	return identifier;
}


ExprBinaryOperator *makeBinaryOperator(CodeLocation &start, EndLocation &end, TokenT op, Expr *left) {
	ExprBinaryOperator *expr = PARSER_NEW(ExprBinaryOperator);
	expr->start = start;
	expr->end = end;
	expr->flavor = ExprFlavor::BINARY_OPERATOR;
	expr->op = op;
	expr->left = left;

	return expr;
}

Declaration *makeIterator(CodeLocation &start, EndLocation &end, String name) {
	Declaration *declaration = PARSER_NEW(Declaration);
	declaration->start = start;
	declaration->end = end;
	declaration->type = nullptr;
	declaration->initialValue = nullptr;
	declaration->name = name;


	return declaration;
}

Importer *createImporterForUsing(Declaration *oldDeclaration, Block *block) {
	auto using_ = PARSER_NEW(ExprIdentifier);
	using_->flavor = ExprFlavor::IDENTIFIER;
	using_->start = oldDeclaration->start;
	using_->end = oldDeclaration->end;
	using_->resolveFrom = block;
	using_->enclosingScope = block;
	using_->indexInBlock = block->currentIndex;
	using_->name = oldDeclaration->name;
	using_->declaration = oldDeclaration;
	using_->structAccess = nullptr;

	auto import = PARSER_NEW(Importer);
	import->import = using_;

	return import;
}

bool parseArguments(LexerFile *lexer, Arguments *args, const char *message);

ExprBlock *parseCase(LexerFile *lexer);

ExprIf *parseStaticIf(LexerFile *lexer) {
	auto staticIf = PARSER_NEW(ExprIf);
	staticIf->flavor = ExprFlavor::STATIC_IF;
	staticIf->start = lexer->token.start;

	lexer->advance();

	staticIf->condition = parseExpr(lexer);

	if (!staticIf->condition)
		return nullptr;

	staticIf->end = lexer->previousTokenEnd;

	if (expectAndConsume(lexer, ';')) {
		staticIf->ifBody = nullptr;
	}
	else {
		staticIf->ifBody = parseBlock(lexer);

		if (!staticIf->ifBody)
			return nullptr;
	}

	staticIf->elseBody = nullptr;

	if (expectAndConsume(lexer, TokenT::ELSE)) {
		if (!expectAndConsume(lexer, ';')) {
			staticIf->elseBody = parseBlock(lexer);

			if (!staticIf->elseBody)
				return nullptr;
		}
	}

	if (currentBlock) {
		auto importer = PARSER_NEW(Importer);

		importer->import = staticIf;

		addImporterToBlock(currentBlock, importer);
	}

	return staticIf;
}

Expr *parseExprStatemenet(LexerFile *lexer, bool allowDeclarations) {
	Expr *expr = parseExpr(lexer);

	if (!expr)
		return nullptr;

	CodeLocation start = lexer->token.start;
	EndLocation end = lexer->token.end;

	if (expectAndConsume(lexer, ',')) {
		auto comma = PARSER_NEW(ExprCommaAssignment);

		comma->flavor = ExprFlavor::COMMA_ASSIGNMENT;

		Array<Expr *> exprs;
		exprs.add(expr);

		while (true) {
			comma->start = lexer->token.start;
			comma->end = lexer->token.end;

			if (expectAndConsume(lexer, '=')) {
				comma->call = parseExpr(lexer);

				break;
			}
			else if (expectAndConsume(lexer, ':')) {
				if (!allowDeclarations) {
					reportError(&lexer->token, "Error: Cannot have a declaration here");
					return nullptr;
				}

				comma->flags |= EXPR_COMMA_ASSIGNMENT_IS_DECLARATION;
				comma->end = lexer->token.end;

				if (!expectAndConsume(lexer, '=')) {
					reportExpectedError(&lexer->token, "Error: Expected = after : in multi declaration");
					return nullptr;
				}

				comma->call = parseExpr(lexer);

				for (auto expr : exprs) {
					if (expr->flavor != ExprFlavor::IDENTIFIER) {
						reportError(expr, "Error: Multi-declarations must only assign to identifiers");
						return nullptr;
					}

					auto identifier = static_cast<ExprIdentifier *>(expr);

					auto declaration = PARSER_NEW(Declaration);

					declaration->start = identifier->start;
					declaration->end = identifier->end;
					declaration->name = identifier->name;
					declaration->flags |= DECLARATION_IS_IN_COMPOUND;
					declaration->type = nullptr;
					declaration->initialValue = nullptr;
					declaration->physicalStorage = 0;

					if (!addDeclarationToCurrentBlock(declaration)) {
						return nullptr;
					}

					identifier->declaration = declaration;
				}

				break;
			}
			else {
				expr = parseExpr(lexer);

				if (!expr)
					return nullptr;

				exprs.add(expr);
			}
		}

		if (comma->call->flavor != ExprFlavor::FUNCTION_CALL) {
			reportError(comma->call, "Error: Multi assignments can only have a function call on the right");
			return nullptr;
		}

		comma->call->flags |= EXPR_FUNCTION_CALL_IS_IN_COMMA_ASSIGNMENT;


		comma->exprCount = exprs.count;
		comma->left = exprs.storage;

		return comma;
	}

#define MODIFY_ASSIGN(type)                                                      \
		else if (expectAndConsume(lexer, type)) {                                \
			ExprBinaryOperator *op = makeBinaryOperator(start, end, type, expr); \
																				 \
			op->right = parseExpr(lexer);										 \
																				 \
			if (!op->right)														 \
				return nullptr;												     \
																				 \
			return op;															 \
		}

	if (false);
	MODIFY_ASSIGN(TOKEN('='))
		MODIFY_ASSIGN(TokenT::PLUS_EQUALS)
		MODIFY_ASSIGN(TokenT::MINUS_EQUALS)
		MODIFY_ASSIGN(TokenT::TIMES_EQUALS)
		MODIFY_ASSIGN(TokenT::DIVIDE_EQUALS)
		MODIFY_ASSIGN(TokenT::MOD_EQUALS)
		MODIFY_ASSIGN(TokenT::SHIFT_LEFT_EQUALS)
		MODIFY_ASSIGN(TokenT::SHIFT_RIGHT_EQUALS)
		MODIFY_ASSIGN(TokenT::XOR_EQUALS)
		MODIFY_ASSIGN(TokenT::AND_EQUALS)
		MODIFY_ASSIGN(TokenT::OR_EQUALS)
	else {
		if (expr->flavor != ExprFlavor::FUNCTION_CALL) {
			auto unary = static_cast<ExprUnaryOperator *>(expr);
			auto binary = static_cast<ExprBinaryOperator *>(expr);

			if (expr->flavor == ExprFlavor::UNARY_OPERATOR && unary->op == TOKEN('+') &&
				unary->value->flavor == ExprFlavor::UNARY_OPERATOR && static_cast<ExprUnaryOperator *>(unary->value)->op == TOKEN('+') &&
				unary->start.locationInMemory + 1 == unary->value->start.locationInMemory) {

				reportError(expr, "Error: '++' is not supported");
			}
			else if (expr->flavor == ExprFlavor::BINARY_OPERATOR && binary->op == TOKEN('+') &&
				binary->right->flavor == ExprFlavor::UNARY_OPERATOR && static_cast<ExprUnaryOperator *>(binary->right)->op == TOKEN('+') &&
				binary->start.locationInMemory + 1 == binary->right->start.locationInMemory) {

				reportError(expr, "Error: '++' is not supported");
			}
			else if (lexer->token.type == TokenT::DOUBLE_DASH) {
				reportError(&lexer->token, "Error: '--' is not supported as an operator");
			}
			else {
				reportError(expr, "Error: Can only have an assignment or function call expression at statement level");
			}
			return nullptr;
		}
		else {
			expr->flags |= EXPR_FUNCTION_CALL_IS_STATEMENT_LEVEL;
		}

		return expr;
	}
}

Expr *parseStatement(LexerFile *lexer, bool allowDeclarations) {
	if (lexer->token.type == TokenT::FOR) {
		ExprLoop *loop = PARSER_NEW(ExprLoop);
		loop->flavor = ExprFlavor::FOR;
		loop->start = lexer->token.start;
		loop->iteratorBlock.flags |= BLOCK_IS_LOOP;

		EndLocation end = lexer->token.end;

		lexer->advance();

		if (expectAndConsume(lexer, '*')) {
			loop->flags |= EXPR_FOR_BY_POINTER;
		}

		if (lexer->token.type == TokenT::IDENTIFIER) {
			TokenT peek;

			lexer->peekTokenTypes(1, &peek); // Check if this identifier is an it declaration, an it, it_index declaration or an identifier
			if (peek == TOKEN(':')) {
				Declaration *it = makeIterator(lexer->token.start, lexer->token.end, lexer->token.text);
				it->flags |= DECLARATION_IS_ITERATOR;

				if (!addDeclarationToBlock(&loop->iteratorBlock, it)) {
					assert(false); // Invalid code path, we should never fail to add something to an empty block
				}
				lexer->advance();

				assert(lexer->token.type == TOKEN(':'));

				Declaration *it_index = makeIterator(lexer->token.start, lexer->token.end, "it_index");
				it_index->flags |= DECLARATION_IS_ITERATOR_INDEX;

				if (!addDeclarationToBlock(&loop->iteratorBlock, it_index)) {
					return nullptr;
				}

				lexer->advance();
			}
			else if (peek == TOKEN(',')) {
				Declaration *it = makeIterator(lexer->token.start, lexer->token.end, lexer->token.text);
				it->flags |= DECLARATION_IS_ITERATOR;

				if (!addDeclarationToBlock(&loop->iteratorBlock, it)) {
					assert(false); // Invalid code path, we should never fail to add something to an empty block
				}
				lexer->advance();

				assert(lexer->token.type == TOKEN(','));

				lexer->advance();

				if (lexer->token.type != TokenT::IDENTIFIER) {
					reportExpectedError(&lexer->token, "Error: Expected index variable name after ,");
					return nullptr;
				}

				Declaration *it_index = makeIterator(lexer->token.start, lexer->token.end, lexer->token.text);
				it_index->flags |= DECLARATION_IS_ITERATOR_INDEX;

				if (!addDeclarationToBlock(&loop->iteratorBlock, it_index)) {
					return nullptr;
				}

				lexer->advance();

				if (!expectAndConsume(lexer, ':')) {
					reportExpectedError(&lexer->token, "Error: Expected a ':' after index variable name");
					return nullptr;
				}
			}
			else {
				goto notDeclaration;
			}
		}
		else {
		notDeclaration:
			Declaration *it = makeIterator(loop->start, end, "it");
			it->flags |= DECLARATION_IS_ITERATOR;
			if (!addDeclarationToBlock(&loop->iteratorBlock, it)) {
				assert(false); // Invalid code path, we should never fail to add something to an empty block
			}


			Declaration *it_index = makeIterator(loop->start, end, "it_index");
			it_index->flags |= DECLARATION_IS_ITERATOR_INDEX;
			if (!addDeclarationToBlock(&loop->iteratorBlock, it_index)) {
				assert(false); // Invalid code path, we should never fail to add this
			}
		}

		loop->forBegin = parseExpr(lexer);
		if (!loop->forBegin)
			return nullptr;

		if (expectAndConsume(lexer, TokenT::DOUBLE_DOT)) {
			loop->forEnd = parseExpr(lexer);

			if (!loop->forEnd)
				return nullptr;
		}
		else {
			loop->forEnd = nullptr;
		}

		if (expectAndConsume(lexer, ';')) {
			loop->body = nullptr;

			loop->iteratorBlock.parentBlock = currentBlock; // Since we never push the block we need to manually set the parent
			loop->end = lexer->previousTokenEnd;
		}
		else {
			loop->end = lexer->previousTokenEnd;

			pushBlock(&loop->iteratorBlock);
			loop->body = parseStatement(lexer, false);

			if (!loop->body)
				return nullptr;

			popBlock(&loop->iteratorBlock);

		}

		// A completed block is executed if the loop finishes without a break statement or a continue to an outer loop
		if (expectAndConsume(lexer, TokenT::COMPLETED)) {
			loop->end = lexer->previousTokenEnd;
			if (expectAndConsume(lexer, ';')) {
				loop->completedBody = nullptr;

			}
			else {
				loop->completedBody = parseStatement(lexer, false);

				if (!loop->completedBody)
					return nullptr;
			}
		}
		else {
			loop->completedBody = nullptr;
		}

		return loop;
	}
	else if (lexer->token.type == TOKEN('{')) {
		return parseBlock(lexer);
	}
	else if (lexer->token.type == TokenT::WHILE) {
		ExprLoop *loop = PARSER_NEW(ExprLoop);
		loop->flavor = ExprFlavor::WHILE;
		loop->start = lexer->token.start;
		loop->iteratorBlock.flags |= BLOCK_IS_LOOP;

		EndLocation end = lexer->token.end;

		lexer->advance();

		if (lexer->token.type == TokenT::IDENTIFIER) {
			TokenT peek;

			lexer->peekTokenTypes(1, &peek); // Check if this identifier is an it declaration, an it, it_index declaration or an identifier
			if (peek == TOKEN(':')) {
				Declaration *it = makeIterator(lexer->token.start, lexer->token.end, lexer->token.text);
				it->flags |= DECLARATION_IS_ITERATOR;

				if (!addDeclarationToBlock(&loop->iteratorBlock, it)) {
					assert(false); // Invalid code path, we should never fail to add something to an empty block
				}
				lexer->advance();

				assert(lexer->token.type == TOKEN(':'));

				lexer->advance();
			}
		}

		loop->whileCondition = parseExpr(lexer);
		if (!loop->whileCondition)
			return nullptr;

		if (expectAndConsume(lexer, ';')) {
			loop->body = nullptr;

			loop->iteratorBlock.parentBlock = currentBlock; // Since we never push the block we need to manually set the parent
			loop->end = lexer->previousTokenEnd;
		}
		else {
			loop->end = lexer->previousTokenEnd;

			pushBlock(&loop->iteratorBlock);
			loop->body = parseStatement(lexer, false);

			if (!loop->body)
				return nullptr;

			popBlock(&loop->iteratorBlock);

		}

		// A completed block is executed if the loop finishes without a break statement or a continue to an outer loop
		if (expectAndConsume(lexer, TokenT::COMPLETED)) {
			loop->end = lexer->previousTokenEnd;
			if (expectAndConsume(lexer, ';')) {
				loop->completedBody = nullptr;

			}
			else {
				loop->completedBody = parseStatement(lexer, false);

				if (!loop->completedBody)
					return nullptr;
			}
		}
		else {
			loop->completedBody = nullptr;
		}

		return loop;
	}
	else if (lexer->token.type == TokenT::IF) {
		auto start = lexer->token.start;

		lexer->advance();

		auto condition = parseExpr(lexer);
		if (!condition)
			return nullptr;

		if (expectAndConsume(lexer, TokenT::EQUAL)) {
			bool complete = expectAndConsume(lexer, TokenT::COMPLETE);

			if (!expectAndConsume(lexer, '{')) {
				reportExpectedError(&lexer->token, "Error: Expected '{' after '==' in switch if");
				return nullptr;
			}

			auto switch_ = PARSER_NEW(ExprSwitch);

			switch_->start = start;
			switch_->end = lexer->previousTokenEnd;
			switch_->flavor = ExprFlavor::SWITCH;
			switch_->condition = condition;

			if (complete) {
				switch_->flags |= EXPR_SWITCH_IS_COMPLETE;
			}


			Block *block = PARSER_NEW(Block);

			pushBlock(block);

			Expr *else_ = nullptr;

			while (true) {			
				if (lexer->token.type == TokenT::CASE) {
					auto start = lexer->token.start;

					lexer->advance();

					ExprSwitch::Case case_;

					case_.fallsThrough = false;
					auto condition = parseExpr(lexer);

					if (!condition)
						return nullptr;

					auto equal = makeBinaryOperator(start, condition->end, TokenT::EQUAL, switch_->condition);
					equal->right = condition;

					case_.condition = equal;
					

					case_.block = parseCase(lexer);


					if (!case_.block) {
						return nullptr;
					}

					if (expectAndConsume(lexer, TokenT::THROUGH)) {
						case_.fallsThrough = true;
					}

					switch_->cases.add(case_);
				}
				else if (lexer->token.type == TokenT::ELSE) {
					if (else_) {
						reportError(&lexer->token, "Error: A switch if cannot have multiple 'else' cases");
						reportError(else_, "   ..: Here is the previous else");
						return nullptr;
					}

					auto start = lexer->token.start;

					lexer->advance();

					ExprSwitch::Case case_;

					case_.fallsThrough = false;
					case_.condition = nullptr;
					case_.block = parseCase(lexer);

					if (!case_.block) {
						return nullptr;
					}

					if (expectAndConsume(lexer, TokenT::THROUGH)) {
						case_.fallsThrough = true;
					}

					case_.block->start = start;
					case_.block->end = lexer->token.end;

					else_ = case_.block;

					switch_->cases.add(case_);
				}
				else if (lexer->token.type == TOKEN('}')) {
					break;
				}
			}

			lexer->advance();


			popBlock(block);

			return switch_;
		}
		else {
			ExprIf *ifElse = PARSER_NEW(ExprIf);
			ifElse->flavor = ExprFlavor::IF;
			ifElse->start = start;
			ifElse->condition = condition;

			if (expectAndConsume(lexer, ';')) {
				ifElse->ifBody = nullptr;

				ifElse->end = lexer->previousTokenEnd;
			}
			else {
				ifElse->end = lexer->previousTokenEnd;

				ifElse->ifBody = parseStatement(lexer, false);

				if (!ifElse->ifBody)
					return nullptr;
			}

			if (expectAndConsume(lexer, TokenT::ELSE)) {
				if (expectAndConsume(lexer, ';')) {
					ifElse->elseBody = nullptr;
				}
				else {
					ifElse->elseBody = parseStatement(lexer, false);

					if (!ifElse->elseBody)
						return nullptr;
				}
			}
			else {
				ifElse->elseBody = nullptr;
			}

			return ifElse;
		}
	}
	else if (lexer->token.type == TokenT::STATIC_IF) {
		return parseStaticIf(lexer);
	}
	else if (lexer->token.type == TokenT::CONTINUE || lexer->token.type == TokenT::BREAK || lexer->token.type == TokenT::REMOVE) {
		ExprBreakOrContinue *continue_ = PARSER_NEW(ExprBreakOrContinue);
		continue_->flavor =
			lexer->token.type == TokenT::CONTINUE ? ExprFlavor::CONTINUE :
			(lexer->token.type == TokenT::REMOVE ? ExprFlavor::REMOVE : ExprFlavor::BREAK);


		continue_->start = lexer->token.start;

		lexer->advance();

		if (lexer->token.type == TokenT::IDENTIFIER) {
			auto identifier = PARSER_NEW(ExprIdentifier);
			identifier->start = lexer->token.start;
			identifier->end = lexer->token.end;
			identifier->name = lexer->token.text;
			identifier->flavor = ExprFlavor::IDENTIFIER;
			identifier->resolveFrom = currentBlock;
			identifier->enclosingScope = currentBlock;
			identifier->flags |= EXPR_IDENTIFIER_IS_BREAK_OR_CONTINUE_LABEL;
			identifier->structAccess = nullptr;

			if (currentBlock) {
				identifier->indexInBlock = currentBlock->currentIndex;
			}

			continue_->label = identifier;

			continue_->end = lexer->token.end;

			lexer->advance();
			return continue_;
		}
		else { // @Cleanup: Currently unlabelled break is resolved during parse but labelled break is resolved far away in type inference, should this change?
			continue_->label = nullptr;
			continue_->end = lexer->previousTokenEnd;

			// Don't pass through arguments blocks since we can't break from an inner function to an outer one
			for (Block *block = currentBlock; block && !(block->flags & BLOCK_IS_ARGUMENTS); block = block->parentBlock) {
				if (block->flags & BLOCK_IS_LOOP) {
					continue_->refersTo = CAST_FROM_SUBSTRUCT(ExprLoop, iteratorBlock, block);
					return continue_;
				}
			}

			reportError(continue_, "Error: Cannot have a %s outside of a loop", lexer->token.type == TokenT::CONTINUE ? "continue" :
				(lexer->token.type == TokenT::REMOVE ? "remove" : "break"));
			return nullptr;
		}
	}
	else if (lexer->token.type == TokenT::RETURN) {
		ExprReturn *return_ = PARSER_NEW(ExprReturn);
		return_->flavor = ExprFlavor::RETURN;
		return_->start = lexer->token.start;
		lexer->advance();


		if (expectAndConsume(lexer, ';') || lexer->token.type == TOKEN('}')) {
			return_->returns.count = 0;
		}
		else {
			if (!parseArguments(lexer, &return_->returns, "returns"))
				return nullptr;

		}

		return_->end = lexer->previousTokenEnd;

		for (Block *block = currentBlock; block; block = block->parentBlock) {
			if (block->flags & BLOCK_IS_ARGUMENTS) {
				return_->returnsFrom = CAST_FROM_SUBSTRUCT(ExprFunction, arguments, block);
				return return_;
			}
		}

		assert(false); // Invalid code path, how do we have a return statement outside a function?
		return nullptr;
	}
	else {
		return parseExprStatemenet(lexer, allowDeclarations);
	}
}

ExprBlock *parseCase(LexerFile *lexer) {
	auto block = PARSER_NEW(ExprBlock);

	block->flavor = ExprFlavor::BLOCK;

	pushBlock(&block->declarations);

	while (true) {
		if (expectAndConsume(lexer, ';')) {
			continue;
		}
		else if (lexer->token.type == TOKEN('}') || lexer->token.type == TokenT::CASE || lexer->token.type == TokenT::ELSE || lexer->token.type == TokenT::THROUGH) {
			break;
		}
		else if (lexer->token.type == TokenT::USING) {
			TokenT peek[2];
			lexer->peekTokenTypes(2, peek);

			if (peek[1] == TOKEN(':')) { // It is a declaration
				Declaration *declaration = parseDeclaration(lexer);

				if (!declaration)
					return nullptr;

				if (!addDeclarationToCurrentBlock(declaration)) {
					return nullptr;
				}

				assert(declaration->flags & DECLARATION_MARKED_AS_USING);
				currentBlock->importers.add(createImporterForUsing(declaration, currentBlock));

				if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IS_UNINITIALIZED))) { // If this declaration is constant or uninitialized don't add an initialization expression
					ExprBinaryOperator *assign = PARSER_NEW(ExprBinaryOperator);
					assign->start = declaration->start;
					assign->end = declaration->end;
					assign->flavor = ExprFlavor::BINARY_OPERATOR;
					assign->op = TOKEN('=');
					assign->left = makeIdentifier(declaration->start, declaration->end, declaration);
					assign->right = nullptr;
					assign->flags |= EXPR_ASSIGN_IS_IMPLICIT_INITIALIZER;

					block->exprs.add(assign);
				}

			}
			else {
				auto start = lexer->token.start;

				lexer->advance();

				auto using_ = parseExpr(lexer);

				if (!using_)
					return nullptr;

				auto importer = PARSER_NEW(Importer);
				importer->import = using_;
				addImporterToBlock(currentBlock, importer);
			}

			continue;
		}
		else if (lexer->token.type == TokenT::IDENTIFIER) { // This could be an expression or a declaration
			TokenT peek;
			lexer->peekTokenTypes(1, &peek);

			if (peek == TOKEN(':')) { // It is a declaration
				Declaration *declaration = parseDeclaration(lexer);

				if (!declaration)
					return nullptr;

				if (!addDeclarationToCurrentBlock(declaration)) {
					return nullptr;
				}

				assert(!(declaration->flags & DECLARATION_MARKED_AS_USING));

				if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IS_UNINITIALIZED))) { // If this declaration is constant or uninitialized don't add an initialization expression
					ExprBinaryOperator *assign = PARSER_NEW(ExprBinaryOperator);
					assign->start = declaration->start;
					assign->end = declaration->end;
					assign->flavor = ExprFlavor::BINARY_OPERATOR;
					assign->op = TOKEN('=');
					assign->left = makeIdentifier(declaration->start, declaration->end, declaration);
					assign->right = nullptr;
					assign->flags |= EXPR_ASSIGN_IS_IMPLICIT_INITIALIZER;

					block->exprs.add(assign);
				}

				continue;
			}
		}

		Expr *expr = parseStatement(lexer, true);

		if (!expr)
			return nullptr;

		block->exprs.add(expr);
	}

	block->end = lexer->previousTokenEnd;

	popBlock(&block->declarations);

	return block;
}

ExprBlock *parseBlock(LexerFile *lexer, ExprBlock *block) {
	if (!block) { // A function may preallocate the block to fill in the 'using's
		block = PARSER_NEW(ExprBlock);
	}

	block->start = lexer->token.start;
	block->flavor = ExprFlavor::BLOCK;

	pushBlock(&block->declarations);

	if (!expectAndConsume(lexer, '{')) {
		reportError(&lexer->token, "Error: Expected '{' at the start of a block");
		return nullptr;
	}

	while (true) {
		if (expectAndConsume(lexer, ';')) {
			continue;
		}
		else if (expectAndConsume(lexer, '}')) {
			break;
		}
		else if (lexer->token.type == TokenT::DEFER) {
			auto defer = PARSER_NEW(ExprDefer);
			defer->flavor = ExprFlavor::DEFER;
			defer->start = lexer->token.start;
			defer->enclosingScope = currentBlock;

			lexer->advance();
			
			defer->expr = parseExprStatemenet(lexer, false);

			if (!defer->expr) {
				return nullptr;
			}

			defer->end = lexer->previousTokenEnd;

			block->exprs.add(defer);

			continue;
		}
		else if (lexer->token.type == TokenT::USING) {
			TokenT peek[2];
			lexer->peekTokenTypes(2, peek);

			if (peek[1] == TOKEN(':')) { // It is a declaration
				Declaration *declaration = parseDeclaration(lexer);

				if (!declaration)
					return nullptr;

				if (!addDeclarationToCurrentBlock(declaration)) {
					return nullptr;
				}

				assert(declaration->flags & DECLARATION_MARKED_AS_USING);
				addImporterToBlock(currentBlock, createImporterForUsing(declaration, currentBlock));

				if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IS_UNINITIALIZED))) { // If this declaration is constant or uninitialized don't add an initialization expression
					ExprBinaryOperator *assign = PARSER_NEW(ExprBinaryOperator);
					assign->start = declaration->start;
					assign->end = declaration->end;
					assign->flavor = ExprFlavor::BINARY_OPERATOR;
					assign->op = TOKEN('=');
					assign->left = makeIdentifier(declaration->start, declaration->end, declaration);
					assign->right = nullptr;
					assign->flags |= EXPR_ASSIGN_IS_IMPLICIT_INITIALIZER;

					block->exprs.add(assign);
				}

			}
			else {
				auto start = lexer->token.start;

				lexer->advance();

				auto using_ = parseExpr(lexer);

				if (!using_)
					return nullptr;

				auto importer = PARSER_NEW(Importer);

				importer->import = using_;
				
				addImporterToBlock(currentBlock, importer);
			}

			continue;
		}
		else if (lexer->token.type == TokenT::IDENTIFIER) { // This could be an expression or a declaration
			TokenT peek;
			lexer->peekTokenTypes(1, &peek);

			if (peek == TOKEN(':')) { // It is a declaration
				Declaration *declaration = parseDeclaration(lexer);

				if (!declaration)
					return nullptr;

				if (!addDeclarationToCurrentBlock(declaration)) {
					return nullptr;
				}

				assert(!(declaration->flags & DECLARATION_MARKED_AS_USING));

				if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IS_UNINITIALIZED))) { // If this declaration is constant or uninitialized don't add an initialization expression
					ExprBinaryOperator *assign = PARSER_NEW(ExprBinaryOperator);
					assign->start = declaration->start;
					assign->end = declaration->end;
					assign->flavor = ExprFlavor::BINARY_OPERATOR;
					assign->op = TOKEN('=');
					assign->left = makeIdentifier(declaration->start, declaration->end, declaration);
					assign->right = nullptr;
					assign->flags |= EXPR_ASSIGN_IS_IMPLICIT_INITIALIZER;

					block->exprs.add(assign);
				}

				continue;
			}
		}

		Expr *expr = parseStatement(lexer, true);

		if (!expr)
			return nullptr;

		block->exprs.add(expr);
	}

	block->end = lexer->previousTokenEnd;

	popBlock(&block->declarations);

	return block;
}

ExprLiteral *makeIntegerLiteral(CodeLocation &start, EndLocation &end, u64 value, Type *type) {
	ExprLiteral *literal = PARSER_NEW(ExprLiteral);
	literal->start = start;
	literal->end = end;
	literal->unsignedValue = value;
	literal->type = type;
	literal->flavor = ExprFlavor::INT_LITERAL;
	return literal;
}

ExprStringLiteral *makeStringLiteral(CodeLocation &start, EndLocation &end, String text) {
	ExprStringLiteral *literal = PARSER_NEW(ExprStringLiteral);
	literal->start = start;
	literal->end = end;
	literal->string = text;
	literal->type = &TYPE_STRING;
	literal->flavor = ExprFlavor::STRING_LITERAL;
	return literal;
}

void addVoidReturn(CodeLocation &start, EndLocation &end, ExprFunction *function) {
	auto returnType = PARSER_NEW(Declaration);

	returnType->type = parserMakeTypeLiteral(start, end, &TYPE_VOID);
	returnType->start = start;
	returnType->end = end;
	returnType->name = String(nullptr, 0ULL);
	returnType->initialValue = nullptr;
	returnType->flags |= DECLARATION_IS_RETURN;

	addDeclarationToBlock(&function->returns, returnType);
}

bool parseFunctionReturnTypes(LexerFile *lexer, ExprFunction *function, bool *hadReturns) {
	pushBlock(&function->returns);

	*hadReturns = true;

	if (expectAndConsume(lexer, TokenT::ARROW)) {
		auto returnType = PARSER_NEW(Declaration);

		if (expectAndConsume(lexer, TokenT::MUST))
			returnType->flags |= DECLARATION_IS_MUST;

		auto expr = parseExpr(lexer);

		if (!expr)
			return false;

		returnType->type = expr;
		returnType->start = expr->start;
		returnType->end = expr->end;
		returnType->name = String(nullptr, 0ULL);
		returnType->initialValue = nullptr;
		returnType->flags |= DECLARATION_IS_RETURN;

		addDeclarationToBlock(&function->returns, returnType);
	}
	else if (lexer->token.type == TOKEN('(')) {
		TokenT peek;
		
		lexer->peekTokenTypes(1, &peek);

		if (peek == TokenT::ARROW) {
			lexer->advance();
			lexer->advance();

			bool must = expectAndConsume(lexer, TokenT::MUST);

			if (lexer->token.type == TOKEN(')')) {
				if (must) {
					reportExpectedError(&lexer->token, "Error: Expected return type after #must directive");
					return false;
				}
				else {
					addVoidReturn(lexer->token.start, lexer->token.end, function);

					lexer->advance();

					return true;
				}
			}

			if (lexer->token.type == TokenT::IDENTIFIER) {
				lexer->peekTokenTypes(1, &peek);

				if (peek == TOKEN(':')) {
					do {
						if (!must) {
							must = expectAndConsume(lexer, TokenT::MUST);
						}

						auto returnType = parseDeclaration(lexer);

						if (!returnType)
							return false;

						if (returnType->flags & DECLARATION_MARKED_AS_USING) {
							reportError(returnType, "Error: Cannot mark return types as using");
							return false;
						}

						if (returnType->flags & DECLARATION_IS_CONSTANT) {
							reportError(returnType, "Error: Return types cannot be constant declarations");
							return false;
						}

						if (returnType->flags & DECLARATION_IS_UNINITIALIZED) {
							reportError(returnType, "Error: Return types cannot be uninitialized");
							return false;
						}

						returnType->flags |= DECLARATION_IS_RETURN;

						if (must) {
							returnType->flags |= DECLARATION_IS_MUST;
							must = false;
						}
						addDeclarationToBlock(&function->returns, returnType);
					} while (expectAndConsume(lexer, ','));

					if (!expectAndConsume(lexer, ')')) {
						reportExpectedError(&lexer->token, "Error: Expected ) after return list");
						return false;
					}

					popBlock(&function->returns);
					return true;
				}
			}

			do {
				if (!must) {
					must = expectAndConsume(lexer, TokenT::MUST);
				}

				auto returnType = PARSER_NEW(Declaration);

				if (must) {
					returnType->flags |= DECLARATION_IS_MUST;
					must = false;
				}

				auto expr = parseExpr(lexer);

				if (!expr)
					return false;

				returnType->type = expr;
				returnType->start = expr->start;
				returnType->end = expr->end;
				returnType->name = String(nullptr, 0ULL);
				returnType->initialValue = nullptr;
				returnType->flags |= DECLARATION_IS_RETURN;

				addDeclarationToBlock(&function->returns, returnType);
			} while (expectAndConsume(lexer, ','));

			if (!expectAndConsume(lexer, ')')) {
				reportExpectedError(&lexer->token, "Error: Expected ) after return list");
				return false;
			}
		}
	}
	else {
		*hadReturns = false;

		addVoidReturn(lexer->token.start, lexer->token.end, function);
	}

	popBlock(&function->returns);

	return true;
}

bool parseFunctionPostfix(LexerFile *lexer, ExprFunction *function, ExprBlock *usingBlock) {
	bool hadReturnType;

	if (!parseFunctionReturnTypes(lexer, function, &hadReturnType)) {
		return false;
	}


	if (lexer->token.type == TOKEN('{')) {
		function->flavor = ExprFlavor::FUNCTION;
		function->end = lexer->previousTokenEnd;
		pushBlock(&function->arguments);

		function->body = parseBlock(lexer, usingBlock);
		if (!function->body) {
			return false;
		}

		popBlock(&function->arguments);
	}
	else if (lexer->token.type == TokenT::EXTERNAL) {
		lexer->advance();

		function->flavor = ExprFlavor::FUNCTION;
		function->end = lexer->previousTokenEnd;

		if (usingBlock) {
			reportError(function, "Error: External functions cannot 'using' their parameters");
			return false;
		}


		function->body = nullptr;
		function->flags |= EXPR_FUNCTION_IS_EXTERNAL;

		if (function->returns.declarations.count != 1) {
			reportError(function, "Error: External functions cannot have multiple return values");
			return false;
		}

		insertBlock(&function->arguments);
	}
	else { // This is a function type
		function->end = lexer->previousTokenEnd;

		if (!hadReturnType) {
			reportError(function, "Error: A function prototype must have a return type");
			return false;
		}

		function->flavor = ExprFlavor::FUNCTION_PROTOTYPE;
		function->type = &TYPE_TYPE;

		insertBlock(&function->arguments);
		
		for (auto return_ : function->returns.declarations) {
			if (return_->flags & DECLARATION_IS_MUST) {
				reportError(function, "Error: A function prototype cannot have its return type marked as #must");
				return false;
			}
		}

		if (usingBlock) {
			reportError(function, "Error: Function prototypes cannot 'using' their parameters");
			return false;
		}

		for (auto declaration : function->arguments.declarations) {
			if (declaration->initialValue) {
				reportError(declaration, "Error: A function prototype cannot have a default argument");
				return false;
			}
		}


		for (auto declaration : function->returns.declarations) {
			if (declaration->initialValue) {
				reportError(declaration, "Error: A function prototype cannot have a default return value");
				return false;
			}
		}
	}

	return true;
}

Expr *parseFunctionOrParentheses(LexerFile *lexer, CodeLocation start) {
	EndLocation end = lexer->token.end;
	Expr *expr = nullptr;

	TokenT peek;

	lexer->peekTokenTypes(1, &peek);

	if (expectAndConsume(lexer, ')')) { // This is a function with no arguments
		ExprFunction *function = PARSER_NEW(ExprFunction);
		function->start = start;
		function->arguments.flags |= BLOCK_IS_ARGUMENTS;
		function->returns.flags |= BLOCK_IS_RETURNS;

		if (!parseFunctionPostfix(lexer, function, nullptr))
			return nullptr;

		expr = function;
	}
	else if ((lexer->token.type == TokenT::IDENTIFIER && peek == TOKEN(':')) || lexer->token.type == TokenT::USING) { // This is an argument declaration
		ExprFunction *function = PARSER_NEW(ExprFunction);
		function->flavor = ExprFlavor::FUNCTION;
		function->start = start;
		function->arguments.flags |= BLOCK_IS_ARGUMENTS;
		function->returns.flags |= BLOCK_IS_RETURNS;

		ExprBlock *usingBlock = nullptr;

		do {
			Declaration *declaration = parseDeclaration(lexer);
			if (!declaration) {
				return nullptr;
			}

			declaration->flags |= DECLARATION_IS_ARGUMENT;

			if (declaration->flags & DECLARATION_IS_CONSTANT) {
				reportError(declaration, "Error: Cannot have a constant default argument");
				return nullptr;
			}

			if (declaration->flags & DECLARATION_IS_UNINITIALIZED) {
				reportError(declaration, "Error: Cannot have an uninitialized default argument");
				return nullptr;
			}

			if (!addDeclarationToBlock(&function->arguments, declaration)) {
				return nullptr;
			}

			if (declaration->flags & DECLARATION_MARKED_AS_USING) {
				if (!usingBlock) {
					usingBlock = PARSER_NEW(ExprBlock);
				}

				addImporterToBlock(&usingBlock->declarations, createImporterForUsing(declaration, &function->arguments));
			}
		} while (expectAndConsume(lexer, ','));

		if (!expectAndConsume(lexer, ')')) {
			reportExpectedError(&lexer->token, "Error: Expected a ')' after function arguments");
			return nullptr;
		}

		if (!parseFunctionPostfix(lexer, function, usingBlock))
			return nullptr;

		expr = function;
	}
	else {
		// This is one or more expressions in parentheses

		expr = parseExpr(lexer);
		if (!expr) {
			return nullptr;
		}

		if (expectAndConsume(lexer, ')')) { // It was a single expression in parentheses
			bool func = false;

			if (lexer->token.type == TokenT::ARROW) {
				func = true;
			}
			else if (lexer->token.type == TOKEN('(')) {
				TokenT peek;

				lexer->peekTokenTypes(1, &peek);

				func = peek == TokenT::ARROW;
			}

			if (func) { // This is a function type since it has a return value
				auto type = PARSER_NEW(ExprFunction);
				type->flavor = ExprFlavor::FUNCTION_PROTOTYPE;
				type->start = start;

				type->arguments.flags |= BLOCK_IS_ARGUMENTS;
				type->returns.flags |= BLOCK_IS_RETURNS;

				bool hadReturnType;

				if (!parseFunctionReturnTypes(lexer, type, &hadReturnType))
					return nullptr;

				type->end = lexer->previousTokenEnd;
				type->type = &TYPE_TYPE;

				pushBlock(&type->arguments);

				auto argument = PARSER_NEW(Declaration);
				argument->type = expr;
				argument->start = expr->start;
				argument->end = expr->end;
				argument->name = String(nullptr, 0ULL);
				argument->initialValue = nullptr;
				argument->flags |= DECLARATION_IS_ARGUMENT;

				addDeclarationToBlock(&type->arguments, argument);

				popBlock(&type->arguments);

				for (auto return_ : type->returns.declarations) {
					if (return_->flags & DECLARATION_IS_MUST) {
						reportError(type, "Error: A function prototype cannot have its return type marked as #must");
						return nullptr;
					}
				}

				if (!type->returns.declarations.count) {
					reportError(type, "Error: A function prototype must have a return type");
					return nullptr;
				}

				for (auto declaration : type->returns.declarations) {
					if (declaration->initialValue) {
						reportError(declaration, "Error: A function prototype cannot have a default return value");
						return nullptr;
					}
				}


				expr = type;
			}
			else {
				// We have put the bracketed expression in expr so we are done
			}
		}
		else if (expectAndConsume(lexer, ',')) { // This is a function type since there are multiple comma separated values in parentheses
			auto *type = PARSER_NEW(ExprFunction);

			type->arguments.flags |= BLOCK_IS_ARGUMENTS;
			type->returns.flags |= BLOCK_IS_RETURNS;

			pushBlock(&type->arguments);

			auto argument = PARSER_NEW(Declaration);
			argument->type = expr;
			argument->start = expr->start;
			argument->end = expr->end;
			argument->name = String(nullptr, 0ULL);
			argument->initialValue = nullptr;
			argument->flags |= DECLARATION_IS_ARGUMENT;

			addDeclarationToBlock(&type->arguments, argument);


			do {
				Expr *arg = parseExpr(lexer);
				if (!arg)
					return nullptr;

				argument = PARSER_NEW(Declaration);
				argument->type = arg;
				argument->start = arg->start;
				argument->end = arg->end;
				argument->name = String(nullptr, 0ULL);
				argument->initialValue = nullptr;
				argument->flags |= DECLARATION_IS_ARGUMENT;

				addDeclarationToBlock(&type->arguments, argument);
			} while (expectAndConsume(lexer, ','));

			popBlock(&type->arguments);

			if (!expectAndConsume(lexer, ')')) {
				reportExpectedError(&lexer->token, "Error: Expected a ')' after function arguments");
				return nullptr;
			}

			type->flavor = ExprFlavor::FUNCTION_PROTOTYPE;

			bool hadReturnType;
			
			if (!parseFunctionReturnTypes(lexer, type, &hadReturnType))
				return nullptr;


			if (hadReturnType) { // Even though this is unambiguously a function type, still require a return type to be given for consistency
				reportExpectedError(&lexer->token, "Error: Expected a return type after function arguments");
				return nullptr;
			}

			type->start = start;
			type->end = lexer->previousTokenEnd;
			type->type = &TYPE_TYPE;


			for (auto return_ : type->returns.declarations) {
				if (return_->flags & DECLARATION_IS_MUST) {
					reportError(type, "Error: A function prototype cannot have its return type marked as #must");
					return nullptr;
				}
			}

			for (auto declaration : type->returns.declarations) {
				if (declaration->initialValue) {
					reportError(declaration, "Error: A function prototype cannot have a default return value");
					return nullptr;
				}
			}

			expr = type;
		}
		else {
			reportExpectedError(&lexer->token, "Error: Expected a ')'");
			return nullptr;
		}
	}

	return expr;
}

bool parseArguments(LexerFile *lexer, Arguments *args, const char *message) {
	Array<Expr *> arguments;
	Array<String> names;

	bool hadNamed = false;

	do {
		String name = { nullptr, 0ULL };

		if (lexer->token.type == TokenT::IDENTIFIER) {
			TokenT peek;

			lexer->peekTokenTypes(1, &peek);

			if (peek == TOKEN('=')) {
				hadNamed = true;
				name = lexer->token.text;

				lexer->advance();

				assert(lexer->token.type == TOKEN('='));

				lexer->advance();
			}
			else {
				goto unnamed;
			}
		}
		else {
		unnamed:

			if (hadNamed) {
				reportError(&lexer->token, "Error: Cannot have unnamed %s after named %s", message, message);
				return false;
			}
		}

		Expr *argument = parseExpr(lexer);

		if (!argument)
			return false;

		arguments.add(argument);
		names.add(name);
	} while (expectAndConsume(lexer, ',')); // @Incomple: We currently don't allow trailing comma in function calls, should we?

	args->count = arguments.count;
	args->values = arguments.storage;
	args->names = names.storage;

	return true;
}

Expr *parsePrimaryExpr(LexerFile *lexer) {
	CodeLocation start = lexer->token.start;
	EndLocation end = lexer->token.end;

	Expr *expr = nullptr;

	if (expectAndConsume(lexer, '(')) {
		expr = parseFunctionOrParentheses(lexer, start);

		if (!expr)
			return nullptr;
	}
	else if (expectAndConsume(lexer, '.')) {
		if (lexer->token.type != TokenT::IDENTIFIER) {
			reportExpectedError(&lexer->token, "Error: Expected identifier after unary '.'");
			return nullptr;
		}

		ExprIdentifier *identifier = PARSER_NEW(ExprIdentifier);
		identifier->start = start;
		identifier->end = lexer->token.end;
		identifier->name = lexer->token.text;
		identifier->flavor = ExprFlavor::IDENTIFIER;


		identifier->resolveFrom = nullptr;
		identifier->enclosingScope = nullptr;
		identifier->declaration = nullptr;
		identifier->structAccess = nullptr;
		identifier->type = &TYPE_UNARY_DOT;

		identifier->indexInBlock = 0;

		expr = identifier;

		lexer->advance();
	}
	else if (lexer->token.type == TokenT::IDENTIFIER) {
		ExprIdentifier *identifier = PARSER_NEW(ExprIdentifier);
		identifier->start = start;
		identifier->end = end;
		identifier->name = lexer->token.text;
		identifier->flavor = ExprFlavor::IDENTIFIER;
		identifier->resolveFrom = currentBlock;
		identifier->enclosingScope = currentBlock;
		identifier->declaration = nullptr;
		identifier->structAccess = nullptr;

		if (currentBlock) {
			identifier->indexInBlock = currentBlock->currentIndex;
		}
		else {
			identifier->indexInBlock = 0;
		}

		lexer->advance();

		expr = identifier;
	}
	else if (lexer->token.type == TokenT::FLOAT_LITERAL) {
		ExprLiteral *literal = PARSER_NEW(ExprLiteral);
		literal->start = start;
		literal->end = end;
		literal->floatValue = lexer->token.floatValue;
		literal->type = &TYPE_FLOAT_LITERAL;
		literal->flavor = ExprFlavor::FLOAT_LITERAL;
		expr = literal;

		lexer->advance();
	}
	else if (lexer->token.type == TokenT::INT_LITERAL) {
		expr = makeIntegerLiteral(start, end, lexer->token.unsignedValue, &TYPE_UNSIGNED_INT_LITERAL);

		lexer->advance();
	}
	else if (expectAndConsume(lexer, TokenT::FALSE)) {
		expr = makeIntegerLiteral(start, end, 0, &TYPE_BOOL);
	}
	else if (expectAndConsume(lexer, TokenT::TRUE)) {
		expr = makeIntegerLiteral(start, end, 1, &TYPE_BOOL);
	}
	else if (expectAndConsume(lexer, TokenT::NULL_)) {
		expr = makeIntegerLiteral(start, end, 0, TYPE_VOID_POINTER);
	}
	else if (lexer->token.type == TokenT::STRING_LITERAL) {
		expr = makeStringLiteral(start, end, lexer->token.text);

		lexer->advance();
	}
	else if (lexer->token.type == TokenT::SIZE_OF || lexer->token.type == TokenT::TYPE_INFO) {
		ExprUnaryOperator *unary = PARSER_NEW(ExprUnaryOperator);
		unary->flavor = ExprFlavor::UNARY_OPERATOR;
		unary->op = lexer->token.type;
		unary->start = start;

		lexer->advance();

		if (!expectAndConsume(lexer, '(')) {
			reportExpectedError(&lexer->token, "Error: Expected '(' after size_of");
			return nullptr;
		}

		unary->value = parseExpr(lexer);
		if (!unary->value) {
			return nullptr;
		}
		unary->end = lexer->token.end;

		if (!expectAndConsume(lexer, ')')) {
			reportExpectedError(&lexer->token, "Error: Expected ')' after type in size_of");
			return nullptr;
		}

		expr = unary;
	}
	else if (expectAndConsume(lexer, TokenT::TYPE_OF)) {
		ExprUnaryOperator *unary = PARSER_NEW(ExprUnaryOperator);
		unary->flavor = ExprFlavor::UNARY_OPERATOR;
		unary->op = TokenT::TYPE_OF;
		unary->start = start;

		if (!expectAndConsume(lexer, '(')) {
			reportExpectedError(&lexer->token, "Error: Expected '(' after type_of");
			return nullptr;
		}

		unary->value = parseExpr(lexer);
		if (!unary->value) {
			return nullptr;
		}
		unary->end = lexer->token.end;

		unary->value->flags |= EXPR_VALUE_NOT_REQUIRED;

		if (!expectAndConsume(lexer, ')')) {
			reportExpectedError(&lexer->token, "Error: Expected ')' after type in type_of");
			return nullptr;
		}

		expr = unary;
	}
	else if (expectAndConsume(lexer, TokenT::U8)) {
		expr = parserMakeTypeLiteral(start, end, &TYPE_U8);
	}
	else if (expectAndConsume(lexer, TokenT::U16)) {
		expr = parserMakeTypeLiteral(start, end, &TYPE_U16);
	}
	else if (expectAndConsume(lexer, TokenT::U32)) {
		expr = parserMakeTypeLiteral(start, end, &TYPE_U32);
	}
	else if (expectAndConsume(lexer, TokenT::U64)) {
		expr = parserMakeTypeLiteral(start, end, &TYPE_U64);
	}
	else if (expectAndConsume(lexer, TokenT::S8)) {
		expr = parserMakeTypeLiteral(start, end, &TYPE_S8);
	}
	else if (expectAndConsume(lexer, TokenT::S16)) {
		expr = parserMakeTypeLiteral(start, end, &TYPE_S16);
	}
	else if (expectAndConsume(lexer, TokenT::S32)) {
		expr = parserMakeTypeLiteral(start, end, &TYPE_S32);
	}
	else if (expectAndConsume(lexer, TokenT::S64)) {
		expr = parserMakeTypeLiteral(start, end, &TYPE_S64);
	}
	else if (expectAndConsume(lexer, TokenT::BOOL)) {
		expr = parserMakeTypeLiteral(start, end, &TYPE_BOOL);
	}
	else if (expectAndConsume(lexer, TokenT::TYPE)) {
		expr = parserMakeTypeLiteral(start, end, &TYPE_TYPE);
	}
	else if (expectAndConsume(lexer, TokenT::VOID)) {
		expr = parserMakeTypeLiteral(start, end, &TYPE_VOID);
	}
	else if (expectAndConsume(lexer, TokenT::F32)) {
		expr = parserMakeTypeLiteral(start, end, &TYPE_F32);
	}
	else if (expectAndConsume(lexer, TokenT::F64)) {
		expr = parserMakeTypeLiteral(start, end, &TYPE_F64);
	}
	else if (expectAndConsume(lexer, TokenT::STRING)) {
		expr = parserMakeTypeLiteral(start, end, &TYPE_STRING);
	}
	else if (lexer->token.type == TokenT::STRUCT || lexer->token.type == TokenT::UNION) {
		TokenT tokenType = lexer->token.type;
		lexer->advance();

		auto type = PARSER_NEW(TypeStruct);
		type->flavor = TypeFlavor::STRUCT;
		type->members.flags |= BLOCK_IS_STRUCT;

		if (tokenType == TokenT::UNION) {
			type->flags |= TYPE_STRUCT_IS_UNION;
		}

		pushBlock(&type->members);

		type->size = 0;
		type->alignment = 1;
		type->hash = 0;

		if (expectAndConsume(lexer, TokenT::PACK)) {
			type->flags |= TYPE_STRUCT_IS_PACKED;
		}

		if (!expectAndConsume(lexer, '{')) {
			reportExpectedError(&lexer->token, "Error: Expected '{' in %s definition", tokenType == TokenT::UNION ? "union" : "struct");
			return nullptr;
		}

		while (true) {
			if (lexer->token.type == TokenT::IDENTIFIER || lexer->token.type == TokenT::USING) {
				auto declaration = parseDeclaration(lexer);

				if (!declaration)
					return nullptr;

				if (!addDeclarationToCurrentBlock(declaration)) {
					return nullptr;
				}

				if (declaration->flags & DECLARATION_MARKED_AS_USING) {
					addImporterToBlock(currentBlock, createImporterForUsing(declaration, currentBlock));
				}
			}
			else if (expectAndConsume(lexer, ';')) {

			}
			else if (expectAndConsume(lexer, '}')) {
				break;
			}
			else {
				reportExpectedError(&lexer->token, "Error: Expected declaration in struct definition");
			}
		}

		popBlock(&type->members);
		expr = parserMakeTypeLiteral(start, lexer->previousTokenEnd, type);
	}
	else if (lexer->token.type == TokenT::ENUM || lexer->token.type == TokenT::ENUM_FLAGS) {
		auto enum_ = PARSER_NEW(ExprEnum);
		enum_->start = start;
		enum_->flavor = ExprFlavor::ENUM;

		enum_->struct_.size = 0;
		enum_->struct_.alignment = 0;
		enum_->struct_.flavor = TypeFlavor::ENUM;
		enum_->struct_.members.flags |= BLOCK_IS_STRUCT;
		enum_->struct_.integerType = nullptr;
		enum_->struct_.hash = 0;

		if (lexer->token.type == TokenT::ENUM_FLAGS) {
			enum_->struct_.flags |= TYPE_ENUM_IS_FLAGS;
		}

		lexer->advance();

		if (lexer->token.type != TOKEN('{')) {
			enum_->integerType = parseExpr(lexer);

			if (!enum_->integerType) {
				return nullptr;
			}
		}
		else {
			enum_->integerType = parserMakeTypeLiteral(start, end, &TYPE_U64); // @Incomplete is u64 the correct default
		}

		pushBlock(&enum_->struct_.members);

		auto members = PARSER_NEW(TypeStruct);
		members->flavor = TypeFlavor::NAMESPACE;
		members->size = 0;
		members->alignment = 0;
		members->flags |= TYPE_IS_INTERNAL;
		members->name = "members";
		members->members.flags |= BLOCK_IS_STRUCT;

		enum_->struct_.values = &members->members;

		auto membersDeclaration = PARSER_NEW(Declaration);
		membersDeclaration->start = lexer->token.start;
		membersDeclaration->end = lexer->token.end;
		membersDeclaration->name = "members";
		membersDeclaration->type = parserMakeTypeLiteral(lexer->token.start, lexer->token.end, &TYPE_TYPE);
		membersDeclaration->initialValue = parserMakeTypeLiteral(lexer->token.start, lexer->token.end, members);
		membersDeclaration->flags |= DECLARATION_IS_CONSTANT | DECLARATION_MARKED_AS_USING;
		membersDeclaration->inferJob = nullptr;

		addDeclarationToCurrentBlock(membersDeclaration);
		addImporterToBlock(currentBlock, createImporterForUsing(membersDeclaration, currentBlock));


		auto integer = PARSER_NEW(Declaration);
		integer->name = "integer";
		integer->start = lexer->token.start;
		integer->end = lexer->token.end;
		integer->type = parserMakeTypeLiteral(lexer->token.start, lexer->token.end, &TYPE_TYPE);
		integer->initialValue = enum_->integerType;
		integer->flags |= DECLARATION_IS_CONSTANT;
		integer->inferJob = nullptr;

		addDeclarationToCurrentBlock(integer);

		auto typeLiteral = parserMakeTypeLiteral(enum_->start, enum_->end, &enum_->struct_);

		pushBlock(&members->members);


		if (!expectAndConsume(lexer, TOKEN('{'))) {
			reportExpectedError(&lexer->token, "Error: Expected '{' to start enum block");
		}


		while (true) {
			if (lexer->token.type == TOKEN(';')) {
				lexer->advance();
				continue;
			}
			else if (lexer->token.type == TOKEN('}')) {
				lexer->advance();
				break;
			}
			else if (lexer->token.type == TokenT::IDENTIFIER) {
				auto declaration = PARSER_NEW(Declaration);
				declaration->name = lexer->token.text;
				declaration->start = lexer->token.start;
				declaration->type = typeLiteral;
				declaration->inferJob = nullptr;
				declaration->flags |= DECLARATION_IS_CONSTANT | DECLARATION_IS_ENUM_VALUE;

				if (declaration->name == "members" || declaration->name == "integer") {
					reportError(&lexer->token, "Error: enum name conflicts with enum data declaration '%.*s'", STRING_PRINTF(declaration->name));
					return nullptr;
				}

				lexer->advance();

				if (expectAndConsume(lexer, TOKEN(':'))) {
					if (!expectAndConsume(lexer, TOKEN(':'))) {
						reportError(&lexer->token, "Error: enums must only declare their values with a :: declaration");
						return nullptr;
					}

					declaration->initialValue = parseExpr(lexer);

					if (!declaration->initialValue)
						return nullptr;

					declaration->end = lexer->previousTokenEnd;
				}
				else {
					declaration->end = lexer->previousTokenEnd;

					if (currentBlock->declarations.count == 0) {
						declaration->initialValue = makeIntegerLiteral(declaration->start, declaration->end, enum_->struct_.flags & TYPE_ENUM_IS_FLAGS ? 1 : 0, &TYPE_UNSIGNED_INT_LITERAL);
					}
					else {
						auto increment = PARSER_NEW(ExprEnumIncrement);
						increment->start = declaration->start;
						increment->end = declaration->end;
						increment->flavor = ExprFlavor::ENUM_INCREMENT;
						increment->previous = currentBlock->declarations[currentBlock->declarations.count - 1];

						declaration->initialValue = increment;
					}

				}

				declaration->initialValue->valueOfDeclaration = declaration;

				if (!addDeclarationToCurrentBlock(declaration)) {
					return nullptr;
				}
			}
			else {
				reportExpectedError(&lexer->token, "Error: Expected enum declaration name");
				return nullptr;
			}
		}
		popBlock(&members->members);


		enum_->end = lexer->previousTokenEnd;

		popBlock(&enum_->struct_.members);


		if (members->members.declarations.count == 0) {
			reportError(enum_, "Error: Cannot have an enum with no values");
		}

		expr = typeLiteral;
	}
	else {
		if (lexer->token.type == TokenT::DOUBLE_DASH) {
			reportError(&lexer->token, "Error: '--' is not supported as an operator");
		}
		else {
			reportExpectedError(&lexer->token, "Error: Expected an expression");
		}
		return nullptr;
	}

	while (true) {
		start = lexer->token.start;

		if (expectAndConsume(lexer, '(')) {
			ExprFunctionCall *call = PARSER_NEW(ExprFunctionCall);
			call->start = start;
			call->function = expr;
			call->flavor = ExprFlavor::FUNCTION_CALL;
			call->end = lexer->token.end;

			if (expectAndConsume(lexer, ')')) {
				call->arguments.count = 0;
				call->arguments.values = nullptr;
			}
			else {
				if (!parseArguments(lexer, &call->arguments, "arguments"))
					return nullptr;
			
				call->end = lexer->token.end;

				if (!expectAndConsume(lexer, ')')) {
					reportExpectedError(&lexer->token, "Error: Expected ')' after function arguments");
					return nullptr;
				}
			}

			expr = call;
		}
		else if (expectAndConsume(lexer, '[')) {
			ExprBinaryOperator *deref = PARSER_NEW(ExprBinaryOperator);
			deref->start = start;
			deref->left = expr;
			deref->flavor = ExprFlavor::BINARY_OPERATOR;
			deref->op = TOKEN('[');

			deref->right = parseExpr(lexer);

			if (!deref->right)
				return nullptr;

			deref->end = lexer->token.end;

			if (!expectAndConsume(lexer, ']')) {
				reportExpectedError(&lexer->token, "Error: Expected ']' after array index");
				return nullptr;
			}
			expr = deref;
		}
		else if (expectAndConsume(lexer, '.')) {
			auto *access = PARSER_NEW(ExprIdentifier);
			access->start = start;
			access->structAccess = expr;
			access->flavor = ExprFlavor::IDENTIFIER;
			access->declaration = nullptr;
			access->resolveFrom = nullptr;
			access->enclosingScope = nullptr;
			access->indexInBlock = 0;

			if (lexer->token.type != TokenT::IDENTIFIER) {
				reportExpectedError(&lexer->token, "Error: Expected member name on right of struct access");
				return nullptr;
			}

			access->end = lexer->token.end;
			access->name = lexer->token.text;
			lexer->advance();
			expr = access;
		}
		else {
			return expr;
		}
	}
}

Expr *parseUnaryExpr(LexerFile *lexer, CodeLocation plusStart = {});

Expr *makeUnaryOperator(LexerFile *lexer, CodeLocation &start, EndLocation &end, TokenT type) {
	ExprUnaryOperator *expr = PARSER_NEW(ExprUnaryOperator);
	expr->start = start;
	expr->end = end;
	expr->flavor = ExprFlavor::UNARY_OPERATOR;
	expr->op = type;

	expr->value = parseUnaryExpr(lexer);
	if (!expr->value) {
		return nullptr;
	}

	return expr;
}

Expr *parseUnaryExpr(LexerFile *lexer, CodeLocation plusStart) {
	CodeLocation start = lexer->token.start;
	EndLocation end = lexer->token.end;

	if (expectAndConsume(lexer, '*')) {
		return makeUnaryOperator(lexer, start, end, TOKEN('*'));
	}
	else if (expectAndConsume(lexer, TokenT::SHIFT_LEFT)) {
		return makeUnaryOperator(lexer, start, end, TokenT::SHIFT_LEFT);
	}
	else if (expectAndConsume(lexer, '-')) {
		return makeUnaryOperator(lexer, start, end, TOKEN('-'));
	}
	else if (expectAndConsume(lexer, '~')) {
		return makeUnaryOperator(lexer, start, end, TOKEN('~'));
	}
	else if (expectAndConsume(lexer, '!')) {
		return makeUnaryOperator(lexer, start, end, TOKEN('!'));
	}
	else if (expectAndConsume(lexer, '+')) {
		auto expr = makeUnaryOperator(lexer, start, end, TOKEN('+'));

		if (!expr && plusStart.line && plusStart.locationInMemory + 1 == start.locationInMemory) {
			reportError(&plusStart, &end, "Error: '++' is not supported");
		}

		return expr;
	}
	else if (expectAndConsume(lexer, TokenT::CAST)) {

		ExprBinaryOperator *expr = PARSER_NEW(ExprBinaryOperator);
		expr->start = start;
		expr->flavor = ExprFlavor::BINARY_OPERATOR;
		expr->op = TokenT::CAST;

		while (expectAndConsume(lexer, ',')) {
			if (lexer->token.type != TokenT::IDENTIFIER) {
				reportExpectedError(&lexer->token, "Error: Expected modifier after ',' in cast");
				return nullptr;
			}

			if (lexer->token.text == "bit") {
				expr->flags |= EXPR_CAST_IS_BITWISE;
			}
			else {
				reportError(&lexer->token, "Error: Unkown modifier for cast: '%.*s'", STRING_PRINTF(lexer->token.text));
				return nullptr;
			}

			lexer->advance();
		}

		if (!expectAndConsume(lexer, '(')) {
			reportExpectedError(&lexer->token, "Error: Expected '(' after cast");
			return nullptr;
		}


		expr->end = lexer->token.end;


		if (expectAndConsume(lexer, ')')) {
			expr->left = nullptr;
			expr->type = &TYPE_AUTO_CAST;
		}
		else {
			expr->left = parseExpr(lexer);

			if (!expr->left) {
				return nullptr;
			}

			expr->end = lexer->token.end;
			if (!expectAndConsume(lexer, ')')) {
				reportExpectedError(&lexer->token, "Error: Expected ')' after cast type");
				return nullptr;
			}
		}

		expr->right = parseUnaryExpr(lexer);
		if (!expr->right) {
			return nullptr;
		}

		return expr;
	}

	else if (expectAndConsume(lexer, '[')) {
		ExprBinaryOperator *array = PARSER_NEW(ExprBinaryOperator);
		array->flavor = ExprFlavor::BINARY_OPERATOR;
		array->start = start;
		array->op = TokenT::ARRAY_TYPE;

		if (expectAndConsume(lexer, TokenT::DOUBLE_DOT)) {
			array->end = lexer->token.end;
			array->left = nullptr;
			array->flags |= EXPR_ARRAY_IS_DYNAMIC;

			if (!expectAndConsume(lexer, ']')) {
				reportExpectedError(&lexer->token, "Error: Expected ']' after array length");
				return nullptr;
			}
		}
		else if (expectAndConsume(lexer, ']')) {
			array->end = lexer->previousTokenEnd;
			array->left = nullptr;
		}
		else {
			array->left = parseExpr(lexer);
			array->end = lexer->token.end;

			if (!array->left) {
				return nullptr;
			}

			if (!expectAndConsume(lexer, ']')) {
				reportExpectedError(&lexer->token, "Error: Expected ']' after array length");

				return nullptr;
			}
		}

		array->right = parseUnaryExpr(lexer);

		if (!array->right) {
			return nullptr;
		}


		return array;
	}
	else {
		return parsePrimaryExpr(lexer);
	}
}


// Shamelessly stolen from GCC, means we don't end up with ridiculous recursion depth when parsing binops
Expr *parseBinaryOperator(LexerFile *lexer) {
	struct BinaryOp {
		Expr *left;

		u64 precedence;
		CodeLocation start;
		EndLocation end;
		TokenT type;
	};

	BinaryOp precedenceStack[ARRAY_COUNT(binaryOpPrecedences)];
	BinaryOp *sp = precedenceStack;

	BinaryOp current;

	current.precedence = 0;
	current.left = parseUnaryExpr(lexer);

	if (!current.left)
		return nullptr;

	Expr *right;
	u64 nextPrecedence;

	while (true) {
		u64 newPrecedence = getTokenPrecedence(lexer->token.type);

		// If the token is an == check if the next token is { or #complete, if it is this is the == for a switch if not an operator
		if (lexer->token.type == TokenT::EQUAL) {
			TokenT peek;

			lexer->peekTokenTypes(1, &peek);

			if (peek == TOKEN('{') || peek == TokenT::COMPLETE) {
				newPrecedence = 0;
			}
		}

		if (newPrecedence <= current.precedence) {
			if (sp == precedenceStack)
				break;
			else
				goto pop;
		}

	right:
		current.type = lexer->token.type;
		current.start = lexer->token.start;
		current.end = lexer->token.end;

		lexer->advance();

		CodeLocation plusStart;

		if (current.type == TOKEN('+')) { // Save the location of a plus operator so we can give error messages for ++
			plusStart = current.start;
		}
		else {
			plusStart = {};
		}

		right = parseUnaryExpr(lexer, plusStart);

		if (!right)
			return nullptr;

		nextPrecedence = getTokenPrecedence(lexer->token.type);

		// If the token is an == check if the next token is {, if it is this is the == for a switch if not an operator
		if (lexer->token.type == TokenT::EQUAL) {
			TokenT peek;

			lexer->peekTokenTypes(1, &peek);

			if (peek == TOKEN('{')) {
				nextPrecedence = 0;
			}
		}


		if (nextPrecedence > newPrecedence) {
			*sp = current;
			++sp;
			current.left = right;
			current.precedence = newPrecedence;
			newPrecedence = nextPrecedence;

			goto right;

		pop:
			nextPrecedence = newPrecedence;
			right = current.left;
			--sp;
			current = *sp;
		}

		ExprBinaryOperator *binary = PARSER_NEW(ExprBinaryOperator);
		binary->flavor = ExprFlavor::BINARY_OPERATOR;
		binary->left = current.left;
		binary->right = right;
		binary->start = current.start;
		binary->end = current.end;
		binary->op = current.type;

		current.left = binary;
	}

	return current.left;
}

// @Incomplete: punting on ternary operator for now because I don't know whether we should actually support it
Expr *parseExpr(LexerFile *lexer) {
	return parseBinaryOperator(lexer);
}


Declaration *parseDeclaration(LexerFile *lexer) {
	PROFILE_FUNC();

	Declaration *declaration = PARSER_NEW(Declaration);
	declaration->flags = 0;

	if (expectAndConsume(lexer, TokenT::USING)) {
		declaration->flags |= DECLARATION_MARKED_AS_USING;
	}

	declaration->name = lexer->token.text;
	declaration->start = lexer->token.start;

	if (lexer->token.type != TokenT::IDENTIFIER) {
		reportExpectedError(&lexer->token, "Error: Expected declaration name");
		return nullptr;
	}
	lexer->advance();

	if (!expectAndConsume(lexer, ':')) {
		reportExpectedError(&lexer->token, "Error: Expected ':' after declaration name");

		if (lexer->token.type == TOKEN('(')) {
			reportError(&lexer->token, "   ..: To declare a function use the syntax f :: (arguments) -> return { ... }");
		}
		return nullptr;
	}

	if (expectAndConsume(lexer, ':')) {
		if (lexer->token.type == TokenT::DOUBLE_DASH) {
			reportError(&lexer->token, "Error: You cannot have an uninitialized constant");

			return nullptr;
		}

		declaration->flags |= DECLARATION_IS_CONSTANT;
		declaration->type = nullptr;

		declaration->initialValue = parseExpr(lexer);

		if (!declaration->initialValue) {
			return nullptr;
		}

		declaration->initialValue->valueOfDeclaration = declaration;

		declaration->end = lexer->previousTokenEnd;
	}
	else if (expectAndConsume(lexer, '=')) {
		declaration->type = nullptr;

		if (lexer->token.type == TokenT::DOUBLE_DASH) {
			reportError(&lexer->token, "Error: Cannot infer the type of an uninitialized value, please specify the type");

			return nullptr;
		}

		declaration->initialValue = parseExpr(lexer);

		if (!declaration->initialValue) {
			return nullptr;
		}

		declaration->initialValue->valueOfDeclaration = declaration;

		declaration->end = lexer->previousTokenEnd;
	}
	else {
		declaration->type = parseExpr(lexer);
		if (!declaration->type) {
			return nullptr;
		}

		if (expectAndConsume(lexer, '=')) {
			if (lexer->token.type == TokenT::DOUBLE_DASH) {
				declaration->flags |= DECLARATION_IS_UNINITIALIZED;
				declaration->initialValue = nullptr;
				declaration->end = lexer->token.end;
				lexer->advance();
			}
			else {
				declaration->initialValue = parseExpr(lexer);

				if (!declaration->initialValue) {
					return nullptr;
				}

				declaration->initialValue->valueOfDeclaration = declaration;

				declaration->end = lexer->previousTokenEnd;
			}
		}
		else if (expectAndConsume(lexer, ':')) {
			declaration->flags |= DECLARATION_IS_CONSTANT;

			if (lexer->token.type == TokenT::DOUBLE_DASH) {
				reportError(&lexer->token, "Error: You cannot have an uninitialized constant");

				return nullptr;
			}
			declaration->initialValue = parseExpr(lexer);

			if (!declaration->initialValue) {
				return nullptr;
			}

			declaration->initialValue->valueOfDeclaration = declaration;

			declaration->end = lexer->previousTokenEnd;
		}
		else {
			declaration->end = lexer->previousTokenEnd;
			declaration->initialValue = nullptr;
		}
	}

	return declaration;
}

bool checkTopLevelStaticIfBlock(Expr *expr) {
	if (!expr) return true;
	
	assert(expr->flavor == ExprFlavor::BLOCK);

	for (auto statement : static_cast<ExprBlock *>(expr)->exprs) {
		if (statement->flavor != ExprFlavor::STATIC_IF && !(statement->flags & EXPR_ASSIGN_IS_IMPLICIT_INITIALIZER)) {
			reportError(statement, "Error: #if can only contain declarations at the top level");
			return false;
		}
	}

	return true;
}

bool checkTopLevelStaticIf(ExprIf *staticIf) {
	if (!checkTopLevelStaticIfBlock(staticIf->ifBody))
		return false;

	if (!checkTopLevelStaticIfBlock(staticIf->elseBody))
		return false;
	
	return true;
}

void parseFile(FileInfo *file) {
	PROFILE_FUNC();

	LexerFile lexer;

	if (!lexer.open(file))
		return;

	lexer.advance();

	while (!hadError) {
		if (lexer.token.type == TokenT::IDENTIFIER) {
			Declaration *declaration = parseDeclaration(&lexer);
			if (!declaration) {
				break;
			}

			declaration->enclosingScope = nullptr;
			declaration->indexInBlock = 0;

			assert(currentBlock == nullptr);

			_ReadWriteBarrier();

			assert(!(declaration->flags & DECLARATION_MARKED_AS_USING));

			inferQueue.add(declaration);
		}
		else if (lexer.token.type == TokenT::USING) {
			TokenT peek[2];
			lexer.peekTokenTypes(2, peek);

			if (peek[1] == TOKEN(':')) { // It is a declaration
				Declaration *declaration = parseDeclaration(&lexer);

				if (!declaration)
					break;

				_ReadWriteBarrier();

				inferQueue.add(declaration);

				assert(declaration->flags & DECLARATION_MARKED_AS_USING);

				inferQueue.add(createImporterForUsing(declaration, currentBlock));


			}
			else {
				auto start = lexer.token.start;

				lexer.advance();

				auto using_ = parseExpr(&lexer);

				if (!using_)
					break;

				auto importer = PARSER_NEW(Importer);
				importer->import = using_;

				inferQueue.add(importer);
			}
		}
		else if (lexer.token.type == TokenT::LOAD) {
			lexer.advance();

			if (lexer.token.type != TokenT::STRING_LITERAL) {
				reportExpectedError(&lexer.token, "Error: Expected file name after #load");
				break;
			}

			if (!loadNewFile(lexer.token.text)) {
				break;
			}

			lexer.advance();
		}
		else if (lexer.token.type == TokenT::END_OF_FILE) {
			break;
		}
		else if (lexer.token.type == TokenT::STATIC_IF) {
			auto staticIf = parseStaticIf(&lexer);

			if (!staticIf)
				break;

			if (!checkTopLevelStaticIf(staticIf)) {
				break;
			}

			auto importer = PARSER_NEW(Importer);

			importer->import = staticIf;

			inferQueue.add(importer);
		}
		else if (expectAndConsume(&lexer, ';')) {
			// We have consumed the semicolon we are done
		}
		else {
			reportExpectedError(&lexer.token, "Error: Expected a declaration at top level");
			break;
		}
	}
}