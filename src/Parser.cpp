#include "Basic.h"

#include "Parser.h"
#include "Lexer.h"
#include "Infer.h"
#include "TypeTable.h"
#include "Error.h"

#if 1
#define PARSER_NEW(T) new (static_cast<T *>(lexer->parserArena.allocate(sizeof(T)))) T
#define PARSER_NEW_ARRAY(T, C) new (static_cast<T *>(lexer->parserArena.allocate((C) * sizeof(T)))) T[C]

#else
#define PARSER_NEW(T) new T
#define PARSER_NEW_ARRAY(T, C) new T[C]
#endif

struct BinaryOperator {
	TokenT tokens[7];
	bool rightAssociative;
};

static const BinaryOperator binaryOpPrecedences[] = {
	{{TokenT::LOGIC_OR, TokenT::LOGIC_AND}},
	{{TokenT::EQUAL, TokenT::NOT_EQUAL, TOKEN('>'), TokenT::LESS_EQUAL, TOKEN('<'), TokenT::GREATER_EQUAL}},
	{{TOKEN('|'), TOKEN('^'), TOKEN('&')}},
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

static void insertBlock(LexerFile *lexer, Block *block) {
	if (lexer->currentBlock) {
		block->serial = lexer->identifierSerial++;
	}
	else {
		block->serial = 0;
	}
	block->parentBlock = lexer->currentBlock;
}

static void pushBlock(LexerFile *lexer, Block *block) {
	insertBlock(lexer, block);

	lexer->currentBlock = block;
}

static void popBlock(LexerFile *lexer, Block *block) { // This only takes the parameter to make sure we are always popping the block we think we are in debug
	assert(lexer->currentBlock == block);

	lexer->currentBlock = lexer->currentBlock->parentBlock;
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

Declaration *parseDeclaration(LexerFile *lexer);
Expr *parseExpr(LexerFile *lexer);
ExprBlock *parseBlock(LexerFile *lexer, bool isCase, ExprBlock *block = nullptr);

ExprLiteral *parserMakeTypeLiteral(LexerFile *lexer, CodeLocation &start, EndLocation &end, Type *type) {
	ExprLiteral *literal = PARSER_NEW(ExprLiteral);
	literal->flavor = ExprFlavor::TYPE_LITERAL;
	literal->start = start;
	literal->end = end;
	literal->typeValue = type;
	literal->type = &TYPE_TYPE;

	return literal;
}


ExprIdentifier *makeIdentifier(LexerFile *lexer, Declaration *declaration) {
	ExprIdentifier *identifier = PARSER_NEW(ExprIdentifier);
	identifier->flavor = ExprFlavor::IDENTIFIER;
	identifier->start = declaration->start;
	identifier->end = declaration->end;
	identifier->declaration = declaration;
	identifier->name = declaration->name;
	identifier->resolveFrom = lexer->currentBlock;
	identifier->enclosingScope = lexer->currentBlock;
	identifier->serial = 0;
	identifier->structAccess = nullptr;

	return identifier;
}


ExprBinaryOperator *makeBinaryOperator(LexerFile *lexer, CodeLocation &start, EndLocation &end, TokenT op, Expr *left) {
	ExprBinaryOperator *expr = PARSER_NEW(ExprBinaryOperator);
	expr->start = start;
	expr->end = end;
	expr->flavor = ExprFlavor::BINARY_OPERATOR;
	expr->op = op;
	expr->left = left;

	return expr;
}

Declaration *makeIterator(LexerFile *lexer, CodeLocation &start, EndLocation &end, String name) {
	Declaration *declaration = PARSER_NEW(Declaration);
	declaration->start = start;
	declaration->end = end;
	declaration->type = nullptr;
	declaration->initialValue = nullptr;
	declaration->name = name;


	return declaration;
}

Importer *createImporterForUsing(LexerFile *lexer, Declaration *declaration) {
	auto import = PARSER_NEW(Importer);
	import->import = makeIdentifier(lexer, declaration);

	if (declaration->flags & DECLARATION_IS_MODULE_SCOPE)
		import->moduleScope = true;

	return import;
}

bool parseArguments(LexerFile *lexer, Arguments *args, const char *message);

void parseModuleOrExportScope(LexerFile *lexer) {
	if (expectAndConsume(lexer, TokenT::SCOPE_EXPORT)) {
		lexer->moduleScope = false;
	}
	else if (expectAndConsume(lexer, TokenT::SCOPE_MODULE)) {
		lexer->moduleScope = true;
	}
}

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
		auto block = PARSER_NEW(ExprBlock);
		block->declarations.flavor = lexer->currentBlock ? lexer->currentBlock->flavor : BlockFlavor::GLOBAL;

		staticIf->ifBody = parseBlock(lexer, false, block);

		if (!staticIf->ifBody)
			return nullptr;
	}

	staticIf->elseBody = nullptr;

	if (expectAndConsume(lexer, TokenT::ELSE)) {
		if (expectAndConsume(lexer, ';')) {
		}
		else {

			auto block = PARSER_NEW(ExprBlock);
			block->declarations.flavor = lexer->currentBlock ? lexer->currentBlock->flavor : BlockFlavor::GLOBAL;

			if (lexer->token.type == TOKEN('{')) {
				staticIf->elseBody = parseBlock(lexer, false, block);

				if (!staticIf->elseBody)
					return nullptr;
			}
			else if (lexer->token.type == TokenT::IF) {
				staticIf->elseBody = block;

				block->start = lexer->token.start;
				block->flavor = ExprFlavor::BLOCK;

				pushBlock(lexer, &block->declarations);

				auto body = parseStaticIf(lexer);

				block->end = lexer->previousTokenEnd;

				if (!body)
					return nullptr;

				if (block->declarations.flavor != BlockFlavor::GLOBAL)
					block->exprs.add(body);


				popBlock(lexer, &block->declarations);
			}
			else {
				reportError("Error: Expected block or if after else");
				return nullptr;
			}
		}
	}

	if (lexer->currentBlock) {
		auto importer = PARSER_NEW(Importer);

		importer->import = staticIf;

		addImporterToBlock(lexer->currentBlock, importer, lexer->identifierSerial++);
	}

	return staticIf;
}

Expr *parseExprStatemenet(LexerFile *lexer, bool allowDeclarations) {
	Expr *expr = parseExpr(lexer);

	if (!expr)
		return nullptr;

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

					if (!addDeclarationToBlock(lexer->currentBlock, declaration, lexer->identifierSerial++)) {
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

#define MODIFY_ASSIGN(type)																			\
		else if (expectAndConsume(lexer, type)) {													\
			ExprBinaryOperator *op = makeBinaryOperator(lexer, expr->start, expr->end, type, expr); \
																									\
			op->right = parseExpr(lexer);															\
			if (!op->right)																			\
				return nullptr;																		\
			op->end = op->right->end;																\
																									\
			return op;																				\
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
				reportError(expr, "Error: Can only have a function call at statement level");
			}
			return nullptr;
		}
		else {
			expr->flags |= EXPR_FUNCTION_CALL_IS_STATEMENT_LEVEL;
		}

		return expr;
	}
}

ExprLoad *parseLoadOrImportExpr(LexerFile *lexer) {
	auto load = PARSER_NEW(ExprLoad);
	load->flavor = lexer->token.type == TokenT::LOAD ? ExprFlavor::LOAD : ExprFlavor::IMPORT;
	load->start = lexer->token.start;
	load->module = lexer->module;

	load->type = &TYPE_MODULE;

	lexer->advance();

	load->file = parseExpr(lexer);
	if (!load->file)
		return nullptr;

	load->end = lexer->previousTokenEnd;

	return load;
}

Importer *parseLoadOrImport(LexerFile *lexer) {
	auto load = parseLoadOrImportExpr(lexer);

	auto importer = PARSER_NEW(Importer);

	importer->import = load;

	if (lexer->currentBlock) {
		addImporterToBlock(lexer->currentBlock, importer, lexer->identifierSerial++);
	}

	return importer;
}

Expr *parseStatement(LexerFile *lexer, bool allowDeclarations) {
	if (lexer->token.type == TokenT::FOR) {
		ExprLoop *loop = PARSER_NEW(ExprLoop);
		loop->flavor = ExprFlavor::FOR;
		loop->start = lexer->token.start;
		loop->iteratorBlock.flavor = BlockFlavor::IMPERATIVE;
		loop->iteratorBlock.loop = true;

		EndLocation end = lexer->token.end;

		lexer->advance();

		if (expectAndConsume(lexer, '*')) {
			loop->flags |= EXPR_FOR_BY_POINTER;
		}

		Declaration *it = nullptr;
		Declaration *it_index = nullptr;

		if (lexer->token.type == TokenT::IDENTIFIER) {
			TokenT peek;

			lexer->peekTokenTypes(1, &peek); // Check if this identifier is an it declaration, an it, it_index declaration or an identifier
			if (peek == TOKEN(':') || peek == TOKEN(',')) {
				it = makeIterator(lexer, lexer->token.start, lexer->token.end, lexer->token.text);
				lexer->advance();


				if (expectAndConsume(lexer, ',')) {
					if (lexer->token.type != TokenT::IDENTIFIER) {
						reportExpectedError(&lexer->token, "Error: Expected index variable name after ,");
						return nullptr;
					}

					it_index = makeIterator(lexer, lexer->token.start, lexer->token.end, lexer->token.text);

					lexer->advance();

					if (!expectAndConsume(lexer, ':')) {
						reportExpectedError(&lexer->token, "Error: Expected a ':' after index variable name");
						return nullptr;
					}
				}
				else {
					assert(lexer->token.type == TOKEN(':'));
					lexer->advance();
				}
			}
		}

		if (!it) {
			it = makeIterator(lexer, loop->start, end, "it");
		}

		if (!it_index) {
			it_index = makeIterator(lexer, loop->start, end, "it_index");
		}

		it->flags |= DECLARATION_IS_ITERATOR;
		if (!addDeclarationToBlock(&loop->iteratorBlock, it, lexer->identifierSerial++))
			return nullptr;


		it_index->flags |= DECLARATION_IS_ITERATOR_INDEX;
		if (!addDeclarationToBlock(&loop->iteratorBlock, it_index, lexer->identifierSerial++))
			return nullptr;

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

		pushBlock(lexer, &loop->iteratorBlock);
		if (expectAndConsume(lexer, ';')) {
			loop->body = nullptr;

			loop->iteratorBlock.parentBlock = lexer->currentBlock; // Since we never push the block we need to manually set the parent
			loop->end = lexer->previousTokenEnd;
		}
		else {
			loop->end = lexer->previousTokenEnd;

			loop->body = parseStatement(lexer, false);

			if (!loop->body)
				return nullptr;


		}
		popBlock(lexer, &loop->iteratorBlock);

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
		return parseBlock(lexer, false);
	}
	else if (lexer->token.type == TokenT::WHILE) {
		ExprLoop *loop = PARSER_NEW(ExprLoop);
		loop->flavor = ExprFlavor::WHILE;
		loop->start = lexer->token.start;
		loop->iteratorBlock.flavor = BlockFlavor::IMPERATIVE;
		loop->iteratorBlock.loop = true;

		EndLocation end = lexer->token.end;

		lexer->advance();

		if (lexer->token.type == TokenT::IDENTIFIER) {
			TokenT peek;

			lexer->peekTokenTypes(1, &peek); // Check if this identifier is an it declaration, an it, it_index declaration or an identifier
			if (peek == TOKEN(':')) {
				Declaration *it = makeIterator(lexer, lexer->token.start, lexer->token.end, lexer->token.text);
				it->flags |= DECLARATION_IS_ITERATOR;

				if (!addDeclarationToBlock(&loop->iteratorBlock, it, lexer->identifierSerial++)) {
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

		pushBlock(lexer, &loop->iteratorBlock);
		if (expectAndConsume(lexer, ';')) {
			loop->body = nullptr;

			loop->iteratorBlock.parentBlock = lexer->currentBlock; // Since we never push the block we need to manually set the parent
			loop->end = lexer->previousTokenEnd;
		}
		else {
			loop->end = lexer->previousTokenEnd;

			loop->body = parseStatement(lexer, false);

			if (!loop->body)
				return nullptr;

		}
		popBlock(lexer, &loop->iteratorBlock);

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

					auto equal = makeBinaryOperator(lexer, start, condition->end, TokenT::EQUAL, switch_->condition);
					equal->right = condition;

					case_.condition = equal;
					

					case_.block = parseBlock(lexer, true);


					if (!case_.block) {
						return nullptr;
					}

					auto through = lexer->token;

					if (expectAndConsume(lexer, TokenT::THROUGH)) {
						case_.fallsThrough = true;
					}

					case_.block->start = start;
					case_.block->end = lexer->token.end;

					if (case_.fallsThrough && lexer->token.type == TOKEN('}')) {
						reportError(&through, "Error: The last case in an if == statement cannot be #through");
						reportError(&lexer->token, "");
						return nullptr;
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
					case_.block = parseBlock(lexer, true);

					if (!case_.block) {
						return nullptr;
					}

					auto through = lexer->token;
					
					if (expectAndConsume(lexer, TokenT::THROUGH)) {
						case_.fallsThrough = true;
					}

					case_.block->start = start;
					case_.block->end = lexer->token.end;

					else_ = case_.block;

					if (case_.fallsThrough && lexer->token.type == TOKEN('}')) {
						reportError(&through, "Error: The last case in an if == statement cannot be #through");
						reportError(&lexer->token, "");
						return nullptr;
					}

					switch_->cases.add(case_);
				}
				else if (lexer->token.type == TOKEN('}')) {
					break;
				}
			}

			lexer->advance();

			if (switch_->cases.count) {
				assert(!switch_->cases[switch_->cases.count - 1].fallsThrough);
			}

			return switch_;
		}
		else {
			ExprIf *ifElse = PARSER_NEW(ExprIf);
			ifElse->flavor = ExprFlavor::IF;
			ifElse->start = start;
			ifElse->condition = condition;

			if (expectAndConsume(lexer, ';') || lexer->token.type == TokenT::ELSE) {
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
			identifier->resolveFrom = lexer->currentBlock;
			identifier->enclosingScope = lexer->currentBlock;
			identifier->declaration = nullptr;
			identifier->flags |= EXPR_IDENTIFIER_IS_BREAK_OR_CONTINUE_LABEL;
			identifier->structAccess = nullptr;

			if (lexer->currentBlock) {
				identifier->serial = lexer->identifierSerial++;
			}

			continue_->label = identifier;
			continue_->refersTo = nullptr;

			continue_->end = lexer->token.end;

			lexer->advance();
			return continue_;
		}
		else { // @Cleanup: Currently unlabelled break is resolved during parse but labelled break is resolved far away in type inference, should this change?
			continue_->label = nullptr;
			continue_->end = lexer->previousTokenEnd;

			// Don't pass through arguments blocks since we can't break from an inner function to an outer one
			for (Block *block = lexer->currentBlock; block && block->flavor == BlockFlavor::IMPERATIVE; block = block->parentBlock) {
				if (block->loop) {
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

		for (Block *block = lexer->currentBlock; block; block = block->parentBlock) {
			if (block->flavor == BlockFlavor::ARGUMENTS) {
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

ExprBlock *parseBlock(LexerFile *lexer, bool isCase, ExprBlock *block) {
	PROFILE_FUNC();
	if (!block) { // A function may preallocate the block to fill in the 'using's
		block = PARSER_NEW(ExprBlock);
		block->declarations.flavor = BlockFlavor::IMPERATIVE;
	}

	bool oldModuleScope = lexer->moduleScope;
	at_exit{
		lexer->moduleScope = oldModuleScope;
	};

	block->start = lexer->token.start;
	block->flavor = ExprFlavor::BLOCK;

	pushBlock(lexer, &block->declarations);

	if (!isCase && !expectAndConsume(lexer, '{')) {
		reportError(&lexer->token, "Error: Expected '{' at the start of a block");
		return nullptr;
	}

	Expr *exitingStatement = nullptr;

	while (true) {
		if (expectAndConsume(lexer, ';')) {
			continue;
		}
		else {
			if (isCase) {
				if (lexer->token.type == TOKEN('}') || lexer->token.type == TokenT::CASE || lexer->token.type == TokenT::ELSE) {
					break;
				}
			}
			else {
				if (expectAndConsume(lexer, '}')) {
					break;
				}
			}
		}


		if (exitingStatement) {
			const char *label = exitingStatement->flavor == ExprFlavor::RETURN ? "return" : exitingStatement->flavor == ExprFlavor::BREAK ? "break" : "continue";
			reportError(&lexer->token,    "Error: Cannot have statements in a %s after a %s", isCase ? "case" : "block", label);
			reportError(exitingStatement, "   ..: Here is the %s", label);
			return nullptr;
		}

		if (isCase && lexer->token.type == TokenT::THROUGH) {
			break;
		}
		else if (lexer->token.type == TokenT::DEFER) {

			auto defer = PARSER_NEW(ExprDefer);
			defer->flavor = ExprFlavor::DEFER;
			defer->start = lexer->token.start;
			defer->enclosingScope = lexer->currentBlock;

			lexer->advance();
			
			defer->expr = parseStatement(lexer, false);

			if (!defer->expr) {
				return nullptr;
			}

			defer->end = lexer->previousTokenEnd;

			if (block->declarations.flavor != BlockFlavor::IMPERATIVE) {
				reportError("Error: Defer statements can only be used in an imperative scope");
				return nullptr;
			}

			block->exprs.add(defer);

			continue;
		}
		else if (lexer->token.type == TokenT::SCOPE_MODULE || lexer->token.type == TokenT::SCOPE_EXPORT) {
			if (lexer->currentBlock->flavor != BlockFlavor::GLOBAL) {
				reportError(&lexer->token, "Error: Scope directives can only be at the top level");
				return nullptr;
			}

			parseModuleOrExportScope(lexer);
			continue;
		}
		else if (lexer->token.type == TokenT::USING || lexer->token.type == TokenT::IDENTIFIER) {
			TokenT peek[2];
			lexer->peekTokenTypes(2, peek);

			if ((lexer->token.type == TokenT::IDENTIFIER && peek[0] == TOKEN(':')) || (lexer->token.type == TokenT::USING && peek[1] == TOKEN(':'))) { // It is a declaration
				Declaration *declaration = parseDeclaration(lexer);

				if (!declaration)
					return nullptr;

				if (!addDeclarationToBlock(lexer->currentBlock, declaration, lexer->identifierSerial++)) {
					return nullptr;
				}

				if (declaration->flags & DECLARATION_MARKED_AS_USING) {
					addImporterToBlock(lexer->currentBlock, createImporterForUsing(lexer, declaration), lexer->identifierSerial++);
				}

				if (block->declarations.flavor == BlockFlavor::IMPERATIVE && !(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IS_UNINITIALIZED))) { // If this declaration is constant or uninitialized don't add an initialization expression
					ExprBinaryOperator *assign = PARSER_NEW(ExprBinaryOperator);
					assign->start = declaration->start;
					assign->end = declaration->end;
					assign->flavor = ExprFlavor::BINARY_OPERATOR;
					assign->op = TOKEN('=');
					assign->left = makeIdentifier(lexer, declaration);
					assign->right = nullptr;
					assign->flags |= EXPR_ASSIGN_IS_IMPLICIT_INITIALIZER;

					block->exprs.add(assign);
				}

				continue;
			}
			else if (lexer->token.type == TokenT::USING) {
				auto start = lexer->token.start;

				lexer->advance();

				auto using_ = parseExpr(lexer);

				if (!using_)
					return nullptr;

				auto importer = PARSER_NEW(Importer);

				importer->import = using_;
				importer->moduleScope = lexer->moduleScope;
				
				addImporterToBlock(lexer->currentBlock, importer, lexer->identifierSerial++);

				continue;
			}

		}
		else if (lexer->token.type == TokenT::STATIC_IF) {
			auto staticIf = parseStaticIf(lexer);

			if (!staticIf)
				return nullptr;

			if (block->declarations.flavor != BlockFlavor::GLOBAL) {
				block->exprs.add(staticIf);
			}

			continue;
		}
		else if (lexer->token.type == TokenT::LOAD || lexer->token.type == TokenT::IMPORT) {
			if (block->declarations.flavor == BlockFlavor::GLOBAL) {
				parseLoadOrImport(lexer);
			}
			else {
				reportError(&lexer->token, "Error: Can only have %s at the top level", lexer->token.type == TokenT::LOAD ? "#load" : "#import");

				return nullptr;
			}

			continue;
		}
		else if (lexer->token.type == TokenT::RUN) {
			if (block->declarations.flavor == BlockFlavor::GLOBAL) {
				auto run = parseExpr(lexer);

				if (run->flavor != ExprFlavor::RUN) {
					reportError(run, "Error: Expected run directive at top level");
					return nullptr;
				}

				block->exprs.add(run);
			}
			else {
				reportError(&lexer->token, "Error: Can only have #run statements at the top level");

				return nullptr;
			}

			continue;
		}


		Expr *expr = parseStatement(lexer, true);
		if (!expr)
			return nullptr;

		if (expr->flavor == ExprFlavor::RETURN || expr->flavor == ExprFlavor::BREAK || expr->flavor == ExprFlavor::CONTINUE) {
			exitingStatement = expr;
		}

		block->exprs.add(expr);


		if (block->declarations.flavor != BlockFlavor::IMPERATIVE) {
			reportError(expr, "Error: Cannot have imperative statements here");
			return nullptr;
		}

	}

	block->end = lexer->previousTokenEnd;

	popBlock(lexer, &block->declarations);

	return block;
}

ExprLiteral *makeIntegerLiteral(LexerFile *lexer, CodeLocation &start, EndLocation &end, u64 value, Type *type) {
	ExprLiteral *literal = PARSER_NEW(ExprLiteral);
	literal->start = start;
	literal->end = end;
	literal->unsignedValue = value;
	literal->type = type;
	literal->flavor = ExprFlavor::INT_LITERAL;
	return literal;
}

ExprStringLiteral *makeStringLiteral(LexerFile *lexer, CodeLocation &start, EndLocation &end, String text) {
	ExprStringLiteral *literal = PARSER_NEW(ExprStringLiteral);
	literal->start = start;
	literal->end = end;
	literal->string = text;
	literal->type = &TYPE_STRING;
	literal->flavor = ExprFlavor::STRING_LITERAL;
	return literal;
}

Declaration *createAnonymousDeclaration(LexerFile *lexer, Expr *type) {
	auto declaration = PARSER_NEW(Declaration);

	declaration->type = type;
	declaration->start = type->start;
	declaration->end = type->end;
	declaration->name = String(nullptr, 0u);
	declaration->initialValue = nullptr;

	return declaration;
}

void addVoidReturn(LexerFile *lexer, CodeLocation &start, EndLocation &end, ExprFunction *function) {
	auto void_ = parserMakeTypeLiteral(lexer, start, end, &TYPE_VOID);

	auto returnType = createAnonymousDeclaration(lexer, void_);
	returnType->flags |= DECLARATION_IS_RETURN;

	addDeclarationToBlock(&function->returns, returnType, function->returns.declarations.count);
}


bool checkForNamedArguments(LexerFile *lexer) {
	TokenT peek[2];
	lexer->peekTokenTypes(2, peek);

	return lexer->token.type == TokenT::USING ||
		(lexer->token.type == TokenT::IDENTIFIER && peek[0] == TOKEN(':')) ||
		(lexer->token.type == TokenT::MUST && peek[0] == TokenT::USING) || // Not actually a valid combination but check for it anyway so we can provide a good error message
		(lexer->token.type == TokenT::MUST && peek[0] == TokenT::IDENTIFIER && peek[1] == TOKEN(':'));
}

bool isFunctionOrFunctionType(LexerFile *lexer) {
	if (lexer->token.type == TOKEN(')'))
		return true;

	if (checkForNamedArguments(lexer))
		return true;

	auto save = lexer->save();

	u32 parenCount = 0;

	auto token = lexer->token.type;

	while (true) {
		if (token == TokenT::END_OF_FILE) {
			return false;
		}
		else if (token == TOKEN(')')) {
			if (parenCount == 0)
				break;

			parenCount--;
		}
		else if (token == TOKEN('(')) {
			parenCount++;
		}
		else if (token == TOKEN(',') && parenCount == 0) {
			lexer->restore(save);
			return true;
		}

		token = lexer->advanceTokenType();
	}

	token = lexer->advanceTokenType();
	lexer->restore(save);

	return token == TokenT::ARROW || token == TokenT::EXTERNAL || token == TokenT::COMPILER || token == TokenT::C_CALL;
}

bool isFunctionType(LexerFile *lexer) {
	auto save = lexer->save();

	u32 parenCount = 0;

	while (true) {
		auto token = lexer->advanceTokenType();

		if (token == TokenT::END_OF_FILE) {
			return false;
		}
		else if (token == TOKEN(')')) {
			if (parenCount == 0)
				break;

			parenCount--;
		}
		else if (token == TOKEN('(')) {
			parenCount++;
		}
	}

	auto token = lexer->advanceTokenType();
	lexer->restore(save);

	return token == TokenT::ARROW;
}

Declaration *parseSingleArgument(LexerFile *lexer, ExprFunction *function, bool arguments, bool named) {
	auto token = lexer->token;

	bool must = expectAndConsume(lexer, TokenT::MUST);

	if (must && arguments) {
		reportError(&lexer->token, "Error: Arguments cannot be marked as #must");
		return nullptr;
	}

	Declaration *declaration;

	if (named) {
		declaration = parseDeclaration(lexer);

		if (!declaration)
			return nullptr;
	}
	else {
		auto expr = parseExpr(lexer);

		if (!expr)
			return nullptr;

		declaration = createAnonymousDeclaration(lexer, expr);
	}


	if (must)
		declaration->flags |= DECLARATION_IS_MUST;

	if (arguments)
		declaration->flags |= DECLARATION_IS_ARGUMENT;
	else
		declaration->flags |= DECLARATION_IS_RETURN;

	if (!arguments && (declaration->flags & DECLARATION_MARKED_AS_USING)) {
		reportError(declaration, "Error: Cannot mark return types as using");
		return nullptr;
	}



	if (declaration->flags & DECLARATION_IS_CONSTANT) {
		reportError(declaration, "Error: %s cannot be constant declarations", arguments ? "Arguments" : "Returns");
		return nullptr;
	}

	if (declaration->flags & DECLARATION_IS_UNINITIALIZED) {
		reportError(declaration, "Error: %s cannot be uninitialized", arguments ? "Arguments" : "Returns");
		return nullptr;
	}

	bool varargs = expectAndConsume(lexer, TokenT::DOUBLE_DOT);

	if (varargs && !arguments) {
		reportError(declaration, "Error: Returns cannot be varargs");
		return nullptr;
	}

	if (varargs) {
		if (declaration->initialValue) {
			reportError(declaration, "Error: Varargs arguments cannot have a default value");
			return nullptr;
		}

		auto arrayType = makeBinaryOperator(lexer, declaration->type->start, declaration->type->end, TokenT::ARRAY_TYPE, nullptr);
		arrayType->type = nullptr;
		arrayType->right = declaration->type;

		declaration->type = arrayType;

		declaration->flags |= DECLARATION_IS_VARARGS;
		function->flags |= EXPR_FUNCTION_HAS_VARARGS;
	}

	if (!addDeclarationToBlock(lexer->currentBlock, declaration, lexer->currentBlock->declarations.count)) {
		return nullptr;
	}

	return declaration;
}

bool parseArgumentList(LexerFile *lexer, ExprFunction *function, bool arguments, ExprBlock **usingBlock) {
	if (expectAndConsume(lexer, ')')) {
		if (!arguments)
			addVoidReturn(lexer, lexer->token.start, lexer->token.end, function);

		return true;
	}

	bool named = checkForNamedArguments(lexer);

	Declaration *hadVarargs = nullptr;

	do {
		if (hadVarargs) {
			reportError(hadVarargs, "Error: Only the final arugment can be varargs");
			return false;
		}

		auto declaration = parseSingleArgument(lexer, function, arguments, named);

		if (!declaration)
			return false;

		if (declaration->flags & DECLARATION_MARKED_AS_USING) {
			if (!*usingBlock) {
				*usingBlock = PARSER_NEW(ExprBlock);
				(*usingBlock)->declarations.flavor = BlockFlavor::IMPERATIVE;
			}

			addImporterToBlock(&(*usingBlock)->declarations, createImporterForUsing(lexer, declaration), lexer->identifierSerial++);
		}

		if (declaration->flags & DECLARATION_IS_VARARGS) {
			hadVarargs = declaration;
		}
	} while (expectAndConsume(lexer, ','));

	if (!expectAndConsume(lexer, ')')) {
		reportExpectedError(&lexer->token, "Error: Expected ) after %s list", arguments ? "argument" : "return");
		return false;
	}

	return true;
}

static ExprFunction *currentFunction = nullptr;

ExprFunction *parseFunctionOrFunctionType(LexerFile *lexer, CodeLocation start, bool allowBody) {
	PROFILE_FUNC();
	ExprFunction *function = PARSER_NEW(ExprFunction);
	function->constants.flavor = BlockFlavor::CONSTANTS;
	function->arguments.flavor = BlockFlavor::ARGUMENTS;
	function->returns.flavor = BlockFlavor::RETURNS;
	function->start = start;

	ExprBlock *usingBlock = nullptr;

	pushBlock(lexer, &function->constants);
	pushBlock(lexer, &function->arguments);

	lexer->functionHeaderStack.add(function);

	if (!parseArgumentList(lexer, function, true, &usingBlock))
		return nullptr;

	lexer->functionHeaderStack.pop();

	bool hadReturnType = true;

	pushBlock(lexer, &function->returns);

	if (expectAndConsume(lexer, TokenT::ARROW)) {
		auto start = lexer->token.start;

		if (expectAndConsume(lexer, '(')) {
			if (isFunctionType(lexer)) {
				auto function = parseFunctionOrFunctionType(lexer, start, false);
				if (!function)
					return nullptr;

				auto declaration = createAnonymousDeclaration(lexer, function);
				declaration->flags |= DECLARATION_IS_RETURN;

				addDeclarationToBlock(lexer->currentBlock, declaration, lexer->currentBlock->declarations.count);
			}
			else {
				if (!parseArgumentList(lexer, function, false, nullptr))
					return nullptr;
			}
		}
		else {
			auto declaration = parseSingleArgument(lexer, function, false, checkForNamedArguments(lexer));

			if (!declaration)
				return nullptr;
		}
	}
	else {
		hadReturnType = false;

		addVoidReturn(lexer, lexer->token.start, lexer->token.end, function);
	}

	popBlock(lexer, &function->returns);

	if (expectAndConsume(lexer, TokenT::C_CALL)) {
		function->flags |= EXPR_FUNCTION_IS_C_CALL;
	}
	else if (allowBody && expectAndConsume(lexer, TokenT::COMPILER)) { // The body of a #compiler function is what is called if the function is used at runtime
		function->flags |= EXPR_FUNCTION_IS_COMPILER;
	}

	function->end = lexer->previousTokenEnd;

	if (allowBody && lexer->token.type == TOKEN('{')) {
		if (function->constants.declarations.count) {
			function->flags |= EXPR_FUNCTION_IS_POLYMORPHIC;
		}


		if (function->arguments.declarations.count && function->arguments.declarations[0]->name.length == 0) {
			reportError(function->arguments.declarations[0], "Error: Non-external functions cannot have anonymous parameters");
			return nullptr;
		}

		function->flavor = ExprFlavor::FUNCTION;

		function->body = parseBlock(lexer, false, usingBlock);
		if (!function->body) {
			return nullptr;
		}
	}
	else if (allowBody && lexer->token.type == TokenT::EXTERNAL) {
		if (function->constants.declarations.count) {
			reportError(function, "Error: External functions cannot be polymorphic");
			return nullptr;
		}
		function->flags |= EXPR_FUNCTION_IS_C_CALL;
		lexer->advance();

		function->flavor = ExprFlavor::FUNCTION;

		if (function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
			reportError(function, "Error: Compiler functions cannot be external");
			return nullptr;
		}

		if (usingBlock) {
			reportError(function, "Error: External functions cannot 'using' their parameters");
			return nullptr;
		}


		function->body = parseExpr(lexer);

		function->flags |= EXPR_FUNCTION_IS_EXTERNAL;

	}
	else { // This is a function type
		function->end = lexer->previousTokenEnd;

		if (!hadReturnType) {
			reportError(function, "Error: A function prototype must have a return type");
			return nullptr;
		}

		if (function->constants.declarations.count) {
			if (lexer->functionHeaderStack.count) {
				if (lexer->currentBlock->flavor != BlockFlavor::ARGUMENTS && lexer->currentBlock->flavor != BlockFlavor::RETURNS) {
					reportError(function->constants.declarations[0], "Error: Cannot have a polymorph variable declartion outside a function header");
					return nullptr;
				}

				auto outerFunction = lexer->functionHeaderStack.peek();


				for (auto declaration : function->constants.declarations) {
					if (!addDeclarationToBlock(&outerFunction->constants, declaration, outerFunction->constants.declarations.count)) {
						return nullptr;
					}
				}

				function->constants.declarations.clear();
			}
			else {
				reportError(function->constants.declarations[0], "Error: Cannot have a polymorph variable declartion outside a function header");
				return nullptr;
			}
		}

		function->flavor = ExprFlavor::FUNCTION_PROTOTYPE;
		function->type = &TYPE_TYPE;

		for (auto return_ : function->returns.declarations) {
			if (return_->flags & DECLARATION_IS_MUST) {
				reportError(function, "Error: A function prototype cannot have its return type marked as #must");
				return nullptr;
			}
		}

		if (usingBlock) {
			reportError(function, "Error: Function prototypes cannot 'using' their parameters");
			return nullptr;
		}

		for (auto declaration : function->arguments.declarations) {
			if (declaration->initialValue) {
				reportError(declaration, "Error: A function prototype cannot have a default argument");
				return nullptr;
			}
		}


		for (auto declaration : function->returns.declarations) {
			if (declaration->initialValue) {
				reportError(declaration, "Error: A function prototype cannot have a default return value");
				return nullptr;
			}
		}
	}

	if (function->flags & EXPR_FUNCTION_IS_C_CALL) {
		if (function->flags & EXPR_FUNCTION_HAS_VARARGS) {
			// @Incomplete allow c style varargs for external functions
			reportError(function, "Error: C call functions cannot have array varargs");
			return nullptr;
		}

		if (function->returns.declarations.count != 1) {
			reportError(function, "Error: C call functions cannot have multiple return values");
			return nullptr;
		}
	}

	popBlock(lexer, &function->arguments);
	popBlock(lexer, &function->constants);

	return function;
}

Expr *parseFunctionOrParentheses(LexerFile *lexer, CodeLocation start) {
	EndLocation end = lexer->token.end;
	Expr *expr = nullptr;

	if (isFunctionOrFunctionType(lexer)) { // This is an argument declaration
		auto function = parseFunctionOrFunctionType(lexer, start, true);
		if (!function)
			return nullptr;

		expr = function;
	}
	else {
		// This is one or more expressions in parentheses

		expr = parseExpr(lexer);
		if (!expr) {
			return nullptr;
		}

		if (!expectAndConsume(lexer, ')')) { // It was a single expression in parentheses
			reportExpectedError(&lexer->token, "Error: Expected a ')'");
			return nullptr;
		}
	}

	return expr;
}

bool parseArguments(LexerFile *lexer, Arguments *args, const char *message) {
	Array<Expr *> arguments;
	Array<String> names;

	do {
		String name = { nullptr, 0u };

		TokenT peek;
		lexer->peekTokenTypes(1, &peek);

		if (lexer->token.type == TokenT::IDENTIFIER && peek == TOKEN('=')) {
			name = lexer->token.text;

			lexer->advance();

			assert(lexer->token.type == TOKEN('='));

			lexer->advance();
		}

		bool spread = expectAndConsume(lexer, TokenT::DOUBLE_DOT);

		Expr *argument = parseExpr(lexer);

		if (!argument)
			return false;

		if (spread) {
			argument->flags |= EXPR_IS_SPREAD;
		}

		arguments.add(argument);

		if (name.length || names.count) {
			for (u64 i = names.count + 1; i < arguments.count; i++) {
				names.add("");
			}

			names.add(name);
		}
	} while (expectAndConsume(lexer, ',')); // @Incomple: We currently don't allow trailing comma in function calls, should we?


	args->count = arguments.count;
	args->values = arguments.storage;
	args->names = names.storage;

	return true;
}

bool parseArrayLiteral(LexerFile *lexer, ExprArrayLiteral *literal) {
	if (lexer->token.type == TOKEN(']')) {
		literal->end = lexer->token.end;
		reportError(literal, "Error: Cannot have an empty array literal");
		return false;
	}

	Array<Expr *> values;

	do {
		auto value = parseExpr(lexer);

		if (!value)
			return false;

		values.add(value);
	} while (expectAndConsume(lexer, ','));

	if (!expectAndConsume(lexer, ']')) {
		reportExpectedError(&lexer->token, "Error: Expected ] or , in array literal");
		return false;
	}

	literal->count = values.count;
	literal->values = values.storage;
	literal->end = lexer->previousTokenEnd;

	return true;
}

bool parseStructLiteral(LexerFile *lexer, ExprStructLiteral *literal) {
	Array<Expr *> initializers;
	Array<String> names;

	if (lexer->token.type != TOKEN('}')) {
		do {
			String name = { nullptr, 0u };

			TokenT peek;
			lexer->peekTokenTypes(1, &peek);

			if (lexer->token.type == TokenT::IDENTIFIER && peek == TOKEN('=')) {
				name = lexer->token.text;

				lexer->advance();

				assert(lexer->token.type == TOKEN('='));

				lexer->advance();
			}
			else if (expectAndConsume(lexer, TokenT::DOUBLE_DASH)) {
				literal->flags |= EXPR_STRUCT_LITERAL_UNSPECIFIED_MEMBERS_UNINITIALZIED;

				if (lexer->token.type == TOKEN(',')) {
					reportError(&lexer->token, "Error: Cannot specify more intializers after marking the literal uninitialized");
					return false;
				}
				break;
			}
			else if (names.count) {
				reportError(&lexer->token, "Error: Cannot have unnamed initializers after named intializers");
				return false;
			}

			Expr *argument = parseExpr(lexer);

			if (!argument)
				return false;

			initializers.add(argument);

			if (name.length) {
				for (u64 i = names.count + 1; i < initializers.count; i++) {
					names.add("");
				}

				names.add(name);
			}
		} while (expectAndConsume(lexer, ','));
	}

	if (!expectAndConsume(lexer, '}')) {
		reportExpectedError(&lexer->token, "Error: Expected , or } in struct literal");
		return false;
	}

	literal->end = lexer->previousTokenEnd;
	literal->initializers.values = initializers.storage;
	literal->initializers.names = names.storage;
	literal->initializers.count = initializers.count;

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
		if (expectAndConsume(lexer, '[')) {
			auto literal = PARSER_NEW(ExprArrayLiteral);
			literal->flavor = ExprFlavor::ARRAY_LITERAL;
			literal->start = start;
			literal->typeValue = nullptr;
			literal->type = &TYPE_ARRAY_LITERAL;
			
			if (!parseArrayLiteral(lexer, literal))
				return nullptr;

			expr = literal;
		}
		else if (expectAndConsume(lexer, '{')) {
			auto literal = PARSER_NEW(ExprStructLiteral);
			literal->flavor = ExprFlavor::STRUCT_LITERAL;
			literal->start = start;
			literal->typeValue = nullptr;
			literal->type = &TYPE_STRUCT_LITERAL;

			if (!parseStructLiteral(lexer, literal))
				return nullptr;

			expr = literal;
		}
		else if (lexer->token.type == TokenT::IDENTIFIER) {
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

			identifier->serial = 0;

			expr = identifier;

			lexer->advance();
		}
		else {
			reportExpectedError(&lexer->token, "Error: Expected identifier, or struct/array literal after unary '.'");
			return nullptr;
		}
	}
	else if (expectAndConsume(lexer, '$')) {
		if (lexer->token.type != TokenT::IDENTIFIER) {
			reportExpectedError(&lexer->token, "Error: Expected polymorph variable name after '$'");
			return nullptr;
		}

		auto declaration = PARSER_NEW(Declaration);
		declaration->type = parserMakeTypeLiteral(lexer, start, lexer->token.end, &TYPE_TYPE);
		declaration->flags |= DECLARATION_IS_CONSTANT | DECLARATION_TYPE_IS_READY | DECLARATION_VALUE_IS_READY;
		declaration->start = start;
		declaration->name = lexer->token.text;
		declaration->end = lexer->token.end;
		declaration->initialValue = nullptr;

		if (!lexer->functionHeaderStack.count) {
			reportError(declaration, "Error: Cannot have a polymorph variable declartion outside a function header");
			return nullptr;
		}

		assert(lexer->currentBlock);

		if (lexer->currentBlock->flavor != BlockFlavor::ARGUMENTS && lexer->currentBlock->flavor != BlockFlavor::RETURNS) {
			reportError(declaration, "Error: Cannot have a polymorph variable declartion outside a function header");
			return nullptr;
		}

		auto function = lexer->functionHeaderStack.peek();

		if (!addDeclarationToBlock(&function->constants, declaration, function->constants.declarations.count)) {
			return nullptr;
		}

		ExprIdentifier *identifier = PARSER_NEW(ExprIdentifier);
		identifier->start = start;
		identifier->end = end;
		identifier->name = lexer->token.text;
		identifier->flavor = ExprFlavor::IDENTIFIER;
		identifier->resolveFrom = lexer->currentBlock;
		identifier->module = lexer->module;
		identifier->enclosingScope = lexer->currentBlock;
		identifier->declaration = declaration;
		identifier->structAccess = nullptr;
		identifier->flags |= EXPR_IDENTIER_DEFINES_POLYMORPH_VARIABLE;

		if (lexer->currentBlock) {
			identifier->serial = lexer->identifierSerial++;
		}
		else {
			identifier->serial = 0;
		}

		lexer->advance();

		expr = identifier;
	}
	else if (lexer->token.type == TokenT::IDENTIFIER) {
		ExprIdentifier *identifier = PARSER_NEW(ExprIdentifier);
		identifier->start = start;
		identifier->end = end;
		identifier->name = lexer->token.text;
		identifier->flavor = ExprFlavor::IDENTIFIER;
		identifier->resolveFrom = lexer->currentBlock;
		identifier->module = lexer->module;
		identifier->enclosingScope = lexer->currentBlock;
		identifier->declaration = nullptr;
		identifier->structAccess = nullptr;

		if (lexer->currentBlock) {
			identifier->serial = lexer->identifierSerial++;
		}
		else {
			identifier->serial = 0;
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
		expr = makeIntegerLiteral(lexer, start, end, lexer->token.unsignedValue, &TYPE_UNSIGNED_INT_LITERAL);

		lexer->advance();
	}
	else if (expectAndConsume(lexer, TokenT::FALSE)) {
		expr = makeIntegerLiteral(lexer, start, end, 0, &TYPE_BOOL);
	}
	else if (expectAndConsume(lexer, TokenT::TRUE)) {
		expr = makeIntegerLiteral(lexer, start, end, 1, &TYPE_BOOL);
	}
	else if (expectAndConsume(lexer, TokenT::NULL_)) {
		expr = makeIntegerLiteral(lexer, start, end, 0, TYPE_VOID_POINTER);
	}
	else if (lexer->token.type == TokenT::STRING_LITERAL) {
		expr = makeStringLiteral(lexer, start, end, lexer->token.text);

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

		if (!expectAndConsume(lexer, ')')) {
			reportExpectedError(&lexer->token, "Error: Expected ')' after type in type_of");
			return nullptr;
		}

		expr = unary;
	}
	else if (expectAndConsume(lexer, TokenT::IS_CONSTANT)) {
		ExprUnaryOperator *unary = PARSER_NEW(ExprUnaryOperator);
		unary->flavor = ExprFlavor::UNARY_OPERATOR;
		unary->op = TokenT::IS_CONSTANT;
		unary->start = start;

		if (!expectAndConsume(lexer, '(')) {
			reportExpectedError(&lexer->token, "Error: Expected '(' after is_constant");
			return nullptr;
		}

		unary->value = parseExpr(lexer);
		if (!unary->value) {
			return nullptr;
		}
		unary->end = lexer->token.end;

		if (!expectAndConsume(lexer, ')')) {
			reportExpectedError(&lexer->token, "Error: Expected ')' after value in is_constant");
			return nullptr;
		}

		expr = unary;
	}
	else if (expectAndConsume(lexer, TokenT::U8)) {
		expr = parserMakeTypeLiteral(lexer, start, end, &TYPE_U8);
	}
	else if (expectAndConsume(lexer, TokenT::U16)) {
		expr = parserMakeTypeLiteral(lexer, start, end, &TYPE_U16);
	}
	else if (expectAndConsume(lexer, TokenT::U32)) {
		expr = parserMakeTypeLiteral(lexer, start, end, &TYPE_U32);
	}
	else if (expectAndConsume(lexer, TokenT::U64)) {
		expr = parserMakeTypeLiteral(lexer, start, end, &TYPE_U64);
	}
	else if (expectAndConsume(lexer, TokenT::S8)) {
		expr = parserMakeTypeLiteral(lexer, start, end, &TYPE_S8);
	}
	else if (expectAndConsume(lexer, TokenT::S16)) {
		expr = parserMakeTypeLiteral(lexer, start, end, &TYPE_S16);
	}
	else if (expectAndConsume(lexer, TokenT::S32)) {
		expr = parserMakeTypeLiteral(lexer, start, end, &TYPE_S32);
	}
	else if (expectAndConsume(lexer, TokenT::S64)) {
		expr = parserMakeTypeLiteral(lexer, start, end, &TYPE_S64);
	}
	else if (expectAndConsume(lexer, TokenT::BOOL)) {
		expr = parserMakeTypeLiteral(lexer, start, end, &TYPE_BOOL);
	}
	else if (expectAndConsume(lexer, TokenT::TYPE)) {
		expr = parserMakeTypeLiteral(lexer, start, end, &TYPE_TYPE);
	}
	else if (expectAndConsume(lexer, TokenT::VOID)) {
		expr = parserMakeTypeLiteral(lexer, start, end, &TYPE_VOID);
	}
	else if (expectAndConsume(lexer, TokenT::F32)) {
		expr = parserMakeTypeLiteral(lexer, start, end, &TYPE_F32);
	}
	else if (expectAndConsume(lexer, TokenT::F64)) {
		expr = parserMakeTypeLiteral(lexer, start, end, &TYPE_F64);
	}
	else if (expectAndConsume(lexer, TokenT::STRING)) {
		expr = parserMakeTypeLiteral(lexer, start, end, &TYPE_STRING);
	}
	else if (lexer->token.type == TokenT::STRUCT || lexer->token.type == TokenT::UNION) {
		TokenT tokenType = lexer->token.type;
		lexer->advance();

		auto type = PARSER_NEW(TypeStruct);
		type->flavor = TypeFlavor::STRUCT;
		type->members.flavor = BlockFlavor::STRUCT;

		if (tokenType == TokenT::UNION) {
			type->flags |= TYPE_STRUCT_IS_UNION;
		}

		pushBlock(lexer, &type->members);

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

		Declaration *initialized = nullptr;

		while (true) {
			if (lexer->token.type == TokenT::IDENTIFIER || lexer->token.type == TokenT::USING) {
				auto declaration = parseDeclaration(lexer);

				if (!declaration)
					return nullptr;

				if (!addDeclarationToBlock(lexer->currentBlock, declaration, lexer->identifierSerial++)) {
					return nullptr;
				}

				if ((type->flags & TYPE_STRUCT_IS_UNION) && !(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IS_UNINITIALIZED))) {
					if (initialized) {
						reportError(declaration, "Error: Only one member of a union can be initialized");
						reportError(initialized, "   ..: Here is the previously initialized member");
						
						return nullptr;
					}
					else {
						initialized = declaration;
					}
				}

				if (declaration->flags & DECLARATION_MARKED_AS_USING) {
					addImporterToBlock(lexer->currentBlock, createImporterForUsing(lexer, declaration), lexer->identifierSerial++);
				}
			}
			else if (expectAndConsume(lexer, ';')) {

			}
			else if (lexer->token.type == TokenT::STATIC_IF) {
				auto staticIf = parseStaticIf(lexer);

				if (!staticIf)
					break;
			}
			else if (expectAndConsume(lexer, '}')) {
				break;
			}
			else {
				reportExpectedError(&lexer->token, "Error: Expected declaration in struct definition");
			}
		}

		popBlock(lexer, &type->members);
		expr = parserMakeTypeLiteral(lexer, start, lexer->previousTokenEnd, type);
	}
	else if (lexer->token.type == TokenT::IMPORT) {
		expr = parseLoadOrImportExpr(lexer);
		
		if (!expr)
			return nullptr;

		// :ImportExprFlagging
		expr->flags |= EXPR_IMPORT_IS_EXPR;
	}
	else if (lexer->token.type == TokenT::ENUM || lexer->token.type == TokenT::ENUM_FLAGS) {
		auto enum_ = PARSER_NEW(ExprEnum);
		enum_->start = start;
		enum_->flavor = ExprFlavor::ENUM;

		enum_->struct_.size = 0;
		enum_->struct_.alignment = 0;
		enum_->struct_.flavor = TypeFlavor::ENUM;
		enum_->struct_.members.flavor = BlockFlavor::ENUM;
		enum_->struct_.integerType = nullptr;
		enum_->struct_.hash = 0;

		if (lexer->token.type == TokenT::ENUM_FLAGS) {
			enum_->struct_.flags |= TYPE_ENUM_IS_FLAGS;
		}

		lexer->advance();

		Expr *integerType;

		if (lexer->token.type != TOKEN('{')) {
			integerType = parseExpr(lexer);

			if (!integerType) {
				return nullptr;
			}
		}
		else {
			integerType = parserMakeTypeLiteral(lexer, start, end, &TYPE_U64); // @Incomplete is u64 the correct default
		}

		pushBlock(lexer, &enum_->struct_.members);

		auto typeLiteral = parserMakeTypeLiteral(lexer, enum_->start, enum_->end, &enum_->struct_);


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

				if (declaration->name == "integer") {
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

					if (lexer->currentBlock->declarations.count == 0) {
						declaration->initialValue = makeIntegerLiteral(lexer, declaration->start, declaration->end, enum_->struct_.flags & TYPE_ENUM_IS_FLAGS ? 1 : 0, &TYPE_UNSIGNED_INT_LITERAL);
					}
					else {
						auto increment = PARSER_NEW(ExprEnumIncrement);
						increment->start = declaration->start;
						increment->end = declaration->end;
						increment->flavor = ExprFlavor::ENUM_INCREMENT;
						increment->previous = lexer->currentBlock->declarations[lexer->currentBlock->declarations.count - 1];

						declaration->initialValue = increment;
					}

				}

				declaration->initialValue->valueOfDeclaration = declaration;

				if (!addDeclarationToBlock(lexer->currentBlock, declaration, lexer->identifierSerial++)) {
					return nullptr;
				}
			}
			else {
				reportExpectedError(&lexer->token, "Error: Expected enum declaration name");
				return nullptr;
			}
		}


		enum_->end = lexer->previousTokenEnd;


		auto integer = PARSER_NEW(Declaration);
		integer->name = "integer";
		integer->start = integerType->start;
		integer->end = integerType->end;
		integer->type = nullptr;
		integer->initialValue = integerType;
		integer->flags |= DECLARATION_IS_CONSTANT;
		integer->inferJob = nullptr;

		addDeclarationToBlock(lexer->currentBlock, integer, lexer->identifierSerial++);

		assert(ENUM_SPECIAL_MEMBER_COUNT == 1);

		popBlock(lexer, &enum_->struct_.members);

		if (enum_->struct_.members.declarations.count <= ENUM_SPECIAL_MEMBER_COUNT) {
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

		if (expectAndConsume(lexer, '(')) {
			ExprFunctionCall *call = PARSER_NEW(ExprFunctionCall);
			call->start = start;
			call->function = expr;
			call->flavor = ExprFlavor::FUNCTION_CALL;
			call->end = lexer->token.end;

			if (expectAndConsume(lexer, ')')) {
				call->arguments.count = 0;
				call->arguments.values = nullptr;
				call->arguments.names = nullptr;
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
			if (expectAndConsume(lexer, TokenT::DOUBLE_DOT)) {
				auto slice = PARSER_NEW(ExprSlice);

				slice->start = start;
				slice->array = expr;
				slice->flavor = ExprFlavor::SLICE;
				slice->sliceStart = nullptr;
				
				if (lexer->token.type == TOKEN(']')) {
					reportError(&lexer->token, "Error: Slices must have either a start or an end");
					return nullptr;
				}

				slice->sliceEnd = parseExpr(lexer);

				if (!slice->sliceEnd)
					return nullptr;

				slice->end = lexer->token.end;

				if (!expectAndConsume(lexer, TOKEN(']'))) {
					reportExpectedError(&lexer->token, "Error: Expected ']' after slice");
					return nullptr;
				}

				expr = slice;
			}
			else {
				auto index = parseExpr(lexer);

				if (!index)
					return nullptr;

				if (expectAndConsume(lexer, TokenT::DOUBLE_DOT)) {
					auto slice = PARSER_NEW(ExprSlice);

					slice->start = start;
					slice->array = expr;
					slice->flavor = ExprFlavor::SLICE;
					slice->sliceStart = index;

					if (lexer->token.type == TOKEN(']')) {
						slice->sliceEnd = nullptr;
						slice->end = lexer->token.end;

						lexer->advance();
						expr = slice;
					}
					else {
						slice->sliceEnd = parseExpr(lexer);

						if (!slice->sliceEnd)
							return nullptr;

						slice->end = lexer->token.end;

						if (!expectAndConsume(lexer, TOKEN(']'))) {
							reportExpectedError(&lexer->token, "Error: Expected ']' after slice");
							return nullptr;
						}

						expr = slice;
					}
				}
				else {
					ExprBinaryOperator *deref = PARSER_NEW(ExprBinaryOperator);
					deref->start = start;
					deref->left = expr;
					deref->flavor = ExprFlavor::BINARY_OPERATOR;
					deref->op = TOKEN('[');

					deref->right = index;

					deref->end = lexer->token.end;

					if (!expectAndConsume(lexer, ']')) {
						reportExpectedError(&lexer->token, "Error: Expected ']' after array index");
						return nullptr;
					}
					expr = deref;
				}
			}
		}
		else if (expectAndConsume(lexer, '.')) {
			if (expectAndConsume(lexer, '[')) {
				auto literal = PARSER_NEW(ExprArrayLiteral);
				literal->flavor = ExprFlavor::ARRAY_LITERAL;
				literal->start = start;
				literal->typeValue = expr;
				literal->type = nullptr;

				if (!parseArrayLiteral(lexer, literal))
					return nullptr;

				expr = literal;
			}
			else if (expectAndConsume(lexer, '{')) {
				auto literal = PARSER_NEW(ExprStructLiteral);
				literal->flavor = ExprFlavor::STRUCT_LITERAL;
				literal->start = start;
				literal->typeValue = expr;
				literal->type = nullptr;

				if (!parseStructLiteral(lexer, literal))
					return nullptr;

				expr = literal;
			}
			else if (lexer->token.type == TokenT::IDENTIFIER) {
				auto access = PARSER_NEW(ExprIdentifier);
				access->start = start;
				access->structAccess = expr;
				access->flavor = ExprFlavor::IDENTIFIER;
				access->declaration = nullptr;
				access->resolveFrom = nullptr;
				access->enclosingScope = nullptr;
				access->serial = 0;

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
				reportExpectedError(&lexer->token, "Error: Expected identifier, or struct/array literal after '.'");
				return nullptr;
			}
		}
		else {
			return expr;
		}
	}
}

Expr *parsePrefixExpr(LexerFile *lexer, CodeLocation plusStart = {});

Expr *makeUnaryOperator(LexerFile *lexer, CodeLocation &start, EndLocation &end, TokenT type) {
	ExprUnaryOperator *expr = PARSER_NEW(ExprUnaryOperator);
	expr->start = start;
	expr->flavor = ExprFlavor::UNARY_OPERATOR;
	expr->op = type;

	expr->value = parsePrefixExpr(lexer);
	if (!expr->value) {
		return nullptr;
	}

	expr->end = expr->value->end;

	return expr;
}

Expr *parsePrefixExpr(LexerFile *lexer, CodeLocation plusStart) {
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
	else if (expectAndConsume(lexer, TokenT::RUN)) {
		auto run = PARSER_NEW(ExprRun);
		run->flavor = ExprFlavor::RUN;
		run->start = start;

		run->module = lexer->module;

		auto function = PARSER_NEW(ExprFunction);
		function->start = lexer->token.start;
		function->flavor = ExprFlavor::FUNCTION;

		function->constants.flavor = BlockFlavor::CONSTANTS;
		pushBlock(lexer, &function->constants);

		function->arguments.flavor = BlockFlavor::ARGUMENTS;
		pushBlock(lexer, &function->arguments);

		function->returns.flavor = BlockFlavor::RETURNS;
		insertBlock(lexer, &function->returns);

		Expr *type = nullptr;
		Expr *returnValue = nullptr;

		if (lexer->token.type == TOKEN('{')) {
			auto block = parseBlock(lexer, false);
			if (!block)
				return nullptr;
			run->end = lexer->previousTokenEnd;

			type = parserMakeTypeLiteral(lexer, run->start, run->end, &TYPE_VOID);

			function->body = block;
		}
		else {
			auto block = PARSER_NEW(ExprBlock);
			block->flavor = ExprFlavor::BLOCK;
			block->declarations.flavor = BlockFlavor::IMPERATIVE;
			pushBlock(lexer, &block->declarations);

			returnValue = parseExpr(lexer);
			if (!returnValue)
				return nullptr;

			block->start = returnValue->start;
			block->end = returnValue->end;
			run->end = lexer->previousTokenEnd;

			popBlock(lexer, &block->declarations);

			function->body = block;
		}

		popBlock(lexer, &function->arguments);
		popBlock(lexer, &function->constants);

		function->end = lexer->previousTokenEnd;
		run->function = function;


		auto returnType = PARSER_NEW(Declaration);
		returnType->start = function->start;
		returnType->end = function->end;
		returnType->flags |= DECLARATION_IS_RETURN | DECLARATION_IS_RUN_RETURN;
		returnType->type = type;
		returnType->initialValue = returnValue;
		returnType->name = "";

		addDeclarationToBlock(&function->returns, returnType, function->returns.declarations.count);

		return run;
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

		expr->right = parsePrefixExpr(lexer);
		if (!expr->right) {
			return nullptr;
		}

		expr->end = expr->right->end;

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
			array->left = nullptr;
		}
		else {
			array->left = parseExpr(lexer);

			if (!array->left) {
				return nullptr;
			}

			if (!expectAndConsume(lexer, ']')) {
				reportExpectedError(&lexer->token, "Error: Expected ']' after array length");

				return nullptr;
			}
		}

		array->right = parsePrefixExpr(lexer);

		if (!array->right) {
			return nullptr;
		}
		array->end = array->right->end;

		if (array->right->flavor == ExprFlavor::STRUCT_LITERAL && array->right->type != &TYPE_STRUCT_LITERAL) {
			auto literal = static_cast<ExprStructLiteral *>(array->right);

			array->end = literal->typeValue->end;
			literal->start = array->start;

			array->right = literal->typeValue;
			literal->typeValue = array;

			return literal;
		}
		else if (array->right->flavor == ExprFlavor::ARRAY_LITERAL && array->right->type != &TYPE_ARRAY_LITERAL) {
			auto literal = static_cast<ExprArrayLiteral *>(array->right);

			array->end = literal->typeValue->end;
			literal->start = array->start;

			array->right = literal->typeValue;
			literal->typeValue = array;

			return literal;
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
	current.left = parsePrefixExpr(lexer);

	if (!current.left)
		return nullptr;

	current.start = current.left->start;

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

		lexer->advance();

		CodeLocation plusStart;

		if (current.type == TOKEN('+')) { // Save the location of a plus operator so we can give error messages for ++
			plusStart = current.start;
		}
		else {
			plusStart = {};
		}

		right = parsePrefixExpr(lexer, plusStart);

		if (!right)
			return nullptr;

		current.end = right->end;

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

u32 getPolymorphCount(LexerFile *lexer) {
	return lexer->functionHeaderStack.count ? lexer->functionHeaderStack.peek()->constants.declarations.count : 0;
}

Declaration *parseDeclaration(LexerFile *lexer) {
	PROFILE_FUNC();

	Declaration *declaration = PARSER_NEW(Declaration);
	declaration->flags = 0;


	if (lexer->moduleScope && (!lexer->currentBlock || lexer->currentBlock->flavor == BlockFlavor::GLOBAL))
		declaration->flags |= DECLARATION_IS_MODULE_SCOPE;

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
		u32 initialPolymorphCount = getPolymorphCount(lexer);
		declaration->type = nullptr;

		if (lexer->token.type == TokenT::DOUBLE_DASH) {
			reportError(&lexer->token, "Error: Cannot infer the type of an uninitialized value, please specify the type");

			return nullptr;
		}
		else if (lexer->token.type == TokenT::DOUBLE_DOT) {
			reportError(&lexer->token, "Error: Cannot infer the type of a default value, please specify the type");

			return nullptr;
		}

		declaration->initialValue = parseExpr(lexer);

		if (!declaration->initialValue) {
			return nullptr;
		}

		declaration->initialValue->valueOfDeclaration = declaration;

		declaration->end = lexer->previousTokenEnd;

		if (getPolymorphCount(lexer) != initialPolymorphCount) {
			reportError(declaration->initialValue, "Error: Polymorph variables cannot be defined in an argument value");
			return nullptr;
		}
	}
	else {
		u32 initialPolymorphCount = getPolymorphCount(lexer);
		declaration->type = parseExpr(lexer);
		if (!declaration->type) {
			return nullptr;
		}

		if (getPolymorphCount(lexer) != initialPolymorphCount) {
			declaration->flags |= DECLARATION_DEFINES_POLYMORPH_VARIABLE;
		}

		if (expectAndConsume(lexer, '=')) {
			if (expectAndConsume(lexer, TokenT::DOUBLE_DASH)) {
				declaration->flags |= DECLARATION_IS_UNINITIALIZED;
				declaration->initialValue = nullptr;
			}
			else if (expectAndConsume(lexer, TokenT::DOUBLE_DOT)) {
				declaration->flags |= DECLARATION_IS_EXPLICIT_DEFAULT;
				declaration->initialValue = nullptr;
			}
			else {
				u32 initialPolymorphCount = getPolymorphCount(lexer);
				declaration->initialValue = parseExpr(lexer);

				if (!declaration->initialValue) {
					return nullptr;
				}

				declaration->initialValue->valueOfDeclaration = declaration;

				if (getPolymorphCount(lexer) != initialPolymorphCount) {
					reportError(declaration->initialValue, "Error: Polymorph variables cannot be defined in an argument value");
					return nullptr;
				}
			}

			declaration->end = lexer->previousTokenEnd;
		}
		else if (expectAndConsume(lexer, ':')) {
			declaration->flags |= DECLARATION_IS_CONSTANT;

			if (lexer->token.type == TokenT::DOUBLE_DASH) {
				reportError(&lexer->token, "Error: You cannot have an uninitialized constant");

				return nullptr;
			}
			else if (expectAndConsume(lexer, TokenT::DOUBLE_DOT)) {
				declaration->flags |= DECLARATION_IS_EXPLICIT_DEFAULT;
				declaration->initialValue = nullptr;
			}
			else {
				declaration->initialValue = parseExpr(lexer);

				if (!declaration->initialValue) {
					return nullptr;
				}

				declaration->initialValue->valueOfDeclaration = declaration;
			}


			declaration->end = lexer->previousTokenEnd;
		}
		else {
			declaration->end = lexer->previousTokenEnd;
			declaration->initialValue = nullptr;
		}
	}

	return declaration;
}

void runParser() {
	PROFILE_FUNC();
	LexerFile lexer_;
	LexerFile *lexer = &lexer_;

	while (true) {
		PROFILE_ZONE("parseFile");
		FileInfo *file = parserQueue.take();

		if (!file) {
			parserQueue.add(nullptr); // Notify other parsing threads to finish
			break;
		}


		if (!lexer->open(file)) {
			inferQueue.add(InferJobType::DONE);
			break;
		}

		lexer->advance();

		while (!hadError) {
			if (lexer->token.type == TokenT::IDENTIFIER) {
				Declaration *declaration = parseDeclaration(lexer);
				if (!declaration) {
					break;
				}

				declaration->enclosingScope = nullptr;
				declaration->serial = 0;

				assert(lexer->currentBlock == nullptr);

				_ReadWriteBarrier();

				assert(!(declaration->flags & DECLARATION_MARKED_AS_USING));

				inferQueue.add(InferQueueJob(declaration, lexer->module));
			}
			else if (lexer->token.type == TokenT::SCOPE_MODULE || lexer->token.type == TokenT::SCOPE_EXPORT) {
				parseModuleOrExportScope(lexer);
			}
			else if (lexer->token.type == TokenT::USING) {
				TokenT peek[2];
				lexer->peekTokenTypes(2, peek);

				if (peek[1] == TOKEN(':')) { // It is a declaration
					Declaration *declaration = parseDeclaration(lexer);

					if (!declaration)
						break;

					declaration->enclosingScope = nullptr;
					declaration->serial = 0;

					_ReadWriteBarrier();

					inferQueue.add(InferQueueJob(declaration, lexer->module));

					assert(declaration->flags & DECLARATION_MARKED_AS_USING);

					inferQueue.add(InferQueueJob(createImporterForUsing(lexer, declaration), lexer->module));


				}
				else {
					auto start = lexer->token.start;

					lexer->advance();

					auto using_ = parseExpr(lexer);

					if (!using_)
						break;

					auto importer = PARSER_NEW(Importer);
					importer->import = using_;

					importer->moduleScope = lexer->moduleScope;

					inferQueue.add(InferQueueJob(importer, lexer->module));
				}
			}
			else if (lexer->token.type == TokenT::LOAD || lexer->token.type == TokenT::IMPORT) {
				auto load = parseLoadOrImport(lexer);

				if (!load)
					break;

				inferQueue.add(InferQueueJob(load, lexer->module));
			}
			else if (lexer->token.type == TokenT::END_OF_FILE) {
				break;
			}
			else if (lexer->token.type == TokenT::STATIC_IF) {
				auto staticIf = parseStaticIf(lexer);

				if (!staticIf)
					break;

				auto importer = PARSER_NEW(Importer);

				importer->import = staticIf;

				inferQueue.add(InferQueueJob(importer, lexer->module));
			}
			else if (lexer->token.type == TokenT::RUN) {
				auto run = parseExpr(lexer);

				if (!run)
					break;

				if (run->flavor != ExprFlavor::RUN) {
					reportError(run, "Error: Expected run directive at top level");
					break;
				}

				inferQueue.add(InferQueueJob(static_cast<ExprRun *>(run), lexer->module));
			}
			else if (expectAndConsume(lexer, ';')) {
				// We have consumed the semicolon we are done
			}
			else {
				reportExpectedError(&lexer->token, "Error: Expected a declaration at top level");
				break;
			}
		}


		inferQueue.add(InferQueueJob(file->fileUid, lexer->module));

		if (hadError) {
			inferQueue.add(InferJobType::DONE);
			break;
		}
	}

	if (printDiagnostics) {
		wchar_t *name;
		GetThreadDescription(GetCurrentThread(), &name);
		reportInfo("%ls memory used %ukb", name, lexer->parserArena.totalSize / 1024);
	}
}