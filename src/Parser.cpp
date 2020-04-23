#include "Basic.h"

#include "Parser.h"
#include "Ast.h"
#include "Lexer.h"
#include "Infer.h"
#include "CompilerMain.h"

BucketedArenaAllocator parserArena(1024 * 1024);

#if 1
#define PARSER_NEW(T) new (static_cast<T *>(assert(std::this_thread::get_id() == mainThread), parserArena.allocate(sizeof(T)))) T
#define PARSER_NEW_ARRAY(T, C) new (static_cast<T *>(assert(std::this_thread::get_id() == mainThread), parserArena.allocate((C) * sizeof(T)))) T[C]

#else
#define PARSER_NEW(T) new T
#define PARSER_NEW_ARRAY(T, C) new T[C]
#endif

Block *currentBlock = nullptr;

bool addDeclarationToBlock(Block *block, Declaration *declaration) {
	assert(block);
	assert(declaration);

	for (auto previous : block->declarations) {
		if (previous->name == declaration->name) {
			reportError(declaration, "Error: Cannot redeclare variable '%.*s' within the same scope", STRING_PRINTF(declaration->name));
			reportError(previous, "   ..: Here is the location it was declared");
		}
	}

	block->declarations.add(declaration);
	declaration->enclosingScope = block;

	return true;
}

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

static void pushBlock(Block *block) {
	if (currentBlock) {
		block->indexInParent = currentBlock->declarations.count;
	}
	else {
		block->indexInParent = 0;
	}
	block->parentBlock = currentBlock;
	currentBlock = block;
}

static void queueBlock(Block *block) {
	if (block->declarations.count) {
		inferQueue.add(makeDeclarationPack(block->declarations.count, block->declarations.storage)); // Queue the entire block at once to avoid locking the mutex too much
	}
}

static void popBlock(Block *block) { // This only takes the parameter to make sure we are always popping the block we think we are in debug
	assert(currentBlock == block);

	currentBlock = currentBlock->parentBlock;

	_ReadWriteBarrier(); // Make sure the compiler doesn't try to move this flag set before a previous add, as this means it is safe for the type inference thread 

	block->flags |= BLOCK_IS_COMPLETE;


	queueBlock(block);
}

static void popBlockWithoutQueueing(Block *block) { // This only takes the parameter to make sure we are always popping the block we think we are in debug
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
ExprBlock *parseBlock(LexerFile *lexer);

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
	identifier->resolveFrom = nullptr;
	identifier->indexInBlock = 0;

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

Expr *parseStatement(LexerFile *lexer) {
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
				it->flags |= DECLARATION_IS_ITERATOR_INDEX;

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
				it->flags |= DECLARATION_IS_ITERATOR_INDEX;

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
			loop->body = parseStatement(lexer);

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
				loop->completedBody = parseStatement(lexer);

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
			loop->body = parseStatement(lexer);

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
				loop->completedBody = parseStatement(lexer);

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
		ExprIf *ifElse = PARSER_NEW(ExprIf);
		ifElse->flavor = ExprFlavor::IF;
		ifElse->start = lexer->token.start;

		lexer->advance();

		ifElse->condition = parseExpr(lexer);
		if (!ifElse->condition)
			return nullptr;

		if (expectAndConsume(lexer, ';')) {
			ifElse->ifBody = nullptr;

			ifElse->end = lexer->previousTokenEnd;
		}
		else {
			ifElse->end = lexer->previousTokenEnd;

			ifElse->ifBody = parseStatement(lexer);

			if (!ifElse->ifBody)
				return nullptr;
		}

		if (expectAndConsume(lexer, TokenT::ELSE)) {
			if (expectAndConsume(lexer, ';')) {
				ifElse->elseBody = nullptr;
			}
			else {
				ifElse->elseBody = parseStatement(lexer);

				if (!ifElse->elseBody)
					return nullptr;
			}
		}
		else {
			ifElse->elseBody = nullptr;
		}

		return ifElse;
	}
	else if (lexer->token.type == TokenT::CONTINUE || lexer->token.type == TokenT::BREAK) {
		ExprBreakOrContinue *continue_ = PARSER_NEW(ExprBreakOrContinue);
		continue_->flavor = lexer->token.type == TokenT::CONTINUE ? ExprFlavor::CONTINUE : ExprFlavor::BREAK;
		continue_->start = lexer->token.start;

		lexer->advance();

		if (lexer->token.type == TokenT::IDENTIFIER) {
			auto identifier = PARSER_NEW(ExprIdentifier);
			identifier->start = lexer->token.start;
			identifier->end = lexer->token.end;
			identifier->name = lexer->token.text;
			identifier->flavor = ExprFlavor::IDENTIFIER;
			identifier->resolveFrom = currentBlock;
			identifier->flags |= EXPR_IDENTIFIER_IS_BREAK_OR_CONTINUE_LABEL;

			if (currentBlock) {
				identifier->indexInBlock = currentBlock->declarations.count;
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

			reportError(continue_, "Error: Cannot have a %s outside of a loop", continue_->flavor == ExprFlavor::BREAK ? "break" : "continue");
			return nullptr;
		}
	}
	else if (lexer->token.type == TokenT::RETURN) {
		ExprReturn *return_ = PARSER_NEW(ExprReturn);
		return_->flavor = ExprFlavor::RETURN;
		return_->start = lexer->token.start;
		lexer->advance();


		if (expectAndConsume(lexer, ';')) {
			return_->value = nullptr;
		}
		else {
			return_->value = parseExpr(lexer);

			if (!return_->value)
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
		Expr *expr = parseExpr(lexer);

		if (!expr)
			return nullptr;

		CodeLocation start = lexer->token.start;
		EndLocation end = lexer->token.end;

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
				reportError(expr, "Error: Can only have an assignment or function call expression at statement level");
				return nullptr;
			}

			return expr;
		}
	}
}

ExprBlock *parseBlock(LexerFile *lexer) {

	ExprBlock *block = PARSER_NEW(ExprBlock);
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

				if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IS_UNINITIALIZED))) { // If this declaration is constant or uninitialized don't add an initialization expression
					ExprBinaryOperator *assign = PARSER_NEW(ExprBinaryOperator);
					assign->start = declaration->start;
					assign->end = declaration->end;
					assign->flavor = ExprFlavor::BINARY_OPERATOR;
					assign->op = TOKEN('=');
					assign->left = makeIdentifier(declaration->start, declaration->end, declaration);
					assign->right = declaration->initialValue; // This may be null, a rhs of null for an assign expression means the default value for that type should be filled in when its type is inferred

					block->exprs.add(assign);
				}

				continue;
			}
		}

		Expr *expr = parseStatement(lexer);

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


Expr *parsePrimaryExpr(LexerFile *lexer) {
	CodeLocation start = lexer->token.start;
	EndLocation end = lexer->token.end;

	Expr *expr = nullptr;

	if (expectAndConsume(lexer, '(')) {
		end = lexer->token.end;

		if (expectAndConsume(lexer, ')')) { // This is a function with no arguments
			if (lexer->token.type == TOKEN('{')) { // It also returns void
				ExprFunction *function = PARSER_NEW(ExprFunction);
				function->flavor = ExprFlavor::FUNCTION;
				function->start = start;
				function->end = lexer->previousTokenEnd;
				function->arguments.flags |= BLOCK_IS_ARGUMENTS;
				function->returnType = parserMakeTypeLiteral(function->start, function->end, &TYPE_VOID);
				pushBlock(&function->arguments);

				function->body = parseBlock(lexer);
				if (!function->body) {
					return nullptr;
				}

				popBlock(&function->arguments);

				_ReadWriteBarrier();
				inferQueue.add(makeDeclarationPack(function));

				expr = function;
			}
			else if (lexer->token.type == TokenT::EXTERNAL) {
				lexer->advance();

				ExprFunction *function = PARSER_NEW(ExprFunction);
				function->flavor = ExprFlavor::FUNCTION;
				function->start = start;
				function->end = lexer->previousTokenEnd;
				function->arguments.flags |= BLOCK_IS_ARGUMENTS;
				function->returnType = parserMakeTypeLiteral(function->start, function->end, &TYPE_VOID);


				function->body = nullptr;
				function->flags |= EXPR_FUNCTION_IS_EXTERNAL;

				pushBlock(&function->arguments); // @Cleanup: These functions deal with properly setting up the blocks parent and queueing the block for infer even if we don't need to 
				popBlock(&function->arguments);

				_ReadWriteBarrier();
				inferQueue.add(makeDeclarationPack(function));

				expr = function;
			}
			else if (expectAndConsume(lexer, TokenT::ARROW)) {
				Expr *returnType = parseExpr(lexer);

				if (lexer->token.type == TOKEN('{')) { // This is a function value
					ExprFunction *function = PARSER_NEW(ExprFunction);
					function->flavor = ExprFlavor::FUNCTION;
					function->start = start;
					function->end = lexer->previousTokenEnd;
					function->returnType = returnType;
					function->arguments.flags |= BLOCK_IS_ARGUMENTS;
					pushBlock(&function->arguments);

					function->body = parseBlock(lexer);

					if (!function->body) {
						return nullptr;
					}

					popBlock(&function->arguments);

					_ReadWriteBarrier();
					inferQueue.add(makeDeclarationPack(function));

					expr = function;
				}
				else if (lexer->token.type == TokenT::EXTERNAL) { // This is a function value
					lexer->advance();

					ExprFunction *function = PARSER_NEW(ExprFunction);
					function->flavor = ExprFlavor::FUNCTION;
					function->start = start;
					function->end = lexer->previousTokenEnd;
					function->returnType = returnType;
					function->arguments.flags |= BLOCK_IS_ARGUMENTS;

					function->body = nullptr;
					function->flags |= EXPR_FUNCTION_IS_EXTERNAL;

					pushBlock(&function->arguments);
					popBlock(&function->arguments);

					_ReadWriteBarrier();
					inferQueue.add(makeDeclarationPack(function));

					expr = function;
				}
				else { // This is a function type
					ExprFunction *type = PARSER_NEW(ExprFunction);
					type->flavor = ExprFlavor::FUNCTION_PROTOTYPE;
					type->start = start;
					type->end = lexer->previousTokenEnd;
					type->returnType = returnType;
					type->type = &TYPE_TYPE;

					expr = type;
				}
			}
			else {
				reportExpectedError(&lexer->token, "Error: Expected a return type after function arguments");
				return nullptr;
			}
		}
		else if (lexer->token.type == TokenT::IDENTIFIER) { // There is an identifier after the function name, this could be an expression or argument delcaration
			TokenT peek;

			lexer->peekTokenTypes(1, &peek);

			if (peek == TOKEN(':')) { // This is an argument declaration
				ExprFunction *function = PARSER_NEW(ExprFunction);
				function->flavor = ExprFlavor::FUNCTION;
				function->start = start;
				function->arguments.flags |= BLOCK_IS_ARGUMENTS;

				pushBlock(&function->arguments);

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
				} while (expectAndConsume(lexer, ','));

				if (!expectAndConsume(lexer, ')')) {
					reportExpectedError(&lexer->token, "Error: Expected a ')' after function arguments");
					return nullptr;
				}

				if (lexer->token.type == TOKEN('{')) {
					function->end = lexer->previousTokenEnd;
					function->returnType = parserMakeTypeLiteral(function->start, function->end, &TYPE_VOID);

					function->body = parseBlock(lexer);
					if (!function->body) {
						return nullptr;
					}

					popBlock(&function->arguments);

					_ReadWriteBarrier();
					inferQueue.add(makeDeclarationPack(function));

					expr = function;
				}
				else if (lexer->token.type == TokenT::EXTERNAL) {
					lexer->advance();

					function->end = lexer->previousTokenEnd;
					function->returnType = parserMakeTypeLiteral(function->start, function->end, &TYPE_VOID);

					function->body = nullptr;
					function->flags |= EXPR_FUNCTION_IS_EXTERNAL;

					popBlock(&function->arguments);


					_ReadWriteBarrier();
					inferQueue.add(makeDeclarationPack(function));

					expr = function;
				}
				else if (expectAndConsume(lexer, TokenT::ARROW)) {
					Expr *returnType = parseExpr(lexer);
					if (!returnType) {
						return nullptr;
					}

					if (lexer->token.type == TOKEN('{')) {
						function->end = lexer->previousTokenEnd;
						function->returnType = returnType;

						function->body = parseBlock(lexer);
						if (!function->body) {
							return nullptr;
						}

						popBlock(&function->arguments);

						_ReadWriteBarrier();
						inferQueue.add(makeDeclarationPack(function));

						expr = function;
					}
					else if (lexer->token.type == TokenT::EXTERNAL) {
						lexer->advance();

						function->end = lexer->previousTokenEnd;
						function->returnType = returnType;

						function->body = nullptr;
						function->flags |= EXPR_FUNCTION_IS_EXTERNAL;

						popBlock(&function->arguments);


						_ReadWriteBarrier();
						inferQueue.add(makeDeclarationPack(function));

						expr = function;
					}
					else {
						popBlockWithoutQueueing(&function->arguments);

						function->flavor = ExprFlavor::FUNCTION_PROTOTYPE;
						function->end = lexer->previousTokenEnd;
						function->returnType = returnType;
						function->type = &TYPE_TYPE;

						for (u64 i = 0; i < function->arguments.declarations.count; i++) {
							auto declaration = function->arguments.declarations[i];

							if (declaration->initialValue) {
								reportError(declaration, "Error: A function prototype cannot have a default value");
								return nullptr;
							}

							assert(declaration->type);
						}

						expr = function;
					}
				}
				else {
					reportExpectedError(&lexer->token, "Error: Expected a function body or return type");
					return nullptr;
				}
			}
			else {
				goto notDeclaration;
			}
		}
		else {
		notDeclaration:
			// This is one or more expressions in parentheses

			expr = parseExpr(lexer);
			if (!expr) {
				return nullptr;
			}

			if (expectAndConsume(lexer, ')')) { // It was a single expression in parentheses
				if (expectAndConsume(lexer, TokenT::ARROW)) { // This is a function type since it has a return value
					Expr *returnType = parseExpr(lexer);
					end = lexer->previousTokenEnd;
					if (!returnType) {
						return nullptr;
					}

					auto type = PARSER_NEW(ExprFunction);
					type->flavor = ExprFlavor::FUNCTION_PROTOTYPE;
					type->returnType = returnType;
					type->start = start;
					type->end = end;
					type->type = &TYPE_TYPE;

					pushBlock(&type->arguments);

					auto argument = PARSER_NEW(Declaration);
					argument->type = expr;
					argument->start = expr->start;
					argument->end = expr->end;
					argument->name = String(nullptr, 0ULL);
					argument->initialValue = nullptr;
					argument->enclosingScope = &type->arguments;
					argument->flags |= DECLARATION_IS_ARGUMENT;

					type->arguments.declarations.add(argument);

					popBlockWithoutQueueing(&type->arguments);

					expr = type;
				}
				else {
					// We have put the bracketed expression in expr so we are done
				}
			}
			else if (expectAndConsume(lexer, ',')) { // This is a function type since there are multiple comma separated values in parentheses
				auto *type = PARSER_NEW(ExprFunction);

				pushBlock(&type->arguments);

				auto argument = PARSER_NEW(Declaration);
				argument->type = expr;
				argument->start = expr->start;
				argument->end = expr->end;
				argument->name = String(nullptr, 0ULL);
				argument->initialValue = nullptr;
				argument->enclosingScope = &type->arguments;
				argument->flags |= DECLARATION_IS_ARGUMENT;

				type->arguments.declarations.add(argument);



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
					argument->enclosingScope = &type->arguments;
					argument->flags |= DECLARATION_IS_ARGUMENT;

					type->arguments.declarations.add(argument);
				} while (expectAndConsume(lexer, ','));

				popBlockWithoutQueueing(&type->arguments);

				if (!expectAndConsume(lexer, ')')) {
					reportExpectedError(&lexer->token, "Error: Expected a ')' after function arguments");
					return nullptr;
				}

				if (!expectAndConsume(lexer, TokenT::ARROW)) { // Even though this is unambiguously a function type, still require a return type to be given for consistency
					reportExpectedError(&lexer->token, "Error: Expected a return type after function arguments");
					return nullptr;
				}

				Expr *returnType = parseExpr(lexer);
				if (!returnType)
					return nullptr;

				type->flavor = ExprFlavor::FUNCTION_PROTOTYPE;
				type->returnType = returnType;
				type->start = start;
				type->end = lexer->previousTokenEnd;
				type->type = &TYPE_TYPE;

				expr = type;
			}
			else {
				reportExpectedError(&lexer->token, "Error: Expected a ')'");
				return nullptr;
			}
		}

	}
	else if (lexer->token.type == TokenT::IDENTIFIER) {
		bool success;

		ExprIdentifier *identifier = PARSER_NEW(ExprIdentifier);
		identifier->start = start;
		identifier->end = end;
		identifier->name = lexer->token.text;
		identifier->flavor = ExprFlavor::IDENTIFIER;
		identifier->resolveFrom = currentBlock;
		identifier->declaration = nullptr;

		if (currentBlock) {
			identifier->indexInBlock = currentBlock->declarations.count;
		}

		/*
		identifier->declaration = resolveIdentifier(&lexer->token, identifier->name, &success); // This may fail to find the identifier which may mean that it is an identifier not yet declared in global scope or an error

		if (!success)
			return nullptr;

		if (!identifier->declaration) {
			identifier->resolveFrom = currentBlock;
		}
		if (identifier->declaration && (identifier->declaration->flags & DECLARATION_IS_ITERATOR)) {
			ExprLoop *loop = CAST_FROM_SUBSTRUCT(ExprLoop, iteratorBlock, identifier->declaration->enclosingScope);

			if (loop->flavor == ExprFlavor::WHILE) {
				reportError(&lexer->token, "Error: Cannot access a while loop label outside of a break or continue");
				return nullptr;
			}
		}
		*/

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
	else if (expectAndConsume(lexer, TokenT::SIZE_OF)) {
		ExprUnaryOperator *unary = PARSER_NEW(ExprUnaryOperator);
		unary->flavor = ExprFlavor::UNARY_OPERATOR;
		unary->op = TokenT::SIZE_OF;
		unary->start = start;

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
	else if (expectAndConsume(lexer, TokenT::STRUCT)) {
		auto type = PARSER_NEW(TypeStruct);
		type->flavor = TypeFlavor::STRUCT;
		pushBlock(&type->members);

		type->size = 0;
		type->alignment = 0;
		type->hash = 0;

		if (!expectAndConsume(lexer, '{')) {
			reportExpectedError(&lexer->token, "Error: Expected '{' in struct definition");
			return nullptr;
		}

		while (true) {
			if (lexer->token.type == TokenT::IDENTIFIER) {
				auto declaration = parseDeclaration(lexer);

				if (!declaration)
					return nullptr;

				declaration->flags |= DECLARATION_IS_STRUCT_MEMBER;

				if (!addDeclarationToCurrentBlock(declaration)) {
					return nullptr;
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

		inferQueue.add(makeDeclarationPack(expr));
	}
	else if (expectAndConsume(lexer, TokenT::UNION)) {
		auto type = PARSER_NEW(TypeStruct);
		type->flavor = TypeFlavor::STRUCT;
		type->flags |= TYPE_STRUCT_IS_UNION;
		pushBlock(&type->members);

		type->size = 0;
		type->alignment = 0;
		type->hash = 0;

		if (!expectAndConsume(lexer, '{')) {
			reportExpectedError(&lexer->token, "Error: Expected '{' in union definition");
			return nullptr;
		}

		while (true) {
			if (lexer->token.type == TokenT::IDENTIFIER) {
				auto declaration = parseDeclaration(lexer);

				if (!declaration)
					return nullptr;

				declaration->flags |= DECLARATION_IS_STRUCT_MEMBER;

				if (!addDeclarationToCurrentBlock(declaration)) {
					return nullptr;
				}
			}
			else if (expectAndConsume(lexer, ';')) {

			}
			else if (expectAndConsume(lexer, '}')) {
				break;
			}
			else {
				reportExpectedError(&lexer->token, "Error: Expected declaration in union definition");
			}
		}

		popBlock(&type->members);
		expr = parserMakeTypeLiteral(start, lexer->previousTokenEnd, type);

		inferQueue.add(makeDeclarationPack(expr));
	}
	else {
		reportExpectedError(&lexer->token, "Error: Expected an expression");
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
				call->argumentCount = 0;
				call->arguments = nullptr;
			}
			else {
				Array<Expr *> arguments;
				Array<String> names;

				bool hadNamed = false;

				// @Incomplete: named arguments?
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
							reportError(&lexer->token, "Error: Cannot have unnamed arguments after named arguments");
						}
					}

					Expr *argument = parseExpr(lexer);

					if (!argument)
						return nullptr;

					arguments.add(argument);
					names.add(name);
				} while (expectAndConsume(lexer, ',')); // @Incomple: We currently don't allow trailing comma in function calls, should we?

				call->end = lexer->token.end;

				if (!expectAndConsume(lexer, ')')) {
					reportExpectedError(&lexer->token, "Error: Expected ')' after function arguments");
					return nullptr;
				}

				call->argumentCount = arguments.count;
				call->arguments = arguments.storage;
				call->argumentNames = names.storage;
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
			ExprStructAccess *access = PARSER_NEW(ExprStructAccess);
			access->start = start;
			access->left = expr;
			access->flavor = ExprFlavor::STRUCT_ACCESS;
			access->declaration = nullptr;

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

Expr *parseUnaryExpr(LexerFile *lexer);

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

Expr *parseUnaryExpr(LexerFile *lexer) {
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
		return makeUnaryOperator(lexer, start, end, TOKEN('+'));
	}
	else if (expectAndConsume(lexer, TokenT::CAST)) {
		if (!expectAndConsume(lexer, '(')) {
			reportExpectedError(&lexer->token, "Error: Expected '(' after cast");
			return nullptr;
		}

		ExprBinaryOperator *expr = PARSER_NEW(ExprBinaryOperator);
		expr->start = start;
		expr->end = lexer->token.end;
		expr->flavor = ExprFlavor::BINARY_OPERATOR;
		expr->op = TokenT::CAST;

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

		right = parseUnaryExpr(lexer);

		if (!right)
			return nullptr;

		nextPrecedence = getTokenPrecedence(lexer->token.type);

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
	assert(lexer->token.type == TokenT::IDENTIFIER);

	Declaration *declaration = PARSER_NEW(Declaration);
	declaration->flags = 0;
	declaration->name = lexer->token.text;
	declaration->start = lexer->token.start;
	lexer->advance();

	if (!expectAndConsume(lexer, ':')) {
		reportExpectedError(&lexer->token, "Error: Expected ':' after declaration name");
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

			assert(currentBlock == nullptr);

			_ReadWriteBarrier();

			inferQueue.add(makeDeclarationPack(1, &declaration));
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
		else if (expectAndConsume(&lexer, ';')) {
			// We have consumed the semicolon we are done
		}
		else {
			reportExpectedError(&lexer.token, "Error: Expected a declaration at top level");
			break;
		}
	}
}