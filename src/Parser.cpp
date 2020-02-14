#include "Basic.h"

#include "Parser.h"
#include "Ast.h"
#include "Lexer.h"
#include "Infer.h"


Block *currentBlock = nullptr;

static Declaration *resolveIdentifier(String name, bool *success) {
	Block *block;

	*success = true;

	bool outsideOfFunction = false;

	for (block = currentBlock; block; block = block->parentBlock) {
		for (auto declaration : block->declarations) {
			if (declaration->name == name) {
				if (outsideOfFunction && !(declaration->flags & DECLARATION_IS_CONSTANT)) {
					*success = false;
					assert(false); // @ErrorMessage only constants in outer functions can be accessed
					return nullptr;
				}

				return declaration;
			}
		}

		if (block->flags & BLOCK_IS_ARGUMENTS) { // If the block we were resolving is is function arguments, we can only resolve globals or constants in outer blocks
			outsideOfFunction = true;
		}
	}

	return nullptr;
}

// @Revisit: should we disallow local variables overwriting variables in parent local blocks
bool addDeclarationToBlock(Block *block, Declaration *declaration) {
	assert(block);
	assert(declaration);

	for (auto previous : block->declarations) {
		if (previous->name == declaration->name) {
			assert(false); // @ErrorMessage Cannot redeclare a variable within the same scope
			return false;
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
	TokenT tokens[4];
	bool rightAssociative;
};

static BinaryOperator binaryOpPrecedences[] = {
	{{TokenT::LOGIC_OR}},
	{{TokenT::LOGIC_AND}},
	{{TOKEN('|')}}, // @Improvement: Change operator precedence away from C++, make bitwise operators higher precedence than equality? 
	{{TOKEN('^')}},
	{{TOKEN('&')}},
	{{TokenT::EQUAL}},
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
	block->parentBlock = currentBlock;
	currentBlock = block;
}

static void popBlock(Block *block) { // This only takes the parameter to make sure we are always popping the block we think we are in debug
	assert(currentBlock == block);

	currentBlock = currentBlock->parentBlock;

	_ReadWriteBarrier(); // Make sure the compiler doesn't try to move this flag set before a previous add, as this means it is safe for the type inference thread 

	block->flags |= BLOCK_IS_COMPLETE;


	if (block->declarations.count) {
		inferQueue.add(makeDeclarationPack(block->declarations.count, block->declarations.storage)); // Queue the entire block at once to avoid locking the mutex too much
	}
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

ExprLiteral *makeTypeLiteral(CodeLocation &start, EndLocation &end, Type *type) {
	ExprLiteral *literal = new ExprLiteral;
	literal->flavor = ExprFlavor::TYPE_LITERAL;
	literal->start = start;
	literal->end = end;
	literal->typeValue = type;
	literal->type = &TYPE_TYPE;

	return literal;
}

ExprIdentifier *makeIdentifier(CodeLocation &start, EndLocation &end, Declaration *declaration) {
	ExprIdentifier *identifier = new ExprIdentifier;
	identifier->flavor = ExprFlavor::IDENTIFIER;
	identifier->start = start;
	identifier->end = end;
	identifier->declaration = declaration;
	identifier->name = declaration->name;
	identifier->resolveFrom = nullptr;
	
	return identifier;
}

ExprBinaryOperator *makeBinaryOperator(CodeLocation &start, EndLocation &end, TokenT op, Expr *left) {
	ExprBinaryOperator *expr = new ExprBinaryOperator;
	expr->start = start;
	expr->end = end;
	expr->flavor = ExprFlavor::BINARY_OPERATOR;
	expr->op = op;
	expr->left = left;

	return expr;
}

Declaration *makeIterator(CodeLocation &start, EndLocation &end, String name) {
	Declaration *declaration = new Declaration;
	declaration->start = start;
	declaration->end = end;
	declaration->type = nullptr;
	declaration->initialValue = nullptr;
	declaration->name = name;


	return declaration;
}

Expr *parseStatement(LexerFile *lexer) {
	if (lexer->token.type == TokenT::FOR) {
		ExprLoop *loop = new ExprLoop;
		loop->flavor = ExprFlavor::FOR;
		loop->start = lexer->token.start;

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
					assert(false); // @ErrorMessage
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
					assert(false); // @ErrorMessage
					return nullptr;
				}

				Declaration *it_index = makeIterator(lexer->token.start, lexer->token.end, lexer->token.text);
				it->flags |= DECLARATION_IS_ITERATOR_INDEX;

				if (!addDeclarationToBlock(&loop->iteratorBlock, it_index)) {
					assert(false); // @ErrorMessage
					return nullptr;
				}

				lexer->advance();

				if (!expectAndConsume(lexer, ':')) {
					assert(false); // @ErrorMessage
					return nullptr;
				}
			}
			else {
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
			popBlock(&loop->iteratorBlock);

			if (!loop->body)
				return nullptr;
		}

		return loop;
	}
	else if (lexer->token.type == TOKEN('{')) {
		return parseBlock(lexer);
	}
	else if (lexer->token.type == TokenT::WHILE) {
		ExprLoop *loop = new ExprLoop;
		loop->flavor = ExprFlavor::WHILE;
		loop->start = lexer->token.start;

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
			else {
				Declaration *it = makeIterator(loop->start, end, ""); // @Hack :AnonymousWhileLoopIterator
				it->flags |= DECLARATION_IS_ITERATOR;

				if (!addDeclarationToBlock(&loop->iteratorBlock, it)) {
					assert(false); // Invalid code path, we should never fail to add something to an empty block
				}
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
			popBlock(&loop->iteratorBlock);

			if (!loop->body)
				return nullptr;
		}

		return loop;
	}
	else if (lexer->token.type == TokenT::IF) {
		ExprIf *ifElse = new ExprIf;
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
		ExprBreakOrContinue *continue_ = new ExprBreakOrContinue;
		continue_->flavor = lexer->token.type == TokenT::CONTINUE ? ExprFlavor::CONTINUE : ExprFlavor::BREAK;
		continue_->start = lexer->token.start;
		
		lexer->advance();

		if (lexer->token.type == TokenT::IDENTIFIER) {
			bool success;
			Declaration *iterator = resolveIdentifier(lexer->token.text, &success);

			if (!success)
				return nullptr;

			if (!iterator) {
				assert(false); // @ErrorMessage
				return nullptr;
			}

			if (!(iterator->flags & DECLARATION_IS_ITERATOR)) {
				assert(false); // @ErrorMessage
				return nullptr;
			}

			continue_->refersTo = CAST_FROM_SUBSTRUCT(ExprLoop, iteratorBlock, iterator->enclosingScope);

			assert(continue_->refersTo->flavor == ExprFlavor::FOR || continue_->refersTo->flavor == ExprFlavor::WHILE);

			continue_->end = lexer->token.end;

			lexer->advance();
			return continue_;
		}
		else {
			continue_->end = lexer->previousTokenEnd;

			// Don't pass through arguments blocks since we can't break from an inner function to an outer one
			for (Block *block = currentBlock; block && !(block->flags & BLOCK_IS_ARGUMENTS); block = block->parentBlock) {

				// @Hack :AnonymousWhileLoopIterator
				// @Hack :AnonymousWhileLoopIterator
				// @Hack :AnonymousWhileLoopIterator
				// @Hack :AnonymousWhileLoopIterator
				// @Hack :AnonymousWhileLoopIterator
				// @Hack :AnonymousWhileLoopIterator
				// To find the loop for break/continue we search through scopes until we find a scope that has its first variable as a loop iterator
				//   - This means that the loop iterator needs to be the first variable in the scope - problematic if we switch to a hash table
				//   - We have to insert an anonymous iterator into for loops declared with no label - a declaration with name ""
				if (block->declarations.count && (block->declarations[0]->flags & DECLARATION_IS_ITERATOR)) {
					continue_->refersTo = CAST_FROM_SUBSTRUCT(ExprLoop, iteratorBlock, block);
					return continue_;
				}
			}

			assert(false); // @ErrorMessage cannot have a break/continue statement outside a loop
			return nullptr;
		}
	}
	else if (lexer->token.type == TokenT::RETURN) {
		ExprReturn *return_ = new ExprReturn;
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
				assert(false); // @ErrorMessage Expression has no side effects
				return nullptr;
			}

			return expr;
		}
	}
}

ExprBlock *parseBlock(LexerFile *lexer) {

	ExprBlock *block = new ExprBlock;
	block->start = lexer->token.start;
	block->flavor = ExprFlavor::BLOCK;

	pushBlock(&block->declarations);

	if (!expectAndConsume(lexer, '{')) {
		assert(false); // @ErrorMessage
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
					assert(false); // @ErrorMessage
					return nullptr;
				}

				if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IS_UNINITIALIZED))) { // If this declaration is constant or uninitialized don't add an initialization expression
					ExprBinaryOperator *assign = new ExprBinaryOperator;
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
	ExprLiteral *literal = new ExprLiteral;
	literal->start = start;
	literal->end = end;
	literal->unsignedValue = value;
	literal->type = type;
	literal->flavor = ExprFlavor::INT_LITERAL;
	return literal;
}


// @Incomplete allow the return type expression of a function to refer to the arguments
Expr *parsePrimaryExpr(LexerFile *lexer) {
	CodeLocation start = lexer->token.start;
	EndLocation end = lexer->token.end;

	Expr *expr = nullptr;

	if (expectAndConsume(lexer, '(')) {
		end = lexer->token.end;

		if (expectAndConsume(lexer, ')')) { // This is a function with no arguments
			if (lexer->token.type == TOKEN('{')) { // It also returns void
				ExprFunction *function = new ExprFunction;
				function->flavor = ExprFlavor::FUNCTION;
				function->start = start;
				function->end = lexer->previousTokenEnd;
				function->arguments.flags |= BLOCK_IS_ARGUMENTS;
				function->returnType = makeTypeLiteral(function->start, function->end, &TYPE_VOID);
				pushBlock(&function->arguments);
				
				function->body = parseBlock(lexer);

				popBlock(&function->arguments);

				_ReadWriteBarrier();
				inferQueue.add(makeDeclarationPack(function));

				expr = function;
			}
			else if (expectAndConsume(lexer, TokenT::ARROW)) {
				Expr *returnType = parseExpr(lexer);

				if (lexer->token.type == TOKEN('{')) { // This is a function value
					ExprFunction *function = new ExprFunction;
					function->flavor = ExprFlavor::FUNCTION;
					function->start = start;
					function->end = lexer->previousTokenEnd;
					function->returnType = returnType;
					function->arguments.flags |= BLOCK_IS_ARGUMENTS;
					pushBlock(&function->arguments);

					function->body = parseBlock(lexer);

					popBlock(&function->arguments);

					_ReadWriteBarrier();
					inferQueue.add(makeDeclarationPack(function));

					expr = function;
				}
				else { // This is a function type
					TypeFunction *type = new TypeFunction;
					type->flavor = TypeFlavor::FUNCTION;
					type->size = 8;
					type->alignment = 8;
					type->argumentCount = 0;
					type->argumentTypes = nullptr;
					type->returnType = returnType;
					
					expr = makeTypeLiteral(start, lexer->previousTokenEnd, type);
				}
			}
			else {
				assert(false); // @ErrorMessage You must specify the void return type for a function type
				return nullptr;
			}
		}
		else if (lexer->token.type == TokenT::IDENTIFIER) { // There is an identifier after the function name, this could be an expression or argument delcaration
			TokenT peek;

			lexer->peekTokenTypes(1, &peek);
			
			if (peek == TOKEN(':')) { // This is an argument declaration
				Block argumentDeclarations;

				do {
					Declaration *declaration = parseDeclaration(lexer);

					if (declaration->flags & DECLARATION_IS_CONSTANT) {
						assert(false); // @ErrorMessage cannot have a constant default argument
						return nullptr;
					}

					if (declaration->flags & DECLARATION_IS_UNINITIALIZED) {
						assert(false); // @ErrorMessage cannot have an uninitialized default argument
						return nullptr;
					}

					if (!addDeclarationToBlock(&argumentDeclarations, declaration)) {
						return nullptr;
					}
				} while (expectAndConsume(lexer, ','));

				if (!expectAndConsume(lexer, ')')) {
					assert(false); // @ErrorMessage
					return nullptr;
				}

				if (expectAndConsume(lexer, '{')) {
					ExprFunction *function = new ExprFunction;
					function->flavor = ExprFlavor::FUNCTION;
					function->start = start;
					function->end = lexer->previousTokenEnd;
					function->arguments = argumentDeclarations;
					function->arguments.flags |= BLOCK_IS_ARGUMENTS;
					function->returnType = makeTypeLiteral(function->start, function->end, &TYPE_VOID);
					pushBlock(&function->arguments);

					function->body = parseBlock(lexer);

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
						ExprFunction *function = new ExprFunction;
						function->flavor = ExprFlavor::FUNCTION;
						function->start = start;
						function->end = lexer->previousTokenEnd;
						function->arguments = argumentDeclarations;
						function->returnType = returnType;
						function->arguments.flags |= BLOCK_IS_ARGUMENTS;
						pushBlock(&function->arguments);

						function->body = parseBlock(lexer);

						popBlock(&function->arguments);

						_ReadWriteBarrier();
						inferQueue.add(makeDeclarationPack(function));

						expr = function;
					}
					else {
						TypeFunction *type = new TypeFunction;
						type->flavor = TypeFlavor::FUNCTION;
						type->size = 8;
						type->alignment = 8;
						type->argumentCount = argumentDeclarations.declarations.count;
						type->argumentTypes = new Expr * [type->argumentCount];
						type->returnType = returnType;

						for (u64 i = 0; i < argumentDeclarations.declarations.count; i++) {
							auto declaration = argumentDeclarations.declarations[i];

							if (declaration->initialValue) {
								assert(false); // @ErrorMessage function types cannot have default values
								return nullptr;
							}

							assert(declaration->type);
							type->argumentTypes[i] = declaration->type;
							delete declaration;
						}

						argumentDeclarations.declarations.free();

						expr = makeTypeLiteral(start, lexer->previousTokenEnd, type);
					}
				}
				else {
					assert(false); // @ErrorMessage Function type must declare the return type
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

					TypeFunction *type = new TypeFunction;
					type->flavor = TypeFlavor::FUNCTION;
					type->returnType = returnType;
					type->size = 8;
					type->alignment = 8;
					type->argumentCount = 1;

					type->argumentTypes = new Expr * [1]{ expr };

					expr = makeTypeLiteral(start, end, type);
				}
				else {
					// We have put the bracketed expression in expr so we are done
				}
			}
			else if (expectAndConsume(lexer, ',')) { // This is a function type since there are multiple comma separated values in parentheses
				Array<Expr *> arguments;
				arguments.add(expr);

				do {
					Expr *argument = parseExpr(lexer);
					if (!argument)
						return nullptr;

					arguments.add(expr);
				} while (expectAndConsume(lexer, ','));

				if (!expectAndConsume(lexer, ')')) {
					assert(false); // @ErrorMessage
					return nullptr;
				}

				if (!expectAndConsume(lexer, TokenT::ARROW)) { // Even though this is unambiguously a function type, still require a return type to be given for consistency
					assert(false); // @ErrorMessage
					return nullptr;
				}

				Expr *returnType = parseExpr(lexer);
				if (!returnType)
					return nullptr;

				TypeFunction *type = new TypeFunction;
				type->flavor = TypeFlavor::FUNCTION;
				type->returnType = returnType;
				type->size = 8;
				type->alignment = 8;
				type->argumentCount = arguments.count;
				type->argumentTypes = arguments.storage;

				expr = makeTypeLiteral(start, lexer->previousTokenEnd, type);
			}
			else {
				assert(false); // @ErrorMessage
				return nullptr;
			}
		}
	
	}
	else if (lexer->token.type == TokenT::IDENTIFIER) {
		bool success;

		ExprIdentifier *identifier = new ExprIdentifier;
		identifier->start = start;
		identifier->end = end;
		identifier->name = lexer->token.text;
		identifier->flavor = ExprFlavor::IDENTIFIER;
		identifier->declaration = resolveIdentifier(identifier->name, &success); // This may fail to find the identifier which may mean that it is an identifier not yet declared in global scope or an error

		if (!success)
			return nullptr;

		if (!identifier->declaration) {
			identifier->resolveFrom = currentBlock;
		}

		if (identifier->declaration && (identifier->declaration->flags & DECLARATION_IS_ITERATOR)) {
			ExprLoop *loop = CAST_FROM_SUBSTRUCT(ExprLoop, iteratorBlock, identifier->declaration->enclosingScope);

			if (loop->flavor == ExprFlavor::WHILE) {
				assert(false); // @ErrorMessage the while loop label is not a real declaration, it can only be used in a break or continue statement
				return nullptr;
			}
		}

		lexer->advance();

		expr = identifier;
	}
	else if (lexer->token.type == TokenT::FLOAT_LITERAL) {
		ExprLiteral *literal = new ExprLiteral;
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
		expr = makeIntegerLiteral(start, end, 0, &TYPE_NULL);
	}
	else if (lexer->token.type == TokenT::STRING_LITERAL) {
		assert(false); // @Incomplete handle strings 
	}
	else if (expectAndConsume(lexer, TokenT::SIZE_OF)) {
		ExprUnaryOperator *unary = new ExprUnaryOperator;
		unary->flavor = ExprFlavor::UNARY_OPERATOR;
		unary->op = TokenT::SIZE_OF;
		unary->start = start;

		if (!expectAndConsume(lexer, '(')) {
			assert(false); // @ErrorMessage
			return nullptr;
		}

		unary->value = parseExpr(lexer);
		if (!unary->value) {
			return nullptr;
		}
		unary->end = lexer->token.end;

		if (!expectAndConsume(lexer, ')')) {
			assert(false); // @ErrorMessage
			return nullptr;
		}

		expr = unary;
	}
	else if (expectAndConsume(lexer, TokenT::TYPE_OF)) {
		ExprUnaryOperator *unary = new ExprUnaryOperator;
		unary->flavor = ExprFlavor::UNARY_OPERATOR;
		unary->op = TokenT::TYPE_OF;
		unary->start = start;

		if (!expectAndConsume(lexer, '(')) {
			assert(false); // @ErrorMessage
			return nullptr;
		}

		unary->value = parseExpr(lexer);
		if (!unary->value) {
			return nullptr;
		}
		unary->end = lexer->token.end;

		if (!expectAndConsume(lexer, ')')) {
			assert(false); // @ErrorMessage
			return nullptr;
		}

		expr = unary;
	}
	else if (expectAndConsume(lexer, TokenT::U8)) {
		expr = makeTypeLiteral(start, end, &TYPE_U8);
	}
	else if (expectAndConsume(lexer, TokenT::U16)) {
		expr = makeTypeLiteral(start, end, &TYPE_U16);
	}
	else if (expectAndConsume(lexer, TokenT::U32)) {
		expr = makeTypeLiteral(start, end, &TYPE_U32);
	}
	else if (expectAndConsume(lexer, TokenT::U64)) {
		expr = makeTypeLiteral(start, end, &TYPE_U64);
	}
	else if (expectAndConsume(lexer, TokenT::S8)) {
		expr = makeTypeLiteral(start, end, &TYPE_S8);
	}
	else if (expectAndConsume(lexer, TokenT::S16)) {
		expr = makeTypeLiteral(start, end, &TYPE_S16);
	}
	else if (expectAndConsume(lexer, TokenT::S32)) {
		expr = makeTypeLiteral(start, end, &TYPE_S32);
	}
	else if (expectAndConsume(lexer, TokenT::S64)) {
		expr = makeTypeLiteral(start, end, &TYPE_S64);
	}
	else if (expectAndConsume(lexer, TokenT::BOOL)) {
		expr = makeTypeLiteral(start, end, &TYPE_BOOL);
	}
	else if (expectAndConsume(lexer, TokenT::TYPE)) {
		expr = makeTypeLiteral(start, end, &TYPE_TYPE);
	}
	else if (expectAndConsume(lexer, TokenT::VOID)) {
		expr = makeTypeLiteral(start, end, &TYPE_VOID);
	}
	else if (expectAndConsume(lexer, TokenT::F32)) {
		expr = makeTypeLiteral(start, end, &TYPE_F32);
	}
	else if (expectAndConsume(lexer, TokenT::F64)) {
		expr = makeTypeLiteral(start, end, &TYPE_F64);
	}
	else if (expectAndConsume(lexer, TokenT::STRING)) {
		assert(false); // @Incomplete
		return nullptr;
	}
	else if (expectAndConsume(lexer, TokenT::STRUCT)) {
		assert(false); // @Incomplete
		return nullptr;
	}
	else {
		assert(false); // @ErrorMessage
	}
	
	while (true) {
		start = lexer->token.start;

		if (expectAndConsume(lexer, '(')) {
			ExprFunctionCall *call = new ExprFunctionCall;
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

				// @Incomplete: named arguments?
				do {
					Expr *argument = parseExpr(lexer);

					if (!argument)
						return nullptr;

					arguments.add(argument);
				} while (expectAndConsume(lexer, ','));

				call->end = lexer->token.end;

				if (!expectAndConsume(lexer, ')')) {
					assert(false); // @ErrorMessage
					return nullptr;
				}

				call->argumentCount = arguments.count;
				call->arguments = arguments.storage;
			}

			expr = call;
		}
		else if (expectAndConsume(lexer, '[')) {
			ExprBinaryOperator *deref = new ExprBinaryOperator;
			deref->start = start;
			deref->left = expr;
			deref->flavor = ExprFlavor::BINARY_OPERATOR;
			deref->op = TOKEN('[');

			deref->right = parseExpr(lexer);

			if (!deref->right)
				return nullptr;

			deref->end = lexer->token.end;

			if (!expectAndConsume(lexer, ']')) {
				assert(false); // @ErrorMessage
				return nullptr;
			}
			expr = deref;
		}
		else if (expectAndConsume(lexer, '.')) {
			ExprStructAccess *access = new ExprStructAccess;
			access->start = start;
			access->left = expr;
			access->flavor = ExprFlavor::STRUCT_ACCESS;

			if (lexer->token.type != TokenT::IDENTIFIER) {
				assert(false); // @ErrorMessage
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
	ExprUnaryOperator *expr = new ExprUnaryOperator();
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
			assert(false); // @ErrorMessage
			return nullptr;
		}

		ExprBinaryOperator *expr = new ExprBinaryOperator();
		expr->start = start;
		expr->end = lexer->token.end;
		expr->flavor = ExprFlavor::BINARY_OPERATOR;
		expr->op = TokenT::CAST;

		if (expectAndConsume(lexer, ')')) {
			expr->left = nullptr;
		}
		else {
			expr->left = parseExpr(lexer);

			if (!expr->left) {
				assert(false); // @ErrorMessage
				return nullptr;
			}

			expr->end = lexer->token.end;
			if (!expectAndConsume(lexer, ')')) {
				assert(false); // @ErrorMessage
				return nullptr;
			}
		}

		expr->right = parseUnaryExpr(lexer);
		if (!expr->right) {
			assert(false); // @ErrorMessage
			return nullptr;
		}

		return expr;
	}

	else if (lexer->token.type == TOKEN('[')) {
		assert(false);
		return nullptr; // @Incomplete array types
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

		ExprBinaryOperator *binary = new ExprBinaryOperator;
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
	assert(lexer->token.type == TokenT::IDENTIFIER);

	Declaration *declaration = new Declaration;
	declaration->flags = 0;
	declaration->name = lexer->token.text;
	declaration->start = lexer->token.start;
	lexer->advance();

	if (!expectAndConsume(lexer, ':')) {
		assert(false); // @ErrorMessage
		return nullptr;
	}

	if (expectAndConsume(lexer, ':')) {
		if (lexer->token.type == TokenT::DOUBLE_DASH) {
			assert(false); // @ErrorMessage cannot have uninitialized constant
			return nullptr;
		}

		declaration->flags |= DECLARATION_IS_CONSTANT;
		declaration->type = nullptr;

		declaration->initialValue = parseExpr(lexer);

		if (!declaration->initialValue) {
			return nullptr;
		}

		declaration->end = lexer->previousTokenEnd;
	}
	else if (expectAndConsume(lexer, '=')) {
		declaration->type = nullptr;

		if (lexer->token.type == TokenT::DOUBLE_DASH) {
			assert(false); // @ErrorMessage cannot infer the type of uninitialized value
			return nullptr;
		}

		declaration->initialValue = parseExpr(lexer);

		if (!declaration->initialValue) {
			return nullptr;
		}

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

				declaration->end = lexer->previousTokenEnd;
			}
		}
		else if (expectAndConsume(lexer, ':')) {
			declaration->flags |= DECLARATION_IS_CONSTANT;

			if (lexer->token.type == TokenT::DOUBLE_DASH) {
				assert(false); // @ErrorMessage constant values cannot be uninitialized

				return nullptr;
			}
			declaration->initialValue = parseExpr(lexer);

			if (!declaration->initialValue) {
				return nullptr;
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

void parseFile(u8 *filename) {
	LexerFile lexer;

	lexer.open(filename);

	lexer.advance();

	while (true) {
		if (lexer.token.type == TokenT::IDENTIFIER) {
			Declaration *declaration = parseDeclaration(&lexer);
			declaration->enclosingScope = nullptr;

			assert(declaration); // @ErrorMessage
			assert(currentBlock == nullptr);

			_ReadWriteBarrier();

			inferQueue.add(makeDeclarationPack(1, &declaration));
		}
		else if (lexer.token.type == TokenT::END_OF_FILE) {
			break;
		}
		else if (expectAndConsume(&lexer, ';')) {
			// We have consumed the semicolon we are done
		}
		else {
			assert(false); // @ErrorMessage
		}
	}

	inferQueue.add(makeStopSignal());

	lexer.close();
}