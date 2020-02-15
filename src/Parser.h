#pragma once

#include "Ast.h"
#include "Lexer.h"

bool addDeclarationToBlock(Block *block, Declaration *declaration);
ExprLiteral *makeTypeLiteral(CodeLocation &start, EndLocation &end, Type *type);

LexerFile parseFile(u8 *filename);