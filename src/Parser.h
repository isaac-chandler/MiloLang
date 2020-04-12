#pragma once

#include "Ast.h"

bool addDeclarationToBlock(Block *block, Declaration *declaration);
ExprLiteral *makeTypeLiteral(CodeLocation &start, EndLocation &end, Type *type);

void parseFile(struct FileInfo *filename);