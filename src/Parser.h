#pragma once

#include "Ast.h"

Declaration *findDeclaration(Block *block, String name);
bool addDeclarationToBlock(Block *block, Declaration *declaration);
ExprLiteral *makeTypeLiteral(CodeLocation &start, EndLocation &end, Type *type);

void parseFile(struct FileInfo *filename);