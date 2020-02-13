#pragma once

#include "Ast.h"

bool addDeclarationToBlock(Block *block, Declaration *declaration);

void parseFile(u8 *filename);