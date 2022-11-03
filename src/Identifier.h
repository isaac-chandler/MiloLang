#pragma once

#include "String.h"
#include "Basic.h"

struct Identifier {
	u32 hash;
	u32 length;
	char characters[];
};

Identifier *getIdentifier(String name);