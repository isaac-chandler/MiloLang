#pragma once

#include "Basic.h"
#include "String.h"

struct Identifier {
	String name;
	u64 hash;
};

Identifier *getIdentifier(String name);
void initIdentTable();

void dumpIdentTable();

inline Identifier *identIt;
inline Identifier *identItIndex;
inline Identifier *identData;
inline Identifier *identCount;
inline Identifier *identCapacity;
inline Identifier *identInteger;