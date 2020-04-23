#pragma once

#include "Basic.h"
#include "TypeTable.h"
#include "Ast.h"

BucketedArenaAllocator typeArena(1024 * 1024);

#define STRUCT_HASH_PRIME 4057
#define POINTER_HASH_PRIME 4241
#define ARRAY_HASH_PRIME 4423
#define DYNAMIC_ARRAY_HASH_PRIME 4603
#define STATIC_ARRAY_SIZE_HASH_PRIME 4789
#define STATIC_ARRAY_TYPE_HASH_PRIME 4969
#define FUNCTION_ARGUMENT_HASH_PRIME 5147
#define FUNCTION_RETURN_HASH_PRINE 5347

static u32 nextStructHash = STRUCT_HASH_PRIME;

struct Entry {
	Type *value;
	u32 hash = 0;
};

static Entry *entries;
static u32 count;
static u32 capacity;

void rehash() {
	u32 newCapacity = capacity * 2;
	Entry *newEntries = new Entry[newCapacity];

	for (u32 i = 0; i < capacity; i++) {
		Entry entry = entries[i];

		if (entry.hash == 0) continue;

		u32 slot = entry.hash & (newCapacity - 1);

		while (newEntries[slot].hash) {
			if (++slot == newCapacity) slot = 0;
		}

		newEntries[slot].hash =entry.hash;
		newEntries[slot].value = entry.value;
	}

	delete[] entries;
	entries = newEntries;
	capacity = newCapacity;
}

void insertIntoTable(Type *type, u32 slot) {
	if (count * 10 > capacity * 7) {
		rehash();

		slot = type->hash & (capacity - 1);

		while (entries[slot].hash) {
			if (++slot == capacity) slot = 0;
		}
	}

	entries[slot].value = type;
	entries[slot].hash = type->hash;
}

void insertIntoTable(Type *type) {
	if (count * 10 > capacity * 7) {
		rehash();
	}

	u32 slot = type->hash & (capacity - 1);

	while (entries[slot].hash) {
		if (++slot == capacity) slot = 0;
	}

	entries[slot].value = type;
	entries[slot].hash = type->hash;
}

void addStruct(TypeStruct *struct_) {
	if (!struct_->hash) {
		u32 hash = nextStructHash;
		if (hash == 0) hash = 1;
		nextStructHash *= STRUCT_HASH_PRIME;

		struct_->hash = hash;

		insertIntoTable(struct_);
	}
}

TypePointer *getPointer(Type *type) {
	u32 hash = (type->hash + 1) * POINTER_HASH_PRIME;
	if (hash == 0) hash = 1;

	u32 slot = hash & (capacity - 1);

	for (Entry entry = entries[slot]; entry.hash; entry = entries[slot]) {
		if (entry.hash == hash && entry.value->flavor == TypeFlavor::POINTER && static_cast<TypePointer *>(entry.value)->pointerTo == type) {
			return static_cast<TypePointer *>(entry.value);
		}

		if (++slot == capacity) slot = 0;
	}

	auto result = TYPE_NEW(TypePointer);
	result->size = 8;
	result->alignment = 8;
	result->pointerTo = type;
	result->hash = hash;
	result->flavor = TypeFlavor::POINTER;

	result->name.length = type->name.length + 1;
	result->name.characters = TYPE_NEW_ARRAY(char, result->name.length);
	
	result->name.characters[0] = '*';
	memcpy(result->name.characters + 1, type->name.characters, type->name.length);

	insertIntoTable(result, slot);

	return result;
}

TypeArray *getArray(Type *type) {
	u32 hash = (type->hash + 1) * ARRAY_HASH_PRIME;
	if (hash == 0) hash = 1;

	u32 slot = hash & (capacity - 1);

	for (Entry entry = entries[slot]; entry.hash; entry = entries[slot]) {
		if (entry.hash == hash && entry.value->flavor == TypeFlavor::ARRAY) {
			auto array = static_cast<TypeArray *>(entry.value);
			
			if (array->flags == 0 && array->arrayOf == type) {
				return array;
			}
		}

		if (++slot == capacity) slot = 0;
	}

	auto result = TYPE_NEW(TypeArray);
	result->size = 16;
	result->alignment = 8;
	result->arrayOf = type;
	result->hash = hash;
	result->count = 0;
	result->flavor = TypeFlavor::ARRAY;

	result->name.length = type->name.length + 2;
	result->name.characters = TYPE_NEW_ARRAY(char, result->name.length);

	result->name.characters[0] = '[';
	result->name.characters[1] = ']';
	memcpy(result->name.characters + 2, type->name.characters, type->name.length);

	insertIntoTable(result, slot);

	return result;
}

TypeArray *getDynamicArray(Type *type) {
	u32 hash = (type->hash + 1) * DYNAMIC_ARRAY_HASH_PRIME;
	if (hash == 0) hash = 1;

	u32 slot = hash & (capacity - 1);

	for (Entry entry = entries[slot]; entry.hash; entry = entries[slot]) {
		if (entry.hash == hash && entry.value->flavor == TypeFlavor::ARRAY) {
			auto array = static_cast<TypeArray *>(entry.value);

			if (array->flags == TYPE_ARRAY_IS_DYNAMIC && array->arrayOf == type) {
				return array;
			}
		}

		if (++slot == capacity) slot = 0;
	}

	auto result = TYPE_NEW(TypeArray);
	result->size = 24;
	result->alignment = 8;
	result->arrayOf = type;
	result->hash = hash;
	result->count = 0;
	result->flavor = TypeFlavor::ARRAY;
	result->flags |= TYPE_ARRAY_IS_DYNAMIC;

	result->name.length = type->name.length + 4;
	result->name.characters = TYPE_NEW_ARRAY(char, result->name.length);

	result->name.characters[0] = '[';
	result->name.characters[1] = '.';
	result->name.characters[2] = '.';
	result->name.characters[3] = ']';
	memcpy(result->name.characters + 4, type->name.characters, type->name.length);

	insertIntoTable(result, slot);

	return result;
}

TypeArray *getStaticArray(Type *type, u64 count) {
	u32 hash = type->hash * STATIC_ARRAY_TYPE_HASH_PRIME + count * STATIC_ARRAY_SIZE_HASH_PRIME;
	if (hash == 0) hash = 1;

	u32 slot = hash & (capacity - 1);

	for (Entry entry = entries[slot]; entry.hash; entry = entries[slot]) {
		if (entry.hash == hash && entry.value->flavor == TypeFlavor::ARRAY) {
			auto array = static_cast<TypeArray *>(entry.value);

			if (array->flags == TYPE_ARRAY_IS_FIXED && array->count == count && array->arrayOf == type) {
				return array;
			}
		}

		if (++slot == capacity) slot = 0;
	}

	auto result = TYPE_NEW(TypeArray);
	result->size = type->size * count;
	result->alignment = type->alignment;
	result->arrayOf = type;
	result->hash = hash;
	result->count = count;
	result->flavor = TypeFlavor::ARRAY;
	result->flags |= TYPE_ARRAY_IS_FIXED;

	constexpr u64 maxCount = getDigitCount(UINT64_MAX);

	result->name.characters = TYPE_NEW_ARRAY(char, type->name.length + 2 + maxCount);

	result->name.characters[0] = '[';
	
	char *buffer = result->name.characters + 1;

	_ui64toa(count, buffer, 10);
	buffer = strchr(buffer, 0);
	buffer[0] = ']';

	memcpy(buffer + 1, type->name.characters, type->name.length);

	result->name.length = (buffer - result->name.characters) + 1 + type->name.length;

	insertIntoTable(result, slot);

	return result;
}


void generateTypeNameForFunction(TypeFunction *function) {
	function->name.length = 6; // "() -> "

	function->name.length += function->returnType->name.length;

	for (u64 i = 0; i < function->argumentCount; i++) {
		function->name.length += function->argumentTypes[i]->name.length;

		if (i + 1 != function->argumentCount) {
			function->name.length += 2;
		}
	}

	function->name.characters = TYPE_NEW_ARRAY(char, function->name.length);

	char *cursor = function->name.characters;
	*cursor++ = '(';

	for (u64 i = 0; i < function->argumentCount; i++) {
		String name = function->argumentTypes[i]->name;

		memcpy(cursor, name.characters, name.length);
		cursor += name.length;

		if (i + 1 != function->argumentCount) {
			*cursor++ = ',';
			*cursor++ = ' ';
		}
	}

	*cursor++ = ')';
	*cursor++ = ' ';
	*cursor++ = '-';
	*cursor++ = '>';
	*cursor++ = ' ';


	String name = function->returnType->name;

	memcpy(cursor, name.characters, name.length);
}

TypeFunction *getFunctionType(ExprFunction *expr) {
	auto returnType = static_cast<ExprLiteral *>(expr->returnType)->typeValue;
	auto &arguments = expr->arguments.declarations;

	u32 hash = returnType->hash * FUNCTION_RETURN_HASH_PRINE;


	for (u64 i = 0; i < arguments.count; i++) {
		hash += static_cast<ExprLiteral *>(arguments[i]->type)->typeValue->hash;
		hash *= FUNCTION_ARGUMENT_HASH_PRIME;
	}

	if (hash == 0) hash = 1;

	u32 slot = hash & (capacity - 1);

	for (Entry entry = entries[slot]; entry.hash; entry = entries[slot]) {
		if (entry.hash == hash && entry.value->flavor == TypeFlavor::FUNCTION) {
			auto function = static_cast<TypeFunction *>(entry.value);

			if (function->argumentCount == arguments.count && function->returnType == returnType) {
				for (u64 i = 0; i < arguments.count; i++) {
					if (function->argumentTypes[i] != static_cast<ExprLiteral *>(arguments[i]->type)->typeValue) {
						goto cont;
					}
				}

				return function;
			}
		}

		cont:
		if (++slot == capacity) slot = 0;
	}

	auto result = TYPE_NEW(TypeFunction);
	result->size = 8;
	result->alignment = 8;
	result->returnType = returnType;
	result->hash = hash;
	result->argumentCount = arguments.count;
	result->argumentTypes = TYPE_NEW_ARRAY(Type *, result->argumentCount);

	for (u64 i = 0; i < arguments.count; i++) {
		result->argumentTypes[i] = static_cast<ExprLiteral *>(arguments[i]->type)->typeValue;
	}

	result->flavor = TypeFlavor::FUNCTION;
	generateTypeNameForFunction(result);

	insertIntoTable(result, slot);

	return result;
}

void setupTypeTable() {
	capacity = 1024;
	entries = new Entry[capacity];
	count = 0;

	insertIntoTable(&TYPE_U8);
	insertIntoTable(&TYPE_U16);
	insertIntoTable(&TYPE_U32);
	insertIntoTable(&TYPE_U64);

	insertIntoTable(&TYPE_S8);
	insertIntoTable(&TYPE_S16);
	insertIntoTable(&TYPE_S32);
	insertIntoTable(&TYPE_S64);

	insertIntoTable(&TYPE_F32);
	insertIntoTable(&TYPE_F64);

	insertIntoTable(&TYPE_BOOL);
	insertIntoTable(&TYPE_VOID);
	insertIntoTable(&TYPE_STRING);
	insertIntoTable(&TYPE_TYPE);

	TYPE_VOID_POINTER = getPointer(&TYPE_VOID);
}