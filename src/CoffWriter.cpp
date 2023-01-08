#include "Basic.h"
#include "CoffWriter.h"
#include "BucketedArenaAllocator.h"
#include "Block.h"
#include "TypeTable.h"
#include "CompilerMain.h"
#include "Error.h"
#include "UTF.h"
#include "CodeView.h"
#include "IrGenerator.h"

union SymbolName {
	char name[8];
	struct {
		u32 zeroes;
		u32 namePointer;
	};
};

struct CoffTypeIndexPatch {
	u32 *location;
	Type *type;
};

struct CoffFunctionIDTypeIndexPatch {
	u32 *location;
	ExprFunction *function;
};

#pragma pack(push, 1)
struct FileHeader {
	u16 machine;
	u16 numberOfSections;
	u32 timestamp;
	u32 pointerToSymbolTable;
	u32 numberOfSymbols;
	u16 sizeOfOptionalHeader;
	u16 characteristics;
};

struct SectionHeader {
	char name[8];
	u32 virtualSize;
	u32 virtualAddress;
	u32 sizeOfRawData;
	u32 pointerToRawData;
	u32 pointerToRelocations;
	u32 pointerToLinenumbers;
	u16 numberOfRelocations;
	u16 numberOfLinenumbers;
	u32 characteristics;
};



union Symbol {
	struct {
		SymbolName name;
		u32 value;
		s16 sectionNumber;
		u16 type;
		u8 storageClass;
		u8 numberOfAuxSymbols;
	};

	u8 aux[18];
};

struct Relocation {
	u32 virtualAddress;
	u32 symbolTableIndex;
	u16 type;
};
#pragma pack(pop)

BucketedArenaAllocator stringTable(65536);
BucketArray<Symbol> symbols;

s64 emptyStringSymbolIndex = -1;

BucketedArenaAllocator code(65536);
BucketedArenaAllocator data(65536);
BucketedArenaAllocator rdata(65536);

void setSectionName(char *header, u64 size, const char *name) {
	u64 len = strlen(name);
	assert(len <= size);

	memcpy(header, name, len);
	memset(header + len, 0, size - len);
}

void setSymbolName(SymbolName *header, String name) {
	if (name.length <= sizeof(header->name)) {
		memcpy(header->name, name.characters, name.length);
		memset(header->name + name.length, 0, sizeof(header->name) - name.length);
	}
	else {
		header->zeroes = 0;
		header->namePointer = static_cast<u32>(stringTable.totalSize + 4);
		stringTable.addNullTerminatedString(name);
	}
}

void setSymbolName(SymbolName *header, u64 value) {
	char buffer[17] = { '@' };

	u32 characters = 0;


	for (u64 shift = value;;) {
		++characters;

		shift >>= 4;
		if (!shift) break;
	}

	for (u32 i = 0; i < characters; i++) {
		buffer[characters - i] = "0123456789ABCDEF"[value & 0xF];
		value >>= 4;
	}

	setSymbolName(header, { buffer, characters + 1 });
}

u32 getParameterSpaceForCallOffset(ExprFunction *function) {
	return 0;
}

u32 getStackSpaceOffset(ExprFunction *function) {
	return getParameterSpaceForCallOffset(function) + AlignPO2(function->state.maxCallArguments * 8, 16);
}

u32 getRegisterOffset(ExprFunction *function) {
	return getStackSpaceOffset(function) + AlignPO2(function->state.stackSpace, 8);
}

u32 getSpaceToAllocate(ExprFunction *function) {
	u32 registerCount = function->state.nextRegister - function->state.parameters;

	return AlignPO2(getRegisterOffset(function) + registerCount * 8 - 8, 16) + 8;
}

u32 getRegisterOffset(ExprFunction *function, u32 regNo) {
	if (regNo >= function->state.parameters) {
		return getRegisterOffset(function) + (regNo - function->state.parameters) * 8;
	}

	return getSpaceToAllocate(function) + regNo * 8 + 24;
}


#define RAX 0
#define RCX 1
#define RDX 2
#define RBX 3
#define RSP 4
#define RBP 5
#define RSI 6
#define RDI 7

#define AH 4
#define CH 5
#define DH 6
#define BH 7

#define C_O 0x0
#define C_NO 0x1

#define C_B 0x2
#define C_NAE 0x2
#define C_C 0x2

#define C_AE 0x3
#define C_NB 0x3
#define C_NC 0x3

#define C_Z 0x4
#define C_E 0x4

#define C_NZ 0x5
#define C_NE 0x5

#define C_BE 0x6
#define C_NA 0x6

#define C_A 0x7
#define C_NBE 0x7

#define C_S 0x8
#define C_NS 0x9

#define C_P 0xA
#define C_NP 0xB

#define C_L 0xC
#define C_NGE 0xC

#define C_GE 0xD
#define C_NL 0xD

#define C_LE 0xE
#define C_NG 0xE

#define C_G 0xF
#define C_NLE 0xF

void writeRSPOffsetByte(u8 physicalRegister, u32 offset) {
	if (offset >= 0x80) {
		code.add1Unchecked(0x84 | (physicalRegister << 3));
		code.add1Unchecked(0x24);
		code.add4Unchecked(offset);
	}
	else if (offset != 0) {
		code.add1Unchecked(0x44 | (physicalRegister << 3));
		code.add1Unchecked(0x24);
		code.add1Unchecked(static_cast<u8>(offset));
	}
	else {
		code.add1Unchecked(0x04 | (physicalRegister << 3));
		code.add1Unchecked(0x24);
	}
}

void writeRSPRegisterByte(ExprFunction *function, u8 physicalRegister, u32 stackRegister, u32 addition = 0) {
	writeRSPOffsetByte(physicalRegister, getRegisterOffset(function, stackRegister) + addition);
}

void loadIntoIntRegister(ExprFunction *function, u64 size, u8 loadInto, u32 regNo) {
	u8 rex = 0x40;

	if (size == 8) {
		rex |= 8;
	}

	if (loadInto >= 8) {
		rex |= 4;
		loadInto -= 8;
	}

	if (size == 2) {
		code.add1Unchecked(0x66);
	}

	if (rex != 0x40) {
		code.add1Unchecked(rex);
	}

	if (size == 1) {
		code.add1Unchecked(0x8a);
	}
	else {
		code.add1Unchecked(0x8b);
	}

	writeRSPRegisterByte(function, loadInto, regNo);
}

void storeFromIntRegister(ExprFunction *function, u64 size, u32 regNo, u8 storeFrom) {
	u8 rex = 0x40;

	if (size == 8) {
		rex |= 8;
	}

	if (storeFrom >= 8) {
		rex |= 4;
		storeFrom -= 8;
	}

	if (size == 2) {
		code.add1Unchecked(0x66);
	}

	if (rex != 0x40) {
		code.add1Unchecked(rex);
	}

	if (size == 1) {
		code.add1Unchecked(0x88);
	}
	else {
		code.add1Unchecked(0x89);
	}

	writeRSPRegisterByte(function, storeFrom, regNo);
}

void loadIntoFloatRegister(ExprFunction *function, u64 size, u8 loadInto, u32 regNo) {
	if (size == 8) {
		code.add1Unchecked(0xF2);
	}
	else {
		code.add1Unchecked(0xF3);
	}

	if (loadInto >= 8) {
		code.add1Unchecked(0x44);
		loadInto -= 8;
	}

	code.add1Unchecked(0x0F);
	code.add1Unchecked(0x10);

	writeRSPRegisterByte(function, loadInto, regNo);
}

void storeFromFloatRegister(ExprFunction *function, u64 size, u32 regNo, u8 storeFrom) {
	if (size == 8) {
		code.add1Unchecked(0xF2);
	}
	else {
		code.add1Unchecked(0xF3);
	}

	if (storeFrom >= 8) {
		code.add1Unchecked(0x44);
		storeFrom -= 8;
	}

	code.add1Unchecked(0x0F);
	code.add1Unchecked(0x11);

	writeRSPRegisterByte(function, storeFrom, regNo);
}

void storeImmediate(ExprFunction *function, u64 size, u32 regNo, u64 immediate) {
	assert(isStandardSize(size));

	if (size == 8 && static_cast<s64>(immediate) != static_cast<s64>(static_cast<s32>(immediate))) {
		code.add1Unchecked(0x48); // mov rax, ir.a
		code.add1Unchecked(0xB8);
		code.add8Unchecked(immediate);

		storeFromIntRegister(function, 8, regNo, RAX);
	}
	else {
		if (size == 2) {
			code.add1Unchecked(0x66);
		}
		else if (size == 8) {
			code.add1Unchecked(0x48);
		}

		if (size == 1) {
			code.add1Unchecked(0xC6);
		}
		else {
			code.add1Unchecked(0xC7);
		}

		writeRSPRegisterByte(function, 0, regNo);

		if (size == 1) {
			code.add1Unchecked(static_cast<u8>(immediate));
		}
		else if (size == 2) {
			code.add2Unchecked(static_cast<u16>(immediate));
		}
		else if (size == 4 || size == 8) {
			code.add4Unchecked(static_cast<u32>(immediate));
		}
	}
}

void setCondition(ExprFunction *function, u32 dest, u8 condition) {
	code.add1Unchecked(0x0F);
	code.add1Unchecked(0x90 | condition);
	writeRSPRegisterByte(function, 0, dest);
}

void setConditionInt(ExprFunction *function, u64 size, u32 dest, u32 a, u32 b, u8 condition) {
	loadIntoIntRegister(function, size, RAX, a);
	loadIntoIntRegister(function, size, RCX, b);

	if (size == 8) {
		code.add1Unchecked(0x48);
	}
	else if (size == 2) {
		code.add1Unchecked(0x66);
	}

	if (size == 1) {
		code.add1Unchecked(0x38);
	}
	else {
		code.add1Unchecked(0x39);
	}

	code.add1Unchecked(0xC8);

	setCondition(function, dest, condition);
}

void setConditionFloat(ExprFunction *function, u64 size, u32 dest, u32 a, u32 b, u8 condition) {
	loadIntoFloatRegister(function, size, 0, a);
	loadIntoFloatRegister(function, size, 1, b);

	if (size == 8) {
		code.add1Unchecked(0x66);
	}

	code.add1Unchecked(0x0F);
	code.add1Unchecked(0x2F);
	code.add1Unchecked(0xC1);

	setCondition(function, dest, condition);
}

void loadImmediateIntoRAX(u64 immediate) {
	if (static_cast<s64>(immediate) != static_cast<s64>(static_cast<s32>(immediate))) {
		code.add1Unchecked(0x48);
		code.add1Unchecked(0xB8);
		code.add8Unchecked(immediate);
	}
	else {
		code.add1Unchecked(0x48);
		code.add1Unchecked(0xC7);
		code.add1Unchecked(0xC0);
		code.add4Unchecked(static_cast<u32>(immediate));
	}
}


void loadImmediateIntoIntRegister(u8 loadInto, u64 immediate) {
	if (loadInto == RAX) {
		loadImmediateIntoRAX(immediate);
	}
	else {
		if (immediate <= 0x7FFF'FFFF) {
			if (loadInto >= 8) {
				code.add1Unchecked(0x41);
				loadInto -= 8;
			}

			code.add1Unchecked(0xB8 | loadInto);
			code.add4Unchecked(static_cast<u32>(immediate));
		}
		else {
			loadImmediateIntoRAX(immediate);

			if (loadInto >= 8) {
				code.add1Unchecked(0x49);
				loadInto -= 8;
			}
			else {
				code.add1Unchecked(0x48);
			}

			code.add1Unchecked(0x89);
			code.add1Unchecked(0xC0 | loadInto);
		}
	}
}

void writeSet(ExprFunction *function, u64 size, u32 dest, u32 src) {
	assert(isStandardSize(size));
	loadIntoIntRegister(function, size, RAX, src);
	storeFromIntRegister(function, size, dest, RAX);
}

#define RDATA_SECTION_NUMBER 1
#define BSS_SECTION_NUMBER 2
#define DATA_SECTION_NUMBER 3
#define TEXT_SECTION_NUMBER 4
#define DEBUG_SYMBOL_SECTION_NUMBER 5
#define DEBUG_TYPE_SECTION_NUMBER 6
#define PDATA_SECTION_NUMBER 7
#define XDATA_SECTION_NUMBER 8

Array<CoffTypeIndexPatch> coffTypePatches;
Array<CoffFunctionIDTypeIndexPatch> coffFunctionIdTypePatches;


u32 *addRelocationToUnkownSymbol(BucketedArenaAllocator *allocator, u32 virtualAddress, u16 type) {
	allocator->ensure(10);
	allocator->add4Unchecked(virtualAddress);
	u32 *value = allocator->add4Unchecked(0);
	allocator->add2Unchecked(type);

	return value;
}

u32 createRdataPointer() {
	Symbol symbol;
	setSymbolName(&symbol.name, symbols.count());
	symbol.value = static_cast<u32>(rdata.totalSize);
	symbol.sectionNumber = RDATA_SECTION_NUMBER;
	symbol.type = 0;
	symbol.storageClass = IMAGE_SYM_CLASS_STATIC;
	symbol.numberOfAuxSymbols = 0;

	symbols.add(symbol);

	return symbols.count() - 1;
}

void addPointerRelocation(BucketedArenaAllocator *relocations, u32 address, u32 symbol) {
	relocations->ensure(10);
	relocations->add4Unchecked(address);
	relocations->add4Unchecked(symbol);
	relocations->add2Unchecked(IMAGE_REL_AMD64_ADDR64);
}

Symbol *allocateSymbol() {
	return reinterpret_cast<Symbol *>(symbols.allocator.allocateUnaligned(sizeof(Symbol)));
}

u32 createSymbolForFunction(ExprFunction *function) {
	if (!(function->flags & EXPR_HAS_STORAGE)) {
		function->flags |= EXPR_HAS_STORAGE;

		function->physicalStorage = static_cast<u32>(symbols.count());
		function->symbol = allocateSymbol();
	}

	return function->physicalStorage;
}

u32 createSymbolForString(ExprStringLiteral *string) {
	if (string->string.length == 0) {
		if (emptyStringSymbolIndex == -1) {
			emptyStringSymbolIndex = symbols.count();

			Symbol emptyString;
			setSymbolName(&emptyString.name, "@emptyString");
			emptyString.value = static_cast<u32>(rdata.totalSize);
			emptyString.sectionNumber = RDATA_SECTION_NUMBER;
			emptyString.type = 0;
			emptyString.storageClass = IMAGE_SYM_CLASS_EXTERNAL;
			emptyString.numberOfAuxSymbols = 0;

			symbols.add(emptyString);

			rdata.add1(0);
		}

		return static_cast<u32>(emptyStringSymbolIndex);
	}

	if (!(string->flags & EXPR_HAS_STORAGE)) {
		string->flags |= EXPR_HAS_STORAGE;

		string->physicalStorage = static_cast<u32>(symbols.count());
		string->symbol = allocateSymbol();

		setSymbolName(&string->symbol->name, symbols.count());
		string->symbol->storageClass = IMAGE_SYM_CLASS_STATIC;
		string->symbol->value = static_cast<u32>(rdata.totalSize);
		string->symbol->sectionNumber = RDATA_SECTION_NUMBER;
		string->symbol->type = 0;
		string->symbol->numberOfAuxSymbols = 0;

		rdata.addNullTerminatedString(string->string);
	}

	return string->physicalStorage;
}


void markUsedTypeInfoInType(Type *type);

void markUsedTypeInfoInExpr(Expr *expr) {
	switch (expr->flavor) {
	case ExprFlavor::ARRAY_LITERAL: {
		auto array = static_cast<ExprArrayLiteral *>(expr);

		for (u64 i = 0; i < array->count; i++) {
			markUsedTypeInfoInExpr(array->values[i] );
		}
		break;
	}
	case ExprFlavor::STRUCT_LITERAL: {
		auto literal = static_cast<ExprStructLiteral *>(expr);

		for (u64 i = 0; i < literal->initializers.count; i++) {
			markUsedTypeInfoInExpr(literal->initializers.values[i]);
		}
		break;
	}
	case ExprFlavor::TYPE_LITERAL: {
		markUsedTypeInfoInType(static_cast<ExprLiteral *>(expr)->typeValue);
		break;
	}
	}
}

void createSymbolForType(Type *type) {
	type->physicalStorage = symbols.count();
	type->symbol = allocateSymbol();
}

void markUsedTypeInfoInType(Type *type) {
	if (type->flags & TYPE_USED_IN_OUTPUT)
		return;

	if (type->flavor == TypeFlavor::MODULE) // @Incomplete Output type info for namespaces
		return;

	type->flags |= TYPE_USED_IN_OUTPUT;

	createSymbolForType(type);

	switch (type->flavor) {
	case TypeFlavor::ARRAY: {
		auto array = static_cast<TypeArray *>(type);

		markUsedTypeInfoInType(array->arrayOf);

		break;
	}
	case TypeFlavor::ENUM: {
		auto enum_ = static_cast<TypeEnum *>(type);

		markUsedTypeInfoInType(enum_->integerType);

		break;
	}
	case TypeFlavor::POINTER: {
		auto pointer = static_cast<TypePointer *>(type);

		markUsedTypeInfoInType(pointer->pointerTo);

		break;
	}
	case TypeFlavor::FUNCTION: {
		auto function = static_cast<TypeFunction *>(type);

		for (u64 i = 0; i < function->argumentCount; i++) {
			markUsedTypeInfoInType(function->argumentTypes[i]);
		}

		for (u64 i = 0; i < function->returnCount; i++) {
			markUsedTypeInfoInType(function->returnTypes[i]);
		}

		break;
	}
	case TypeFlavor::STRUCT: {
		auto struct_ = static_cast<TypeStruct *>(type);

		for (auto member : struct_->members.declarations) {
			if (member->flags & (DECLARATION_IMPORTED_BY_USING)) continue;

			auto memberType = getDeclarationType(member);

			if (memberType == &TYPE_UNSIGNED_INT_LITERAL || memberType == &TYPE_SIGNED_INT_LITERAL || memberType == &TYPE_FLOAT_LITERAL) {
				assert(member->initialValue);
				memberType = getTypeForExpr(member->initialValue);
			}

			markUsedTypeInfoInType(memberType);

			if (member->flags & DECLARATION_IS_UNINITIALIZED) continue;

			markUsedTypeInfoInExpr(member->initialValue);
		}

		break;
	}
	}
}


u32 createSymbolForDeclaration(Declaration *declaration) {
	if (!(declaration->flags & DECLARATION_HAS_STORAGE)) {
		declaration->flags |= DECLARATION_HAS_STORAGE;
		declaration->physicalStorage = symbols.count();
		declaration->symbol = allocateSymbol();
	}

	return static_cast<u32>(declaration->physicalStorage);
}

struct JumpPatch {
	u32 opToPatch;
	s32 *location;
	u64 rip;
};

struct TypePatch {
	Type *type;
	u64 *location;
};

void writeValue(u32 dataSize, u8 *data, BucketedArenaAllocator *dataAllocator, BucketedArenaAllocator *dataRelocations, Expr *value);

void writeArrayLiteral(u32 dataSize, u8 *data, BucketedArenaAllocator *dataAllocator, BucketedArenaAllocator *dataRelocations, ExprArrayLiteral *array) {
	auto arrayType = static_cast<TypeArray *>(array->type);

	auto elementSize = arrayType->arrayOf->size;
	auto arrayCount = arrayType->flags & TYPE_ARRAY_IS_FIXED ? arrayType->count : array->count;

	for (u64 i = 0; i < arrayCount; i++) {
		writeValue(dataSize, data, dataAllocator, dataRelocations, array->values[i]);

		dataSize += elementSize;
		data += elementSize;

		if (i + 1 == array->count && arrayCount > array->count) {
			for (u64 j = i + 1; j < arrayCount; j++) {
				writeValue(dataSize, data, dataAllocator, dataRelocations, array->values[i]);
				dataSize += elementSize;
				data += elementSize;
			}

			break;
		}
	}
}


void writeValue(u32 dataSize, u8 *data, BucketedArenaAllocator *dataAllocator, BucketedArenaAllocator *dataRelocations, Expr *value) {
	assert(dataAllocator == &rdata || dataAllocator == &::data);

	auto type = getTypeForExpr(value);


	assert(value->flavor == ExprFlavor::FLOAT_LITERAL ||
		value->flavor == ExprFlavor::INT_LITERAL ||
		value->flavor == ExprFlavor::FUNCTION ||
		value->flavor == ExprFlavor::STRING_LITERAL ||
		value->flavor == ExprFlavor::ARRAY_LITERAL ||
		value->flavor == ExprFlavor::TYPE_LITERAL || 
		value->flavor == ExprFlavor::STRUCT_LITERAL);

	if (value->flavor == ExprFlavor::FUNCTION) {
		assert(type->size == 8);

		addPointerRelocation(dataRelocations, dataSize, createSymbolForFunction(static_cast<ExprFunction *>(value)));

		*reinterpret_cast<u64 *>(data) = 0;
	}
	else if (value->flavor == ExprFlavor::STRING_LITERAL) {
		u32 string = createSymbolForString(static_cast<ExprStringLiteral *>(value));

		addPointerRelocation(dataRelocations, dataSize, string);

		reinterpret_cast<u64 *>(data)[0] = 0;
		reinterpret_cast<u64 *>(data)[1] = static_cast<ExprStringLiteral *>(value)->string.length;
	}
	else if (value->flavor == ExprFlavor::ARRAY_LITERAL) {
		auto array = static_cast<ExprArrayLiteral *>(value);
		
		if (array->type->flags & TYPE_ARRAY_IS_FIXED) {
			writeArrayLiteral(dataSize, data, dataAllocator, dataRelocations, array);
		}
		else {
			auto arrayType = static_cast<TypeArray *>(array->type);

			u32 newSize = dataAllocator->totalSize;
			
			dataAllocator->allocateUnaligned(AlignPO2(rdata.totalSize, arrayType->arrayOf->alignment) - rdata.totalSize);

			u32 symbolId = symbols.count();
			auto symbol = allocateSymbol();

			setSymbolName(&symbol->name, symbols.count());
			symbol->storageClass = IMAGE_SYM_CLASS_STATIC;
			symbol->value = static_cast<u32>(dataAllocator->totalSize);
			symbol->sectionNumber = dataAllocator == &rdata ? RDATA_SECTION_NUMBER : DATA_SECTION_NUMBER;
			symbol->type = 0;
			symbol->numberOfAuxSymbols = 0;

			u32 offset = dataAllocator->totalSize; // do this in a separate statement because within a c++ statement, subexpressions can execute in any order
										  // which meant that allocateUnaligned happened _before_ rdata.totalSize was evaluated, causing the addresses 
										  // for any patch applied to the literal to be wrong
			writeArrayLiteral(offset, static_cast<u8 *>(dataAllocator->allocateUnaligned(arrayType->arrayOf->size * array->count)), dataAllocator, dataRelocations, array);


			addPointerRelocation(dataRelocations, dataSize, symbolId);

			reinterpret_cast<u64 *>(data)[0] = 0;
			reinterpret_cast<u64 *>(data)[1] = array->count;
		}
	}
	else if (value->flavor == ExprFlavor::FLOAT_LITERAL) {
		if (value->type->size == 4) {
			*reinterpret_cast<float *>(data) = static_cast<float>(static_cast<ExprLiteral *>(value)->floatValue);
		}
		else if (value->type->size == 8 || value->type->size == 0 /* Any values of float literal type should be a double */) {
			*reinterpret_cast<double *>(data) = static_cast<ExprLiteral *>(value)->floatValue;
		}
		else {
			assert(false);
		}
	}
	else if (value->flavor == ExprFlavor::INT_LITERAL) {
		if (type->size != 0 && !isStandardSize(type->size)) {
			assert(static_cast<ExprLiteral *>(value)->unsignedValue == 0);

			memset(data, 0, type->size);
		}
		else {
			memcpy(data, &static_cast<ExprLiteral *>(value)->unsignedValue, type->size == 0 ? 8 : type->size /* Any values of int literal type should be 64 bit */);
		}
	}
	else if (value->flavor == ExprFlavor::TYPE_LITERAL) {
		markUsedTypeInfoInType(static_cast<ExprLiteral *>(value)->typeValue);

		addPointerRelocation(dataRelocations, dataSize, static_cast<ExprLiteral *>(value)->typeValue->physicalStorage);

		*reinterpret_cast<u64 *>(data) = 0;
	}
	else if (value->flavor == ExprFlavor::STRUCT_LITERAL) {
		auto literal = static_cast<ExprStructLiteral *>(value);

		for (u32 i = 0; i < literal->initializers.count; i++) {
			auto offset = literal->initializers.declarations[i]->physicalStorage;
			writeValue(dataSize + offset, data + offset, dataAllocator, dataRelocations, literal->initializers.values[i]);
		}
	}
	else {
		assert(false);
	}
}

void alignAllocator(BucketedArenaAllocator *allocator, u64 alignment) {
	u64 padding[2] = {};

	allocator->add(padding, AlignPO2(allocator->totalSize, alignment) - allocator->totalSize);
}

struct LineInfo {
	u32 offset;
	u32 line;
};

struct ColumnInfo {
	u16 start;
	u16 end;
};


void addLineInfo(Array<LineInfo> *lineInfo, Array<ColumnInfo> *columnInfo, u32 offset, CodeLocation start, EndLocation end) {
	s64 delta = end.line - start.line;

	if (delta < 0) {
		delta = 0;
	}

	auto &line = lineInfo->add();

	line.offset = offset;
	line.line = (start.line & 0xFFF) | ((delta & 0x7F) << 24) | (1 << 31);


	auto &column = columnInfo->add();

	column.start = start.column;
	column.end = end.column + 1;
}


void emitUDT(BucketedArenaAllocator *debugSymbols, Type *type) {
	debugSymbols->ensure(8);
	u16 *size = debugSymbols->add2Unchecked(0);
	u32 start = debugSymbols->totalSize;
	debugSymbols->add2Unchecked(S_UDT);
	auto patch = debugSymbols->add4Unchecked(0);
	coffTypePatches.add({ patch, type });
	appendCoffName(debugSymbols, type);
	debugSymbols->add1(0);
	*size = static_cast<u16>(debugSymbols->totalSize - start);
}

#pragma pack(push, 1)
struct PROCSYM32 {
	u16 reclen;     // Record length
	u16 rectyp;     // S_GPROC32, S_LPROC32, S_GPROC32_ID, S_LPROC32_ID, S_LPROC32_DPC or S_LPROC32_DPC_ID
	u32 pParent;    // pointer to the parent
	u32 pEnd;       // pointer to this blocks end
	u32 pNext;      // pointer to next symbol
	u32 len;        // Proc length
	u32 DbgStart;   // Debug start offset
	u32 DbgEnd;     // Debug end offset
	u32 typind;     // Type index or ID
	u32 off;
	u16 seg;
	u8  flags;      // Proc flags
};

struct DATASYM32 {
	u16 reclen;
	u16 rectyp;
	u32 typind;
	u32 off;
	u16 seg;
};

struct FRAMEPROCSYM {
	u16 reclen = sizeof(FRAMEPROCSYM) - 2;     // Record length
	u16 rectyp = 0x1012;     // S_FRAMEPROC
	u32 cbFrame;    // count of bytes of total frame of procedure
	u32 cbPad = 0;      // count of bytes of padding in the frame
	u32 offPad = 0;     // offset (relative to frame poniter) to where   padding starts
	u32 cbSaveRegs = 0; // count of bytes of callee save registers
	u32 offExHdlr = 0;  // offset of exception handler
	u16  sectExHdlr = 0; // section id of exception handler

	struct {
		unsigned long   unused : 14;   // function uses _alloca()
		unsigned long   encodedLocalBasePointer : 2;  // record function's local pointer explicitly.
		unsigned long   encodedParamBasePointer : 2;  // record function's parameter pointer explicitly.
		unsigned long   pad : 14;   // must be zero
	} flags;
};

struct COMPILESYM3 {
	u16 rectyp = 0x113C;     // Record type
	struct {
		u32 iLanguage : 8;   // language index
		u32 unused : 24;
	} flags;
	u16  machine = 0xD0;    // target processor
	u16  verFEMajor = 0; // front end major version #
	u16  verFEMinor = 1; // front end minor version #
	u16  verFEBuild = 1; // front end build version #
	u16  verFEQFE = 1;   // front end QFE version #
	u16  verMajor = 0;   // back end major version #
	u16  verMinor = 1;   // back end minor version #
	u16  verBuild = 1;   // back end build version #
	u16  verQFE = 1;     // back end QFE version #
};

struct REGREL32 {
	u16 rectyp = 0x1111;     // S_REGREL32
	u32 off;        // offset of symbol
	u32 typind;     // Type index or metadata token
	u16 reg = 335; // RSP
};
#pragma pack(pop)

void emitBasicType(BucketedArenaAllocator *debugSymbols, u32 type, const char *name) {
	debugSymbols->ensure(8);
	debugSymbols->add2Unchecked(static_cast<u16>(7 + strlen(name)));
	debugSymbols->add2Unchecked(S_UDT);
	debugSymbols->add4Unchecked(type);
	debugSymbols->addNullTerminatedString(name);
}

void emitBasicTypeDebugInfo(BucketedArenaAllocator *debugSymbols) {
	debugSymbols->add4(0xF1);
	u32 *subsectionSizePatch = debugSymbols->add4(0);

	u32 previousSize = debugSymbols->totalSize;

	emitBasicType(debugSymbols, T_INT1, "s8");
	emitBasicType(debugSymbols, T_INT2, "s16");
	emitBasicType(debugSymbols, T_INT4, "s32");
	emitBasicType(debugSymbols, T_INT8, "s64");
	emitBasicType(debugSymbols, T_UINT1, "u8");
	emitBasicType(debugSymbols, T_UINT2, "u16");
	emitBasicType(debugSymbols, T_UINT4, "u32");
	emitBasicType(debugSymbols, T_UINT8, "u64");
	emitBasicType(debugSymbols, T_REAL32, "f32");
	emitBasicType(debugSymbols, T_REAL64, "f64");

	*subsectionSizePatch = debugSymbols->totalSize - previousSize;
	alignAllocator(debugSymbols, 4);
}

bool placeValueInBSSSection(Declaration *declaration) {
	return (declaration->flags & DECLARATION_IS_UNINITIALIZED) ||
		((declaration->initialValue->flavor == ExprFlavor::INT_LITERAL || declaration->initialValue->flavor == ExprFlavor::FLOAT_LITERAL)
			&& static_cast<ExprLiteral *>(declaration->initialValue)->unsignedValue == 0);
}

void runCoffWriter() {
	PROFILE_FUNC();
	if (hadError)
		return;

	BucketedArenaAllocator codeRelocations(65536);
	BucketedArenaAllocator dataRelocations(65536);
	BucketedArenaAllocator rdataRelocations(65536);
	BucketedArenaAllocator debugSymbols(65536);
	BucketedArenaAllocator debugSymbolsRelocations(65536);
	BucketedArenaAllocator debugTypes(65536);
	BucketedArenaAllocator pdata(65536);
	BucketedArenaAllocator pdataRelocations(65536);
	BucketedArenaAllocator xdata(65536);

	SectionHeader bssSection = {};
	bssSection.sizeOfRawData = 0;

	s64 f32ToU64ConstantSymbolIndex = -1;
	s64 f64ToU64ConstantSymbolIndex = -1;
	s64 chkstkSymbolIndex = -1;

	u64 alignmentPadding = 0;

	Array<u64> instructionOffsets;
	Array<JumpPatch> jumpPatches;

	Array<LineInfo> lineInfo;
	Array<ColumnInfo> columnInfo;
	Array<u32 *> blockOffsetStack;

	u32 textSectionSymbolIndex = symbols.count();

	Symbol textSectionSymbol;
	setSymbolName(&textSectionSymbol.name, ".text");
	textSectionSymbol.value = 0;
	textSectionSymbol.sectionNumber = TEXT_SECTION_NUMBER;
	textSectionSymbol.type = IMAGE_SYM_TYPE_NULL;
	textSectionSymbol.storageClass = IMAGE_SYM_CLASS_STATIC;
	textSectionSymbol.numberOfAuxSymbols = 0;

	symbols.add(textSectionSymbol);

	debugSymbols.add4(4);
	debugTypes.add4(4);

	{
		debugSymbols.add4(0xF1);
		auto subsectionSizePatch = debugSymbols.add4(0);
		u32 subsectionOffset = debugSymbols.totalSize;

		COMPILESYM3 compileFlags;
		compileFlags.flags.iLanguage = 20; // @Cleanup Check no other language uses this
		compileFlags.flags.unused = 0;

#if BUILD_WINDOWS
		const char *compilerName = "Milo Compiler 0.1.1 (Windows-x64)";
#endif

		debugSymbols.add2(static_cast<u16>(sizeof(compileFlags) + strlen(compilerName) + 1));
		debugSymbols.add(&compileFlags, sizeof(compileFlags));

		debugSymbols.addNullTerminatedString(compilerName);

		*subsectionSizePatch = debugSymbols.totalSize - subsectionOffset;

		alignAllocator(&debugSymbols, 4);
	}

	emitBasicTypeDebugInfo(&debugSymbols);

	while (true) {

		CoffJob job = coffWriterQueue.take();

		if (!job.function)
			break;
		if (job.flavor == CoffJobFlavor::FUNCTION) {
			PROFILE_ZONE("Write Function");
			auto function = job.function;

			createSymbolForFunction(function);

			if (function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
				assert(function->valueOfDeclaration);
				auto symbol = function->symbol;

				if (symbol) {
					setSymbolName(&symbol->name, function->valueOfDeclaration->name);

					symbol->storageClass = IMAGE_SYM_CLASS_EXTERNAL;
					symbol->value = 0;
					symbol->sectionNumber = 0;
					symbol->type = 0x20;


					symbol->numberOfAuxSymbols = 0;
				}
				continue;
			}

			instructionOffsets.clear();

			jumpPatches.clear();

			lineInfo.clear();
			columnInfo.clear();

			u32 functionStart = code.totalSize;


			{
				addLineInfo(&lineInfo, &columnInfo, code.totalSize - functionStart, function->start, function->end);

				auto symbol = function->symbol;

				if (function == programStart) {
					symbol->storageClass = IMAGE_SYM_CLASS_EXTERNAL;
				}
				else {
					symbol->storageClass = IMAGE_SYM_CLASS_STATIC;
				}
				
				if (function->valueOfDeclaration) {
					String name = function->valueOfDeclaration->name;

					setSymbolName(&symbol->name, function->valueOfDeclaration->name);
				}
				else {
					setSymbolName(&symbol->name, symbols.count());
				}

				symbol->value = static_cast<u32>(code.totalSize);
				symbol->sectionNumber = TEXT_SECTION_NUMBER;
				symbol->type = 0x20;


				symbol->numberOfAuxSymbols = 0;
			}

			u32 spaceToAllocate = getSpaceToAllocate(function);

			debugSymbols.ensure(8);
			debugSymbols.add4Unchecked(0xF1);
			auto subsectionSizePatch = debugSymbols.add4Unchecked(0);
			u32 subsectionOffset = debugSymbols.totalSize;

			auto name = function->valueOfDeclaration ? function->valueOfDeclaration->name : "__unnamed";

			debugSymbols.ensure(39);
			debugSymbolsRelocations.ensure(20);
			debugSymbols.add2Unchecked(static_cast<u16>(sizeof(PROCSYM32) + name.length - 1));
			debugSymbols.add2Unchecked(0x1147); // S_GPROC32_ID
			debugSymbols.add4Unchecked(0);
			debugSymbols.add4Unchecked(0);
			debugSymbols.add4Unchecked(0);
			u32 *functionLengthPatch = debugSymbols.add4Unchecked(code.totalSize - functionStart);
			u32 *functionPreambleEndPatch = debugSymbols.add4Unchecked(0);
			u32 *functionPostambleStartPatch = debugSymbols.add4Unchecked(0);
			u32 *patch = debugSymbols.add4Unchecked(0);
			coffFunctionIdTypePatches.add({ patch, function });
			

			debugSymbolsRelocations.add4Unchecked(debugSymbols.totalSize);
			debugSymbolsRelocations.add4Unchecked(function->physicalStorage);
			debugSymbolsRelocations.add2Unchecked(IMAGE_REL_AMD64_SECREL);

			debugSymbols.add4Unchecked(0);

			debugSymbolsRelocations.add4Unchecked(debugSymbols.totalSize);
			debugSymbolsRelocations.add4Unchecked(function->physicalStorage);
			debugSymbolsRelocations.add2Unchecked(IMAGE_REL_AMD64_SECTION);

			debugSymbols.add2Unchecked(0);

			debugSymbols.add1Unchecked(0);
			debugSymbols.addNullTerminatedString(name);

			FRAMEPROCSYM frame;
			frame.cbFrame = spaceToAllocate;
			frame.flags.unused = 0;
			frame.flags.encodedLocalBasePointer = 1; // RSP
			frame.flags.encodedParamBasePointer = 1; // RSP
			frame.flags.pad = 0;

			debugSymbols.add(&frame, sizeof(frame));

			u32 paramOffset = 0;

			code.ensure(256);


			constexpr u8 intRegisters[4] = { 0x4C, 0x54, 0x44, 0x4C };
			if (!isStandardSize(getDeclarationType(function->returns.declarations[0])->size)) {
				code.add1Unchecked(0x48);
				code.add1Unchecked(0x89);
				code.add1Unchecked(intRegisters[paramOffset]);
				code.add1Unchecked(0x24);
				code.add1Unchecked((paramOffset + 1) << 3);
				paramOffset++;
			}
			
			if (!(function->flags & EXPR_FUNCTION_IS_C_CALL)) {
				code.add1Unchecked(0x48);
				code.add1Unchecked(0x89);
				code.add1Unchecked(intRegisters[paramOffset]);
				code.add1Unchecked(0x24);
				code.add1Unchecked((paramOffset + 1) << 3);
				paramOffset++;

				REGREL32 contextInfo;

				contextInfo.off = getRegisterOffset(function, function->state.contextRegister);
				contextInfo.typind = 0;

				debugSymbols.ensure(2 + sizeof(contextInfo));
				debugSymbols.add2Unchecked(static_cast<u16>(sizeof(contextInfo) + 1 + 7));
				REGREL32 *patch = (REGREL32 *) debugSymbols.addUnchecked(&contextInfo, sizeof(contextInfo));
				coffTypePatches.add({ &patch->typind, &TYPE_CONTEXT });

				debugSymbols.addNullTerminatedString("context");
			}
			

			for (u32 i = 0; i < function->arguments.declarations.count; i++) {
				auto argument = function->arguments.declarations[i];
				
				REGREL32 argumentInfo;

				if (isStandardSize(getDeclarationType(argument)->size) && isStoredByPointer(getDeclarationType(argument))) {
					argumentInfo.off = getStackSpaceOffset(function) + argument->physicalStorage;

				}
				else {
					argumentInfo.off = getRegisterOffset(function, argument->registerOfStorage);
				}

				argumentInfo.typind = 0;

				debugSymbols.ensure(2 + sizeof(argumentInfo));
				debugSymbols.add2Unchecked(static_cast<u16>(sizeof(argumentInfo) + 1 + argument->name.length));
				REGREL32 *patch = (REGREL32 *)debugSymbols.addUnchecked(&argumentInfo, sizeof(argumentInfo));
				coffTypePatches.add({ &patch->typind, getDeclarationType(argument) });

				debugSymbols.addNullTerminatedString(argument->name);
			}

			for (u32 i = 0; i < my_min(4 - paramOffset, function->arguments.declarations.count + function->returns.declarations.count - 1); i++) {
				Type *type;

				if (i < function->arguments.declarations.count) {
					type = getDeclarationType(function->arguments.declarations[i]);
				}
				else {
					type = TYPE_VOID_POINTER;
				}

				if (type->flavor == TypeFlavor::FLOAT) {
					if (type->size == 4) {
						code.add1Unchecked(0xF3);
					}
					else if (type->size == 8) {
						code.add1Unchecked(0xF2);
					}

					code.add1Unchecked(0x0F);
					code.add1Unchecked(0x11);
					code.add1Unchecked(0x44 | (static_cast<u8>(i + paramOffset) << 3));
				}
				else {
					if (type->size == 2) {
						code.add1Unchecked(0x66);
					}

					u8 rex = 0x40;

					if (i + paramOffset >= 2) rex |= 0x04;
					if (type->size == 8 || !isStandardSize(type->size)) rex |= 0x08;

					if (rex != 0x40) {
						code.add1Unchecked(rex);
					}

					if (type->size == 1) {
						code.add1Unchecked(0x88);
					}
					else {
						code.add1Unchecked(0x89);
					}

					code.add1Unchecked(intRegisters[i + paramOffset]);
				}

				code.add1Unchecked(0x24);
				code.add1Unchecked((static_cast<u8>(i + paramOffset) + 1) * 8);
			}

			code.add1Unchecked(0x56); // push rsi
			u32 pushRsiOffset = code.totalSize - functionStart;
			code.add1Unchecked(0x57); // push rdi
			u32 pushRdiOffset = code.totalSize - functionStart;

			if (spaceToAllocate >= 4096) {
				loadImmediateIntoRAX(spaceToAllocate);

				// call __chkstk
				code.add1(0xE8);

				if (chkstkSymbolIndex == -1) {
					chkstkSymbolIndex = symbols.count();

					Symbol chkstk;
					setSymbolName(&chkstk.name, "__chkstk");
					chkstk.value = 0;
					chkstk.sectionNumber = 0;
					chkstk.type = 0x20;
					chkstk.storageClass = IMAGE_SYM_CLASS_EXTERNAL;
					chkstk.numberOfAuxSymbols = 0;

					symbols.add(chkstk);
				}

				codeRelocations.ensure(10);
				codeRelocations.add4Unchecked(code.totalSize);
				codeRelocations.add4Unchecked(static_cast<u32>(chkstkSymbolIndex));
				codeRelocations.add2Unchecked(IMAGE_REL_AMD64_REL32);
				code.add4(0);

				// sub rsp, rax
				code.add1(0x48);
				code.add1(0x29);
				code.add1(0xC4);
			}
			else if (spaceToAllocate < 0x80) {
				// sub rsp, spaceToAllocate
				code.add1Unchecked(0x48);
				code.add1Unchecked(0x83);
				code.add1Unchecked(0xEC);
				code.add1Unchecked(static_cast<u8>(spaceToAllocate));
			}
			else {
				// sub rsp, spaceToAllocate
				code.add1Unchecked(0x48);
				code.add1Unchecked(0x81);
				code.add1Unchecked(0xEC);
				code.add4Unchecked(static_cast<u32>(spaceToAllocate));
			}
			u32 subRspOffset = code.totalSize - functionStart;

			u32 functionPreambleEnd = code.totalSize - functionStart;
			*functionPreambleEndPatch = functionPreambleEnd;

			for (u32 index = 0; index < function->state.ir.count; index++) {
				auto &ir = function->state.ir[index];

				instructionOffsets.add(code.totalSize);

				code.ensure(128);

				switch (ir.op) {
				case IrOp::TYPE: {
					auto type = static_cast<Type *>(ir.data);
					markUsedTypeInfoInType(type);

					code.add1Unchecked(0x48);
					code.add1Unchecked(0xB8);

					addPointerRelocation(&codeRelocations, code.totalSize, type->physicalStorage);

					code.add8Unchecked(0);

					storeFromIntRegister(function, 8, ir.dest, RAX);


					break;
				}
				case IrOp::ADD: {
					if (ir.flags & IR_FLOAT_OP) {
						loadIntoFloatRegister(function, ir.opSize, 0, ir.a);

						if (ir.opSize == 8) {
							code.add1Unchecked(0xF2);
						}
						else {
							code.add1Unchecked(0xF3);
						}

						code.add1Unchecked(0x0F);
						code.add1Unchecked(0x58);
						writeRSPRegisterByte(function, 0, ir.b);

						storeFromFloatRegister(function, ir.opSize, ir.dest, 0);
					}
					else {
						loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

						if (ir.opSize == 8) {
							code.add1Unchecked(0x48);
						}
						else if (ir.opSize == 2) {
							code.add1Unchecked(0x66);
						}

						if (ir.opSize == 1) {
							code.add1Unchecked(0x02);
						}
						else {
							code.add1Unchecked(0x03);
						}

						writeRSPRegisterByte(function, RAX, ir.b);

						storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
					}
				} break;
				case IrOp::ADD_CONSTANT: {
					if (ir.immediate == 0) {
						writeSet(function, ir.opSize, ir.dest, ir.a);
					}
					else {
						loadImmediateIntoRAX(ir.immediate);

						if (ir.opSize == 8) {
							code.add1Unchecked(0x48);
						}
						else if (ir.opSize == 2) {
							code.add1Unchecked(0x48);
						}

						if (ir.opSize == 1) {
							code.add1Unchecked(0x02);
						}
						else {
							code.add1Unchecked(0x03);
						}

						writeRSPRegisterByte(function, RAX, ir.a);

						storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
					}
				} break;
				case IrOp::SUB: {
					if (ir.flags & IR_FLOAT_OP) {
						loadIntoFloatRegister(function, ir.opSize, 0, ir.a);

						if (ir.opSize == 8) {
							code.add1Unchecked(0xF2);
						}
						else {
							code.add1Unchecked(0xF3);
						}

						code.add1Unchecked(0x0F);
						code.add1Unchecked(0x5C);

						writeRSPRegisterByte(function, 0, ir.b);

						storeFromFloatRegister(function, ir.opSize, ir.dest, 0);
					}
					else {
						loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

						if (ir.opSize == 8) {
							code.add1Unchecked(0x48);
						}
						else if (ir.opSize == 2) {
							code.add1Unchecked(0x66);
						}

						if (ir.opSize == 1) {
							code.add1Unchecked(0x2A);
						}
						else {
							code.add1Unchecked(0x2B);
						}

						writeRSPRegisterByte(function, RAX, ir.b);

						storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
					}
				} break;
				case IrOp::MUL: {
					if (ir.flags & IR_FLOAT_OP) {
						loadIntoFloatRegister(function, ir.opSize, 0, ir.a);

						if (ir.opSize == 8) {
							code.add1Unchecked(0xF2);
						}
						else {
							code.add1Unchecked(0xF3);
						}

						code.add1Unchecked(0x0F);
						code.add1Unchecked(0x59);

						writeRSPRegisterByte(function, 0, ir.b);

						storeFromFloatRegister(function, ir.opSize, ir.dest, 0);
					}
					else {
						if (ir.opSize == 1) {
							loadIntoIntRegister(function, 1, RAX, ir.a);

							code.add1Unchecked(0xF6);
							writeRSPRegisterByte(function, 5, ir.b);

							storeFromIntRegister(function, 1, ir.dest, RAX);
						}
						else {
							loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

							if (ir.opSize == 8) {
								code.add1Unchecked(0x48);
							}
							else if (ir.opSize == 2) {
								code.add1Unchecked(0x66);
							}

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0xAF);
							writeRSPRegisterByte(function, RAX, ir.b);

							storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
						}
					}
				} break;
				case IrOp::MUL_BY_CONSTANT: {
					assert(ir.opSize == 8);

					if (ir.immediate == 0) {
						storeImmediate(function, ir.opSize, ir.dest, 0);
					}
					else {
						loadImmediateIntoRAX(ir.immediate);

						code.add1Unchecked(0x48);
						code.add1Unchecked(0x0F);
						code.add1Unchecked(0xAF);
						writeRSPRegisterByte(function, RAX, ir.a);

						storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
					}
				} break;
				case IrOp::DIV: {
					if (ir.flags & IR_FLOAT_OP) {
						loadIntoFloatRegister(function, ir.opSize, 0, ir.a);
						loadIntoFloatRegister(function, ir.opSize, 1, ir.b);

						if (ir.opSize == 8) {
							code.add1Unchecked(0xF2);
						}
						else {
							code.add1Unchecked(0xF3);
						}
						code.add1Unchecked(0x0F);
						code.add1Unchecked(0x5E);
						code.add1Unchecked(0xC1);

						storeFromFloatRegister(function, ir.opSize, ir.dest, 0);
					}
					else {
						loadIntoIntRegister(function, ir.opSize, RAX, ir.a);
						loadIntoIntRegister(function, ir.opSize, RCX, ir.b);

						if (ir.flags & IR_SIGNED_OP) {
							if (ir.opSize == 1) {
								code.add1Unchecked(0x66); // cbw
								code.add1Unchecked(0x98);

								code.add1Unchecked(0xF6); // idiv cl
								code.add1Unchecked(0xF9);

							}
							else {
								if (ir.opSize == 2) {
									code.add1Unchecked(0x66);
								}
								else if (ir.opSize == 8) {
									code.add1Unchecked(0x48);
								}

								code.add1Unchecked(0x99);

								if (ir.opSize == 2) {
									code.add1Unchecked(0x66);
								}
								else if (ir.opSize == 8) {
									code.add1Unchecked(0x48);
								}
								code.add1Unchecked(0xF7);
								code.add1Unchecked(0xF9);
							}
						}
						else {
							if (ir.opSize == 1) {
								code.add1Unchecked(0x66); // movzx ax, al
								code.add1Unchecked(0x0F);
								code.add1Unchecked(0xB6);
								code.add1Unchecked(0xC0);

								code.add1Unchecked(0xF6); // div cl
								code.add1Unchecked(0xF1);

							}
							else {
								if (ir.opSize == 2) {
									code.add1Unchecked(0x66);
								}
								else if (ir.opSize == 8) {
									code.add1Unchecked(0x48);
								}

								code.add1Unchecked(0x31);
								code.add1Unchecked(0xD2);

								if (ir.opSize == 2) {
									code.add1Unchecked(0x66);
								}
								else if (ir.opSize == 8) {
									code.add1Unchecked(0x48);
								}
								code.add1Unchecked(0xF7);
								code.add1Unchecked(0xF1);
							}
						}

						storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
					}
				} break;
				case IrOp::DIVIDE_BY_CONSTANT: {
					loadImmediateIntoRAX(ir.immediate);

					code.add1Unchecked(0x48); // mov rcx, rax
					code.add1Unchecked(0x89);
					code.add1Unchecked(0xC1);

					loadIntoIntRegister(function, 8, RAX, ir.a);

					code.add1Unchecked(0x48); // cqo
					code.add1Unchecked(0x99);

					code.add1Unchecked(0x48); // div rcx
					code.add1Unchecked(0xF7);
					code.add1Unchecked(0xF1);

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::MOD: {
					if (ir.flags & IR_FLOAT_OP) {
						// @Incomplete: x64 doesn't have native fmod, call out to fmod in crt or implement our own
						assert(false);
					}
					else {
						loadIntoIntRegister(function, ir.opSize, RAX, ir.a);
						loadIntoIntRegister(function, ir.opSize, RCX, ir.b);

						if (ir.flags & IR_SIGNED_OP) {
							if (ir.opSize == 1) {
								code.add1Unchecked(0x66); // cbw
								code.add1Unchecked(0x98);

								code.add1Unchecked(0xF6); // idiv cl
								code.add1Unchecked(0xF9);

							}
							else {
								if (ir.opSize == 2) {
									code.add1Unchecked(0x66);
								}
								else if (ir.opSize == 8) {
									code.add1Unchecked(0x48);
								}

								code.add1Unchecked(0x99);

								if (ir.opSize == 2) {
									code.add1Unchecked(0x66);
								}
								else if (ir.opSize == 8) {
									code.add1Unchecked(0x48);
								}
								code.add1Unchecked(0xF7);
								code.add1Unchecked(0xF9);
							}
						}
						else {
							if (ir.opSize == 1) {
								code.add1Unchecked(0x66); // movzx ax, al
								code.add1Unchecked(0x0F);
								code.add1Unchecked(0xB6);
								code.add1Unchecked(0xC0);

								code.add1Unchecked(0xF6); // div cl
								code.add1Unchecked(0xF1);

							}
							else {
								if (ir.opSize == 2) {
									code.add1Unchecked(0x66);
								}
								else if (ir.opSize == 8) {
									code.add1Unchecked(0x48);
								}

								code.add1Unchecked(0x31);
								code.add1Unchecked(0xD2);

								if (ir.opSize == 2) {
									code.add1Unchecked(0x66);
								}
								else if (ir.opSize == 8) {
									code.add1Unchecked(0x48);
								}
								code.add1Unchecked(0xF7);
								code.add1Unchecked(0xF1);
							}
						}

						storeFromIntRegister(function, ir.opSize, ir.dest, ir.opSize == 1 ? AH : RDX);
					}
				} break;
				case IrOp::AND: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

					if (ir.opSize == 8) {
						code.add1Unchecked(0x48);
					}
					else if (ir.opSize == 2) {
						code.add1Unchecked(0x66);
					}

					if (ir.opSize == 1) {
						code.add1Unchecked(0x22);
					}
					else {
						code.add1Unchecked(0x23);
					}
					writeRSPRegisterByte(function, RAX, ir.b);

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::OR: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

					if (ir.opSize == 8) {
						code.add1Unchecked(0x48);
					}
					else if (ir.opSize == 2) {
						code.add1Unchecked(0x66);
					}

					if (ir.opSize == 1) {
						code.add1Unchecked(0x0A);
					}
					else {
						code.add1Unchecked(0x0B);
					}
					writeRSPRegisterByte(function, RAX, ir.b);

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::XOR: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

					if (ir.opSize == 8) {
						code.add1Unchecked(0x48);
					}
					else if (ir.opSize == 2) {
						code.add1Unchecked(0x66);
					}

					if (ir.opSize == 1) {
						code.add1Unchecked(0x32);
					}
					else {
						code.add1Unchecked(0x33);
					}
					writeRSPRegisterByte(function, RAX, ir.b);

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::NOT: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);
					if (ir.opSize == 8) {
						code.add1Unchecked(0x48);
					}
					else if (ir.opSize == 2) {
						code.add1Unchecked(0x66);
					}

					if (ir.opSize == 1) {
						code.add1Unchecked(0xF6);
					}
					else {
						code.add1Unchecked(0xF7);
					}

					code.add1Unchecked(0xD0);

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::POP_COUNT: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

					// There is no 8 bit version of popcnt
					if (ir.opSize == 1) {
						code.add1Unchecked(0x0f); // movzx eax, al
						code.add1Unchecked(0xb6);
						code.add1Unchecked(0xc0);
					}

					if (ir.opSize == 8) {
						code.add1Unchecked(0xf3);
						code.add1Unchecked(0x48);
					}
					else if (ir.opSize == 2) {
						code.add1Unchecked(0x66);
						code.add1Unchecked(0xf3);
					}
					else {
						code.add1Unchecked(0xf3);
					}

					code.add1Unchecked(0x0F);
					code.add1Unchecked(0xB8);
					code.add1Unchecked(0xC0);

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::BIT_SCAN_FORWARD: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

					// There is no 8 bit version of bsf
					if (ir.opSize == 1) {
						code.add1Unchecked(0x0f); // movzx eax, al
						code.add1Unchecked(0xb6);
						code.add1Unchecked(0xc0);
					}

					if (ir.opSize == 8) {
						code.add1Unchecked(0x48);
					}
					else if (ir.opSize == 2) {
						code.add1Unchecked(0x66);
					}

					code.add1Unchecked(0x0F);
					code.add1Unchecked(0xBC);
					code.add1Unchecked(0xC0);
					
					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);

					setCondition(function, ir.b, C_Z);
				} break;
				case IrOp::SHIFT_LEFT: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);
					loadIntoIntRegister(function, ir.opSize, RCX, ir.b);

					if (ir.opSize == 8) {
						code.add1Unchecked(0x48);
					}
					else if (ir.opSize == 2) {
						code.add1Unchecked(0x66);
					}

					if (ir.opSize == 1) {
						code.add1Unchecked(0xD2);
					}
					else {
						code.add1Unchecked(0xD3);
					}

					code.add1Unchecked(0xE0);

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::BIT_SCAN_REVERSE: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

					// There is no 8 bit version of bsr
					if (ir.opSize == 1) {
						code.add1Unchecked(0x0f); // movzx eax, al
						code.add1Unchecked(0xb6);
						code.add1Unchecked(0xc0);
					}

					if (ir.opSize == 8) {
						code.add1Unchecked(0x48);
					}
					else if (ir.opSize == 2) {
						code.add1Unchecked(0x66);
					}

					code.add1Unchecked(0x0F);
					code.add1Unchecked(0xBD);
					code.add1Unchecked(0xC0);

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);

					setCondition(function, ir.b, C_Z);
				} break;
				case IrOp::SHIFT_RIGHT: {

					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);
					loadIntoIntRegister(function, ir.opSize, RCX, ir.b);

					if (ir.opSize == 8) {
						code.add1Unchecked(0x48);
					}
					else if (ir.opSize == 2) {
						code.add1Unchecked(0x66);
					}

					if (ir.opSize == 1) {
						code.add1Unchecked(0xD2);
					}
					else {
						code.add1Unchecked(0xD3);
					}

					if (ir.flags & IR_SIGNED_OP) {
						code.add1Unchecked(0xF8);
					}
					else {
						code.add1Unchecked(0xE8);
					}

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::READ: {
					assert(isStandardSize(ir.opSize));
					loadIntoIntRegister(function, 8, RAX, ir.a);

					if (ir.opSize == 8) {
						code.add1Unchecked(0x48);
					}
					else if (ir.opSize == 2) {
						code.add1Unchecked(0x66);
					}

					if (ir.opSize == 1) {
						code.add1Unchecked(0x8A);
					}
					else {
						code.add1Unchecked(0x8B);
					}

					if (!ir.immediate) {
						code.add1Unchecked(0x00);
					}
					else if (ir.immediate <= 0x7f) {
						code.add1Unchecked(0x40);
						code.add1Unchecked(ir.immediate);
					}
					else {
						code.add1Unchecked(0x80);
						code.add4Unchecked(ir.immediate);
					}

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::WRITE: {
					assert(isStandardSize(ir.opSize));

					loadIntoIntRegister(function, 8, RAX, ir.dest);
					loadIntoIntRegister(function, ir.opSize, RCX, ir.a);

					if (ir.opSize == 2) {
						code.add1Unchecked(0x66);
					}
					else if (ir.opSize == 8) {
						code.add1Unchecked(0x48);
					}

					if (ir.opSize == 1) {
						code.add1Unchecked(0x88);
					}
					else {
						code.add1Unchecked(0x89);
					}

					if (!ir.immediate) {
						code.add1Unchecked(0x08);
					}
					else if (ir.immediate <= 0x7f) {
						code.add1Unchecked(0x48);
						code.add1Unchecked(ir.immediate);
					}
					else {
						code.add1Unchecked(0x88);
						code.add4Unchecked(ir.immediate);
					}
				} break;
				case IrOp::COPY: {					
					if (isStandardSize(ir.opSize)) {
						loadIntoIntRegister(function, 8, RAX, ir.a);
						loadIntoIntRegister(function, 8, RCX, ir.dest);

						if (ir.immediate) {
							code.add1Unchecked(0x48);
							code.add1Unchecked(0x81);
							code.add1Unchecked(0xC1);
							code.add4Unchecked(ir.immediate);
						}

						if (ir.opSize == 8) {
							code.add1Unchecked(0x48);
						}
						else if (ir.opSize == 2) {
							code.add1Unchecked(0x66);
						}

						if (ir.opSize == 1) {
							code.add1Unchecked(0x8A);
						}
						else {
							code.add1Unchecked(0x8B);
						}

						code.add1Unchecked(0x00);

						if (ir.opSize == 8) {
							code.add1Unchecked(0x48);
						}
						else if (ir.opSize == 2) {
							code.add1Unchecked(0x66);
						}

						if (ir.opSize == 1) {
							code.add1Unchecked(0x88);
						}
						else {
							code.add1Unchecked(0x89);
						}

						code.add1Unchecked(0x01);
					}
					else {
						loadIntoIntRegister(function, 8, RDI, ir.dest);
						loadIntoIntRegister(function, 8, RSI, ir.a);

						if (ir.immediate) {
							code.add1Unchecked(0x48);
							code.add1Unchecked(0x81);
							code.add1Unchecked(0xC7);
							code.add4Unchecked(ir.immediate);
						}

						loadImmediateIntoIntRegister(RCX, ir.opSize);

						code.add1Unchecked(0xF3);
						code.add1Unchecked(0xA4);
					}
				} break;
				case IrOp::ZERO_MEMORY: {
					assert(!ir.immediate);

					code.add1Unchecked(0x31);
					code.add1Unchecked(0xC0);

					if (isStandardSize(ir.opSize)) {
						loadIntoIntRegister(function, 8, RCX, ir.dest);

						if (ir.opSize == 8) {
							code.add1Unchecked(0x48);
						}
						else if (ir.opSize == 2) {
							code.add1Unchecked(0x66);
						}

						if (ir.opSize == 1) {
							code.add1Unchecked(0x88);
						}
						else {
							code.add1Unchecked(0x89);
						}

						code.add1Unchecked(0x01);
					}
					else {
						loadIntoIntRegister(function, 8, RDI, ir.dest);

						loadImmediateIntoIntRegister(RCX, ir.opSize);

						code.add1Unchecked(0xF3);
						code.add1Unchecked(0xAA);
					}
				} break;
				case IrOp::SET: {
					writeSet(function, ir.opSize, ir.dest, ir.a);
				} break;
				case IrOp::TYPE_INFO: {
					writeSet(function, 8, ir.dest, ir.a);
				} break;
				case IrOp::FLOAT_CAST: {
					if (ir.opSize == 8) {
						assert(ir.b == 4);

						code.add1Unchecked(0xF2);
					}
					else {
						assert(ir.opSize == 4);
						assert(ir.b == 8);

						code.add1Unchecked(0xF3);
					}

					code.add1Unchecked(0x0F);
					code.add1Unchecked(0x5A);

					writeRSPRegisterByte(function, 0, ir.a);
					storeFromFloatRegister(function, ir.b, ir.dest, 0);
				} break;
				case IrOp::EXTEND_INT: {
					if (ir.flags & IR_SIGNED_OP) {
						if (ir.opSize == 1) {
							if (ir.b == 2) {
								code.add1Unchecked(0x66);
							}
							else if (ir.b == 8) {
								code.add1Unchecked(0x48);
							}

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0xBE);
							writeRSPRegisterByte(function, RAX, ir.a);
						}
						else if (ir.opSize == 2) {
							if (ir.b == 8) {
								code.add1Unchecked(0x48);
							}

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0xBF);
							writeRSPRegisterByte(function, RAX, ir.a);
						}
						else if (ir.opSize == 4) {
							code.add1Unchecked(0x48);
							code.add1Unchecked(0x63);
							writeRSPRegisterByte(function, RAX, ir.a);
						}
					}
					else {
						if (ir.opSize == 1) {
							if (ir.b == 2) {
								code.add1Unchecked(0x66);
							}

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0xB6);
							writeRSPRegisterByte(function, RAX, ir.a);


						}
						else if (ir.opSize == 2) {
							code.add1Unchecked(0x0F);
							code.add1Unchecked(0xB7);
							writeRSPRegisterByte(function, RAX, ir.a);
						}
						else if (ir.opSize == 4) {
							loadIntoIntRegister(function, ir.opSize, RAX, ir.a);
						}
					}

					storeFromIntRegister(function, ir.b, ir.dest, RAX);
				} break;
				case IrOp::GOTO: {
					code.add1Unchecked(0xE9);

					JumpPatch patch;
					patch.opToPatch = ir.b;
					patch.location = reinterpret_cast<s32 *>(code.add4Unchecked(0));
					patch.rip = code.totalSize;

					jumpPatches.add(patch);
				} break;
				case IrOp::IF_Z_GOTO: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

					if (ir.opSize == 8) {
						code.add1Unchecked(0x48);
					}
					else if (ir.opSize == 2) {
						code.add1Unchecked(0x66);
					}

					if (ir.opSize == 1) {
						code.add1Unchecked(0x84);
					}
					else {
						code.add1Unchecked(0x85);
					}
					code.add1Unchecked(0xC0);

					code.add1Unchecked(0x0F);
					code.add1Unchecked(0x80 | C_Z);

					JumpPatch patch;
					patch.opToPatch = ir.b;
					patch.location = reinterpret_cast<s32 *>(code.add4Unchecked(0));
					patch.rip = code.totalSize;

					jumpPatches.add(patch);
				} break;
				case IrOp::IF_NZ_GOTO: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

					if (ir.opSize == 8) {
						code.add1Unchecked(0x48);
					}
					else if (ir.opSize == 2) {
						code.add1Unchecked(0x66);
					}

					if (ir.opSize == 1) {
						code.add1Unchecked(0x84);
					}
					else {
						code.add1Unchecked(0x85);
					}
					code.add1Unchecked(0xC0);

					code.add1Unchecked(0x0F);
					code.add1Unchecked(0x80 | C_NZ);

					JumpPatch patch;
					patch.opToPatch = ir.b;
					patch.location = reinterpret_cast<s32 *>(code.add4Unchecked(0));
					patch.rip = code.totalSize;

					jumpPatches.add(patch);
				} break;
				case IrOp::LESS: {
					if (ir.flags & IR_FLOAT_OP) {
						setConditionFloat(function, ir.opSize, ir.dest, ir.a, ir.b, C_B);
					}
					else {
						if (ir.flags & IR_SIGNED_OP) {
							setConditionInt(function, ir.opSize, ir.dest, ir.a, ir.b, C_L);
						}
						else {
							setConditionInt(function, ir.opSize, ir.dest, ir.a, ir.b, C_B);
						}
					}
				} break;
				case IrOp::GREATER: {
					if (ir.flags & IR_FLOAT_OP) {
						setConditionFloat(function, ir.opSize, ir.dest, ir.a, ir.b, C_A);
					}
					else {
						if (ir.flags & IR_SIGNED_OP) {
							setConditionInt(function, ir.opSize, ir.dest, ir.a, ir.b, C_G);
						}
						else {
							setConditionInt(function, ir.opSize, ir.dest, ir.a, ir.b, C_A);
						}
					}
				} break;
				case IrOp::LESS_EQUAL: {
					if (ir.flags & IR_FLOAT_OP) {
						setConditionFloat(function, ir.opSize, ir.dest, ir.a, ir.b, C_BE);
					}
					else {
						if (ir.flags & IR_SIGNED_OP) {
							setConditionInt(function, ir.opSize, ir.dest, ir.a, ir.b, C_LE);
						}
						else {
							setConditionInt(function, ir.opSize, ir.dest, ir.a, ir.b, C_BE);
						}
					}
				} break;
				case IrOp::GREATER_EQUAL: {
					if (ir.flags & IR_FLOAT_OP) {
						setConditionFloat(function, ir.opSize, ir.dest, ir.a, ir.b, C_AE);
					}
					else {
						if (ir.flags & IR_SIGNED_OP) {
							setConditionInt(function, ir.opSize, ir.dest, ir.a, ir.b, C_GE);
						}
						else {
							setConditionInt(function, ir.opSize, ir.dest, ir.a, ir.b, C_AE);
						}
					}
				} break;
				case IrOp::NOT_EQUAL: {
					if (ir.flags & IR_FLOAT_OP) {
						setConditionFloat(function, ir.opSize, ir.dest, ir.a, ir.b, C_NE);
					}
					else {
						setConditionInt(function, ir.opSize, ir.dest, ir.a, ir.b, C_NE);
					}
				} break;
				case IrOp::EQUAL: {
					if (ir.flags & IR_FLOAT_OP) {
						setConditionFloat(function, ir.opSize, ir.dest, ir.a, ir.b, C_E);
					}
					else {
						setConditionInt(function, ir.opSize, ir.dest, ir.a, ir.b, C_E);
					}
				} break;
				case IrOp::ADDRESS_OF_GLOBAL: {
					auto declaration = static_cast<Declaration *>(ir.data);

					assert(declaration->enclosingScope->flavor == BlockFlavor::GLOBAL);
					assert(!(declaration->flags & DECLARATION_IS_CONSTANT));

					code.add1Unchecked(0x48);
					code.add1Unchecked(0x8D);
					code.add1Unchecked(0x05);

					codeRelocations.ensure(10);
					codeRelocations.add4Unchecked(static_cast<u32>(code.totalSize));
					codeRelocations.add4Unchecked(createSymbolForDeclaration(declaration));
					codeRelocations.add2Unchecked(IMAGE_REL_AMD64_REL32);

					code.add4Unchecked(ir.a);

					storeFromIntRegister(function, 8, ir.dest, RAX);
				} break;
				case IrOp::STACK_ADDRESS: {
					code.add1Unchecked(0x48);
					code.add1Unchecked(0x8D);
					writeRSPOffsetByte(RAX, getStackSpaceOffset(function) + ir.immediate);

					storeFromIntRegister(function, 8, ir.dest, RAX);
				} break;
				case IrOp::IMMEDIATE: {
					storeImmediate(function, ir.opSize, ir.dest, ir.immediate);
				} break;
				case IrOp::FLOAT_TO_INT: {
					if (ir.flags & IR_SIGNED_OP) {
						if (ir.opSize == 8) {
							code.add1Unchecked(0xF2);
						}
						else {
							code.add1Unchecked(0xF3);
						}

						if (ir.b == 8) {
							code.add1Unchecked(0x48);
						}

						code.add1Unchecked(0x0F);
						code.add1Unchecked(0x2C);
						writeRSPRegisterByte(function, RAX, ir.a);
						storeFromIntRegister(function, ir.b, ir.dest, RAX);
					}
					else {
						if (ir.b == 8) { // Aww sheet
							loadIntoFloatRegister(function, ir.opSize, 0, ir.a);

							if (ir.opSize == 8) {
								if (f64ToU64ConstantSymbolIndex == -1) {
									f64ToU64ConstantSymbolIndex = symbols.count();

									rdata.allocateUnaligned(AlignPO2(rdata.totalSize, 8) - rdata.totalSize);

									Symbol f64ToU64Constant;
									setSymbolName(&f64ToU64Constant.name, "@f64ToU64Constant");
									f64ToU64Constant.value = static_cast<u32>(rdata.totalSize);
									f64ToU64Constant.sectionNumber = RDATA_SECTION_NUMBER;
									f64ToU64Constant.type = 0;
									f64ToU64Constant.storageClass = IMAGE_SYM_CLASS_STATIC;
									f64ToU64Constant.numberOfAuxSymbols = 0;

									symbols.add(f64ToU64Constant);

									rdata.add8(0x43E0000000000000);
								}

								code.add1Unchecked(0xF2); // movsd xmm1, f32ToU64Constant
								code.add1Unchecked(0x0F);
								code.add1Unchecked(0x10);
								code.add1Unchecked(0x0D);

								codeRelocations.ensure(10);
								codeRelocations.add4Unchecked(static_cast<u32>(code.totalSize));
								codeRelocations.add4Unchecked(static_cast<u32>(f64ToU64ConstantSymbolIndex));
								codeRelocations.add2Unchecked(IMAGE_REL_AMD64_REL32);

								code.add4Unchecked(0);

								code.add1Unchecked(0x31); // xor eax, eax
								code.add1Unchecked(0xC0);

								code.add1Unchecked(0x66); // comisd xmm0, xmm1
								code.add1Unchecked(0x0F);
								code.add1Unchecked(0x2F);
								code.add1Unchecked(0xC1);

								code.add1Unchecked(0x70 | C_B); // jb .cvt
								u8 *firstJumpPatch = code.add1Unchecked(0);
								u64 firstJumpRel = code.totalSize;

								code.add1Unchecked(0xF2); // subsd xmm0, xmm1
								code.add1Unchecked(0x0F);
								code.add1Unchecked(0x5C);
								code.add1Unchecked(0xC1);

								code.add1Unchecked(0x66); // comisd xmm0, xmm1
								code.add1Unchecked(0x0F);
								code.add1Unchecked(0x2F);
								code.add1Unchecked(0xC1);

								code.add1Unchecked(0x70 | C_AE); // jae .cvt
								u8 *secondJumpPatch = code.add1Unchecked(0);
								u64 secondJumpRel = code.totalSize;

								code.add1Unchecked(0x48); // mov rax, 0x8000'0000'0000'0000
								code.add1Unchecked(0xB8);
								code.add8Unchecked(0x8000'0000'0000'0000);

								*firstJumpPatch = static_cast<u8>(code.totalSize - firstJumpRel);
								*secondJumpPatch = static_cast<u8>(code.totalSize - secondJumpRel);

								// .cvt
								code.add1Unchecked(0xF2); // cvttsd2si rcx, xmm0
								code.add1Unchecked(0x48);
								code.add1Unchecked(0x0F);
								code.add1Unchecked(0x2C);
								code.add1Unchecked(0xC8);

								code.add1Unchecked(0x48); // add rax, rcx
								code.add1Unchecked(0x01);
								code.add1Unchecked(0xC8);
							}
							else {
								if (f32ToU64ConstantSymbolIndex == -1) {
									f32ToU64ConstantSymbolIndex = symbols.count();

									rdata.allocateUnaligned(AlignPO2(rdata.totalSize, 4) - rdata.totalSize);

									Symbol f32ToU64Constant;
									setSymbolName(&f32ToU64Constant.name, "@f32ToU64Constant");
									f32ToU64Constant.value = static_cast<u32>(rdata.totalSize);
									f32ToU64Constant.sectionNumber = RDATA_SECTION_NUMBER;
									f32ToU64Constant.type = 0;
									f32ToU64Constant.storageClass = IMAGE_SYM_CLASS_STATIC;
									f32ToU64Constant.numberOfAuxSymbols = 0;

									symbols.add(f32ToU64Constant);

									rdata.add4(0x5F000000);
								}

								code.add1Unchecked(0xF3); // movss xmm1, f32ToU64Constant
								code.add1Unchecked(0x0F);
								code.add1Unchecked(0x10);
								code.add1Unchecked(0x0D);

								codeRelocations.ensure(10);
								codeRelocations.add4Unchecked(code.totalSize);
								codeRelocations.add4Unchecked(static_cast<u32>(f32ToU64ConstantSymbolIndex));
								codeRelocations.add2Unchecked(IMAGE_REL_AMD64_REL32);

								code.add4Unchecked(0);

								code.add1Unchecked(0x31); // xor eax, eax
								code.add1Unchecked(0xC0);

								code.add1Unchecked(0x0F); // comiss xmm0, xmm1
								code.add1Unchecked(0x2F);
								code.add1Unchecked(0xC1);

								code.add1Unchecked(0x70 | C_B); // jb .cvt
								u8 *firstJumpPatch = code.add1Unchecked(0);
								u64 firstJumpRel = code.totalSize;

								code.add1Unchecked(0xF3); // subss xmm0, xmm1
								code.add1Unchecked(0x0F);
								code.add1Unchecked(0x5C);
								code.add1Unchecked(0xC1);

								code.add1Unchecked(0x0F); // comiss xmm0, xmm1
								code.add1Unchecked(0x2F);
								code.add1Unchecked(0xC1);

								code.add1Unchecked(0x70 | C_AE); // jae .cvt
								u8 *secondJumpPatch = code.add1Unchecked(0);
								u64 secondJumpRel = code.totalSize;

								code.add1Unchecked(0x48); // mov rax, 0x8000'0000'0000'0000
								code.add1Unchecked(0xB8);
								code.add8Unchecked(0x8000'0000'0000'0000);

								*firstJumpPatch = static_cast<u8>(code.totalSize - firstJumpRel);
								*secondJumpPatch = static_cast<u8>(code.totalSize - secondJumpRel);

								// .cvt
								code.add1Unchecked(0xF3); // cvttss2si rcx, xmm0
								code.add1Unchecked(0x48);
								code.add1Unchecked(0x0F);
								code.add1Unchecked(0x2C);
								code.add1Unchecked(0xC8);

								code.add1Unchecked(0x48); // add rax, rcx
								code.add1Unchecked(0x01);
								code.add1Unchecked(0xC8);
							}

							storeFromIntRegister(function, ir.b, ir.dest, RAX);
						}

						else {
							if (ir.opSize == 8) {
								code.add1Unchecked(0xF2);
							}
							else {
								code.add1Unchecked(0xF3);
							}

							if (ir.b == 4) {
								code.add1Unchecked(0x48);
							}

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0x2C);


							writeRSPRegisterByte(function, RAX, ir.a);
							storeFromIntRegister(function, ir.b, ir.dest, RAX);
						}
					}
				} break;
				case IrOp::INT_TO_FLOAT: {
					if (ir.flags & IR_SIGNED_OP) {
						if (ir.opSize >= 4) {
							if (ir.b == 8) {
								code.add1Unchecked(0xF2);
							}
							else {
								code.add1Unchecked(0xF3);
							}

							if (ir.opSize == 8) {
								code.add1Unchecked(0x48);
							}

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0x2A);
							writeRSPRegisterByte(function, 0, ir.a);
							storeFromFloatRegister(function, ir.b, ir.dest, 0);
						}
						else {
							code.add1Unchecked(0x0F);
							if (ir.opSize == 2) {
								code.add1Unchecked(0xBF);
							}
							else {
								code.add1Unchecked(0xBE);
							}
							writeRSPRegisterByte(function, RAX, ir.a);

							if (ir.b == 8) {
								code.add1Unchecked(0xF2);
							}
							else {
								code.add1Unchecked(0xF3);
							}

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0x2A);
							code.add1Unchecked(0xC0);

							storeFromFloatRegister(function, ir.b, ir.dest, 0);
						}
					}
					else {
						if (ir.opSize == 8) {
							loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

							code.add1Unchecked(0x0F); // xorps xmm0, xmm0
							code.add1Unchecked(0x57);
							code.add1Unchecked(0xC0);

							code.add1Unchecked(0x48); // test rax, rax
							code.add1Unchecked(0x85);
							code.add1Unchecked(0xC0);

							code.add1Unchecked(0x70 | C_S); // js .large

							u8 *firstJumpPatch = code.add1Unchecked(0);
							u32 firstJumpRel = code.totalSize;


							if (ir.b == 8) {
								code.add1Unchecked(0xF2);
							}
							else {
								code.add1Unchecked(0xF3);
							}

							code.add1Unchecked(0x48);
							code.add1Unchecked(0x0F);
							code.add1Unchecked(0x2a);
							code.add1Unchecked(0xC0);

							code.add1Unchecked(0xEB); // jmp .done

							u8 *secondJumpPatch = code.add1Unchecked(0);
							u32 secondJumpRel = code.totalSize;


							*firstJumpPatch = code.totalSize - firstJumpRel;

							// .large
							code.add1Unchecked(0x48); // mov rcx, rax
							code.add1Unchecked(0x89);
							code.add1Unchecked(0xC1);

							code.add1Unchecked(0x83); // and ecx, 1
							code.add1Unchecked(0xE1);
							code.add1Unchecked(0x01);

							code.add1Unchecked(0x48); // shr rax, 1
							code.add1Unchecked(0xD1);
							code.add1Unchecked(0xE8);

							code.add1Unchecked(0x48); // or rax, rcx
							code.add1Unchecked(0x09);
							code.add1Unchecked(0xC8);

							if (ir.b == 8) {
								code.add1Unchecked(0xF2);
							}
							else {
								code.add1Unchecked(0xF3);
							}

							code.add1Unchecked(0x48);
							code.add1Unchecked(0x0F);
							code.add1Unchecked(0x2A);
							code.add1Unchecked(0xC0);

							if (ir.b == 8) {
								code.add1Unchecked(0xF2);
							}
							else {
								code.add1Unchecked(0xF3);
							}

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0x58);
							code.add1Unchecked(0xC0);

							*secondJumpPatch = code.totalSize - secondJumpRel;

							// .done
							storeFromFloatRegister(function, ir.b, ir.dest, 0);
						}
						else if (ir.opSize == 4) {
							loadIntoIntRegister(function, 4, RAX, ir.a);

							if (ir.b == 8) {
								code.add1Unchecked(0xF2);
							}
							else {
								code.add1Unchecked(0xF3);
							}

							code.add1Unchecked(0x48);

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0x2A);
							code.add1Unchecked(0xC0);
							storeFromFloatRegister(function, ir.b, ir.dest, 0);
						}
						else {
							code.add1Unchecked(0x0F);
							if (ir.opSize == 2) {
								code.add1Unchecked(0xB7);
							}
							else {
								code.add1Unchecked(0xB6);
							}
							writeRSPRegisterByte(function, RAX, ir.a);

							if (ir.b == 8) {
								code.add1Unchecked(0xF2);
							}
							else {
								code.add1Unchecked(0xF3);
							}

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0x2A);
							code.add1Unchecked(0xC0);

							storeFromFloatRegister(function, ir.b, ir.dest, 0);
						}
					}
				} break;
				case IrOp::RETURN: {
					if (ir.opSize) {
						assert(isStandardSize(ir.opSize));

						if (ir.flags & IR_FLOAT_OP) {
							loadIntoFloatRegister(function, ir.opSize, 0, ir.a);
						}
						else {
							loadIntoIntRegister(function, ir.opSize, RAX, ir.a);
						}
					}

					code.add1Unchecked(0xE9);

					JumpPatch patch;
					patch.opToPatch = function->state.ir.count;
					patch.location = reinterpret_cast<s32 *>(code.add4Unchecked(0));
					patch.rip = code.totalSize;

					jumpPatches.add(patch);
				} break;
				case IrOp::CALL: {
					constexpr int intRegisters[4] = { RCX, RDX, 8, 9 };

					auto arguments = static_cast<FunctionCall *>(ir.data);

					code.ensure(128);

					for (u8 i = 0; i < my_min(4, arguments->argCount); i++) {
						auto type = arguments->args[i].type;


						if (type->flavor == TypeFlavor::FLOAT) {
							loadIntoFloatRegister(function, type->size, i, arguments->args[i].number);
						}
						else {
							assert(isStandardSize(type->size));
							loadIntoIntRegister(function, type->size, intRegisters[i], arguments->args[i].number);
						}
					}

					for (u32 i = 4; i < arguments->argCount; i++) {
						u64 size = arguments->args[i].type->size;

						assert(isStandardSize(size));
						loadIntoIntRegister(function, arguments->args[i].type->size, RAX, arguments->args[i].number);
						

						code.add1Unchecked(0x48);
						code.add1Unchecked(0x89);
						writeRSPOffsetByte(RAX, getParameterSpaceForCallOffset(function) + i * 8);

						code.ensure(128);
					}

					assert(isStandardSize(arguments->returnType->size));

					code.add1Unchecked(0xFF);
					writeRSPRegisterByte(function, 2, ir.a);

					if (arguments->returnType != &TYPE_VOID && ir.opSize) {
						assert(isStandardSize(arguments->returnType->size));

						if (arguments->returnType->flavor == TypeFlavor::FLOAT) {
							storeFromFloatRegister(function, arguments->returnType->size, ir.dest, 0);
						}
						else {
							storeFromIntRegister(function, arguments->returnType->size, ir.dest, RAX);
						}
					}
				} break;
				case IrOp::NEG: {
					if (ir.flags & IR_FLOAT_OP) {
						code.add1Unchecked(0x0F); // xorps xmm0, xmm0
						code.add1Unchecked(0x57);
						code.add1Unchecked(0xC0);

						if (ir.opSize == 8) {
							code.add1Unchecked(0xF2);
						}
						else {
							code.add1Unchecked(0xF3);
						}

						code.add1Unchecked(0x0F);
						code.add1Unchecked(0x5C);
						writeRSPRegisterByte(function, 0, ir.a);

						storeFromFloatRegister(function, ir.opSize, ir.dest, 0);
					}
					else {
						loadIntoIntRegister(function, ir.opSize, RAX, ir.a);
						if (ir.opSize == 8) {
							code.add1Unchecked(0x48);
						}
						else if (ir.opSize == 2) {
							code.add1Unchecked(0x66);
						}

						if (ir.opSize == 1) {
							code.add1Unchecked(0xF6);
						}
						else {
							code.add1Unchecked(0xF7);
						}

						code.add1Unchecked(0xD8);

						storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
					}
				} break;
				case IrOp::NOOP: {
					// we are done
				} break;
				case IrOp::FUNCTION: {
					code.add1Unchecked(0x48);
					code.add1Unchecked(0x8D);
					code.add1Unchecked(0x05);

					codeRelocations.ensure(10);
					codeRelocations.add4Unchecked(code.totalSize);
					codeRelocations.add4Unchecked(createSymbolForFunction(ir.function));
					codeRelocations.add2Unchecked(IMAGE_REL_AMD64_REL32);

					code.add4Unchecked(0);

					storeFromIntRegister(function, 8, ir.dest, RAX);
				} break;
				case IrOp::STRING: {
					code.add1Unchecked(0x48);
					code.add1Unchecked(0x8D);
					code.add1Unchecked(0x05);

					codeRelocations.ensure(10);
					codeRelocations.add4Unchecked(code.totalSize);
					codeRelocations.add4Unchecked(createSymbolForString(static_cast<ExprStringLiteral *>(ir.data)));
					codeRelocations.add2Unchecked(IMAGE_REL_AMD64_REL32);

					code.add4Unchecked(0);

					storeFromIntRegister(function, 8, ir.dest, RAX);
				} break;
				case IrOp::ARRAY_LITERAL: {
					auto array = static_cast<ExprArrayLiteral *>(ir.data);

					code.add1Unchecked(0x48);
					code.add1Unchecked(0x8D);
					code.add1Unchecked(0x05);

					if (!(array->flags & EXPR_HAS_STORAGE)) {
						array->flags |= EXPR_HAS_STORAGE;

						array->physicalStorage = static_cast<u32>(symbols.count());
						array->symbol = allocateSymbol();

						alignAllocator(&rdata, array->type->alignment);

						setSymbolName(&array->symbol->name, symbols.count());
						array->symbol->storageClass = IMAGE_SYM_CLASS_STATIC;
						array->symbol->value = static_cast<u32>(rdata.totalSize);
						array->symbol->sectionNumber = RDATA_SECTION_NUMBER;
						array->symbol->type = 0;
						array->symbol->numberOfAuxSymbols = 0;

						u32 offset = rdata.totalSize; // do this in a separate statement because within a c++ statement, subexpressions can execute in any order
						                              // which meant that allocateUnaligned happened _before_ rdata.totalSize was evaluated, causing the addresses 
						                              // for any patch applied to the literal to be wrong
						writeValue(offset, static_cast<u8 *>(rdata.allocateUnaligned(array->type->size)), &rdata, &rdataRelocations, array);
					}

					codeRelocations.ensure(10);
					codeRelocations.add4Unchecked(code.totalSize);
					codeRelocations.add4Unchecked(array->physicalStorage);
					codeRelocations.add2Unchecked(IMAGE_REL_AMD64_REL32);

					code.add4Unchecked(0);

					storeFromIntRegister(function, 8, ir.dest, RAX);
				} break;
				case IrOp::STRUCT_LITERAL: {
					auto literal = static_cast<ExprStructLiteral *>(ir.data);

					code.add1Unchecked(0x48);
					code.add1Unchecked(0x8D);
					code.add1Unchecked(0x05);

					if (!(literal->flags & EXPR_HAS_STORAGE)) {
						literal->flags |= EXPR_HAS_STORAGE;

						literal->physicalStorage = static_cast<u32>(symbols.count());
						literal->symbol = allocateSymbol();

						rdata.allocateUnaligned(AlignPO2(rdata.totalSize, literal->type->alignment) - rdata.totalSize);

						setSymbolName(&literal->symbol->name, symbols.count());
						literal->symbol->storageClass = IMAGE_SYM_CLASS_STATIC;
						literal->symbol->value = static_cast<u32>(rdata.totalSize);
						literal->symbol->sectionNumber = RDATA_SECTION_NUMBER;
						literal->symbol->type = 0;
						literal->symbol->numberOfAuxSymbols = 0;

						u32 offset = rdata.totalSize; // do this in a separate statement because within a c++ statement, subexpressions can execute in any order
													  // which meant that allocateUnaligned happened _before_ rdata.totalSize was evaluated, causing the addresses 
													  // for any patch applied to the literal to be wrong
						writeValue(offset, static_cast<u8 *>(rdata.allocateUnaligned(literal->type->size)), &rdata, &rdataRelocations, literal);
					}

					codeRelocations.ensure(10);
					codeRelocations.add4Unchecked(code.totalSize);
					codeRelocations.add4Unchecked(literal->physicalStorage);
					codeRelocations.add2Unchecked(IMAGE_REL_AMD64_REL32);

					code.add4Unchecked(0);

					storeFromIntRegister(function, 8, ir.dest, RAX);
				} break;
				case IrOp::LINE_MARKER: {
					addLineInfo(&lineInfo, &columnInfo, code.totalSize - functionStart, ir.location.start, ir.location.end);
				} break;
				case IrOp::BLOCK: {
					auto block = static_cast<Block *>(ir.data);

					if (block) {
						debugSymbols.ensure(23);
						debugSymbolsRelocations.ensure(20);

						debugSymbols.add2Unchecked(21);
						debugSymbols.add2Unchecked(0x1103); // S_BLOCK32

						debugSymbols.add4Unchecked(0);
						debugSymbols.add4Unchecked(0);

						blockOffsetStack.add(debugSymbols.add4(0));
						debugSymbolsRelocations.add4Unchecked(debugSymbols.totalSize);
						debugSymbolsRelocations.add4Unchecked(function->physicalStorage);
						debugSymbolsRelocations.add2Unchecked(IMAGE_REL_AMD64_SECREL);

						debugSymbols.add4Unchecked(code.totalSize - functionStart);

						debugSymbolsRelocations.add4Unchecked(debugSymbols.totalSize);
						debugSymbolsRelocations.add4Unchecked(function->physicalStorage);
						debugSymbolsRelocations.add2Unchecked(IMAGE_REL_AMD64_SECTION);

						debugSymbols.add2Unchecked(0);
						debugSymbols.add1Unchecked(0);

						for (auto declaration : block->declarations) {
							if (declaration->flags & (DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_CONSTANT)) continue;

							REGREL32 variableInfo;

							u32 offset;

							if (declarationIsStoredByPointer(declaration)) {
								offset = getStackSpaceOffset(function) + declaration->physicalStorage;
							}
							else {
								offset = getRegisterOffset(function, declaration->registerOfStorage);
							}

							variableInfo.off = offset;
							variableInfo.typind = 0;

							debugSymbols.ensure(2 + sizeof(variableInfo));
							debugSymbols.add2Unchecked(static_cast<u16>(sizeof(variableInfo) + declaration->name.length + 1));
							REGREL32 *patch = (REGREL32 *) debugSymbols.addUnchecked(&variableInfo, sizeof(variableInfo));
							coffTypePatches.add({ &patch->typind, getDeclarationType(declaration) });
							debugSymbols.addNullTerminatedString(declaration->name);
						}
					}
					else {
						u32 *length = blockOffsetStack.pop();
						*length = code.totalSize - functionStart - length[1];

						debugSymbols.ensure(4);
						debugSymbols.add2Unchecked(2);
						debugSymbols.add2Unchecked(6); // S_END
					}
				} break;
				default: {
					assert(false);
				}
				}
			}
			
			*functionPostambleStartPatch = code.totalSize - functionStart;
			u32 functionPostambleStart = code.totalSize;

			code.ensure(64);

			// add rsp, spaceToAllocate
			if (spaceToAllocate < 0x80) {
				code.add1Unchecked(0x48);
				code.add1Unchecked(0x83);
				code.add1Unchecked(0xC4);
				code.add1Unchecked(static_cast<u8>(spaceToAllocate));
			}
			else {
				code.add1Unchecked(0x48);
				code.add1Unchecked(0x81);
				code.add1Unchecked(0xC4);
				code.add4Unchecked(static_cast<u32>(spaceToAllocate));
			}

			code.add1Unchecked(0x5F); // pop rdi
			code.add1Unchecked(0x5E); // pop rsi

			code.add1Unchecked(0xC3);

			*functionLengthPatch = code.totalSize - functionStart;

			instructionOffsets.add(functionPostambleStart);

			for (auto patch : jumpPatches) {
				*patch.location = static_cast<s32>(instructionOffsets[patch.opToPatch]) - static_cast<s32>(patch.rip);
			}

			{
				PROFILE_ZONE("Write Function Debug Symbols");
				debugSymbols.ensure(4);
				debugSymbols.add2Unchecked(2); // S_PROC_ID_END
				debugSymbols.add2Unchecked(0x114f);

				*subsectionSizePatch = debugSymbols.totalSize - subsectionOffset;

				alignAllocator(&debugSymbols, 4);

				pdata.ensure(12);

				pdataRelocations.ensure(30);
				pdataRelocations.add4Unchecked(pdata.totalSize);
				pdataRelocations.add4Unchecked(function->physicalStorage);
				pdataRelocations.add2Unchecked(IMAGE_REL_AMD64_ADDR32NB);

				pdata.add4Unchecked(0);

				pdataRelocations.add4Unchecked(pdata.totalSize);
				pdataRelocations.add4Unchecked(function->physicalStorage);
				pdataRelocations.add2Unchecked(IMAGE_REL_AMD64_ADDR32NB);

				pdata.add4Unchecked(code.totalSize - functionStart);

				pdataRelocations.add4Unchecked(pdata.totalSize);
				pdataRelocations.add4Unchecked(symbols.count());
				pdataRelocations.add2Unchecked(IMAGE_REL_AMD64_ADDR32NB);

				Symbol xdataSymbol;
				setSymbolName(&xdataSymbol.name, symbols.count());
				xdataSymbol.value = xdata.totalSize;
				xdataSymbol.type = 0;
				xdataSymbol.sectionNumber = XDATA_SECTION_NUMBER;
				xdataSymbol.storageClass = IMAGE_SYM_CLASS_STATIC;
				xdataSymbol.numberOfAuxSymbols = 0;

				symbols.add(xdataSymbol);

				pdata.add4Unchecked(0);

				xdata.ensure(11);
				xdata.add1Unchecked(1);
				xdata.add1Unchecked(functionPreambleEnd);
				xdata.add1Unchecked(4);
				xdata.add1Unchecked(0);

				xdata.add1Unchecked(subRspOffset);
				xdata.add1Unchecked(0x01);
				xdata.add2Unchecked(spaceToAllocate / 8);
				xdata.add1Unchecked(pushRdiOffset);
				xdata.add1Unchecked(0x70);
				xdata.add1Unchecked(pushRsiOffset);
				xdata.add1Unchecked(0x60);
			}

			{
				PROFILE_ZONE("Write Function Debug Lines");
				debugSymbols.ensure(32 + lineInfo.count * 12);
				debugSymbolsRelocations.ensure(20);
				
				debugSymbols.add4Unchecked(0xF2);
				debugSymbols.add4Unchecked(24 + lineInfo.count * 12);


				debugSymbolsRelocations.add4Unchecked(debugSymbols.totalSize);
				debugSymbolsRelocations.add4Unchecked(function->physicalStorage);
				debugSymbolsRelocations.add2Unchecked(IMAGE_REL_AMD64_SECREL);

				debugSymbols.add4Unchecked(0);

				debugSymbolsRelocations.add4Unchecked(debugSymbols.totalSize);
				debugSymbolsRelocations.add4Unchecked(function->physicalStorage);
				debugSymbolsRelocations.add2Unchecked(IMAGE_REL_AMD64_SECTION);

				debugSymbols.add2Unchecked(0);

				debugSymbols.add2Unchecked(1); // fHasColumns
				debugSymbols.add4Unchecked(code.totalSize - functionStart);

				debugSymbols.add4Unchecked(function->start.fileUid * 8);
				debugSymbols.add4Unchecked(lineInfo.count);
				debugSymbols.add4Unchecked(12 + lineInfo.count * 12);
				debugSymbols.addUnchecked(lineInfo.storage, lineInfo.count * sizeof(LineInfo));
				debugSymbols.addUnchecked(columnInfo.storage, columnInfo.count * sizeof(ColumnInfo));
			}

		}
		else if (job.flavor == CoffJobFlavor::GLOBAL_DECLARATION) {
			PROFILE_ZONE("Write Declaration");
			auto declaration = job.declaration;

			assert(declaration->enclosingScope->flavor == BlockFlavor::GLOBAL);
			assert(!(declaration->flags & DECLARATION_IS_CONSTANT));

			createSymbolForDeclaration(declaration);

			debugSymbols.ensure(22);
			debugSymbolsRelocations.ensure(20);

			debugSymbols.add4Unchecked(0xF1);
			auto subsectionSizePatch = debugSymbols.add4Unchecked(0);
			u32 subsectionOffset = debugSymbols.totalSize;

			debugSymbols.add2Unchecked(static_cast<u16>(sizeof(DATASYM32) + declaration->name.length - 1));
			debugSymbols.add2Unchecked(0x110d); // S_GDATA32

			u32 *patch = debugSymbols.add4Unchecked(0);
			coffTypePatches.add({ patch, getDeclarationType(declaration) });

			debugSymbolsRelocations.add4Unchecked(debugSymbols.totalSize);
			debugSymbolsRelocations.add4Unchecked(declaration->physicalStorage);
			debugSymbolsRelocations.add2Unchecked(IMAGE_REL_AMD64_SECREL);

			debugSymbols.add4Unchecked(0);

			debugSymbolsRelocations.add4Unchecked(debugSymbols.totalSize);
			debugSymbolsRelocations.add4Unchecked(declaration->physicalStorage);
			debugSymbolsRelocations.add2Unchecked(IMAGE_REL_AMD64_SECTION);

			debugSymbols.add2Unchecked(0);

			debugSymbols.addNullTerminatedString(declaration->name);

			*subsectionSizePatch = debugSymbols.totalSize - subsectionOffset;

			alignAllocator(&debugSymbols, 4);

			auto symbol = declaration->symbol;
			auto type = getDeclarationType(declaration);

			setSymbolName(&symbol->name, declaration->name);

			symbol->storageClass = IMAGE_SYM_CLASS_EXTERNAL;
			symbol->type = 0;

			if (placeValueInBSSSection(declaration)) {
				bssSection.sizeOfRawData = AlignPO2(bssSection.sizeOfRawData, type->alignment);

				symbol->value = bssSection.sizeOfRawData;
				symbol->sectionNumber = BSS_SECTION_NUMBER;

				bssSection.sizeOfRawData += type->size;
			}
			else {
				data.allocateUnaligned(AlignPO2(data.totalSize, type->alignment) - data.totalSize);

				symbol->value = data.totalSize;
				symbol->sectionNumber = DATA_SECTION_NUMBER;

				u32 dataSize = data.totalSize;
				u8 *allocation = static_cast<u8 *>(data.allocateUnaligned(type->size));

				writeValue(dataSize, allocation, &data, &dataRelocations, declaration->initialValue);
			}

			symbol->numberOfAuxSymbols = 0;
		}
	}

	if (!hadError) {
		PROFILE_ZONE("Write types");

		/*
		for (u64 i = 0; i < typeTableCapacity; i++) {
			auto entry = typeTableEntries[i];

			if (entry.hash) {
				if (!entry.value->symbol) {
					createSymbolForType(entry.value);
				}
			}
		}
		*/

		debugSymbols.ensure(8);

		debugSymbols.add4Unchecked(0xF1);
		u32 *subsectionSizePatch = debugSymbols.add4(0);

		u32 previousSize = debugSymbols.totalSize;

		exportTypeTableToDebugTSection(&debugTypes);

		{
			PROFILE_ZONE("Patch debug types");
			for (auto patch : coffTypePatches) {
				*patch.location = getCoffTypeIndex(patch.type);
			}

			for (auto patch : coffFunctionIdTypePatches) {
				*patch.location = createFunctionIDType(patch.function);
			}
		}

		for (u64 i = 0; i < typeTableCapacity; i++) {
			auto entry = typeTableEntries[i];

			if (entry.hash) {
				auto type = entry.value;



				if (type->flavor == TypeFlavor::STRUCT || type->flavor == TypeFlavor::ARRAY || type->flavor == TypeFlavor::ENUM) {
					if (!(type->flags & (TYPE_ARRAY_IS_FIXED | TYPE_ENUM_IS_FLAGS)))
						emitUDT(&debugSymbols, type);
				}

				if (!(type->flags & TYPE_USED_IN_OUTPUT))
					continue;

				auto symbol = type->symbol;

				u32 name = createRdataPointer();
				rdata.addNullTerminatedString(type->name);

				assert(type->name.length);

				setSymbolName(&symbol->name, entry.value->physicalStorage);
				symbol->storageClass = IMAGE_SYM_CLASS_STATIC;
				symbol->type = 0;

				symbol->sectionNumber = RDATA_SECTION_NUMBER;
				symbol->numberOfAuxSymbols = 0;

				Type_Info::Tag infoTag;

				switch (type->flavor) {
				case TypeFlavor::VOID: {
					infoTag = Type_Info::Tag::VOID;
					break;
				}
				case TypeFlavor::INTEGER: {
					infoTag = Type_Info::Tag::INTEGER;
					break;
				}
				case TypeFlavor::FLOAT: {
					infoTag = Type_Info::Tag::FLOAT; 
					break; 
				}
				case TypeFlavor::POINTER: {
					infoTag = Type_Info::Tag::POINTER; 
					break; 
				}
				case TypeFlavor::BOOL: {
					infoTag = Type_Info::Tag::BOOL; 
					break; 
				}
				case TypeFlavor::FUNCTION: {
					infoTag = Type_Info::Tag::FUNCTION; 
					break; 
				}
				case TypeFlavor::TYPE: {
					infoTag = Type_Info::Tag::TYPE; 
					break; 
				}
				case TypeFlavor::STRING: {
					infoTag = Type_Info::Tag::STRING; 
					break; 
				}
				case TypeFlavor::ARRAY: {
					infoTag = Type_Info::Tag::ARRAY; 
					break; 
				}
				case TypeFlavor::STRUCT: {
					infoTag = Type_Info::Tag::STRUCT; 
					break; 
				}
				case TypeFlavor::ENUM: {
					infoTag = Type_Info::Tag::ENUM; 
					break; 
				}
				default:
					assert(false);
				}

				switch (infoTag) {
				case Type_Info::Tag::VOID:
				case Type_Info::Tag::FLOAT:
				case Type_Info::Tag::BOOL:
				case Type_Info::Tag::TYPE:
				case Type_Info::Tag::STRING: {
					Type_Info info;

					rdata.allocateUnaligned(AlignPO2(rdata.totalSize, 8) - rdata.totalSize);

					symbol->value = rdata.totalSize;

					info.tag = infoTag;
					info.size = type->size;
					info.alignment = type->alignment;
					info.name = { nullptr, type->name.length };

					if (name)
						addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(info), name), name);

					rdata.add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::INTEGER: {
					Type_Info_Integer info;

					rdata.allocateUnaligned(AlignPO2(rdata.totalSize, 8) - rdata.totalSize);

					symbol->value = rdata.totalSize;

					info.tag = infoTag;
					info.size = type->size;
					info.alignment = type->alignment;
					info.name = { nullptr, type->name.length };
					info.signed_ = type->flags & TYPE_INTEGER_IS_SIGNED;

					if (name)
						addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(info), name), name);

					rdata.add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::POINTER: {
					Type_Info_Pointer info;

					rdata.allocateUnaligned(AlignPO2(rdata.totalSize, 8) - rdata.totalSize);

					symbol->value = rdata.totalSize;

					info.tag = infoTag;
					info.size = type->size;
					info.alignment = type->alignment;
					info.name = { nullptr, type->name.length };
					info.value_type = nullptr;

					if (name)
						addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(info), name), name);

					addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(info), value_type),
						static_cast<TypePointer *>(type)->pointerTo->physicalStorage);

					rdata.add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::FUNCTION: {
					auto function = static_cast<TypeFunction *>(type);

					rdata.allocateUnaligned(AlignPO2(rdata.totalSize, 8) - rdata.totalSize);

					u32 arguments = createRdataPointer();

					for (u64 i = 0; i < function->argumentCount; i++) {
						addPointerRelocation(&rdataRelocations, rdata.totalSize, function->argumentTypes[i]->physicalStorage);
						rdata.add8(0);
					}
					
					u32 returns = createRdataPointer();

					for (u64 i = 0; i < function->returnCount; i++) {
						addPointerRelocation(&rdataRelocations, rdata.totalSize, function->returnTypes[i]->physicalStorage);
						rdata.add8(0);
					}

					Type_Info_Function info;

					symbol->value = rdata.totalSize;

					info.tag = infoTag;
					info.size = type->size;
					info.alignment = type->alignment;
					info.name = { nullptr, type->name.length };
					info.arguments.data = nullptr;
					info.arguments.count = function->argumentCount;
					info.returns.data = nullptr;
					info.returns.count = function->returnCount;

					if (name)
						addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(info), name), name);

					addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(info), arguments.data), arguments);
					addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(info), returns.data), returns);

					rdata.add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::ARRAY: {
					Type_Info_Array info;

					rdata.allocateUnaligned(AlignPO2(rdata.totalSize, 8) - rdata.totalSize);

					symbol->value = rdata.totalSize;

					info.tag = infoTag;
					info.size = type->size;
					info.alignment = type->alignment;
					info.name = { nullptr, type->name.length };
					info.flavor = type->flags & TYPE_ARRAY_IS_FIXED ?
						Type_Info_Array::Flavor::FIXED : type->flags & TYPE_ARRAY_IS_DYNAMIC ?
						Type_Info_Array::Flavor::DYNMAIC : Type_Info_Array::Flavor::NORMAL;
					info.count = type->flags & TYPE_ARRAY_IS_FIXED ? static_cast<TypeArray *>(type)->count : 0;
					info.element_type = nullptr;

					if (name)
						addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(info), name), name);

					addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(info), element_type),
						static_cast<TypeArray *>(type)->arrayOf->physicalStorage);

					rdata.add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::STRUCT: {
					auto struct_ = static_cast<TypeStruct *>(type);

					u32 names = symbols.count();

					for (auto member : struct_->members.declarations) {
						if (member->flags & DECLARATION_IMPORTED_BY_USING) continue;


						createRdataPointer();
						rdata.addNullTerminatedString(member->name);
					}

					u32 values = symbols.count();

					for (auto member : struct_->members.declarations) {
						if (member->flags & DECLARATION_IMPORTED_BY_USING) continue;

						if (!member->initialValue) continue;

						auto type = getTypeForExpr(member->initialValue);
						
						if (member->initialValue->flavor == ExprFlavor::TYPE_LITERAL && static_cast<ExprLiteral *>(member->initialValue)->typeValue->flavor == TypeFlavor::MODULE)
							continue;

						rdata.allocateUnaligned(AlignPO2(rdata.totalSize, type->alignment) - rdata.totalSize);

						createRdataPointer();

						u32 dataSize = rdata.totalSize;
						u8 *allocation = static_cast<u8 *>(rdata.allocateUnaligned(type->size));

						writeValue(dataSize, allocation, &rdata, &rdataRelocations, member->initialValue);
					}

					rdata.allocateUnaligned(AlignPO2(rdata.totalSize, 8) - rdata.totalSize);
					u32 members = createRdataPointer();

					u32 nameCount = 0;
					u32 valueCount = 0;

					for (auto member : struct_->members.declarations) {
						if (member->flags & DECLARATION_IMPORTED_BY_USING) continue;


						Type_Info_Struct::Member data;

						data.name = { nullptr, member->name.length };
						data.offset = (member->flags & DECLARATION_IS_CONSTANT) ? 0 : member->physicalStorage;
						data.member_type = nullptr;
						data.initial_value = nullptr;
						data.flags = 0;

						if (member->flags & DECLARATION_IS_UNINITIALIZED) data.flags |= Type_Info_Struct::Member::Flags::UNINITIALIZED;
						if (member->flags & DECLARATION_IS_CONSTANT) data.flags |= Type_Info_Struct::Member::Flags::CONSTANT;
						if (member->flags & DECLARATION_MARKED_AS_USING) data.flags |= Type_Info_Struct::Member::Flags::USING;


						addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(data), name.data), names + nameCount);

						if (member->initialValue) {
							addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(data), member_type),
								getTypeForExpr(member->initialValue)->physicalStorage);
						}
						else {
							addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(data), member_type),
								getDeclarationType(member)->physicalStorage);
						}

						if (member->initialValue) { // @Incomplete: Export info for namespaces
							if (member->initialValue->flavor != ExprFlavor::TYPE_LITERAL || static_cast<ExprLiteral *>(member->initialValue)->typeValue->flavor != TypeFlavor::MODULE) {
								addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(data), initial_value), values + valueCount);
								++valueCount;
							}
						}

						rdata.add(&data, sizeof(data));

						++nameCount;
					}

					Type_Info_Struct info;

					symbol->value = rdata.totalSize;

					info.tag = infoTag;
					info.size = type->size;
					info.alignment = type->alignment;
					info.name = { nullptr, type->name.length };
					info.flags = 0;

					if (struct_->flags & TYPE_STRUCT_IS_UNION) info.flags |= Type_Info_Struct::Flags::UNION;
					if (struct_->flags & TYPE_STRUCT_IS_PACKED) info.flags |= Type_Info_Struct::Flags::PACKED;

					info.members.data = nullptr;
					info.members.count = nameCount;

					if (name)
						addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(info), name), name);

					addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(info), members.data), members);

					rdata.add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::ENUM: {
					auto enum_ = static_cast<TypeEnum *>(type);

					u32 names = symbols.count();

					for (auto member : enum_->members.declarations) {
						if (!(member->flags & DECLARATION_IS_ENUM_VALUE))
							continue;
						createRdataPointer();
						rdata.addNullTerminatedString(member->name);
					}

					rdata.allocateUnaligned(AlignPO2(rdata.totalSize, 8) - rdata.totalSize);
					u32 values = createRdataPointer();

					for (u32 i = 0; i < enum_->members.declarations.count; i++) {
						auto member = enum_->members.declarations[i];
						if (!(member->flags & DECLARATION_IS_ENUM_VALUE))
							continue;

						Type_Info_Enum::Value data;

						data.name = { nullptr, member->name.length };
						data.value = static_cast<ExprLiteral *>(member->initialValue)->unsignedValue;

						addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(data), name.data), names + i);

						rdata.add(&data, sizeof(data));
					}

					Type_Info_Enum info;

					symbol->value = rdata.totalSize;

					info.tag = infoTag;
					info.size = type->size;
					info.alignment = type->alignment;
					info.name = { nullptr, type->name.length };
					info.base_type = nullptr;
					info.is_flags = enum_->flags & TYPE_ENUM_IS_FLAGS ? true : false;
				
					info.values.data = nullptr;
					info.values.count = enum_->members.declarations.count - ENUM_SPECIAL_MEMBER_COUNT;

					if (name)
						addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(info), name), name);

					addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(info), base_type), 
						enum_->integerType->physicalStorage);
					addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(info), values.data), values);

					rdata.add(&info, sizeof(info));

					break;
				}
				default:
					assert(false);
				}
			}
		}

		*subsectionSizePatch = debugSymbols.totalSize - previousSize;
		alignAllocator(&debugSymbols, 4);
	}

	{
		PROFILE_ZONE("Write output");
		debugSymbols.add4(0xF3);

		u32 *sizePointer = debugSymbols.add4(0);

		u32 totalSize = 0;

		for (auto file : compilerFiles) {
			char buffer[1024]; // @Robustness

			file->offsetInStringTable = totalSize;

			GetFullPathNameA(toCString(file->path) /* @Leak */, sizeof(buffer), buffer, 0);

			u32 len = static_cast<u32>(strlen(buffer));
			totalSize += len + 1;

			debugSymbols.addNullTerminatedString({ buffer, len });
		}

		*sizePointer = totalSize;

		alignAllocator(&debugSymbols, 4);

		debugSymbols.add4(0xF4);
		debugSymbols.add4(8 * compilerFiles.count);

		for (auto &file : compilerFiles) {
			debugSymbols.add4(file->offsetInStringTable);
			debugSymbols.add4(0);
		}


		u32 stringTableSize = sizeof(u32) + stringTable.totalSize;

		struct Section {
			SectionHeader *header;
			BucketedArenaAllocator *data;
			BucketedArenaAllocator *relocations;
		};

		Array<Section> sections;


		SectionHeader rdataSection = {};
		setSectionName(rdataSection.name, sizeof(rdataSection.name), ".rdata");
		rdataSection.characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_ALIGN_16BYTES;

		setSectionName(bssSection.name, sizeof(bssSection.name), ".bss");
		bssSection.characteristics = IMAGE_SCN_CNT_UNINITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE | IMAGE_SCN_ALIGN_16BYTES;

		SectionHeader dataSection = {};
		setSectionName(dataSection.name, sizeof(dataSection.name), ".data");
		dataSection.characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE | IMAGE_SCN_ALIGN_16BYTES;

		SectionHeader textSection = {};
		setSectionName(textSection.name, sizeof(textSection.name), ".text");
		textSection.characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_ALIGN_16BYTES;

		SectionHeader debugSymbolSection = {};
		setSectionName(debugSymbolSection.name, sizeof(debugSymbolSection.name), ".debug$S");
		debugSymbolSection.characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_DISCARDABLE | IMAGE_SCN_ALIGN_1BYTES;

		alignDebugTypes(&debugTypes);

		SectionHeader debugTypeSection = {};
		setSectionName(debugTypeSection.name, sizeof(debugTypeSection.name), ".debug$T");
		debugTypeSection.characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_DISCARDABLE | IMAGE_SCN_ALIGN_1BYTES;

		SectionHeader pdataSection = {};
		setSectionName(pdataSection.name, sizeof(pdataSection.name), ".pdata");
		pdataSection.characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_ALIGN_4BYTES;
		
		SectionHeader xdataSection = {};
		setSectionName(xdataSection.name, sizeof(xdataSection.name), ".xdata");
		xdataSection.characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_ALIGN_4BYTES;

		sections.add({ &rdataSection, &rdata, &rdataRelocations });
		sections.add({ &bssSection });
		sections.add({ &dataSection, &data, &dataRelocations });
		sections.add({ &textSection, &code, &codeRelocations });

		sections.add({ &debugSymbolSection, &debugSymbols, &debugSymbolsRelocations });
		sections.add({ &debugTypeSection, &debugTypes });
		sections.add({ &pdataSection, &pdata, &pdataRelocations });
		sections.add({ &xdataSection, &xdata });



		FileHeader header = {};
		header.machine = IMAGE_FILE_MACHINE_AMD64;
		header.numberOfSections = sections.count;
		header.timestamp = (DWORD) time(0);
		header.pointerToSymbolTable = sizeof(FileHeader) + sizeof(SectionHeader) * sections.count;
		header.numberOfSymbols = symbols.count();
		header.sizeOfOptionalHeader = 0;
		header.characteristics = 0;

		u32 prefixSize = header.pointerToSymbolTable + sizeof(Symbol) * symbols.count() + stringTableSize;

		u32 sectionPointer = AlignPO2(prefixSize, 4);

		for (auto section : sections) {
			section.header->virtualAddress = 0;

			if (section.data) {
				alignAllocator(section.data, 4);
				section.header->sizeOfRawData = section.data->totalSize;
				section.header->virtualSize = 0;
			}
			else {
				section.header->sizeOfRawData = AlignPO2(section.header->sizeOfRawData, 4);
			}

			if (section.data) {
				section.header->pointerToRawData = sectionPointer;

				sectionPointer += section.header->sizeOfRawData;

				if (section.relocations) {
					section.header->pointerToRelocations = sectionPointer;

					u32 relocationCount = section.relocations->totalSize / sizeof(Relocation);

					if (relocationCount > UINT16_MAX) {
						section.header->characteristics |= IMAGE_SCN_LNK_NRELOC_OVFL;
						section.header->numberOfRelocations = UINT16_MAX;
					}
					else {
						section.header->numberOfRelocations = static_cast<u16>(relocationCount);
					}

					alignAllocator(section.relocations, 4);

					if (relocationCount > UINT16_MAX) {
						sectionPointer += 10;

						section.relocations->add2(0); // @Hack We padded to what we though was a multiple of 4 bytes, but since an extra relocation is added at the beginning, 
						                              // we need to add 2 more bytes to ensure alignment
					}

					sectionPointer += section.relocations->totalSize;
				}
				else {
					section.header->pointerToRelocations = 0;
					section.header->numberOfRelocations = 0;
				}
			}
			else {
				section.header->pointerToRawData = 0;
				section.header->pointerToRelocations = 0;
				section.header->numberOfRelocations = 0;
			}


			section.header->pointerToLinenumbers = 0;
			section.header->numberOfLinenumbers = 0;
		}

		if (!hadError) {
			HANDLE out = CreateFileW(utf8ToWString(objectFileName), GENERIC_WRITE | GENERIC_READ, 0, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);


			HANDLE mapping = CreateFileMappingA(out, 0, PAGE_READWRITE, 0, sectionPointer, 0);

			u8 *view = (u8 *) MapViewOfFile(mapping, FILE_MAP_WRITE, 0, 0, 0);

			const auto syncWriteFile = [&](void *data, u32 size) {
				DWORD written;

				WriteFile(out, data, size, &written, 0);
			};

			const auto mappedWriteFile = [&](void *data, u32 size) {
				memcpy(view, data, size);
				view += size;
			};

			const auto doWrite = mappedWriteFile;

			const auto writeAllocator = [&](HANDLE out, BucketedArenaAllocator allocator) {
				for (auto bucket = allocator.first; bucket; bucket = bucket->next) {
					u32 count = (bucket->size - bucket->remaining);

					doWrite(bucket->memory - count, count);
				}
			};

			if (out == INVALID_HANDLE_VALUE) {
				reportError("Error: Could not open %s intermediate for writing", objectFileName);
				goto error;
			}
			
			doWrite(&header, sizeof(header));

			for (auto section : sections) {
				doWrite(section.header, sizeof(*section.header));
			}
			

			//assert(ftell(out) == header.pointerToSymbolTable);
			writeAllocator(out, symbols.allocator);

			if (printDiagnostics) {
				reportInfo("%u COFF symbols", symbols.count());
			}

			doWrite(&stringTableSize, sizeof(stringTableSize));
			writeAllocator(out, stringTable);

			doWrite(&alignmentPadding, AlignPO2(prefixSize, 4) - prefixSize);

			for (u32 i = 0; i < sections.count; i++) {
				auto section = sections[i];

				if (printDiagnostics) {
					reportInfo("%s size: %u", section.header->name, section.header->sizeOfRawData);
				}

				if (section.data) {

					//assert(section.header->pointerToRawData == ftell(out));
					writeAllocator(out, *section.data);


				}

				if (section.relocations) {
					u32 relocationCount = section.relocations->totalSize / sizeof(Relocation);

					if (printDiagnostics) {
						reportInfo("%s relocations: %u", section.header->name, relocationCount);
					}

					if (relocationCount > UINT16_MAX) {
						Relocation count;
						count.virtualAddress = relocationCount + 1;
						count.symbolTableIndex = 0;
						count.type = IMAGE_REL_AMD64_ABSOLUTE;

						doWrite(&count, sizeof(count));
					}
					//assert(section.header->pointerToRelocations == ftell(out));

					writeAllocator(out, *section.relocations);
				}
			}

			{
				PROFILE_ZONE("fclose");

				UnmapViewOfFile(view);
				CloseHandle(mapping);
				CloseHandle(out);
			}
		}
	}
error:
	;
}