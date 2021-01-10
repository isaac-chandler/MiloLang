#include "Basic.h"
#include "CoffWriter.h"
#include "BucketedArenaAllocator.h"
#include "Block.h"
#include "TypeTable.h"
#include "CompilerMain.h"
#include "Error.h"

union SymbolName {
	char name[8];
	struct {
		u32 zeroes;
		u32 namePointer;
	};
};

void setSectionName(char *header, u64 size, const char *name) {
	u64 len = strlen(name);
	assert(len <= size);

	memcpy(header, name, len);
	memset(header + len, 0, size - len);
}

void setSymbolName(BucketedArenaAllocator *stringTable, SymbolName *header, String name) {
	if (name.length <= sizeof(header->name)) {
		memcpy(header->name, name.characters, name.length);
		memset(header->name + name.length, 0, sizeof(header->name) - name.length);
	}
	else {
		header->zeroes = 0;
		header->namePointer = static_cast<u32>(stringTable->totalSize + 4);
		stringTable->addNullTerminatedString(name);
	}
}

void setSymbolName(BucketedArenaAllocator *stringTable, SymbolName *header, u64 value) {
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

	setSymbolName(stringTable, header, { buffer, characters + 1 });
}

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

u32 getRegisterOffset(ExprFunction *function, u32 regNo) {
	assert(regNo != 0);

	if (regNo > function->state.parameterSpace) {
		return (regNo - function->state.parameterSpace - 1 + function->state.callAuxStorage) * 8 + 16;
	}

	u32 registerCount = function->state.nextRegister - function->state.parameterSpace - 1 + function->state.callAuxStorage;

	u32 spaceToAllocate = (registerCount >> 1) * 16 + 8;

	return spaceToAllocate + regNo * 8 + 16;
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

void writeRSPOffsetByte(BucketedArenaAllocator *code, u8 physicalRegister, u32 offset) {
	if (offset >= 0x80) {
		code->add1Unchecked(0x84 | (physicalRegister << 3));
		code->add1Unchecked(0x24);
		code->add4Unchecked(offset);
	}
	else if (offset != 0) {
		code->add1Unchecked(0x44 | (physicalRegister << 3));
		code->add1Unchecked(0x24);
		code->add1Unchecked(static_cast<u8>(offset));
	}
	else {
		code->add1Unchecked(0x04 | (physicalRegister << 3));
		code->add1Unchecked(0x24);
	}
}

void writeRSPRegisterByte(BucketedArenaAllocator *code, ExprFunction *function, u8 physicalRegister, u32 stackRegister, u32 addition = 0) {
	writeRSPOffsetByte(code, physicalRegister, getRegisterOffset(function, stackRegister) + addition);
}

void loadIntoIntRegister(BucketedArenaAllocator *code, ExprFunction *function, u64 size, u8 loadInto, u32 regNo) {
	if (regNo == 0) {
		if (loadInto >= 8) {
			code->add1Unchecked(0x45);
			loadInto -= 8;
		}

		code->add1Unchecked(0x31);
		code->add1Unchecked(0xC0 | loadInto | (loadInto << 3));
		return;
	}

	u8 rex = 0x40;

	if (size == 8) {
		rex |= 8;
	}

	if (loadInto >= 8) {
		rex |= 4;
		loadInto -= 8;
	}

	if (size == 2) {
		code->add1Unchecked(0x66);
	}

	if (rex != 0x40) {
		code->add1Unchecked(rex);
	}

	if (size == 1) {
		code->add1Unchecked(0x8a);
	}
	else {
		code->add1Unchecked(0x8b);
	}

	writeRSPRegisterByte(code, function, loadInto, regNo);
}

void storeFromIntRegister(BucketedArenaAllocator *code, ExprFunction *function, u64 size, u32 regNo, u8 storeFrom) {
	u8 rex = 0x40;

	if (size == 8) {
		rex |= 8;
	}

	if (storeFrom >= 8) {
		rex |= 4;
		storeFrom -= 8;
	}

	if (size == 2) {
		code->add1Unchecked(0x66);
	}

	if (rex != 0x40) {
		code->add1Unchecked(rex);
	}

	if (size == 1) {
		code->add1Unchecked(0x88);
	}
	else {
		code->add1Unchecked(0x89);
	}

	writeRSPRegisterByte(code, function, storeFrom, regNo);
}

void loadIntoFloatRegister(BucketedArenaAllocator *code, ExprFunction *function, u64 size, u8 loadInto, u32 regNo) {
	if (regNo == 0) {
		if (regNo >= 8) {
			code->add1Unchecked(0x45);
			regNo -= 8;
		}

		code->add1Unchecked(0x0F);
		code->add1Unchecked(0x57);
		code->add1Unchecked(0xC0 | loadInto | (loadInto << 3));
		return;
	}

	if (size == 8) {
		code->add1Unchecked(0xF2);
	}
	else {
		code->add1Unchecked(0xF3);
	}

	if (loadInto >= 8) {
		code->add1Unchecked(0x44);
		loadInto -= 8;
	}

	code->add1Unchecked(0x0F);
	code->add1Unchecked(0x10);

	writeRSPRegisterByte(code, function, loadInto, regNo);
}

void storeFromFloatRegister(BucketedArenaAllocator *code, ExprFunction *function, u64 size, u32 regNo, u8 storeFrom) {
	if (size == 8) {
		code->add1Unchecked(0xF2);
	}
	else {
		code->add1Unchecked(0xF3);
	}

	if (storeFrom >= 8) {
		code->add1Unchecked(0x44);
		storeFrom -= 8;
	}

	code->add1Unchecked(0x0F);
	code->add1Unchecked(0x11);

	writeRSPRegisterByte(code, function, storeFrom, regNo);
}

void storeImmediate(BucketedArenaAllocator *code, ExprFunction *function, u64 size, u32 regNo, u64 immediate) {
	assert(isStandardSize(size));

	if (size == 8 && static_cast<s64>(immediate) != static_cast<s64>(static_cast<s32>(immediate))) {
		code->add1Unchecked(0x48); // mov rax, ir.a
		code->add1Unchecked(0xB8);
		code->add8Unchecked(immediate);

		storeFromIntRegister(code, function, 8, regNo, RAX);
	}
	else {
		if (size == 2) {
			code->add1Unchecked(0x66);
		}
		else if (size == 8) {
			code->add1Unchecked(0x48);
		}

		if (size == 1) {
			code->add1Unchecked(0xC6);
		}
		else {
			code->add1Unchecked(0xC7);
		}

		writeRSPRegisterByte(code, function, 0, regNo);

		if (size == 1) {
			code->add1Unchecked(static_cast<u8>(immediate));
		}
		else if (size == 2) {
			code->add2Unchecked(static_cast<u16>(immediate));
		}
		else if (size == 4 || size == 8) {
			code->add4Unchecked(static_cast<u32>(immediate));
		}
	}
}

void setCondition(BucketedArenaAllocator *code, ExprFunction *function, u32 dest, u8 condition) {
	code->add1Unchecked(0x0F);
	code->add1Unchecked(0x90 | condition);
	writeRSPRegisterByte(code, function, 0, dest);
}

void setConditionInt(BucketedArenaAllocator *code, ExprFunction *function, u64 size, u32 dest, u32 a, u32 b, u8 condition) {
	loadIntoIntRegister(code, function, size, RAX, a);
	loadIntoIntRegister(code, function, size, RCX, b);

	if (size == 8) {
		code->add1Unchecked(0x48);
	}
	else if (size == 2) {
		code->add1Unchecked(0x66);
	}

	if (size == 1) {
		code->add1Unchecked(0x38);
	}
	else {
		code->add1Unchecked(0x39);
	}

	code->add1Unchecked(0xC8);

	setCondition(code, function, dest, condition);
}

void setConditionFloat(BucketedArenaAllocator *code, ExprFunction *function, u64 size, u32 dest, u32 a, u32 b, u8 condition) {
	loadIntoFloatRegister(code, function, size, 0, a);
	loadIntoFloatRegister(code, function, size, 1, b);

	if (size == 8) {
		code->add1Unchecked(0x66);
	}

	code->add1Unchecked(0x0F);
	code->add1Unchecked(0x2F);
	code->add1Unchecked(0xC1);

	setCondition(code, function, dest, condition);
}

void loadImmediateIntoRAX(BucketedArenaAllocator *code, u64 immediate) {
	if (static_cast<s64>(immediate) != static_cast<s64>(static_cast<s32>(immediate))) {
		code->add1Unchecked(0x48);
		code->add1Unchecked(0xB8);
		code->add8Unchecked(immediate);
	}
	else {
		code->add1Unchecked(0x48);
		code->add1Unchecked(0xC7);
		code->add1Unchecked(0xC0);
		code->add4Unchecked(static_cast<u32>(immediate));
	}
}


void loadImmediateIntoIntRegister(BucketedArenaAllocator *code, u8 loadInto, u64 immediate) {
	if (loadInto == RAX) {
		loadImmediateIntoRAX(code, immediate);
	}
	else {
		if (immediate <= 0x7FFF'FFFF) {
			if (loadInto >= 8) {
				code->add1Unchecked(0x41);
				loadInto -= 8;
			}

			code->add1Unchecked(0xB8 | loadInto);
			code->add4Unchecked(static_cast<u32>(immediate));
		}
		else {
			loadImmediateIntoRAX(code, immediate);

			if (loadInto >= 8) {
				code->add1Unchecked(0x49);
				loadInto -= 8;
			}
			else {
				code->add1Unchecked(0x48);
			}

			code->add1Unchecked(0x89);
			code->add1Unchecked(0xC0 | loadInto);
		}
	}
}

void writeSet(BucketedArenaAllocator *code, ExprFunction *function, u64 size, u32 dest, u32 src) {
	if (src == 0) {
		if (isStandardSize(size)) {
			storeImmediate(code, function, size, dest, 0);
		}
		else {
			code->add1Unchecked(0x48);
			code->add1Unchecked(0x8D);
			writeRSPRegisterByte(code, function, RDI, dest);

			u64 count = size;

			if (size % 8 == 0) {
				count = size / 8;
			}

			loadImmediateIntoIntRegister(code, RCX, count);

			code->add1Unchecked(0x31); // xor eax, eax
			code->add1Unchecked(0xC0);

			code->add1Unchecked(0xF3);
			code->add1Unchecked(0x48);

			if (size % 8 == 0) {
				code->add1Unchecked(0xAB);
			}
			else {
				code->add1Unchecked(0xAA);
			}
		}
	}
	else {
		if (isStandardSize(size)) {
			loadIntoIntRegister(code, function, size, RAX, src);
			storeFromIntRegister(code, function, size, dest, RAX);
		}
		else {
			code->add1Unchecked(0x48);
			code->add1Unchecked(0x8D);
			writeRSPRegisterByte(code, function, RSI, src);

			code->add1Unchecked(0x48);
			code->add1Unchecked(0x8D);
			writeRSPRegisterByte(code, function, RDI, dest);

			u64 count = size;

			if (size % 8 == 0) {
				loadImmediateIntoIntRegister(code, RCX, size / 8);

				code->add1Unchecked(0xF3); // rep movsq
				code->add1Unchecked(0x48);
				code->add1Unchecked(0xA5);
			}
			else {
				loadImmediateIntoIntRegister(code, RCX, size);

				code->add1Unchecked(0xF3); // rep movsb
				code->add1Unchecked(0x48);
				code->add1Unchecked(0xA4);
			}
		}
	}
}

#define RDATA_SECTION_NUMBER 1
#define BSS_SECTION_NUMBER 2
#define DATA_SECTION_NUMBER 3
#define TEXT_SECTION_NUMBER 4
#define DEBUG_SYMBOL_SECTION_NUMBER 5
#define DEBUG_TYPE_SECTION_NUMBER 6
#define PDATA_SECTION_NUMBER 7
#define XDATA_SECTION_NUMBER 8

u32 *addRelocationToUnkownSymbol(BucketedArenaAllocator *allocator, u32 virtualAddress, u16 type) {
	allocator->ensure(10);
	allocator->add4Unchecked(virtualAddress);
	u32 *value = allocator->add4Unchecked(0);
	allocator->add2Unchecked(type);

	return value;
}

u32 createRdataPointer(BucketedArenaAllocator *stringTable, BucketArray<Symbol> *symbols, BucketedArenaAllocator *rdata) {
	Symbol symbol;
	setSymbolName(stringTable, &symbol.name, symbols->count());
	symbol.value = static_cast<u32>(rdata->totalSize);
	symbol.sectionNumber = RDATA_SECTION_NUMBER;
	symbol.type = 0;
	symbol.storageClass = IMAGE_SYM_CLASS_STATIC;
	symbol.numberOfAuxSymbols = 0;

	symbols->add(symbol);

	return symbols->count() - 1;
}

void addPointerRelocation(BucketedArenaAllocator *relocations, u32 address, u32 symbol) {
	relocations->ensure(10);
	relocations->add4Unchecked(address);
	relocations->add4Unchecked(symbol);
	relocations->add2Unchecked(IMAGE_REL_AMD64_ADDR64);
}

u32 createSymbolForFunction(BucketArray<Symbol> *symbols, ExprFunction *function) {
	if (!(function->flags & EXPR_HAS_STORAGE)) {
		function->flags |= EXPR_HAS_STORAGE;

		function->physicalStorage = static_cast<u32>(symbols->count());
		function->symbol = reinterpret_cast<Symbol *>(symbols->allocator.allocateUnaligned(sizeof(Symbol)));
	}

	return function->physicalStorage;
}

u32 createSymbolForString(s64 *emptyStringSymbolIndex, BucketArray<Symbol> *symbols, BucketedArenaAllocator *stringTable, BucketedArenaAllocator *rdata, ExprStringLiteral *string) {
	if (string->string.length == 0) {
		if (*emptyStringSymbolIndex == -1) {
			*emptyStringSymbolIndex = symbols->count();

			Symbol emptyString;
			setSymbolName(stringTable, &emptyString.name, "@emptyString");
			emptyString.value = static_cast<u32>(rdata->totalSize);
			emptyString.sectionNumber = RDATA_SECTION_NUMBER;
			emptyString.type = 0;
			emptyString.storageClass = IMAGE_SYM_CLASS_EXTERNAL;
			emptyString.numberOfAuxSymbols = 0;

			symbols->add(emptyString);

			rdata->add1(0);
		}

		return static_cast<u32>(*emptyStringSymbolIndex);
	}

	if (!(string->flags & EXPR_HAS_STORAGE)) {
		string->flags |= EXPR_HAS_STORAGE;

		string->physicalStorage = static_cast<u32>(symbols->count());
		string->symbol = reinterpret_cast<Symbol *>(symbols->allocator.allocateUnaligned(sizeof(Symbol)));

		setSymbolName(stringTable, &string->symbol->name, symbols->count());
		string->symbol->storageClass = IMAGE_SYM_CLASS_STATIC;
		string->symbol->value = static_cast<u32>(rdata->totalSize);
		string->symbol->sectionNumber = RDATA_SECTION_NUMBER;
		string->symbol->type = 0;
		string->symbol->numberOfAuxSymbols = 0;

		rdata->addNullTerminatedString(string->string);
	}

	return string->physicalStorage;
}

u32 createSymbolForType(BucketArray<Symbol> *symbols, Type *type) {
	if (!type->symbol) {
		type->physicalStorage = symbols->count();
		type->symbol = reinterpret_cast<Symbol *>(symbols->allocator.allocateUnaligned(sizeof(Symbol)));
	}

	return type->physicalStorage;
}


u32 createSymbolForDeclaration(BucketArray<Symbol> *symbols, Declaration *declaration) {
	if (!(declaration->flags & DECLARATION_HAS_STORAGE)) {
		declaration->flags |= DECLARATION_HAS_STORAGE;
		declaration->physicalStorage = symbols->count();
		declaration->symbol = reinterpret_cast<Symbol *>(symbols->allocator.allocateUnaligned(sizeof(Symbol)));
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


void writeValue(u32 dataSize, u8 *data, BucketedArenaAllocator *dataRelocations, BucketArray<Symbol> *symbols, BucketedArenaAllocator *stringTable, Expr *value, s64 *emptyStringSymbolIndex, BucketedArenaAllocator *rdata) {
	auto type = getTypeForExpr(value);


	assert(value->flavor == ExprFlavor::FLOAT_LITERAL ||
		value->flavor == ExprFlavor::INT_LITERAL ||
		value->flavor == ExprFlavor::FUNCTION ||
		value->flavor == ExprFlavor::STRING_LITERAL ||
		value->flavor == ExprFlavor::ARRAY ||
		value->flavor == ExprFlavor::TYPE_LITERAL || 
		value->flavor == ExprFlavor::STRUCT_DEFAULT);

	if (value->flavor == ExprFlavor::FUNCTION) {
		assert(type->size == 8);

		dataRelocations->ensure(10);
		dataRelocations->add4Unchecked(dataSize);
		dataRelocations->add4Unchecked(createSymbolForFunction(symbols, static_cast<ExprFunction *>(value)));
		dataRelocations->add2Unchecked(IMAGE_REL_AMD64_ADDR64);

		*reinterpret_cast<u64 *>(data) = 0;
	}
	else if (value->flavor == ExprFlavor::STRING_LITERAL) {

		u32 string = createSymbolForString(emptyStringSymbolIndex, symbols, stringTable, rdata, static_cast<ExprStringLiteral *>(value));


		assert(type->size == 8);
		dataRelocations->ensure(10);
		dataRelocations->add4Unchecked(dataSize);
		dataRelocations->add4Unchecked(string);
		dataRelocations->add2Unchecked(IMAGE_REL_AMD64_ADDR64);

		reinterpret_cast<u64 *>(data)[0] = 0;
		reinterpret_cast<u64 *>(data)[1] = static_cast<ExprStringLiteral *>(value)->string.length;
	}
	else if (value->flavor == ExprFlavor::ARRAY) {
		auto array = static_cast<ExprArray *>(value);

		if (value->type->flags & TYPE_ARRAY_IS_FIXED) {
			auto elementSize = static_cast<TypeArray *>(type)->arrayOf->size;
			
			for (u64 i = 0; i < array->count; i++) {
				writeValue(dataSize, data, dataRelocations, symbols, stringTable, array->storage[i], emptyStringSymbolIndex, rdata);
				
				if (i + 1 < array->count && array->storage[i + 1] == nullptr) {
					for (u64 j = i + 1; j < array->count; j++) {
						writeValue(dataSize, data, dataRelocations, symbols, stringTable, array->storage[i], emptyStringSymbolIndex, rdata);

						dataSize += elementSize;
						data += elementSize;
					}

					break;
				}

				dataSize += elementSize;
				data += elementSize;
			}
		}
		else {
			assert(array->count == 0);

			memset(data, 0, type->size);
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
		dataRelocations->ensure(10);
		dataRelocations->add4Unchecked(dataSize);
		dataRelocations->add4Unchecked(createSymbolForType(symbols, static_cast<ExprLiteral *>(value)->typeValue));
		dataRelocations->add2Unchecked(IMAGE_REL_AMD64_ADDR64);

		*reinterpret_cast<u64 *>(data) = 0;
	}
	else if (value->flavor == ExprFlavor::STRUCT_DEFAULT) {
		auto struct_ = static_cast<TypeStruct *>(type);

		for (auto member : struct_->members.declarations) {
			if (member->flags & (DECLARATION_IS_UNINITIALIZED | DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING)) continue;


			writeValue(dataSize + member->physicalStorage, data + member->physicalStorage, dataRelocations, symbols, stringTable,
				member->initialValue, emptyStringSymbolIndex, rdata);
		}
	}
	else {
		assert(false);
	}
}

void alignAllocator(BucketedArenaAllocator *allocator, u64 alignment) {
	u64 padding = 0;

	allocator->add(&padding, AlignPO2(allocator->totalSize, alignment) - allocator->totalSize);
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

const u32 S_UDT = 0x1108;
const u32 T_VOID = 0x0003;
const u32 T_64PVOID = 0x0603;
const u32 T_64PUCHAR = 0x0620;
const u32 T_INT1 = 0x0068;
const u32 T_64PINT1 = 0x0668;
const u32 T_UINT1 = 0x0069;
const u32 T_64PUINT1 = 0x0669;
const u32 T_INT2 = 0x0072;
const u32 T_64PINT2 = 0x0672;
const u32 T_UINT2 = 0x0073;
const u32 T_64PUINT2 = 0x0673;
const u32 T_INT4 = 0x0074;
const u32 T_64PINT4 = 0x0674;
const u32 T_UINT4 = 0x0075;
const u32 T_64PUINT4 = 0x0675;
const u32 T_INT8 = 0x0076;
const u32 T_64PINT8 = 0x0676;
const u32 T_UINT8 = 0x0077;
const u32 T_64PUINT8 = 0x0677;
const u32 T_REAL32 = 0x0040;
const u32 T_64PREAL32 = 0x0640;
const u32 T_REAL64 = 0x0041;
const u32 T_64PREAL64 = 0x0641;
const u32 T_BOOL08 = 0x0030;
const u32 T_64PBOOL08 = 0x0630;


u32 getCoffTypeIndex(BucketedArenaAllocator *debugTypes, Type *type);

u32 debugTypeId = 0x1000;

void alignDebugTypes(BucketedArenaAllocator *debugTypes) {
	u32 padding = AlignPO2(debugTypes->totalSize, 4) - debugTypes->totalSize;

	debugTypes->ensure(3);
	for (u32 i = 0; i < padding; i++) {
		debugTypes->add1Unchecked(0xF0 + padding - i);
	}

	assert(!(debugTypes->totalSize & 3));
}

struct DebugLeaf {
	BucketedArenaAllocator *debugTypes;

	DebugLeaf(BucketedArenaAllocator *debugTypes) : debugTypes(debugTypes) {}

	DebugLeaf(BucketedArenaAllocator &debugTypes) : debugTypes(&debugTypes) {}

	template<typename T>
	u32 operator+(T t) {
		u16 *patch = debugTypes->add2(0);
		u32 start = debugTypes->totalSize;
		t();
		alignDebugTypes(debugTypes);
		*patch = static_cast<u16>(debugTypes->totalSize - start);
		return debugTypeId++;
	}
};

#define DEBUG_LEAF DebugLeaf(debugTypes) + [&]()

void addStructUniqueName(BucketedArenaAllocator *debugTypes, u32 name = debugTypeId) {
	debugTypes->add1('@');

	char buffer[32];
	
	_itoa(static_cast<int>(name - 0x1000), buffer, 16);

	debugTypes->addNullTerminatedString(buffer);
}


bool appendCoffName(BucketedArenaAllocator *debugSymbols, Type *type) {
	if (type->flavor == TypeFlavor::ARRAY) {
		auto array = static_cast<TypeArray *>(type);

		if (array->flags & TYPE_ARRAY_IS_FIXED) {
			appendCoffName(debugSymbols, array->arrayOf);
			debugSymbols->addString(" [");

			char buffer[16];
			_itoa(array->count, buffer, 10);

			debugSymbols->addString(buffer);
			debugSymbols->add1(']');
		}
		else if (array->flags & TYPE_ARRAY_IS_DYNAMIC) {
			debugSymbols->addString("Dynamic_Array<");
			if (appendCoffName(debugSymbols, array->arrayOf)) {
				debugSymbols->add1(' ');
			}

			debugSymbols->add1('>');
			return true;
		}
		else {
			debugSymbols->addString("Array<");
			if (appendCoffName(debugSymbols, array->arrayOf)) {
				debugSymbols->add1(' ');
			}

			debugSymbols->add1('>');
			return true;
		}
	}
	else {
		debugSymbols->addString(type->name);
	}

	return false;
}


void emitUDT(BucketedArenaAllocator *debugSymbols, Type *type) {
	debugSymbols->ensure(8);
	u16 *size = debugSymbols->add2Unchecked(0);
	u32 start = debugSymbols->totalSize;
	debugSymbols->add2Unchecked(S_UDT);
	debugSymbols->add4Unchecked(type->codeviewTypeIndex);
	appendCoffName(debugSymbols, type);
	debugSymbols->add1(0);
	*size = static_cast<u16>(debugSymbols->totalSize - start);
}

u32 createCoffType(BucketedArenaAllocator *debugTypes, Type *type) {
	if (type == &TYPE_BOOL) {
		return T_BOOL08;
	}
	else if (type == &TYPE_VOID) {
		return T_VOID;
	}
	else if (type == &TYPE_F32) {
		return T_REAL32;
	}
	else if (type == &TYPE_F64) {
		return T_REAL64;
	}
	else if (type == &TYPE_S8) {
		return T_INT1;
	}
	else if (type == &TYPE_S16) {
		return T_INT2;
	}
	else if (type == &TYPE_S32) {
		return T_INT4;
	}
	else if (type == &TYPE_S64) {
		return T_INT8;
	}
	else if (type == &TYPE_U8) {
		return T_UINT1;
	}
	else if (type == &TYPE_U16) {
		return T_UINT2;
	}
	else if (type == &TYPE_U32) {
		return T_UINT4;
	}
	else if (type == &TYPE_U64) {
		return T_UINT8;
	}
	else if (type->flavor == TypeFlavor::POINTER) {
		auto pointer = static_cast<TypePointer *>(type);

		auto indexOfType = getCoffTypeIndex(debugTypes, pointer->pointerTo);


		// @Incomplete change 0x100 to a more concrete value
		if (indexOfType < 0x100) {
			return indexOfType | 0x600;
		}

		return DEBUG_LEAF{
			debugTypes->ensure(2);
			debugTypes->add2Unchecked(0x1002); // LF_POINTER
			debugTypes->add4Unchecked(indexOfType);
			debugTypes->add4Unchecked(0x1000c); // 64 bit normal pointer
		};
	}
	else if (type->flavor == TypeFlavor::STRING) {
		auto fieldList = DEBUG_LEAF{
			debugTypes->ensure(12);
			debugTypes->add2Unchecked(0x1203); // LF_FIELDLIST

			debugTypes->add2Unchecked(0x150D); // LF_MEMBER
			debugTypes->add2Unchecked(0x3); // public
			debugTypes->add4Unchecked(T_64PUCHAR);
			debugTypes->add2Unchecked(0); // offset 0
			debugTypes->addNullTerminatedString("data");
			alignDebugTypes(debugTypes);

			debugTypes->ensure(10);
			debugTypes->add2Unchecked(0x150D); // LF_MEMBER
			debugTypes->add2Unchecked(0x3); // public
			debugTypes->add4Unchecked(T_UINT8);
			debugTypes->add2Unchecked(8); // offset 8
			debugTypes->addNullTerminatedString("count");
		};

		return DEBUG_LEAF{
			debugTypes->ensure(20);
			debugTypes->add2Unchecked(0x1505); // LF_STRUCTURE
			debugTypes->add2Unchecked(2); // 2 members
			debugTypes->add2Unchecked(0x200); // Has a unique name
			debugTypes->add4Unchecked(fieldList); // field list
			debugTypes->add4Unchecked(0); // super class
			debugTypes->add4Unchecked(0); // vtable
			debugTypes->add2Unchecked(16);
			debugTypes->addNullTerminatedString("string");
			addStructUniqueName(debugTypes);
		};
	}
	else if (type->flavor == TypeFlavor::ARRAY) {
		auto array = static_cast<TypeArray *>(type);

		if (array->flags & TYPE_ARRAY_IS_FIXED) {
			u32 arrayOf = getCoffTypeIndex(debugTypes, array->arrayOf);

			return DEBUG_LEAF{
				debugTypes->ensure(17);
				debugTypes->add2Unchecked(0x1503); // LF_ARRAY
				debugTypes->add4Unchecked(arrayOf);
				debugTypes->add4Unchecked(T_INT8);
				debugTypes->add2Unchecked(0x8004); // LF_ULONG
				debugTypes->add4Unchecked(array->size);
				debugTypes->add1Unchecked(0); // No name
			};
		}
		else {
			u32 dataType = getCoffTypeIndex(debugTypes, static_cast<ExprLiteral *>(array->members.declarations[0]->type)->typeValue);

			u32 fieldList = DEBUG_LEAF{
				debugTypes->ensure(12);
				debugTypes->add2Unchecked(0x1203); // LF_FIELDLIST

				debugTypes->add2Unchecked(0x150D); // LF_MEMBER
				debugTypes->add2Unchecked(0x3); // public
				debugTypes->add4Unchecked(dataType);
				debugTypes->add2Unchecked(0); // offset 0
				debugTypes->addNullTerminatedString("data");
				alignDebugTypes(debugTypes);

				debugTypes->ensure(10);
				debugTypes->add2Unchecked(0x150D); // LF_MEMBER
				debugTypes->add2Unchecked(0x3); // public
				debugTypes->add4Unchecked(T_UINT8);
				debugTypes->add2Unchecked(8); // offset 8
				debugTypes->addNullTerminatedString("count");

				if (array->flags & TYPE_ARRAY_IS_DYNAMIC) {
					alignDebugTypes(debugTypes);

					debugTypes->ensure(10);
					debugTypes->add2Unchecked(0x150D); // LF_MEMBER
					debugTypes->add2Unchecked(0x3); // public
					debugTypes->add4Unchecked(T_UINT8);
					debugTypes->add2Unchecked(16); // offset 8
					debugTypes->addNullTerminatedString("capacity");
				}
			};

			return DEBUG_LEAF{
				debugTypes->ensure(20);
				debugTypes->add2Unchecked(0x1505); // LF_STRUCTURE
				debugTypes->add2Unchecked(array->flags & TYPE_ARRAY_IS_DYNAMIC ? 3 : 2); // 2 members
				debugTypes->add2Unchecked(0x200); // Has a unique name
				debugTypes->add4Unchecked(fieldList); // field list
				debugTypes->add4Unchecked(0); // super class
				debugTypes->add4Unchecked(0); // vtable
				debugTypes->add2Unchecked(array->size);
				appendCoffName(debugTypes, array);
				debugTypes->add1(0);
				addStructUniqueName(debugTypes);
			};
		}
	}
	else if (type->flavor == TypeFlavor::FUNCTION) {
		auto function = static_cast<TypeFunction *>(type);

		for (u32 i = 0; i < function->argumentCount; i++) {
			getCoffTypeIndex(debugTypes, function->argumentTypes[i]);
		}

		for (u32 i = 0; i < function->returnCount; i++) {
			getCoffTypeIndex(debugTypes, function->returnTypes[i]);
		}

		u32 firstReturn = debugTypeId;

		for (u32 i = 1; i < function->returnCount; i++) {
			DEBUG_LEAF{
				debugTypes->ensure(10);
				debugTypes->add2Unchecked(0x1002); // LF_POINTER
				debugTypes->add4Unchecked(function->returnTypes[i]->codeviewTypeIndex);
				debugTypes->add4Unchecked(0x1000c); // 64 bit normal pointer
			};
		}

		u32 argList = DEBUG_LEAF{
			debugTypes->ensure(6 + (function->argumentCount + function->returnCount - 1) * 4);
			debugTypes->add2Unchecked(0x1201); // LF_ARGLIST
			debugTypes->add4Unchecked(function->argumentCount + function->returnCount - 1);

			for (u32 i = 0; i < function->argumentCount; i++) {
				debugTypes->add4Unchecked(function->argumentTypes[i]->codeviewTypeIndex);
			}

			for (u32 i = 1;	i < function->returnCount; i++) {
				debugTypes->add4Unchecked(firstReturn + i - 1);
			}
		};

		u32 functionType = DEBUG_LEAF{
			debugTypes->ensure(14);
			debugTypes->add2Unchecked(0x1008); // LF_PROCEDURE
			debugTypes->add4Unchecked(function->returnTypes[0]->codeviewTypeIndex);
			debugTypes->add1Unchecked(0); // C near call
			debugTypes->add1Unchecked(0);
			debugTypes->add2Unchecked(function->argumentCount + function->returnCount - 1); // 0 parameters
			debugTypes->add4Unchecked(argList);
		};

		return DEBUG_LEAF {
			debugTypes->ensure(10);
			debugTypes->add2Unchecked(0x1002); // LF_POINTER
			debugTypes->add4Unchecked(functionType);
			debugTypes->add4Unchecked(0x1000c); // 64 bit normal pointer
		};
	}
	else if (type->flavor == TypeFlavor::ENUM) {
		auto enumeration = static_cast<TypeEnum *>(type);

		u32 integerType = getCoffTypeIndex(debugTypes, enumeration->integerType);
		
		// Use a struct with bitfields to represent a flags enum
		if (enumeration->flags & TYPE_ENUM_IS_FLAGS) {
			u32 firstFlag = debugTypeId;

			for (auto declaration : enumeration->values->declarations) {
				assert(declaration->initialValue->flavor == ExprFlavor::INT_LITERAL);
				assert(!(declaration->initialValue->type->flags & TYPE_INTEGER_IS_SIGNED));
				
				u64 value = static_cast<ExprLiteral *>(declaration->initialValue)->unsignedValue;

				if (value && !(value & value - 1)) { // If exactly one bit is set
					unsigned long bit;

					_BitScanForward64(&bit, value);
					
					DEBUG_LEAF{
						debugTypes->ensure(8);
						debugTypes->add2Unchecked(0x1205); // LF_BITFIELD
						debugTypes->add4Unchecked(integerType);
						debugTypes->add1Unchecked(1); // 1 bit
						debugTypes->add1Unchecked(static_cast<u8>(bit));
					};
				}
			}

			u32 flagCount = 0;

			u32 fieldList = DEBUG_LEAF{
				debugTypes->add2(0x1203); // LF_FIELDLIST

				for (auto declaration : enumeration->values->declarations) {

					u64 value = static_cast<ExprLiteral *>(declaration->initialValue)->unsignedValue;

					if (value && !(value & value - 1)) { // If exactly one bit is set
						alignDebugTypes(debugTypes);

						debugTypes->ensure(10);
						debugTypes->add2Unchecked(0x150D); // LF_MEMBER
						debugTypes->add2Unchecked(0x3); // public
						debugTypes->add4Unchecked(firstFlag + flagCount++);
						debugTypes->add2Unchecked(0); // offset 0
						debugTypes->addNullTerminatedString(declaration->name);
					}
				}
			};

			return DEBUG_LEAF{
				debugTypes->ensure(20);
				debugTypes->add2Unchecked(0x1505); // LF_STRUCTURE
				debugTypes->add2Unchecked(flagCount);
				debugTypes->add2Unchecked(0x200); // Has a unique name
				debugTypes->add4Unchecked(fieldList); // field list
				debugTypes->add4Unchecked(0); // super class
				debugTypes->add4Unchecked(0); // vtable
				debugTypes->add2Unchecked(enumeration->size);
				debugTypes->addNullTerminatedString(enumeration->name);
				addStructUniqueName(debugTypes);
			};
		}
		else {
			u32 fieldList = DEBUG_LEAF {
				debugTypes->add4(0x1203); // LF_FIELDLIST

				for (auto declaration : enumeration->values->declarations) {
					assert(declaration->initialValue->flavor == ExprFlavor::INT_LITERAL);
					assert(!(declaration->initialValue->type->flags & TYPE_INTEGER_IS_SIGNED));

					alignDebugTypes(debugTypes);
					debugTypes->ensure(14);
					debugTypes->add2Unchecked(0x1502); // LF_ENUMERATE
					debugTypes->add2Unchecked(0x3); // Public
					debugTypes->add2Unchecked(0x800A); // LF_UQUADWORD
					debugTypes->add8Unchecked(static_cast<ExprLiteral *>(declaration->initialValue)->unsignedValue);
					debugTypes->addNullTerminatedString(declaration->name);
				}
			};

			return DEBUG_LEAF{
				debugTypes->ensure(14);
				debugTypes->add2Unchecked(0x1507); // LF_ENUM
				debugTypes->add2Unchecked(static_cast<u16>(enumeration->values->declarations.count));
				debugTypes->add2Unchecked(0x200); // Has a unique name
				debugTypes->add4Unchecked(integerType);
				debugTypes->add4Unchecked(fieldList);
				debugTypes->addNullTerminatedString(enumeration->name);
				addStructUniqueName(debugTypes);
			};
		}
	}
	else if (type->flavor == TypeFlavor::TYPE) {
		u32 fieldList = DEBUG_LEAF{
			debugTypes->ensure(12);
			debugTypes->add2Unchecked(0x1203); // LF_FIELDLIST

			debugTypes->add2Unchecked(0x150D); // LF_MEMBER
			debugTypes->add2Unchecked(0x3); // public
			debugTypes->add4Unchecked(T_64PVOID);
			debugTypes->add2Unchecked(0); // offset 0
			debugTypes->addNullTerminatedString("value");
		};

		return DEBUG_LEAF{
			debugTypes->ensure(20);
			debugTypes->add2Unchecked(0x1505); // LF_STRUCTURE
			debugTypes->add2Unchecked(1); // 1 member
			debugTypes->add2Unchecked(0x200); // Has a unique name
			debugTypes->add4Unchecked(fieldList); // field list
			debugTypes->add4Unchecked(0); // super class
			debugTypes->add4Unchecked(0); // vtable
			debugTypes->add2Unchecked(8);
			debugTypes->addNullTerminatedString("type");
			addStructUniqueName(debugTypes);
		};
	}
	else if (type->flavor == TypeFlavor::STRUCT) {
		auto structure = static_cast<TypeStruct *>(type);

		u16 packed = structure->flags & TYPE_STRUCT_IS_PACKED ? 1 : 0;

		structure->codeviewTypeIndex = DEBUG_LEAF{
			debugTypes->ensure(20);
			debugTypes->add2Unchecked(0x1505); // LF_STRUCTURE
			debugTypes->add2Unchecked(0); // 1 member
			debugTypes->add2Unchecked(0x280 | packed); // Has a unique name and is a forward declaration
			debugTypes->add4Unchecked(0); // field list
			debugTypes->add4Unchecked(0); // super class
			debugTypes->add4Unchecked(0); // vtable
			debugTypes->add2Unchecked(0); // size
			debugTypes->addNullTerminatedString(structure->name);
			addStructUniqueName(debugTypes);
		};

		for (auto member : structure->members.declarations) {
			if (member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING)) continue;

			auto type = static_cast<ExprLiteral *>(member->type)->typeValue;

			getCoffTypeIndex(debugTypes, type);
		}

		u32 memberCount = 0;

		u32 fieldList = DEBUG_LEAF{
			debugTypes->add2(0x1203); // LF_FIELDLIST

			auto struct_ = static_cast<TypeStruct *>(type);

			for (auto member : struct_->members.declarations) {
				if (member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING)) continue;

				auto type = static_cast<ExprLiteral *>(member->type)->typeValue;

				alignDebugTypes(debugTypes);
				debugTypes->ensure(10);
				debugTypes->add2Unchecked(0x150D); // LF_MEMBER
				debugTypes->add2Unchecked(0x3); // public
				debugTypes->add4Unchecked(type->codeviewTypeIndex);
				debugTypes->add2Unchecked(member->physicalStorage); // offset
				debugTypes->addNullTerminatedString(member->name);

				memberCount++;
			}
		};

		bool isUnion = structure->flags & TYPE_STRUCT_IS_UNION ? true : false;

		return DEBUG_LEAF{
			debugTypes->ensure(20);
			debugTypes->add2Unchecked(isUnion ? 0x1506 : 0x1505); //LF_UNION : LF_STRUCTURE
			debugTypes->add2Unchecked(memberCount);
			debugTypes->add2Unchecked(0x200 | packed); // Has a unique name
			debugTypes->add4Unchecked(fieldList); // field list
			debugTypes->add4Unchecked(0); // super class
			debugTypes->add4Unchecked(0); // vtable
			debugTypes->add2Unchecked(structure->size);
			debugTypes->addNullTerminatedString(structure->name);
			addStructUniqueName(debugTypes, structure->codeviewTypeIndex); // Use the same unique name as the forward declaration
		};
	}
	
	// Unhandled case
	assert(false);

	return 0;
}

u32 getCoffTypeIndex(BucketedArenaAllocator *debugTypes, Type *type) {
	if (!type->codeviewTypeIndex)
		type->codeviewTypeIndex = createCoffType(debugTypes, type);

	return type->codeviewTypeIndex;
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

void runCoffWriter() {
	PROFILE_FUNC();
	if (hadError)
		return;

	BucketedArenaAllocator code(65536);
	BucketedArenaAllocator codeRelocations(65536);
	BucketedArenaAllocator data(65536);
	BucketedArenaAllocator dataRelocations(65536);
	BucketedArenaAllocator rdata(65536);
	BucketedArenaAllocator rdataRelocations(65536);
	BucketedArenaAllocator stringTable(65536);
	BucketedArenaAllocator debugSymbols(65536);
	BucketedArenaAllocator debugSymbolsRelocations(65536);
	BucketedArenaAllocator debugTypes(65536);
	BucketedArenaAllocator pdata(65536);
	BucketedArenaAllocator pdataRelocations(65536);
	BucketedArenaAllocator xdata(65536);

	SectionHeader bssSection = {};
	bssSection.virtualSize = 0;
	BucketArray<Symbol> symbols;

	s64 f32ToU64ConstantSymbolIndex = -1;
	s64 f64ToU64ConstantSymbolIndex = -1;
	s64 emptyStringSymbolIndex = -1;

	u64 alignmentPadding = 0;

	Array<u64> instructionOffsets;
	Array<JumpPatch> jumpPatches;

	Array<LineInfo> lineInfo;
	Array<ColumnInfo> columnInfo;
	Array<u32 *> blockOffsetStack;

	u32 textSectionSymbolIndex = symbols.count();

	Symbol textSectionSymbol;
	setSymbolName(&stringTable, &textSectionSymbol.name, ".text");
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
		compileFlags.flags.iLanguage = 1; // @Cleanup Check no other language uses this
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

			createSymbolForFunction(&symbols, function);

			if (function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
				assert(function->valueOfDeclaration);
				auto symbol = function->symbol;

				if (symbol) {
					setSymbolName(&stringTable, &symbol->name, function->valueOfDeclaration->name);

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

				if (function->valueOfDeclaration) {
					setSymbolName(&stringTable, &symbol->name, function->valueOfDeclaration->name);

					if (function->valueOfDeclaration->enclosingScope->flavor == BlockFlavor::GLOBAL) {
						symbol->storageClass = IMAGE_SYM_CLASS_EXTERNAL;
					}
					else {
						symbol->storageClass = IMAGE_SYM_CLASS_STATIC;
					}
				}
				else {
					symbol->storageClass = IMAGE_SYM_CLASS_STATIC;

					setSymbolName(&stringTable, &symbol->name, symbols.count());
				}

				symbol->value = static_cast<u32>(code.totalSize);
				symbol->sectionNumber = TEXT_SECTION_NUMBER;
				symbol->type = 0x20;


				symbol->numberOfAuxSymbols = 0;
			}

			u32 registerCount = function->state.nextRegister - function->state.parameterSpace - 1 + function->state.callAuxStorage;

			u32 spaceToAllocate = (registerCount >> 1) * 16 + 8;

			debugSymbols.ensure(8);
			debugSymbols.add4Unchecked(0xF1);
			auto subsectionSizePatch = debugSymbols.add4Unchecked(0);
			u32 subsectionOffset = debugSymbols.totalSize;

			auto name = function->valueOfDeclaration ? function->valueOfDeclaration->name : "__unnamed";

			// @Volatile: This relies on the LF_PROCEDURE node being directly before the LF_POINTER node that defines the function pointer type
			u32 funcType = getCoffTypeIndex(&debugTypes, function->type) - 1;

			u32 funcId = DEBUG_LEAF{
				debugTypes.ensure(10);
				debugTypes.add2Unchecked(0x1601); // LF_FUNC_ID
				debugTypes.add4Unchecked(0);
				debugTypes.add4Unchecked(funcType);
				debugTypes.addNullTerminatedString(name);
			};

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
			debugSymbols.add4Unchecked(funcId);

			debugSymbolsRelocations.add4Unchecked(debugSymbols.totalSize);
			debugSymbolsRelocations.add4Unchecked(function->physicalStorage);
			debugSymbolsRelocations.add2Unchecked(IMAGE_REL_AMD64_SECREL);

			debugSymbols.add4Unchecked(0);

			debugSymbolsRelocations.add4Unchecked(debugSymbols.totalSize);
			debugSymbolsRelocations.add4Unchecked(textSectionSymbolIndex);
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

			u32 paramOffset;

			code.ensure(256);

			if (!isStandardSize(static_cast<ExprLiteral *>(function->returns.declarations[0]->type)->typeValue->size)) {
				paramOffset = 1;

				code.add1Unchecked(0x48); // mov qword ptr[rsp + 8], rcx
				code.add1Unchecked(0x89);
				code.add1Unchecked(0x4C);
				code.add1Unchecked(0x24);
				code.add1Unchecked(0x08);
			}
			else {
				paramOffset = 0;
			}

			for (u32 i = 0; i < function->arguments.declarations.count; i++) {
				auto argument = function->arguments.declarations[i];
				REGREL32 argumentInfo;
				argumentInfo.off = getRegisterOffset(function, i + 1 + paramOffset);
				argumentInfo.typind = getCoffTypeIndex(&debugTypes, static_cast<ExprLiteral *>(argument->type)->typeValue);

				debugSymbols.ensure(2 + sizeof(argumentInfo));
				debugSymbols.add2Unchecked(static_cast<u16>(sizeof(argumentInfo) + 1 + argument->name.length));
				debugSymbols.addUnchecked(&argumentInfo, sizeof(argumentInfo));
				debugSymbols.addNullTerminatedString(argument->name);
			}

			constexpr u8 intRegisters[4] = { 0x4C, 0x54, 0x44, 0x4C };

			for (u32 i = 0; i < my_min(4 - paramOffset, function->arguments.declarations.count + function->returns.declarations.count - 1); i++) {
				Type *type;

				if (i < function->arguments.declarations.count) {
					type = static_cast<ExprLiteral *>(function->arguments.declarations[i]->type)->typeValue;
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

			// sub rsp, spaceToAllocate
			if (spaceToAllocate < 0x80) {
				code.add1Unchecked(0x48);
				code.add1Unchecked(0x83);
				code.add1Unchecked(0xEC);
				code.add1Unchecked(static_cast<u8>(spaceToAllocate));
			}
			else {
				code.add1Unchecked(0x48);
				code.add1Unchecked(0x81);
				code.add1Unchecked(0xEC);
				code.add4Unchecked(static_cast<u32>(spaceToAllocate));
			}
			u32 subRspOffset = code.totalSize - functionStart;

			u32 functionPreambleEnd = code.totalSize - functionStart;
			*functionPreambleEndPatch = functionPreambleEnd;

			for (u32 i = 0; i < function->arguments.declarations.count; i++) {
				auto type = static_cast<ExprLiteral *>(function->arguments.declarations[i]->type)->typeValue;

				if (!isStandardSize(type->size)) {
					loadIntoIntRegister(&code, function, 8, RSI, i + 1 + paramOffset);

					code.add1Unchecked(0x48);
					code.add1Unchecked(0x8D);
					writeRSPRegisterByte(&code, function, RDI, function->arguments.declarations[i]->physicalStorage);

					if (type->size % 8 == 0) {
						loadImmediateIntoIntRegister(&code, RCX, type->size / 8);

						code.add1Unchecked(0xF3); //  rep movsq
						code.add1Unchecked(0x48);
						code.add1Unchecked(0xA5);
					}
					else {
						loadImmediateIntoIntRegister(&code, RCX, type->size);

						code.add1Unchecked(0xF3); //  rep movsb
						code.add1Unchecked(0x48);
						code.add1Unchecked(0xA4);
					}
				}

				code.ensure(64);
			}

			for (u32 index = 0; index < function->state.ir.count; index++) {
				auto &ir = function->state.ir[index];

				instructionOffsets.add(code.totalSize);

				code.ensure(128);

				switch (ir.op) {
				case IrOp::TYPE: {
					code.add1Unchecked(0x48);
					code.add1Unchecked(0xB8);

					codeRelocations.ensure(10);
					codeRelocations.add4Unchecked(code.totalSize);
					codeRelocations.add4Unchecked(createSymbolForType(&symbols, ir.type));
					codeRelocations.add2Unchecked(IMAGE_REL_AMD64_ADDR64);

					code.add8Unchecked(0);

					storeFromIntRegister(&code, function, 8, ir.dest, RAX);

					break;
				}
				case IrOp::ADD: {
					if (ir.a == 0 && ir.b == 0) {
						code.add1Unchecked(0x31); // xor eax, eax
						code.add1Unchecked(0xC0);

						storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
					}
					if (ir.a == 0) {
						writeSet(&code, function, ir.opSize, ir.dest, ir.b);
					}
					else if (ir.b == 0) {
						writeSet(&code, function, ir.opSize, ir.dest, ir.a);
					}
					else {
						if (ir.flags & IR_FLOAT_OP) {
							loadIntoFloatRegister(&code, function, ir.opSize, 0, ir.a);

							if (ir.opSize == 8) {
								code.add1Unchecked(0xF2);
							}
							else {
								code.add1Unchecked(0xF3);
							}

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0x58);
							writeRSPRegisterByte(&code, function, 0, ir.b);

							storeFromFloatRegister(&code, function, ir.opSize, ir.dest, 0);
						}
						else {
							loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);

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

							writeRSPRegisterByte(&code, function, RAX, ir.b);

							storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
						}
					}
				} break;
				case IrOp::ADD_CONSTANT: {
					if (ir.a == 0) {
						storeImmediate(&code, function, ir.opSize, ir.dest, ir.immediate);
					}
					else if (ir.b == 0) {
						writeSet(&code, function, ir.opSize, ir.dest, ir.a);
					}
					else {
						loadImmediateIntoRAX(&code, ir.immediate);

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

						writeRSPRegisterByte(&code, function, RAX, ir.a);

						storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
					}
				} break;
				case IrOp::SUB: {
					if (ir.b == 0) {
						writeSet(&code, function, ir.opSize, ir.dest, ir.a);
					}
					else {
						if (ir.flags & IR_FLOAT_OP) {
							loadIntoFloatRegister(&code, function, ir.opSize, 0, ir.a);

							if (ir.opSize == 8) {
								code.add1Unchecked(0xF2);
							}
							else {
								code.add1Unchecked(0xF3);
							}

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0x5C);

							writeRSPRegisterByte(&code, function, 0, ir.b);

							storeFromFloatRegister(&code, function, ir.opSize, ir.dest, 0);
						}
						else {
							loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);

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

							writeRSPRegisterByte(&code, function, RAX, ir.b);

							storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
						}
					}
				} break;
				case IrOp::MUL: {
					if (ir.a == 0 || ir.b == 0) {
						storeImmediate(&code, function, ir.opSize, ir.dest, 0);
					}
					else {
						if (ir.flags & IR_FLOAT_OP) {
							loadIntoFloatRegister(&code, function, ir.opSize, 0, ir.a);

							if (ir.opSize == 8) {
								code.add1Unchecked(0xF2);
							}
							else {
								code.add1Unchecked(0xF3);
							}

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0x59);

							writeRSPRegisterByte(&code, function, 0, ir.b);

							storeFromFloatRegister(&code, function, ir.opSize, ir.dest, 0);
						}
						else {
							if (ir.opSize == 1) {
								loadIntoIntRegister(&code, function, 1, RAX, ir.a);

								code.add1Unchecked(0xF6);
								writeRSPRegisterByte(&code, function, 5, ir.b);

								storeFromIntRegister(&code, function, 1, ir.dest, RAX);
							}
							else {
								loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);

								if (ir.opSize == 8) {
									code.add1Unchecked(0x48);
								}
								else if (ir.opSize == 2) {
									code.add1Unchecked(0x66);
								}

								code.add1Unchecked(0x0F);
								code.add1Unchecked(0xAF);
								writeRSPRegisterByte(&code, function, RAX, ir.b);

								storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
							}
						}
					}
				} break;
				case IrOp::MUL_BY_CONSTANT: {
					assert(ir.opSize == 8);

					if (ir.a == 0 || ir.b == 0) {
						storeImmediate(&code, function, ir.opSize, ir.dest, 0);
					}
					else {
						loadImmediateIntoRAX(&code, ir.immediate);

						code.add1Unchecked(0x48);
						code.add1Unchecked(0x0F);
						code.add1Unchecked(0xAF);
						writeRSPRegisterByte(&code, function, RAX, ir.a);

						storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
					}
				} break;
				case IrOp::DIV: {
					if (ir.flags & IR_FLOAT_OP) {
						loadIntoFloatRegister(&code, function, ir.opSize, 0, ir.a);
						loadIntoFloatRegister(&code, function, ir.opSize, 1, ir.b);

						if (ir.opSize == 8) {
							code.add1Unchecked(0xF2);
						}
						else {
							code.add1Unchecked(0xF3);
						}
						code.add1Unchecked(0x0F);
						code.add1Unchecked(0x5E);
						code.add1Unchecked(0xC1);

						storeFromFloatRegister(&code, function, ir.opSize, ir.dest, 0);
					}
					else {
						loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);
						loadIntoIntRegister(&code, function, ir.opSize, RCX, ir.b);

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

						storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
					}
				} break;
				case IrOp::DIVIDE_BY_CONSTANT: {
					loadImmediateIntoRAX(&code, ir.immediate);

					code.add1Unchecked(0x48); // mov rcx, rax
					code.add1Unchecked(0x89);
					code.add1Unchecked(0xC1);

					loadIntoIntRegister(&code, function, 8, RAX, ir.a);

					code.add1Unchecked(0x48); // cqo
					code.add1Unchecked(0x99);

					code.add1Unchecked(0x48); // div rcx
					code.add1Unchecked(0xF7);
					code.add1Unchecked(0xF1);

					storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::MOD: {
					if (ir.flags & IR_FLOAT_OP) {
						// @Incomplete: x64 doesn't have native fmod, call out to fmod in crt or implement our own
						assert(false);
					}
					else {
						loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);
						loadIntoIntRegister(&code, function, ir.opSize, RCX, ir.b);

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

						storeFromIntRegister(&code, function, ir.opSize, ir.dest, ir.opSize == 1 ? AH : RDX);
					}
				} break;
				case IrOp::AND: {
					if (ir.a == 0 || ir.b == 0) {
						storeImmediate(&code, function, ir.opSize, ir.dest, 0);
					}
					else {
						loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);

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
						writeRSPRegisterByte(&code, function, RAX, ir.b);

						storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
					}
				} break;
				case IrOp::OR: {
					if (ir.a == 0) {
						writeSet(&code, function, ir.opSize, ir.dest, ir.b);
					}
					else if (ir.b == 0) {
						writeSet(&code, function, ir.opSize, ir.dest, ir.a);
					}
					else {
						loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);

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
						writeRSPRegisterByte(&code, function, RAX, ir.b);

						storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
					}
				} break;
				case IrOp::XOR: {
					if (ir.a == 0) {
						writeSet(&code, function, ir.opSize, ir.dest, ir.b);
					}
					else if (ir.b == 0) {
						writeSet(&code, function, ir.opSize, ir.dest, ir.a);
					}
					else {
						loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);

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
						writeRSPRegisterByte(&code, function, RAX, ir.b);

						storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
					}
				} break;
				case IrOp::NOT: {
					loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);
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

					storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::SHIFT_LEFT: {
					loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);
					loadIntoIntRegister(&code, function, ir.opSize, RCX, ir.b);

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

					storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::SHIFT_RIGHT: {

					loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);
					loadIntoIntRegister(&code, function, ir.opSize, RCX, ir.b);

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

					storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::READ: {
					assert(ir.opSize == 8);

					if (isStandardSize(ir.destSize)) {
						loadIntoIntRegister(&code, function, 8, RAX, ir.a);

						if (ir.destSize == 8) {
							code.add1Unchecked(0x48);
						}
						else if (ir.destSize == 2) {
							code.add1Unchecked(0x66);
						}

						if (ir.destSize == 1) {
							code.add1Unchecked(0x8A);
						}
						else {
							code.add1Unchecked(0x8B);
						}

						code.add1Unchecked(0x00);

						storeFromIntRegister(&code, function, ir.destSize, ir.dest, RAX);
					}
					else {

						loadIntoIntRegister(&code, function, 8, RSI, ir.a);

						code.add1Unchecked(0x48);
						code.add1Unchecked(0x8D);
						writeRSPRegisterByte(&code, function, RDI, ir.dest);

						if (ir.destSize % 8 == 0) {
							loadImmediateIntoIntRegister(&code, RCX, ir.destSize / 8);

							code.add1Unchecked(0xF3); // rep movsq
							code.add1Unchecked(0x48);
							code.add1Unchecked(0xA5);
						}
						else {
							loadImmediateIntoIntRegister(&code, RCX, ir.destSize);

							code.add1Unchecked(0xF3); // rep movsb
							code.add1Unchecked(0x48);
							code.add1Unchecked(0xA4);
						}
					}
				} break;
				case IrOp::WRITE: {
					if (isStandardSize(ir.opSize)) {
						loadIntoIntRegister(&code, function, 8, RAX, ir.a);
						loadIntoIntRegister(&code, function, ir.opSize, RCX, ir.b);

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

						code.add1Unchecked(0x08);
					}
					else {
						if (ir.b == 0) {
							loadIntoIntRegister(&code, function, 8, RDI, ir.a);

							if (ir.opSize % 8 == 0) {
								loadImmediateIntoIntRegister(&code, RCX, ir.opSize / 8);

								code.add1Unchecked(0x31); // xor eax, eax
								code.add1Unchecked(0xC0);

								code.add1Unchecked(0xF3); // rep stosq
								code.add1Unchecked(0x48);
								code.add1Unchecked(0xAB);
							}
							else {
								loadImmediateIntoIntRegister(&code, RCX, ir.opSize);

								code.add1Unchecked(0x30); // xor al, al
								code.add1Unchecked(0xC0);

								code.add1Unchecked(0xF3); // rep stosb
								code.add1Unchecked(0x48);
								code.add1Unchecked(0xAA);
							}
						}
						else {
							code.add1Unchecked(0x48);
							code.add1Unchecked(0x8D);
							writeRSPRegisterByte(&code, function, RSI, ir.b);

							loadIntoIntRegister(&code, function, 8, RDI, ir.a);

							if (ir.opSize % 8 == 0) {
								loadImmediateIntoIntRegister(&code, RCX, ir.opSize / 8);

								code.add1Unchecked(0xF3); // rep movsq
								code.add1Unchecked(0x48);
								code.add1Unchecked(0xA5);
							}
							else {
								loadImmediateIntoIntRegister(&code, RCX, ir.opSize);

								code.add1Unchecked(0xF3); // rep movsb
								code.add1Unchecked(0x48);
								code.add1Unchecked(0xA4);
							}
						}
					}
				} break;
				case IrOp::TYPE_INFO: // Currently at runtime a type is just a *Type_Info so a TYPE_INFO instruction is equivalent to a transfer
				case IrOp::SET: {
					if (ir.opSize == ir.destSize || ir.a == 0) {
						writeSet(&code, function, ir.opSize, ir.dest, ir.a);
					}
					else {
						if (ir.flags & IR_FLOAT_OP) {
							if (ir.opSize == 8) {
								assert(ir.destSize == 4);

								code.add1Unchecked(0xF2);
							}
							else {
								assert(ir.opSize == 4);
								assert(ir.destSize == 8);

								code.add1Unchecked(0xF3);
							}

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0x5A);

							writeRSPRegisterByte(&code, function, 0, ir.a);
							storeFromFloatRegister(&code, function, ir.destSize, ir.dest, 0);
						}
						else {
							if (ir.destSize < ir.opSize) {
								writeSet(&code, function, ir.destSize, ir.dest, ir.a);
							}
							else {
								if (ir.flags & IR_SIGNED_OP) {
									if (ir.opSize == 1) {
										if (ir.destSize == 2) {
											code.add1Unchecked(0x66);
										}
										else if (ir.destSize == 8) {
											code.add1Unchecked(0x48);
										}

										code.add1Unchecked(0x0F);
										code.add1Unchecked(0xBE);
										writeRSPRegisterByte(&code, function, RAX, ir.a);
									}
									else if (ir.opSize == 2) {
										if (ir.destSize == 8) {
											code.add1Unchecked(0x48);
										}

										code.add1Unchecked(0x0F);
										code.add1Unchecked(0xBF);
										writeRSPRegisterByte(&code, function, RAX, ir.a);
									}
									else if (ir.opSize == 4) {
										code.add1Unchecked(0x48);
										code.add1Unchecked(0x63);
										writeRSPRegisterByte(&code, function, RAX, ir.a);
									}
								}
								else {
									if (ir.opSize == 1) {
										if (ir.destSize == 2) {
											code.add1Unchecked(0x66);
										}

										code.add1Unchecked(0x0F);
										code.add1Unchecked(0xB6);
										writeRSPRegisterByte(&code, function, RAX, ir.a);


									}
									else if (ir.opSize == 2) {
										code.add1Unchecked(0x0F);
										code.add1Unchecked(0xB7);
										writeRSPRegisterByte(&code, function, RAX, ir.a);
									}
									else if (ir.opSize == 4) {
										loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);
									}
								}

								storeFromIntRegister(&code, function, ir.destSize, ir.dest, RAX);
							}
						}
					}
				} break;
				case IrOp::GOTO: {
					code.add1Unchecked(0xE9);

					JumpPatch patch;
					patch.opToPatch = ir.branchTarget;
					patch.location = reinterpret_cast<s32 *>(code.add4Unchecked(0));
					patch.rip = code.totalSize;

					jumpPatches.add(patch);
				} break;
				case IrOp::IF_Z_GOTO: {
					loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);

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
					patch.opToPatch = ir.branchTarget;
					patch.location = reinterpret_cast<s32 *>(code.add4Unchecked(0));
					patch.rip = code.totalSize;

					jumpPatches.add(patch);
				} break;
				case IrOp::IF_NZ_GOTO: {
					loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);

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
					patch.opToPatch = ir.branchTarget;
					patch.location = reinterpret_cast<s32 *>(code.add4Unchecked(0));
					patch.rip = code.totalSize;

					jumpPatches.add(patch);
				} break;
				case IrOp::LESS: {
					if (ir.flags & IR_FLOAT_OP) {
						setConditionFloat(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_B);
					}
					else {
						if (ir.flags & IR_SIGNED_OP) {
							setConditionInt(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_L);
						}
						else {
							setConditionInt(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_B);
						}
					}
				} break;
				case IrOp::GREATER: {
					if (ir.flags & IR_FLOAT_OP) {
						setConditionFloat(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_A);
					}
					else {
						if (ir.flags & IR_SIGNED_OP) {
							setConditionInt(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_G);
						}
						else {
							setConditionInt(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_A);
						}
					}
				} break;
				case IrOp::LESS_EQUAL: {
					if (ir.flags & IR_FLOAT_OP) {
						setConditionFloat(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_BE);
					}
					else {
						if (ir.flags & IR_SIGNED_OP) {
							setConditionInt(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_LE);
						}
						else {
							setConditionInt(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_BE);
						}
					}
				} break;
				case IrOp::GREATER_EQUAL: {
					if (ir.flags & IR_FLOAT_OP) {
						setConditionFloat(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_AE);
					}
					else {
						if (ir.flags & IR_SIGNED_OP) {
							setConditionInt(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_GE);
						}
						else {
							setConditionInt(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_AE);
						}
					}
				} break;
				case IrOp::NOT_EQUAL: {
					if (ir.flags & IR_FLOAT_OP) {
						setConditionFloat(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_NE);
					}
					else {
						setConditionInt(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_NE);
					}
				} break;
				case IrOp::EQUAL: {
					if (ir.flags & IR_FLOAT_OP) {
						setConditionFloat(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_E);
					}
					else {
						setConditionInt(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_E);
					}
				} break;
				case IrOp::ADDRESS_OF_GLOBAL: {
					assert(ir.declaration->enclosingScope->flavor == BlockFlavor::GLOBAL);
					assert(!(ir.declaration->flags & DECLARATION_IS_CONSTANT));

					code.add1Unchecked(0x48);
					code.add1Unchecked(0x8D);
					code.add1Unchecked(0x05);

					codeRelocations.ensure(10);
					codeRelocations.add4Unchecked(static_cast<u32>(code.totalSize));
					codeRelocations.add4Unchecked(createSymbolForDeclaration(&symbols, ir.declaration));
					codeRelocations.add2Unchecked(IMAGE_REL_AMD64_REL32);

					code.add4Unchecked(ir.a);

					storeFromIntRegister(&code, function, 8, ir.dest, RAX);
				} break;
				case IrOp::ADDRESS_OF_LOCAL: {
					code.add1Unchecked(0x48);
					code.add1Unchecked(0x8D);

					writeRSPRegisterByte(&code, function, RAX, ir.a, static_cast<u32>(ir.immediate));
					storeFromIntRegister(&code, function, 8, ir.dest, RAX);
				} break;
				case IrOp::IMMEDIATE: {
					storeImmediate(&code, function, ir.opSize, ir.dest, ir.immediate);
				} break;
				case IrOp::FLOAT_TO_INT: {
					if (ir.a == 0) {
						writeSet(&code, function, ir.destSize, ir.dest, 0);
						break;
					}

					if (ir.flags & IR_SIGNED_OP) {
						if (ir.opSize == 8) {
							code.add1Unchecked(0xF2);
						}
						else {
							code.add1Unchecked(0xF3);
						}

						if (ir.destSize == 8) {
							code.add1Unchecked(0x48);
						}

						code.add1Unchecked(0x0F);
						code.add1Unchecked(0x2C);
						writeRSPRegisterByte(&code, function, RAX, ir.a);
						storeFromIntRegister(&code, function, ir.destSize, ir.dest, RAX);
					}
					else {
						if (ir.destSize == 8) { // Aww sheet
							loadIntoFloatRegister(&code, function, ir.opSize, 0, ir.a);

							if (ir.opSize == 8) {
								if (f64ToU64ConstantSymbolIndex == -1) {
									f64ToU64ConstantSymbolIndex = symbols.count();

									rdata.allocateUnaligned(AlignPO2(rdata.totalSize, 8) - rdata.totalSize);

									Symbol f64ToU64Constant;
									setSymbolName(&stringTable, &f64ToU64Constant.name, "@f64ToU64Constant");
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
									setSymbolName(&stringTable, &f32ToU64Constant.name, "@f32ToU64Constant");
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

							storeFromIntRegister(&code, function, ir.destSize, ir.dest, RAX);
						}

						else {
							if (ir.opSize == 8) {
								code.add1Unchecked(0xF2);
							}
							else {
								code.add1Unchecked(0xF3);
							}

							if (ir.destSize == 4) {
								code.add1Unchecked(0x48);
							}

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0x2C);


							writeRSPRegisterByte(&code, function, RAX, ir.a);
							storeFromIntRegister(&code, function, ir.destSize, ir.dest, RAX);
						}
					}
				} break;
				case IrOp::INT_TO_FLOAT: {
					if (ir.a == 0) {
						writeSet(&code, function, ir.destSize, ir.dest, 0);
						break;
					}

					if (ir.flags & IR_SIGNED_OP) {
						if (ir.opSize >= 4) {
							if (ir.destSize == 8) {
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
							writeRSPRegisterByte(&code, function, 0, ir.a);
							storeFromFloatRegister(&code, function, ir.destSize, ir.dest, 0);
						}
						else {
							code.add1Unchecked(0x0F);
							if (ir.opSize == 2) {
								code.add1Unchecked(0xBF);
							}
							else {
								code.add1Unchecked(0xBE);
							}
							writeRSPRegisterByte(&code, function, RAX, ir.a);

							if (ir.destSize == 8) {
								code.add1Unchecked(0xF2);
							}
							else {
								code.add1Unchecked(0xF3);
							}

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0x2A);
							code.add1Unchecked(0xC0);

							storeFromFloatRegister(&code, function, ir.destSize, ir.dest, 0);
						}
					}
					else {
						if (ir.opSize == 8) {
							loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);

							code.add1Unchecked(0x0F); // xorps xmm0, xmm0
							code.add1Unchecked(0x57);
							code.add1Unchecked(0xC0);

							code.add1Unchecked(0x48); // test rax, rax
							code.add1Unchecked(0x85);
							code.add1Unchecked(0xC0);

							code.add1Unchecked(0x70 | C_S); // js .large

							u8 *firstJumpPatch = code.add1Unchecked(0);
							u32 firstJumpRel = code.totalSize;


							if (ir.destSize == 8) {
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

							if (ir.destSize == 8) {
								code.add1Unchecked(0xF2);
							}
							else {
								code.add1Unchecked(0xF3);
							}

							code.add1Unchecked(0x48);
							code.add1Unchecked(0x0F);
							code.add1Unchecked(0x2A);
							code.add1Unchecked(0xC0);

							if (ir.destSize == 8) {
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
							storeFromFloatRegister(&code, function, ir.destSize, ir.dest, 0);
						}
						else if (ir.opSize == 4) {
							loadIntoIntRegister(&code, function, 4, RAX, ir.a);

							if (ir.destSize == 8) {
								code.add1Unchecked(0xF2);
							}
							else {
								code.add1Unchecked(0xF3);
							}

							code.add1Unchecked(0x48);

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0x2A);
							code.add1Unchecked(0xC0);
							storeFromFloatRegister(&code, function, ir.destSize, ir.dest, 0);
						}
						else {
							code.add1Unchecked(0x0F);
							if (ir.opSize == 2) {
								code.add1Unchecked(0xB7);
							}
							else {
								code.add1Unchecked(0xB6);
							}
							writeRSPRegisterByte(&code, function, RAX, ir.a);

							if (ir.destSize == 8) {
								code.add1Unchecked(0xF2);
							}
							else {
								code.add1Unchecked(0xF3);
							}

							code.add1Unchecked(0x0F);
							code.add1Unchecked(0x2A);
							code.add1Unchecked(0xC0);

							storeFromFloatRegister(&code, function, ir.destSize, ir.dest, 0);
						}
					}
				} break;
				case IrOp::RETURN: {
					if (ir.opSize) {
						if (isStandardSize(ir.opSize)) {
							if (ir.flags & IR_FLOAT_OP) {
								loadIntoFloatRegister(&code, function, ir.opSize, 0, ir.a);
							}
							else {
								loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);
							}
						}
						else {
							loadIntoIntRegister(&code, function, 8, RDI, 1);

							if (ir.a == 0) {
								if (ir.opSize % 8 == 0) {
									loadImmediateIntoIntRegister(&code, RCX, ir.opSize / 8);

									code.add1Unchecked(0x31); // xor eax, eax
									code.add1Unchecked(0xC0);

									code.add1Unchecked(0xF3); // rep stosq
									code.add1Unchecked(0x48);
									code.add1Unchecked(0xAB);
								}
								else {
									loadImmediateIntoIntRegister(&code, RCX, ir.opSize);

									code.add1Unchecked(0x30); // xor al, al
									code.add1Unchecked(0xC0);

									code.add1Unchecked(0xF3); // rep stosb
									code.add1Unchecked(0x48);
									code.add1Unchecked(0xAA);
								}
							}
							else {
								code.add1Unchecked(0x48);
								code.add1Unchecked(0x8D);
								writeRSPRegisterByte(&code, function, RSI, ir.a);

								if (ir.opSize % 8 == 0) {
									loadImmediateIntoIntRegister(&code, RCX, ir.opSize / 8);

									code.add1Unchecked(0xF3); // rep movsq
									code.add1Unchecked(0x48);
									code.add1Unchecked(0xA5);
								}
								else {
									loadImmediateIntoIntRegister(&code, RCX, ir.opSize);

									code.add1Unchecked(0xF3); // rep movsb
									code.add1Unchecked(0x48);
									code.add1Unchecked(0xA4);
								}
							}

							loadIntoIntRegister(&code, function, 8, RAX, 1);
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
					u32 parameterOffset;

					if (!isStandardSize(ir.arguments->returnType->size)) {
						parameterOffset = 1;
					}
					else {
						parameterOffset = 0;
					}

					u32 parameterSpace = my_max(4, ir.arguments->argCount + parameterOffset);

					u32 largeStorage = parameterSpace;

					for (u64 i = 0; i < ir.arguments->argCount; i++) {
						u32 size = ir.arguments->args[i].type->size;
						u32 reg = ir.arguments->args[i].number;

						if (reg == static_cast<u32>(-1)) {
							continue;
						}

						if (!isStandardSize(size)) {
							if (largeStorage & 1) {
								++largeStorage; // Align to 16 bytes
							}

							code.add1Unchecked(0x48);
							code.add1Unchecked(0x8D);
							writeRSPOffsetByte(&code, RDI, largeStorage * 8);

							if (reg == 0) {
								if (size % 8 == 0) {
									loadImmediateIntoIntRegister(&code, RCX, size / 8);

									code.add1Unchecked(0x31); // xor eax, eax
									code.add1Unchecked(0xC0);

									code.add1Unchecked(0xF3); // rep stosq
									code.add1Unchecked(0x48);
									code.add1Unchecked(0xAB);
								}
								else {
									loadImmediateIntoIntRegister(&code, RCX, size);

									code.add1Unchecked(0x30); // xor al, al
									code.add1Unchecked(0xC0);

									code.add1Unchecked(0xF3); // rep stosb
									code.add1Unchecked(0x48);
									code.add1Unchecked(0xAA);
								}
							}
							else {
								code.add1Unchecked(0x48);
								code.add1Unchecked(0x8D);
								writeRSPRegisterByte(&code, function, RSI, reg);

								if (size % 8 == 0) {
									loadImmediateIntoIntRegister(&code, RCX, size / 8);

									code.add1Unchecked(0xF3); // rep movsq
									code.add1Unchecked(0x48);
									code.add1Unchecked(0xA5);
								}
								else {
									loadImmediateIntoIntRegister(&code, RCX, size);

									code.add1Unchecked(0xF3); // rep movsb
									code.add1Unchecked(0x48);
									code.add1Unchecked(0xA4);
								}
							}

							largeStorage += (size + 7) / 8;
							code.ensure(128);
						}
					}

					u32 dumpSpace = largeStorage;

					largeStorage = parameterSpace;

					constexpr int intRegisters[4] = { RCX, RDX, 8, 9 };


					for (u8 i = 0; i < my_min(4 - parameterOffset, ir.arguments->argCount); i++) {
						auto type = ir.arguments->args[i].type;


						if (ir.arguments->args[i].number == static_cast<u64>(-1LL)) {

							code.add1Unchecked(intRegisters[i + parameterOffset] >= 8 ? 0x4C : 0x48);
							code.add1Unchecked(0x8D);
							writeRSPOffsetByte(&code, intRegisters[i + parameterOffset] & 7, dumpSpace * 8);
						}
						else if (type->flavor == TypeFlavor::FLOAT) {
							loadIntoFloatRegister(&code, function, type->size, i + parameterOffset, ir.arguments->args[i].number);
						}
						else {
							if (isStandardSize(type->size)) {
								loadIntoIntRegister(&code, function, type->size, intRegisters[i + parameterOffset], ir.arguments->args[i].number);
							}
							else {
								if (largeStorage & 1) {
									++largeStorage; // Align to 16 bytes
								}

								code.add1Unchecked(intRegisters[i + parameterOffset] >= 8 ? 0x4C : 0x48);
								code.add1Unchecked(0x8D);
								writeRSPOffsetByte(&code, intRegisters[i + parameterOffset] & 7, largeStorage * 8);

								u32 size = type->size;
								largeStorage += (size + 7) / 8;
							}
						}
					}

					for (u32 i = 4 - parameterOffset; i < ir.arguments->argCount; i++) {
						u64 size = ir.arguments->args[i].type->size;

						if (ir.arguments->args[i].number == static_cast<u64>(-1LL)) {

							code.add1Unchecked(0x48);
							code.add1Unchecked(0x8D);
							writeRSPOffsetByte(&code, RAX, dumpSpace * 8);
						}
						else if (isStandardSize(size)) {
							loadIntoIntRegister(&code, function, ir.arguments->args[i].type->size, RAX, ir.arguments->args[i].number);
						}
						else {
							if (largeStorage & 1) {
								++largeStorage; // Align to 16 bytes
							}

							code.add1Unchecked(0x48);
							code.add1Unchecked(0x8D);
							writeRSPOffsetByte(&code, RAX, largeStorage * 8);

							largeStorage += (largeStorage + 7) / 8;
						}

						code.add1Unchecked(0x48);
						code.add1Unchecked(0x89);
						writeRSPOffsetByte(&code, RAX, (i + parameterOffset) * 8);

						code.ensure(128);
					}

					if (!isStandardSize(ir.arguments->returnType->size)) {
						code.add1Unchecked(0x48);
						code.add1Unchecked(0x8D);

						if (ir.dest) {
							writeRSPRegisterByte(&code, function, RCX, ir.dest);
						}
						else {
							writeRSPOffsetByte(&code, RCX, dumpSpace * 8);
						}
					}

					code.add1Unchecked(0xFF);
					writeRSPRegisterByte(&code, function, 2, ir.a);

					if (ir.dest != 0) {
						if (isStandardSize(ir.arguments->returnType->size)) {
							if (ir.arguments->returnType->flavor == TypeFlavor::FLOAT) {
								storeFromFloatRegister(&code, function, ir.arguments->returnType->size, ir.dest, 0);
							}
							else {
								storeFromIntRegister(&code, function, ir.arguments->returnType->size, ir.dest, RAX);
							}
						}
					}
				} break;
				case IrOp::NEG: {
					if (ir.a == 0) {
						storeImmediate(&code, function, ir.opSize, ir.dest, 0);
					}
					else if (ir.flags & IR_FLOAT_OP) {
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
						writeRSPRegisterByte(&code, function, 0, ir.a);

						storeFromFloatRegister(&code, function, ir.opSize, ir.dest, 0);
					}
					else {
						loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);
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

						storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
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
					codeRelocations.add4Unchecked(createSymbolForFunction(&symbols, ir.function));
					codeRelocations.add2Unchecked(IMAGE_REL_AMD64_REL32);

					code.add4Unchecked(0);

					storeFromIntRegister(&code, function, 8, ir.dest, RAX);
				} break;
				case IrOp::STRING: {
					code.add1Unchecked(0x48);
					code.add1Unchecked(0x8D);
					code.add1Unchecked(0x05);

					codeRelocations.ensure(10);
					codeRelocations.add4Unchecked(code.totalSize);
					codeRelocations.add4Unchecked(createSymbolForString(&emptyStringSymbolIndex, &symbols, &stringTable, &rdata, ir.string));
					codeRelocations.add2Unchecked(IMAGE_REL_AMD64_REL32);

					code.add4Unchecked(0);

					storeFromIntRegister(&code, function, 8, ir.dest, RAX);

					loadImmediateIntoRAX(&code, ir.string->string.length);
					storeFromIntRegister(&code, function, 8, ir.dest + 1, RAX);
				} break;
				case IrOp::LINE_MARKER: {
					addLineInfo(&lineInfo, &columnInfo, code.totalSize - functionStart, ir.location.start, ir.location.end);
				} break;
				case IrOp::BLOCK: {
					if (ir.block) {
						debugSymbols.ensure(19);
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
						debugSymbolsRelocations.add4Unchecked(textSectionSymbolIndex);
						debugSymbolsRelocations.add2Unchecked(IMAGE_REL_AMD64_SECTION);

						debugSymbols.add2Unchecked(0);
						debugSymbols.add1Unchecked(0);

						for (auto declaration : ir.block->declarations) {
							if (declaration->flags & (DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_CONSTANT)) continue;

							REGREL32 variableInfo;

							variableInfo.off = getRegisterOffset(function, declaration->physicalStorage);
							variableInfo.typind = getCoffTypeIndex(&debugTypes, static_cast<ExprLiteral *>(declaration->type)->typeValue);

							debugSymbols.ensure(2 + sizeof(variableInfo));
							debugSymbols.add2Unchecked(static_cast<u16>(sizeof(variableInfo) + declaration->name.length + 1));
							debugSymbols.addUnchecked(&variableInfo, sizeof(variableInfo));
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
				setSymbolName(&stringTable, &xdataSymbol.name, symbols.count());
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
				debugSymbolsRelocations.add4Unchecked(textSectionSymbolIndex);
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

			createSymbolForDeclaration(&symbols, declaration);

			debugSymbols.ensure(22);
			debugSymbolsRelocations.ensure(20);

			debugSymbols.add4Unchecked(0xF1);
			auto subsectionSizePatch = debugSymbols.add4Unchecked(0);
			u32 subsectionOffset = debugSymbols.totalSize;

			debugSymbols.add2Unchecked(static_cast<u16>(sizeof(DATASYM32) + declaration->name.length - 1));
			debugSymbols.add2Unchecked(0x110d); // S_GDATA32

			debugSymbols.add4Unchecked(getCoffTypeIndex(&debugTypes, static_cast<ExprLiteral *>(declaration->type)->typeValue));

			debugSymbolsRelocations.add4Unchecked(debugSymbols.totalSize);
			debugSymbolsRelocations.add4Unchecked(declaration->physicalStorage);
			debugSymbolsRelocations.add2Unchecked(IMAGE_REL_AMD64_SECREL);

			debugSymbols.add4Unchecked(0);

			debugSymbolsRelocations.add4Unchecked(debugSymbols.totalSize);
			debugSymbolsRelocations.add4Unchecked(declaration->flags & DECLARATION_IS_UNINITIALIZED ? BSS_SECTION_NUMBER : DATA_SECTION_NUMBER);
			debugSymbolsRelocations.add2Unchecked(IMAGE_REL_AMD64_SECTION);

			debugSymbols.add2Unchecked(0);

			debugSymbols.addNullTerminatedString(declaration->name);

			*subsectionSizePatch = debugSymbols.totalSize - subsectionOffset;

			alignAllocator(&debugSymbols, 4);

			auto symbol = declaration->symbol;
			auto type = static_cast<ExprLiteral *>(declaration->type)->typeValue;

			setSymbolName(&stringTable, &symbol->name, declaration->name);

			symbol->storageClass = IMAGE_SYM_CLASS_EXTERNAL;
			symbol->type = 0;

			if ((declaration->flags & DECLARATION_IS_UNINITIALIZED) ||
				((declaration->initialValue->flavor == ExprFlavor::INT_LITERAL || declaration->initialValue->flavor == ExprFlavor::FLOAT_LITERAL)
					&& static_cast<ExprLiteral *>(declaration->initialValue)->unsignedValue == 0)) {
				bssSection.virtualSize = AlignPO2(bssSection.virtualSize, type->alignment);

				symbol->value = bssSection.virtualSize;
				symbol->sectionNumber = BSS_SECTION_NUMBER;

				bssSection.virtualSize += type->size;
			}
			else {
				data.allocateUnaligned(AlignPO2(data.totalSize, type->alignment) - data.totalSize);

				symbol->value = data.totalSize;
				symbol->sectionNumber = DATA_SECTION_NUMBER;

				u32 dataSize = data.totalSize;
				u8 *allocation = static_cast<u8 *>(data.allocateUnaligned(type->size));

				writeValue(dataSize, allocation, &dataRelocations, &symbols, &stringTable, declaration->initialValue, &emptyStringSymbolIndex, &rdata);
			}

			symbol->numberOfAuxSymbols = 0;
		}
	}

	if (!hadError) {
		PROFILE_ZONE("Write types");

		for (u64 i = 0; i < typeTableCapacity; i++) {
			auto entry = typeTableEntries[i];

			if (entry.hash) {
				auto type = entry.value;

				createSymbolForType(&symbols, type);
				auto symbol = type->symbol;

				u32 name = createRdataPointer(&stringTable, &symbols, &rdata);
				rdata.addNullTerminatedString(type->name);

				assert(type->name.length);

				setSymbolName(&stringTable, &symbol->name, entry.value->physicalStorage);
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
						createSymbolForType(&symbols, static_cast<TypePointer *>(type)->pointerTo));

					rdata.add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::FUNCTION: {
					auto function = static_cast<TypeFunction *>(type);

					rdata.allocateUnaligned(AlignPO2(rdata.totalSize, 8) - rdata.totalSize);

					u32 arguments = createRdataPointer(&stringTable, &symbols, &rdata);

					for (u64 i = 0; i < function->argumentCount; i++) {
						addPointerRelocation(&rdataRelocations, rdata.totalSize, createSymbolForType(&symbols, function->argumentTypes[i]));
						rdata.add8(0);
					}
					
					u32 returns = createRdataPointer(&stringTable, &symbols, &rdata);

					for (u64 i = 0; i < function->returnCount; i++) {
						addPointerRelocation(&rdataRelocations, rdata.totalSize, createSymbolForType(&symbols, function->returnTypes[i]));
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
						createSymbolForType(&symbols, static_cast<TypeArray *>(type)->arrayOf));

					rdata.add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::STRUCT: {
					auto struct_ = static_cast<TypeStruct *>(type);

					u32 names = symbols.count();

					for (auto member : struct_->members.declarations) {
						if (member->flags & DECLARATION_IMPORTED_BY_USING) continue;


						createRdataPointer(&stringTable, &symbols, &rdata);
						rdata.addNullTerminatedString(member->name);
					}

					u32 values = symbols.count();

					for (auto member : struct_->members.declarations) {
						if (member->flags & DECLARATION_IMPORTED_BY_USING) continue;

						if (!member->initialValue) continue;

						auto type = getTypeForExpr(member->initialValue);

						if (member->initialValue->flavor == ExprFlavor::TYPE_LITERAL && static_cast<ExprLiteral *>(member->initialValue)->typeValue->flavor == TypeFlavor::NAMESPACE)
							continue;

						rdata.allocateUnaligned(AlignPO2(rdata.totalSize, type->alignment) - rdata.totalSize);

						createRdataPointer(&stringTable, &symbols, &rdata);

						u32 dataSize = rdata.totalSize;
						u8 *allocation = static_cast<u8 *>(rdata.allocateUnaligned(type->size));

						writeValue(dataSize, allocation, &rdataRelocations, &symbols, &stringTable, member->initialValue, &emptyStringSymbolIndex, &rdata);
					}

					rdata.allocateUnaligned(AlignPO2(rdata.totalSize, 8) - rdata.totalSize);
					u32 members = createRdataPointer(&stringTable, &symbols, &rdata);

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


						addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(data), name.characters), names + nameCount);

						if (member->initialValue) {
							addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(data), member_type),
								createSymbolForType(&symbols, getTypeForExpr(member->initialValue)));
						}
						else {
							addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(data), member_type),
								createSymbolForType(&symbols, static_cast<ExprLiteral *>(member->type)->typeValue));
						}

						if (member->initialValue) { // @Incomplete: Export info for namespaces
							if (member->initialValue->flavor != ExprFlavor::TYPE_LITERAL || static_cast<ExprLiteral *>(member->initialValue)->typeValue->flavor != TypeFlavor::NAMESPACE) {
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

					for (auto member : enum_->values->declarations) {
						createRdataPointer(&stringTable, &symbols, &rdata);
						rdata.addNullTerminatedString(member->name);
					}

					rdata.allocateUnaligned(AlignPO2(rdata.totalSize, 8) - rdata.totalSize);
					u32 values = createRdataPointer(&stringTable, &symbols, &rdata);

					for (u32 i = 0; i < enum_->values->declarations.count; i++) {
						auto member = enum_->values->declarations[i];

						Type_Info_Enum::Value data;

						data.name = { nullptr, member->name.length };
						data.value = static_cast<ExprLiteral *>(member->initialValue)->unsignedValue;

						addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(data), name.characters), names + i);

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
					info.values.count = enum_->values->declarations.count;

					if (name)
						addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(info), name), name);

					addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(info), base_type), 
						createSymbolForType(&symbols, enum_->integerType));
					addPointerRelocation(&rdataRelocations, rdata.totalSize + offsetof(decltype(info), values.data), values);

					rdata.add(&info, sizeof(info));

					break;
				}
				default:
					assert(false);
				}
			}
		}

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
				section.header->sizeOfRawData = AlignPO2(section.header->virtualSize, 4);
				section.header->virtualSize = 0;
			}

			if (section.header->sizeOfRawData) {
				section.header->pointerToRawData = sectionPointer;

				sectionPointer += section.header->sizeOfRawData;

				if (section.relocations) {
					section.header->pointerToRelocations = sectionPointer;
					assert(section.relocations->totalSize / sizeof(Relocation) < UINT16_MAX);

					// @Incomplete @Robustness, we need to do something else if there are more than 65535 relocations
					section.header->numberOfRelocations = static_cast<u16>(section.relocations->totalSize / sizeof(Relocation));

					alignAllocator(section.relocations, 4);
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
			HANDLE out = CreateFileA("out.obj", GENERIC_WRITE | GENERIC_READ, 0, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);


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
					u32 count = (allocator.bucketSize - bucket->remaining);

					doWrite(bucket->memory - count, count);
				}
			};

			if (out == INVALID_HANDLE_VALUE) {
				reportError("Error: Could not open out.obj intermediate for writing");
				goto error;
			}
			
			doWrite(&header, sizeof(header));

			for (auto section : sections) {
				doWrite(section.header, sizeof(*section.header));
			}
			

			//assert(ftell(out) == header.pointerToSymbolTable);
			writeAllocator(out, symbols.allocator);

			doWrite(&stringTableSize, sizeof(stringTableSize));
			writeAllocator(out, stringTable);

			doWrite(&alignmentPadding, AlignPO2(prefixSize, 4) - prefixSize);

			for (u32 i = 0; i < sections.count; i++) {
				auto section = sections[i];

				if (section.data) {
					//assert(section.header->pointerToRawData == ftell(out));
					writeAllocator(out, *section.data);
				}
				else if (section.header->sizeOfRawData) {
					char zero[1024] = {};

					for (s64 write = section.header->sizeOfRawData; write > 0; write -= sizeof(zero)) {
						doWrite(zero, static_cast<u32>(my_min(sizeof(zero), write)));
					}
				}

				if (section.relocations) {
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