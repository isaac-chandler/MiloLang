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

#if !BUILD_WINDOWS
#define IMAGE_SYM_TYPE_NULL      0
#define IMAGE_SYM_CLASS_EXTERNAL 2
#define IMAGE_SYM_CLASS_STATIC   3

#define IMAGE_REL_AMD64_ABSOLUTE 0
#define IMAGE_REL_AMD64_ADDR64   1
#define IMAGE_REL_AMD64_ADDR32NB 3
#define IMAGE_REL_AMD64_REL32    4
#define IMAGE_REL_AMD64_SECREL   11
#define IMAGE_REL_AMD64_SECTION  10

#define IMAGE_SCN_CNT_CODE               0x0000'0020
#define IMAGE_SCN_CNT_INITIALIZED_DATA   0x0000'0040
#define IMAGE_SCN_CNT_UNINITIALIZED_DATA 0x0000'0080
#define IMAGE_SCN_ALIGN_1BYTES           0x0010'0000
#define IMAGE_SCN_ALIGN_2BYTES           0x0020'0000
#define IMAGE_SCN_ALIGN_4BYTES           0x0030'0000
#define IMAGE_SCN_ALIGN_8BYTES           0x0040'0000
#define IMAGE_SCN_ALIGN_16BYTES          0x0050'0000
#define IMAGE_SCN_ALIGN_32BYTES          0x0060'0000
#define IMAGE_SCN_ALIGN_64BYTES          0x0070'0000
#define IMAGE_SCN_ALIGN_128BYTES         0x0080'0000
#define IMAGE_SCN_ALIGN_256BYTES         0x0090'0000
#define IMAGE_SCN_ALIGN_512BYTES         0x00A0'0000
#define IMAGE_SCN_ALIGN_1024BYTES        0x00B0'0000
#define IMAGE_SCN_ALIGN_2048BYTES        0x00C0'0000
#define IMAGE_SCN_ALIGN_4096BYTES        0x00D0'0000
#define IMAGE_SCN_ALIGN_8192BYTES        0x00E0'0000
#define IMAGE_SCN_LNK_NRELOC_OVFL        0x0100'0000
#define IMAGE_SCN_MEM_DISCARDABLE        0x0200'0000    
#define IMAGE_SCN_MEM_EXECUTE            0x2000'0000
#define IMAGE_SCN_MEM_READ               0x4000'0000
#define IMAGE_SCN_MEM_WRITE              0x8000'0000

#define IMAGE_FILE_MACHINE_AMD64 0x8664      

#endif

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

#define SECTION_READ        0x01
#define SECTION_WRITE       0x02
#define SECTION_EXECUTE     0x04
#define SECTION_DISCARD     0x08
#define SECTION_INITIALIZED 0x10

struct Section : public BucketedArenaAllocator {
	Section() : BucketedArenaAllocator(65536) {}

	const char *name;
	u64 sectionNumber;
	u64 alignment;
	u64 flags;
	BucketArray<Relocation> relocations;

	void addRelocation(u32 symbolTableIndex, u16 type) {
		relocations.add(Relocation{ totalSize, symbolTableIndex, type });
	}

	u32 *addRel32Relocation(u32 symbolTableIndex) {
		addRelocation(symbolTableIndex, IMAGE_REL_AMD64_REL32);
		return add4Unchecked(0);
	}

	u64 *addPointerRelocation(u32 symbolTableIndex) {
		relocations.add(Relocation{ totalSize, symbolTableIndex, IMAGE_REL_AMD64_ADDR64 });
		return add8Unchecked(0);
	}

	u32 *addAddr32NBRelocation(u32 symbolTableIndex) {
		relocations.add(Relocation{ totalSize, symbolTableIndex, IMAGE_REL_AMD64_ADDR32NB });
		return add4Unchecked(0);
	}

	u32 *addSectionRelocations(u32 symbolTableInex) {
		relocations.add(Relocation{ totalSize, symbolTableInex, IMAGE_REL_AMD64_SECREL });
		auto result = add4Unchecked(0);
		relocations.add(Relocation{ totalSize, symbolTableInex, IMAGE_REL_AMD64_SECTION });
		add2Unchecked(0);
		
		return result;
	}

	void addPointerRelocation(u32 symbolTableIndex, u32 offset) {
		relocations.add(Relocation{ offset, symbolTableIndex, IMAGE_REL_AMD64_ADDR64 });
	}
};

Array<Section *> sections;

Section *makeSection(const char *name, u64 alignment, u64 flags) {	
	Section *result = sections.add(new Section);
	result->name = name;
	result->sectionNumber = sections.count;
	result->alignment = alignment;
	result->flags = flags;

	return result;
}

BucketedArenaAllocator stringTable(65536);
BucketArray<Symbol> symbols;

s64 emptyStringSymbolIndex = -1;

Section *code;
Section *data;
Section *rdata;
Section *bss;
Section *debugSymbols;
Section *debugTypes;
Section *pdata;
Section *xdata;

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

#define REX   0x40
#define REX_B 0x41
#define REX_R 0x42
#define REX_X 0x44
#define REX_W 0x48

#define OPERAND_SIZE_OVERRIDE 0x66

#define MODRM_MOD_INDIRECT           0b00
#define MODRM_MOD_INDIRECT_OFFSET_8  0b01
#define MODRM_MOD_INDIRECT_OFFSET_32 0b10
#define MODRM_MOD_DIRECT             0b11

#define MODRM_RM_SIB       0b100
#define MODRM_RM_RIP_OFFSET_32 0b101

#define SIB_SCALE_1 0b00
#define SIB_SCALE_2 0b01
#define SIB_SCALE_4 0b10
#define SIB_SCALE_8 0b11

#define SIB_INDEX_NONE 0b100

#define SIB_BASE_NONE 0b101 // Requires ModR/M mod to be 0b00, otherwise this uses RBP as expected

#define FLOAT_64_PREFIX 0xF2
#define FLOAT_32_PREFIX 0xF3

#define REPNE_PREFIX 0xF2
#define REP_PREFIX   0xF3


#define ADD_REG_MEM_BASE 0x02

#define OR_REG_MEM_BASE 0x0A

#define AND_REG_MEM_BASE 0x22

#define SUB_MEM_REG_BASE 0x28
#define SUB_REG_MEM_BASE 0x2A

#define XOR_REG_MEM_BASE 0x32

#define CMP_REG_MEM_BASE 0x3A

#define PUSH_BASE 0x50
#define POP_BASE 0x58

#define MOVSX_REG_MEM_32 0x63

#define JCC_DIRECT_8_BASE 0x70

#define INT_OP_MEM_IMM_32_MODRM_X 0x81
#define INT_OP_MEM_IMM_8_MODRM_X 0x83

#define INT_OP_MODRM_ADD 0
#define INT_OP_MODRM_AND 4
#define INT_OP_MODRM_SUB 5

#define TEST_MEM_REG_BASE 0x84

#define MOV_MEM_REG_BASE 0x88
#define MOV_REG_MEM_BASE 0x8A

#define LEA	0x8D

#define CBW 0x98
#define CDQ 0x99

#define MOVS_BASE 0xA4

#define STOS_BASE 0xAA

#define MOV_REG_IMM_WORD_BASE 0xB8

#define SHIFT_IMM8_BASE_MODRM_X 0xC0

#define RET 0xC3

#define MOV_MEM_IMM_BASE_MODRM_0 0xC6

#define SHIFT_1_BASE_MODRM_X 0xD0
#define SHIFT_RCX_BASE_MODRM_X 0xD2

#define SHIFT_MODRM_SHL 4
#define SHIFT_MODRM_SHR 5
#define SHIFT_MODRM_SAR 7

#define CALL_DIRECT 0xE8
#define JMP_DIRECT 0xE9

#define JMP_DIRECT_8 0xEB

#define UNARY_INT_OP_BASE_MODRM_X 0xF6

#define UNARY_INT_OP_MODRM_NOT 2
#define UNARY_INT_OP_MODRM_NEG 3
#define UNARY_INT_OP_MODRM_IMUL_MEM 5
#define UNARY_INT_OP_MODRM_DIV_MEM  6
#define UNARY_INT_OP_MODRM_IDIV_MEM 7

#define CALL_INDIRECT_MODRM_2 0xFF

#define OPCODE_EXT 0x0F

#define EXT_MOVSf_REG_MEM 0x10
#define EXT_MOVSf_MEM_REG 0x11

#define EXT_CVTSI2Sf_REG_MEM 0x2A
#define EXT_CVTTSf2SI_REG_MEM 0x2C

#define EXT_COMISf_REG_MEM 0x2F

#define EXT_XORPf_REG_MEM 0x57
#define EXT_ADDSf_REG_MEM 0x58
#define EXT_MULSf_REG_MEM 0x59

#define EXT_SUBSf_REG_MEM 0x5C

#define EXT_CVTSf2Sf_REG_MEM 0x5A

#define EXT_DIVSf_REG_MEM 0x5E

#define EXT_JCC_DIRECT_BASE 0x80

#define EXT_SETCC_MEM_BASE 0x90

#define EXT_IMUL_REG_MEM 0xAF

#define EXT_MOVZX_REG_MEM_BASE 0xB6

#define EXT_BSF_REG_MEM 0xBC
#define EXT_BSR_REG_MEM 0xBC
#define EXT_MOVSX_REG_MEM_BASE 0xBE

#define REP_EXT_POPCNT_REG_MEM 0xB8


void sizedIntInstruction(u64 size, u8 baseOpcode) {
	if (size == 1) {
		code->add1Unchecked(baseOpcode);
	}
	else {
		code->add1Unchecked(baseOpcode | 1);
	}
}

void writeModRM(u8 mod, u8 reg, u8 rm) {
	code->add1Unchecked((mod << 6) | (reg << 3) | rm);
}

void writeSIB(u8 scale, u8 index, u8 base) {
	code->add1Unchecked((scale << 6) | (index << 3) | base);
}

void writeRSPOffsetByte(u8 physicalRegister, u32 offset) {
	if (offset >= 0x80) {
		writeModRM(MODRM_MOD_INDIRECT_OFFSET_32, physicalRegister, MODRM_RM_SIB);
		writeSIB(SIB_SCALE_1, SIB_INDEX_NONE, RSP);
		code->add4Unchecked(offset);
	}
	else if (offset != 0) {
		writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, physicalRegister, MODRM_RM_SIB);
		writeSIB(SIB_SCALE_1, SIB_INDEX_NONE, RSP);
		code->add1Unchecked(static_cast<u8>(offset));
	}
	else {
		writeModRM(MODRM_MOD_INDIRECT, physicalRegister, MODRM_RM_SIB);
		writeSIB(SIB_SCALE_1, SIB_INDEX_NONE, RSP);
	}
}

void writeRSPRegisterByte(ExprFunction *function, u8 physicalRegister, u32 stackRegister, u32 addition = 0) {
	writeRSPOffsetByte(physicalRegister, getRegisterOffset(function, stackRegister) + addition);
}

void writeIntegerPrefixXB(u64 size, u8 *xReg, u8 *bReg) {
	u8 rex = 0;

	if (size == 1 && (*xReg > 4 || *bReg > 4))
		rex |= REX;
	else if (size == 8)
		rex |= REX_W;
	else if (size == 2)
		code->add1Unchecked(OPERAND_SIZE_OVERRIDE);

	if (*xReg >= 8)
		rex |= REX_X;
	if (*bReg >= 8)
		rex |= REX_B;

	// Separate these because xReg and bReg could be the same pointer

	if (*xReg >= 8)
		*xReg -= 8;
	if (*bReg >= 8)
		*bReg -= 8;


	if (rex)
		code->add1Unchecked(rex);
}

void writeIntegerPrefixX(u64 size, u8 *xReg) {
	u8 rex = 0;
	
	if (size == 1 && *xReg > 4)
		rex |= REX;
	else if (size == 8)
		rex |= REX_W;
	else if (size == 2)
		code->add1Unchecked(OPERAND_SIZE_OVERRIDE);
	
	if (*xReg >= 8) {
		*xReg -= 8;
		rex |= REX_X;
	}

	if (rex)
		code->add1Unchecked(rex);
}

void writeIntegerPrefix(u64 size) {
	if (size == 8)
			code->add1Unchecked(REX_W);
	else if (size == 2)
		code->add1Unchecked(OPERAND_SIZE_OVERRIDE);
}

void writeIntegerPrefixRep(u64 size) {
	if (size == 2)
		code->add1Unchecked(OPERAND_SIZE_OVERRIDE);

	code->add1Unchecked(REP_PREFIX);

	if (size == 8)
		code->add1Unchecked(REX_W);
}

void writeFloatPrefix(u64 size) {
	if (size == 8)
		code->add1Unchecked(FLOAT_64_PREFIX);
	else
		code->add1Unchecked(FLOAT_32_PREFIX);
}

void writeFloatPrefixX(u64 size, u8 *xReg) {
	writeFloatPrefix(size);

	if (*xReg >= 8) {
		*xReg -= 8;
		code->add1Unchecked(REX_X);
	}
}

void writeFloatLegacyPrefix(u64 size) {
	if (size == 8) {
		code->add1Unchecked(OPERAND_SIZE_OVERRIDE);
	}
}

void loadIntoIntRegister(ExprFunction *function, u64 size, u8 loadInto, u32 regNo) {
	writeIntegerPrefixX(size, &loadInto);
	sizedIntInstruction(size, MOV_REG_MEM_BASE);
	writeRSPRegisterByte(function, loadInto, regNo);
}

void storeFromIntRegister(ExprFunction *function, u64 size, u32 regNo, u8 storeFrom) {
	writeIntegerPrefixX(size, &storeFrom);
	sizedIntInstruction(size, MOV_MEM_REG_BASE);
	writeRSPRegisterByte(function, storeFrom, regNo);
}

void loadIntoFloatRegister(ExprFunction *function, u64 size, u8 loadInto, u32 regNo) {
	writeFloatPrefixX(size, &loadInto);

	code->add1Unchecked(OPCODE_EXT);
	code->add1Unchecked(EXT_MOVSf_REG_MEM);

	writeRSPRegisterByte(function, loadInto, regNo);
}

void storeFromFloatRegister(ExprFunction *function, u64 size, u32 regNo, u8 storeFrom) {
	writeFloatPrefixX(size, &storeFrom);

	code->add1Unchecked(OPCODE_EXT);
	code->add1Unchecked(EXT_MOVSf_MEM_REG);

	writeRSPRegisterByte(function, storeFrom, regNo);
}

void storeImmediate(ExprFunction *function, u64 size, u32 regNo, u64 immediate) {
	assert(isStandardSize(size));

	if (size == 8 && static_cast<s64>(immediate) != static_cast<s64>(static_cast<s32>(immediate))) {
		writeIntegerPrefix(8);
		code->add1Unchecked(MOV_REG_IMM_WORD_BASE | RAX);
		code->add8Unchecked(immediate);

		storeFromIntRegister(function, 8, regNo, RAX);
	}
	else {
		writeIntegerPrefix(size);
		sizedIntInstruction(size, MOV_MEM_IMM_BASE_MODRM_0);
		writeRSPRegisterByte(function, 0, regNo);

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

void setCondition(ExprFunction *function, u32 dest, u8 condition) {
	code->add1Unchecked(OPCODE_EXT);
	code->add1Unchecked(EXT_SETCC_MEM_BASE | condition);
	writeRSPRegisterByte(function, 0, dest);
}

void setConditionInt(ExprFunction *function, u64 size, u32 dest, u32 a, u32 b, u8 condition) {
	loadIntoIntRegister(function, size, RAX, a);

	writeIntegerPrefix(size);
	sizedIntInstruction(size, CMP_REG_MEM_BASE);
	writeRSPRegisterByte(function, RAX, b);

	setCondition(function, dest, condition);
}

void setConditionFloat(ExprFunction *function, u64 size, u32 dest, u32 a, u32 b, u8 condition) {
	loadIntoFloatRegister(function, size, 0, a);

	writeFloatLegacyPrefix(size);


	code->add1Unchecked(OPCODE_EXT);
	code->add1Unchecked(EXT_COMISf_REG_MEM);
	writeRSPRegisterByte(function, 0, b);

	setCondition(function, dest, condition);
}

void loadImmediateIntoIntRegister64(u8 regNo, u64 immediate) {
	if (immediate == 0) {
		writeIntegerPrefixXB(4, &regNo, &regNo);
		sizedIntInstruction(4, XOR_REG_MEM_BASE);
		writeModRM(MODRM_MOD_DIRECT, regNo, regNo);
	} else if (immediate <= 0xFFFF'FFFFULL) {
		writeIntegerPrefixX(4, &regNo);
		code->add1Unchecked(MOV_REG_IMM_WORD_BASE | regNo);
		code->add4Unchecked(static_cast<u32>(immediate));
	}
	else if (static_cast<s64>(immediate) == static_cast<s64>(static_cast<s32>(immediate))) {
		writeIntegerPrefix(8);
		sizedIntInstruction(8, MOV_MEM_IMM_BASE_MODRM_0);
		writeModRM(MODRM_MOD_DIRECT, 0, regNo);
		code->add4Unchecked(static_cast<u32>(immediate));
	}
	else {
		writeIntegerPrefix(8);
		code->add1Unchecked(MOV_REG_IMM_WORD_BASE | regNo);
		code->add8Unchecked(immediate);
	}
}

void writeSet(ExprFunction *function, u64 size, u32 dest, u32 src) {
	assert(isStandardSize(size));
	loadIntoIntRegister(function, size, RAX, src);
	storeFromIntRegister(function, size, dest, RAX);
}

Array<CoffTypeIndexPatch> coffTypePatches;
Array<CoffFunctionIDTypeIndexPatch> coffFunctionIdTypePatches;

u32 createRdataPointer() {
	Symbol symbol;
	setSymbolName(&symbol.name, symbols.count());
	symbol.value = static_cast<u32>(rdata->totalSize);
	symbol.sectionNumber = rdata->sectionNumber;
	symbol.type = 0;
	symbol.storageClass = IMAGE_SYM_CLASS_STATIC;
	symbol.numberOfAuxSymbols = 0;

	symbols.add(symbol);

	return symbols.count() - 1;
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
			emptyString.value = static_cast<u32>(rdata->totalSize);
			emptyString.sectionNumber = rdata->sectionNumber;
			emptyString.type = 0;
			emptyString.storageClass = IMAGE_SYM_CLASS_EXTERNAL;
			emptyString.numberOfAuxSymbols = 0;

			symbols.add(emptyString);

			rdata->add1(0);
		}

		return static_cast<u32>(emptyStringSymbolIndex);
	}

	if (!(string->flags & EXPR_HAS_STORAGE)) {
		string->flags |= EXPR_HAS_STORAGE;

		string->physicalStorage = static_cast<u32>(symbols.count());
		string->symbol = allocateSymbol();

		setSymbolName(&string->symbol->name, symbols.count());
		string->symbol->storageClass = IMAGE_SYM_CLASS_STATIC;
		string->symbol->value = static_cast<u32>(rdata->totalSize);
		string->symbol->sectionNumber = rdata->sectionNumber;
		string->symbol->type = 0;
		string->symbol->numberOfAuxSymbols = 0;

		rdata->addNullTerminatedString(string->string);
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

void writeValue(u32 dataSize, u8 *data, Section *section, Expr *value);

void writeArrayLiteral(u32 dataSize, u8 *data, Section *section, ExprArrayLiteral *array) {
	auto arrayType = static_cast<TypeArray *>(array->type);

	auto elementSize = arrayType->arrayOf->size;
	auto arrayCount = arrayType->flags & TYPE_ARRAY_IS_FIXED ? arrayType->count : array->count;

	for (u64 i = 0; i < arrayCount; i++) {
		writeValue(dataSize, data, section, array->values[i]);

		dataSize += elementSize;
		data += elementSize;

		if (i + 1 == array->count && arrayCount > array->count) {
			for (u64 j = i + 1; j < arrayCount; j++) {
				writeValue(dataSize, data, section, array->values[i]);
				dataSize += elementSize;
				data += elementSize;
			}

			break;
		}
	}
}


void writeValue(u32 dataSize, u8 *data, Section *section, Expr *value) {
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

		section->addPointerRelocation(createSymbolForFunction(static_cast<ExprFunction *>(value)), dataSize);

		*reinterpret_cast<u64 *>(data) = 0;
	}
	else if (value->flavor == ExprFlavor::STRING_LITERAL) {
		u32 string = createSymbolForString(static_cast<ExprStringLiteral *>(value));

		section->addPointerRelocation(string, dataSize + offsetof(MiloString, data));

		auto stringData = reinterpret_cast<MiloString *>(data);
		stringData->data = nullptr;
		stringData->count = static_cast<ExprStringLiteral *>(value)->string.length;
	}
	else if (value->flavor == ExprFlavor::ARRAY_LITERAL) {
		auto array = static_cast<ExprArrayLiteral *>(value);
		
		if (array->type->flags & TYPE_ARRAY_IS_FIXED) {
			writeArrayLiteral(dataSize, data, section, array);
		}
		else {
			auto arrayType = static_cast<TypeArray *>(array->type);

			u32 newSize = section->totalSize;
			
			section->allocateUnaligned(AlignPO2(rdata->totalSize, arrayType->arrayOf->alignment) - rdata->totalSize);

			u32 symbolId = symbols.count();
			auto symbol = allocateSymbol();

			setSymbolName(&symbol->name, symbols.count());
			symbol->storageClass = IMAGE_SYM_CLASS_STATIC;
			symbol->value = static_cast<u32>(section->totalSize);
			symbol->sectionNumber = section->sectionNumber;
			symbol->type = 0;
			symbol->numberOfAuxSymbols = 0;

			u32 offset = section->totalSize;
			auto arrayData = static_cast<u8 *>(section->allocateUnaligned(arrayType->arrayOf->size * array->count));
			writeArrayLiteral(offset, arrayData, section, array);

			section->addPointerRelocation(symbolId, dataSize + offsetof(MiloArray<u8>, data));

			auto arrayPointer = reinterpret_cast<MiloArray<u8> *>(data);
			arrayPointer->data = nullptr;
			arrayPointer->count = array->count;
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
		assert(type->size == 8);

		markUsedTypeInfoInType(static_cast<ExprLiteral *>(value)->typeValue);

		section->addPointerRelocation(static_cast<ExprLiteral *>(value)->typeValue->physicalStorage, dataSize);

		*reinterpret_cast<u64 *>(data) = 0;
	}
	else if (value->flavor == ExprFlavor::STRUCT_LITERAL) {
		auto literal = static_cast<ExprStructLiteral *>(value);

		for (u32 i = 0; i < literal->initializers.count; i++) {
			auto offset = literal->initializers.declarations[i]->physicalStorage;
			writeValue(dataSize + offset, data + offset, section, literal->initializers.values[i]);
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

	code = makeSection(".text", 16, SECTION_EXECUTE | SECTION_READ | SECTION_INITIALIZED);
	data = makeSection(".data", 16, SECTION_READ | SECTION_WRITE | SECTION_INITIALIZED);
	rdata = makeSection(".rdata", 16, SECTION_READ | SECTION_INITIALIZED);
	bss = makeSection(".bss", 16, SECTION_READ | SECTION_WRITE);
	debugSymbols = makeSection(".debug$S", 1, SECTION_READ | SECTION_INITIALIZED | SECTION_DISCARD);
	debugTypes = makeSection(".debug$T", 1, SECTION_READ | SECTION_INITIALIZED | SECTION_DISCARD);
	pdata = makeSection(".pdata", 4, SECTION_READ | SECTION_INITIALIZED);
	xdata = makeSection(".xdata", 4, SECTION_READ | SECTION_INITIALIZED);

	if (hadError)
		return;

	s64 f32ToU64ConstantSymbolIndex = -1;
	s64 f64ToU64ConstantSymbolIndex = -1;
	s64 u64ToF32ConstantSymbolIndex = -1;
	s64 u64ToF64ConstantSymbolIndex = -1;
	s64 chkstkSymbolIndex = -1;

	u64 alignmentPadding = 0;

	Array<u64> instructionOffsets;
	Array<JumpPatch> jumpPatches;

	Array<LineInfo> lineInfo;
	Array<ColumnInfo> columnInfo;
	Array<u32 *> blockOffsetStack;

	debugSymbols->add4(4);
	debugTypes->add4(4);

	{
		debugSymbols->add4(0xF1);
		auto subsectionSizePatch = debugSymbols->add4(0);
		u32 subsectionOffset = debugSymbols->totalSize;

		COMPILESYM3 compileFlags;
		compileFlags.flags.iLanguage = 20; // @Cleanup Check no other language uses this
		compileFlags.flags.unused = 0;

#if BUILD_WINDOWS
		const char *compilerName = "Milo Compiler 0.1.1 (Windows-x64)";
#elif BUILD_LINUX
		const char *compilerName = "Milo Compiler 0.1.1 (Linux-x64)";
#endif

		debugSymbols->add2(static_cast<u16>(sizeof(compileFlags) + strlen(compilerName) + 1));
		debugSymbols->add(&compileFlags, sizeof(compileFlags));

		debugSymbols->addNullTerminatedString(compilerName);

		*subsectionSizePatch = debugSymbols->totalSize - subsectionOffset;

		alignAllocator(debugSymbols, 4);
	}

	emitBasicTypeDebugInfo(debugSymbols);

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

			u32 functionStart = code->totalSize;


			{
				addLineInfo(&lineInfo, &columnInfo, code->totalSize - functionStart, function->start, function->end);

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

				symbol->value = static_cast<u32>(code->totalSize);
				symbol->sectionNumber = code->sectionNumber;
				symbol->type = 0x20;


				symbol->numberOfAuxSymbols = 0;
			}

			u32 spaceToAllocate = getSpaceToAllocate(function);

			debugSymbols->ensure(8);
			debugSymbols->add4Unchecked(0xF1);
			auto subsectionSizePatch = debugSymbols->add4Unchecked(0);
			u32 subsectionOffset = debugSymbols->totalSize;

			auto name = function->valueOfDeclaration ? function->valueOfDeclaration->name : "__unnamed";

			debugSymbols->ensure(39);
			debugSymbols->add2Unchecked(static_cast<u16>(sizeof(PROCSYM32) + name.length - 1));
			debugSymbols->add2Unchecked(0x1147); // S_GPROC32_ID
			debugSymbols->add4Unchecked(0);
			debugSymbols->add4Unchecked(0);
			debugSymbols->add4Unchecked(0);
			u32 *functionLengthPatch = debugSymbols->add4Unchecked(code->totalSize - functionStart);
			u32 *functionPreambleEndPatch = debugSymbols->add4Unchecked(0);
			u32 *functionPostambleStartPatch = debugSymbols->add4Unchecked(0);
			u32 *patch = debugSymbols->add4Unchecked(0);
			coffFunctionIdTypePatches.add({ patch, function });
			
			debugSymbols->addSectionRelocations(function->physicalStorage);

			debugSymbols->add1Unchecked(0);
			debugSymbols->addNullTerminatedString(name);

			FRAMEPROCSYM frame;
			frame.cbFrame = spaceToAllocate;
			frame.flags.unused = 0;
			frame.flags.encodedLocalBasePointer = 1; // RSP
			frame.flags.encodedParamBasePointer = 1; // RSP
			frame.flags.pad = 0;

			debugSymbols->add(&frame, sizeof(frame));

			u32 paramOffset = 0;

			code->ensure(256);


			constexpr u8 intRegisters[4] = { RCX, RDX, 8, 9 };
			if (!isStandardSize(getDeclarationType(function->returns.declarations[0])->size)) {
				u8 reg = paramOffset + 1;
				writeIntegerPrefixX(8, &reg);
				sizedIntInstruction(8, MOV_MEM_REG_BASE);
				writeRSPOffsetByte(reg, (paramOffset + 1) * 8);
				paramOffset++;
			}
			
			if (!(function->flags & EXPR_FUNCTION_IS_C_CALL)) {
				u8 reg = paramOffset + 1;
				writeIntegerPrefixX(8, &reg);
				sizedIntInstruction(8, MOV_MEM_REG_BASE);
				writeRSPOffsetByte(reg, (paramOffset + 1) * 8);
				paramOffset++;

				REGREL32 contextInfo;

				contextInfo.off = getRegisterOffset(function, function->state.contextRegister);
				contextInfo.typind = 0;

				debugSymbols->ensure(2 + sizeof(contextInfo));
				debugSymbols->add2Unchecked(static_cast<u16>(sizeof(contextInfo) + 1 + 7));
				REGREL32 *patch = (REGREL32 *) debugSymbols->addUnchecked(&contextInfo, sizeof(contextInfo));
				coffTypePatches.add({ &patch->typind, &TYPE_CONTEXT });

				debugSymbols->addNullTerminatedString("context");
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

				debugSymbols->ensure(2 + sizeof(argumentInfo));
				debugSymbols->add2Unchecked(static_cast<u16>(sizeof(argumentInfo) + 1 + argument->name.length));
				REGREL32 *patch = (REGREL32 *)debugSymbols->addUnchecked(&argumentInfo, sizeof(argumentInfo));
				coffTypePatches.add({ &patch->typind, getDeclarationType(argument) });

				debugSymbols->addNullTerminatedString(argument->name);
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
					writeFloatPrefix(type->size);

					code->add1Unchecked(OPCODE_EXT);
					code->add1Unchecked(EXT_MOVSf_MEM_REG);
					writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, i + paramOffset, MODRM_RM_SIB);
				}
				else {
					u8 reg = intRegisters[i + paramOffset];

					u64 size = isStandardSize(type->size) ? type->size : 8;

					writeIntegerPrefixX(size, &reg);
					sizedIntInstruction(size, MOV_MEM_REG_BASE);
					writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, reg, MODRM_RM_SIB);
				}

				writeSIB(SIB_SCALE_1, SIB_INDEX_NONE, RSP);
				code->add1Unchecked((static_cast<u8>(i + paramOffset) + 1) * 8);
			}

			code->add1Unchecked(PUSH_BASE | RSI);
			u32 pushRsiOffset = code->totalSize - functionStart;
			code->add1Unchecked(PUSH_BASE | RDI);
			u32 pushRdiOffset = code->totalSize - functionStart;

			if (spaceToAllocate >= 4096) {
				loadImmediateIntoIntRegister64(RAX, spaceToAllocate);

				code->add1(CALL_DIRECT);

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
				
				code->addRel32Relocation(static_cast<u32>(chkstkSymbolIndex));

				writeIntegerPrefix(8);
				sizedIntInstruction(8, SUB_MEM_REG_BASE);
				writeModRM(MODRM_MOD_DIRECT, RAX, RSP);
			}
			else if (spaceToAllocate < 0x80) {
				writeIntegerPrefix(8);
				sizedIntInstruction(8, INT_OP_MEM_IMM_8_MODRM_X);
				writeModRM(MODRM_MOD_DIRECT, INT_OP_MODRM_SUB, RSP);
				code->add1Unchecked(static_cast<u8>(spaceToAllocate));
			}
			else {
				writeIntegerPrefix(8);
				sizedIntInstruction(8, INT_OP_MEM_IMM_32_MODRM_X);
				writeModRM(MODRM_MOD_DIRECT, INT_OP_MODRM_SUB, RSP);
				code->add4Unchecked(static_cast<u32>(spaceToAllocate));
			}
			u32 subRspOffset = code->totalSize - functionStart;

			u32 functionPreambleEnd = code->totalSize - functionStart;
			*functionPreambleEndPatch = functionPreambleEnd;

			for (u32 index = 0; index < function->state.ir.count; index++) {
				auto &ir = function->state.ir[index];

				instructionOffsets.add(code->totalSize);

				code->ensure(128);

				switch (ir.op) {
				case IrOp::TYPE: {
					auto type = static_cast<Type *>(ir.data);
					markUsedTypeInfoInType(type);

					writeIntegerPrefix(8);
					code->add1Unchecked(MOV_REG_IMM_WORD_BASE | RAX);
					code->addPointerRelocation(type->physicalStorage);

					storeFromIntRegister(function, 8, ir.dest, RAX);


					break;
				}
				case IrOp::ADD: {
					if (ir.flags & IR_FLOAT_OP) {
						loadIntoFloatRegister(function, ir.opSize, 0, ir.a);

						writeFloatPrefix(ir.opSize);
						code->add1Unchecked(OPCODE_EXT);
						code->add1Unchecked(EXT_ADDSf_REG_MEM);
						writeRSPRegisterByte(function, 0, ir.b);

						storeFromFloatRegister(function, ir.opSize, ir.dest, 0);
					}
					else {
						loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

						writeIntegerPrefix(ir.opSize);
						sizedIntInstruction(ir.opSize, ADD_REG_MEM_BASE);
						writeRSPRegisterByte(function, RAX, ir.b);

						storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
					}
				} break;
				case IrOp::ADD_CONSTANT: {
					if (ir.immediate == 0) {
						writeSet(function, ir.opSize, ir.dest, ir.a);
					}
					else {
						loadImmediateIntoIntRegister64(RAX, ir.immediate);

						writeIntegerPrefix(ir.opSize);
						sizedIntInstruction(ir.opSize, ADD_REG_MEM_BASE);
						writeRSPRegisterByte(function, RAX, ir.a);

						storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
					}
				} break;
				case IrOp::SUB: {
					if (ir.flags & IR_FLOAT_OP) {
						loadIntoFloatRegister(function, ir.opSize, 0, ir.a);


						writeFloatPrefix(ir.opSize);
						code->add1Unchecked(OPCODE_EXT);
						code->add1Unchecked(EXT_SUBSf_REG_MEM);
						writeRSPRegisterByte(function, 0, ir.b);

						storeFromFloatRegister(function, ir.opSize, ir.dest, 0);
					}
					else {
						loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

						writeIntegerPrefix(ir.opSize);
						sizedIntInstruction(ir.opSize, SUB_REG_MEM_BASE);
						writeRSPRegisterByte(function, RAX, ir.b);

						storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
					}
				} break;
				case IrOp::MUL: {
					if (ir.flags & IR_FLOAT_OP) {
						loadIntoFloatRegister(function, ir.opSize, 0, ir.a);

						writeFloatPrefix(ir.opSize);
						code->add1Unchecked(OPCODE_EXT);
						code->add1Unchecked(EXT_MULSf_REG_MEM);
						writeRSPRegisterByte(function, 0, ir.b);

						storeFromFloatRegister(function, ir.opSize, ir.dest, 0);
					}
					else {
						if (ir.opSize == 1) {
							loadIntoIntRegister(function, 1, RAX, ir.a);

							sizedIntInstruction(1, UNARY_INT_OP_BASE_MODRM_X);
							writeRSPRegisterByte(function, UNARY_INT_OP_MODRM_IMUL_MEM, ir.b);

							storeFromIntRegister(function, 1, ir.dest, RAX);
						}
						else {
							loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

							writeIntegerPrefix(ir.opSize);
							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_IMUL_REG_MEM);
							writeRSPRegisterByte(function, RAX, ir.b);

							storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
						}
					}
				} break;
				case IrOp::MUL_BY_CONSTANT: {
					assert(ir.opSize == 8);

					loadImmediateIntoIntRegister64(RAX, ir.immediate);

					writeIntegerPrefix(8);
					code->add1Unchecked(OPCODE_EXT);
					code->add1Unchecked(EXT_IMUL_REG_MEM);
					writeRSPRegisterByte(function, RAX, ir.a);

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::DIV: {
					if (ir.flags & IR_FLOAT_OP) {
						loadIntoFloatRegister(function, ir.opSize, 0, ir.a);

						writeFloatPrefix(ir.opSize);
						code->add1Unchecked(OPCODE_EXT);
						code->add1Unchecked(EXT_DIVSf_REG_MEM);
						writeRSPRegisterByte(function, 0, ir.b);

						storeFromFloatRegister(function, ir.opSize, ir.dest, 0);
					}
					else {

						if (ir.flags & IR_SIGNED_OP) {
							loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

							if (ir.opSize == 1) {
								code->add1Unchecked(OPERAND_SIZE_OVERRIDE);
								code->add1Unchecked(CBW);

								sizedIntInstruction(1, UNARY_INT_OP_BASE_MODRM_X);
								writeRSPRegisterByte(function, UNARY_INT_OP_MODRM_IDIV_MEM, ir.b);
							}
							else {
								writeIntegerPrefix(ir.opSize);
								code->add1Unchecked(CDQ);

								writeIntegerPrefix(ir.opSize);
								sizedIntInstruction(ir.opSize, UNARY_INT_OP_BASE_MODRM_X);
								writeRSPRegisterByte(function, UNARY_INT_OP_MODRM_IDIV_MEM, ir.b);
							}
						}
						else {
							if (ir.opSize == 1) {
								code->add1Unchecked(OPERAND_SIZE_OVERRIDE);
								code->add1Unchecked(OPCODE_EXT);
								code->add1Unchecked(EXT_MOVZX_REG_MEM_BASE);
								writeRSPRegisterByte(function, RAX, ir.a);

								sizedIntInstruction(1, UNARY_INT_OP_BASE_MODRM_X);
								writeRSPRegisterByte(function, UNARY_INT_OP_MODRM_DIV_MEM, ir.b);
							}
							else {
								loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

								sizedIntInstruction(4, XOR_REG_MEM_BASE);
								writeModRM(MODRM_MOD_DIRECT, RDX, RDX);


								writeIntegerPrefix(ir.opSize);
								sizedIntInstruction(ir.opSize, UNARY_INT_OP_BASE_MODRM_X);
								writeRSPRegisterByte(function, UNARY_INT_OP_MODRM_DIV_MEM, ir.b);
							}
						}

						storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
					}
				} break;
				case IrOp::DIVIDE_BY_CONSTANT: {
					assert(ir.opSize == 8);
					loadImmediateIntoIntRegister64(RCX, ir.immediate);

					loadIntoIntRegister(function, 8, RAX, ir.a);

					writeIntegerPrefix(8);
					code->add1Unchecked(CDQ);

					writeIntegerPrefix(8);
					sizedIntInstruction(ir.opSize, UNARY_INT_OP_BASE_MODRM_X);
					writeModRM(MODRM_MOD_DIRECT, UNARY_INT_OP_MODRM_DIV_MEM, RCX);

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::MOD: {
					if (ir.flags & IR_FLOAT_OP) {
						// @Incomplete: x64 doesn't have native fmod, call out to fmod in crt or implement our own
						assert(false);
					}
					else {
						if (ir.flags & IR_FLOAT_OP) {
							loadIntoFloatRegister(function, ir.opSize, 0, ir.a);

							writeFloatPrefix(ir.opSize);
							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_DIVSf_REG_MEM);
							writeRSPRegisterByte(function, 0, ir.b);

							storeFromFloatRegister(function, ir.opSize, ir.dest, 0);
						}
						else {

							if (ir.flags & IR_SIGNED_OP) {
								loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

								if (ir.opSize == 1) {
									code->add1Unchecked(OPERAND_SIZE_OVERRIDE);
									code->add1Unchecked(CBW);

									sizedIntInstruction(1, UNARY_INT_OP_BASE_MODRM_X);
									writeRSPRegisterByte(function, UNARY_INT_OP_MODRM_IDIV_MEM, ir.b);

									sizedIntInstruction(1, MOV_REG_MEM_BASE);
									writeModRM(MODRM_MOD_DIRECT, RDX, AH);
								}
								else {
									writeIntegerPrefix(ir.opSize);
									code->add1Unchecked(CDQ);

									writeIntegerPrefix(ir.opSize);
									sizedIntInstruction(ir.opSize, UNARY_INT_OP_BASE_MODRM_X);
									writeRSPRegisterByte(function, UNARY_INT_OP_MODRM_IDIV_MEM, ir.b);
								}
							}
							else {
								if (ir.opSize == 1) {
									code->add1Unchecked(OPERAND_SIZE_OVERRIDE);
									code->add1Unchecked(OPCODE_EXT);
									code->add1Unchecked(EXT_MOVZX_REG_MEM_BASE);
									writeRSPRegisterByte(function, RAX, ir.a);

									sizedIntInstruction(1, UNARY_INT_OP_BASE_MODRM_X);
									writeRSPRegisterByte(function, UNARY_INT_OP_MODRM_DIV_MEM, ir.b);

									sizedIntInstruction(1, MOV_REG_MEM_BASE);
									writeModRM(MODRM_MOD_DIRECT, RDX, AH);
								}
								else {
									loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

									sizedIntInstruction(4, XOR_REG_MEM_BASE);
									writeModRM(MODRM_MOD_DIRECT, RDX, RDX);


									writeIntegerPrefix(ir.opSize);
									sizedIntInstruction(ir.opSize, UNARY_INT_OP_BASE_MODRM_X);
									writeRSPRegisterByte(function, UNARY_INT_OP_MODRM_DIV_MEM, ir.b);
								}
							}

							storeFromIntRegister(function, ir.opSize, ir.dest, RDX);
						}
					}
				} break;
				case IrOp::AND: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

					writeIntegerPrefix(ir.opSize);
					sizedIntInstruction(ir.opSize, AND_REG_MEM_BASE);
					writeRSPRegisterByte(function, RAX, ir.b);

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::OR: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

					writeIntegerPrefix(ir.opSize);
					sizedIntInstruction(ir.opSize, OR_REG_MEM_BASE);
					writeRSPRegisterByte(function, RAX, ir.b);

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::XOR: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

					writeIntegerPrefix(ir.opSize);
					sizedIntInstruction(ir.opSize, XOR_REG_MEM_BASE);
					writeRSPRegisterByte(function, RAX, ir.b);

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::NOT: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

					writeIntegerPrefix(ir.opSize);
					sizedIntInstruction(ir.opSize, UNARY_INT_OP_BASE_MODRM_X);
					writeModRM(MODRM_MOD_DIRECT, UNARY_INT_OP_MODRM_NOT, RAX);

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::POP_COUNT: {

					// There is no 8 bit version of popcnt
					if (ir.opSize == 1) {
						code->add1Unchecked(OPCODE_EXT);
						sizedIntInstruction(1, EXT_MOVZX_REG_MEM_BASE);
						writeRSPRegisterByte(function, RAX, ir.a);

						writeIntegerPrefixRep(1);
						code->add1Unchecked(OPCODE_EXT);
						code->add1Unchecked(REP_EXT_POPCNT_REG_MEM);
						writeModRM(MODRM_MOD_DIRECT, RAX, RAX);
					}
					else {
						writeIntegerPrefixRep(ir.opSize);
						code->add1Unchecked(OPCODE_EXT);
						code->add1Unchecked(REP_EXT_POPCNT_REG_MEM);
						writeRSPRegisterByte(function, RAX, ir.a);
					}


					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::BIT_SCAN_FORWARD: {

					// There is no 8 bit version of bsf
					if (ir.opSize == 1) {
						code->add1Unchecked(OPCODE_EXT);
						sizedIntInstruction(1, EXT_MOVZX_REG_MEM_BASE);
						writeRSPRegisterByte(function, RAX, ir.a);

						writeIntegerPrefix(1);
						code->add1Unchecked(OPCODE_EXT);
						code->add1Unchecked(EXT_BSF_REG_MEM);
						writeModRM(MODRM_MOD_DIRECT, RAX, RAX);
					}
					else {
						writeIntegerPrefix(ir.opSize);
						code->add1Unchecked(OPCODE_EXT);
						code->add1Unchecked(EXT_BSF_REG_MEM);
						writeRSPRegisterByte(function, RAX, ir.a);
					}


					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);

					setCondition(function, ir.b, C_Z);
				} break;
				case IrOp::BIT_SCAN_REVERSE: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

					// There is no 8 bit version of bsf
					if (ir.opSize == 1) {
						code->add1Unchecked(OPCODE_EXT);
						sizedIntInstruction(1, EXT_MOVZX_REG_MEM_BASE);
						writeRSPRegisterByte(function, RAX, ir.a);

						writeIntegerPrefix(1);
						code->add1Unchecked(OPCODE_EXT);
						code->add1Unchecked(EXT_BSR_REG_MEM);
						writeModRM(MODRM_MOD_DIRECT, RAX, RAX);
					}
					else {
						writeIntegerPrefix(ir.opSize);
						code->add1Unchecked(OPCODE_EXT);
						code->add1Unchecked(EXT_BSR_REG_MEM);
						writeRSPRegisterByte(function, RAX, ir.a);
					}

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);

					setCondition(function, ir.b, C_Z);
				} break;
				case IrOp::SHIFT_LEFT: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);
					loadIntoIntRegister(function, ir.opSize, RCX, ir.b);

					writeIntegerPrefix(ir.opSize);
					sizedIntInstruction(ir.opSize, SHIFT_RCX_BASE_MODRM_X);
					writeModRM(MODRM_MOD_DIRECT, SHIFT_MODRM_SHL, RAX);

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::SHIFT_RIGHT: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);
					loadIntoIntRegister(function, ir.opSize, RCX, ir.b);

					writeIntegerPrefix(ir.opSize);
					sizedIntInstruction(ir.opSize, SHIFT_RCX_BASE_MODRM_X);
					writeModRM(MODRM_MOD_DIRECT, ir.flags & IR_SIGNED_OP ? SHIFT_MODRM_SAR : SHIFT_MODRM_SHR, RAX);

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::READ: {
					assert(isStandardSize(ir.opSize));
					loadIntoIntRegister(function, 8, RAX, ir.a);

					writeIntegerPrefix(ir.opSize);
					sizedIntInstruction(ir.opSize, MOV_REG_MEM_BASE);

					if (!ir.immediate) {
						writeModRM(MODRM_MOD_INDIRECT, RAX, RAX);
					}
					else if (ir.immediate <= 0x7f) {
						writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, RAX, RAX);
						code->add1Unchecked(static_cast<u8>(ir.immediate));
					}
					else {
						writeModRM(MODRM_MOD_INDIRECT_OFFSET_32, RAX, RAX);
						code->add4Unchecked(static_cast<u32>(ir.immediate));
					}

					storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
				} break;
				case IrOp::WRITE: {
					assert(isStandardSize(ir.opSize));

					loadIntoIntRegister(function, 8, RAX, ir.dest);
					loadIntoIntRegister(function, ir.opSize, RCX, ir.a);


					writeIntegerPrefix(ir.opSize);
					sizedIntInstruction(ir.opSize, MOV_MEM_REG_BASE);

					if (!ir.immediate) {
						writeModRM(MODRM_MOD_INDIRECT, RCX, RAX);
					}
					else if (ir.immediate <= 0x7f) {
						writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, RCX, RAX);
						code->add1Unchecked(static_cast<u8>(ir.immediate));
					}
					else {
						writeModRM(MODRM_MOD_INDIRECT_OFFSET_32, RCX, RAX);
						code->add4Unchecked(static_cast<u32>(ir.immediate));
					}
				} break;
				case IrOp::COPY: {
					assert(!ir.immediate);
					if (isStandardSize(ir.opSize)) {
						loadIntoIntRegister(function, 8, RAX, ir.a);
						loadIntoIntRegister(function, 8, RCX, ir.dest);

						writeIntegerPrefix(ir.opSize);
						sizedIntInstruction(ir.opSize, MOV_REG_MEM_BASE);
						writeModRM(MODRM_MOD_INDIRECT, RAX, RAX);

						writeIntegerPrefix(ir.opSize);
						sizedIntInstruction(ir.opSize, MOV_MEM_REG_BASE);
						writeModRM(MODRM_MOD_INDIRECT, RAX, RCX);
					}
					else {
						loadIntoIntRegister(function, 8, RDI, ir.dest);
						loadIntoIntRegister(function, 8, RSI, ir.a);
						loadImmediateIntoIntRegister64(RCX, ir.opSize);

						code->add1Unchecked(REP_PREFIX);
						sizedIntInstruction(1, MOVS_BASE);
					}
				} break;
				case IrOp::ZERO_MEMORY: {
					assert(!ir.immediate);

					sizedIntInstruction(4, XOR_REG_MEM_BASE);
					writeModRM(MODRM_MOD_DIRECT, RAX, RAX);

					if (isStandardSize(ir.opSize)) {
						loadIntoIntRegister(function, 8, RCX, ir.dest);

						writeIntegerPrefix(ir.opSize);
						sizedIntInstruction(ir.opSize, MOV_MEM_REG_BASE);
						writeModRM(MODRM_MOD_INDIRECT, RAX, RCX);
					}
					else {
						loadIntoIntRegister(function, 8, RDI, ir.dest);

						loadImmediateIntoIntRegister64(RCX, ir.opSize);

						code->add1Unchecked(REP_PREFIX);
						sizedIntInstruction(1, STOS_BASE);
					}
				} break;
				case IrOp::SET: {
					writeSet(function, ir.opSize, ir.dest, ir.a);
				} break;
				case IrOp::TYPE_INFO: {
					writeSet(function, 8, ir.dest, ir.a);
				} break;
				case IrOp::FLOAT_CAST: {
					writeFloatPrefix(ir.opSize);
					code->add1Unchecked(OPCODE_EXT);
					code->add1Unchecked(EXT_CVTSf2Sf_REG_MEM);
					writeRSPRegisterByte(function, 0, ir.a);

					storeFromFloatRegister(function, ir.b, ir.dest, 0);
				} break;
				case IrOp::EXTEND_INT: {
					if (ir.flags & IR_SIGNED_OP) {
						if (ir.opSize == 4) {
							writeIntegerPrefix(ir.b);
							code->add1Unchecked(MOVSX_REG_MEM_32);
							writeRSPRegisterByte(function, RAX, ir.a);
						}
						else {
							writeIntegerPrefix(ir.b);
							code->add1Unchecked(OPCODE_EXT);
							sizedIntInstruction(ir.opSize, EXT_MOVSX_REG_MEM_BASE);
							writeRSPRegisterByte(function, RAX, ir.a);
						}
					}
					else {
						if (ir.opSize == 4) {
							loadIntoIntRegister(function, ir.opSize, RAX, ir.a);
						}
						else {
							writeIntegerPrefix(ir.b);
							code->add1Unchecked(OPCODE_EXT);
							sizedIntInstruction(ir.opSize, EXT_MOVZX_REG_MEM_BASE);
							writeRSPRegisterByte(function, RAX, ir.a);
						}
					}

					storeFromIntRegister(function, ir.b, ir.dest, RAX);
				} break;
				case IrOp::GOTO: {
					code->add1Unchecked(JMP_DIRECT);

					JumpPatch patch;
					patch.opToPatch = ir.b;
					patch.location = reinterpret_cast<s32 *>(code->add4Unchecked(0));
					patch.rip = code->totalSize;

					jumpPatches.add(patch);
				} break;
				case IrOp::IF_Z_GOTO: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);

					writeIntegerPrefix(ir.opSize);
					sizedIntInstruction(ir.opSize, TEST_MEM_REG_BASE);
					writeModRM(MODRM_MOD_DIRECT, RAX, RAX);

					code->add1Unchecked(OPCODE_EXT);
					code->add1Unchecked(EXT_JCC_DIRECT_BASE | C_Z);

					JumpPatch patch;
					patch.opToPatch = ir.b;
					patch.location = reinterpret_cast<s32 *>(code->add4Unchecked(0));
					patch.rip = code->totalSize;

					jumpPatches.add(patch);
				} break;
				case IrOp::IF_NZ_GOTO: {
					loadIntoIntRegister(function, ir.opSize, RAX, ir.a);


					writeIntegerPrefix(ir.opSize);
					sizedIntInstruction(ir.opSize, TEST_MEM_REG_BASE);
					writeModRM(MODRM_MOD_DIRECT, RAX, RAX);

					code->add1Unchecked(OPCODE_EXT);
					code->add1Unchecked(EXT_JCC_DIRECT_BASE | C_NZ);

					JumpPatch patch;
					patch.opToPatch = ir.b;
					patch.location = reinterpret_cast<s32 *>(code->add4Unchecked(0));
					patch.rip = code->totalSize;

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

					writeIntegerPrefix(8);
					code->add1Unchecked(LEA);
					writeModRM(MODRM_MOD_INDIRECT, RAX, MODRM_RM_RIP_OFFSET_32);
					*code->addRel32Relocation(createSymbolForDeclaration(declaration)) = ir.a;

					storeFromIntRegister(function, 8, ir.dest, RAX);
				} break;
				case IrOp::STACK_ADDRESS: {
					writeIntegerPrefix(8);
					code->add1Unchecked(LEA);
					writeRSPOffsetByte(RAX, getStackSpaceOffset(function) + ir.immediate);

					storeFromIntRegister(function, 8, ir.dest, RAX);
				} break;
				case IrOp::IMMEDIATE: {
					storeImmediate(function, ir.opSize, ir.dest, ir.immediate);
				} break;
				case IrOp::FLOAT_TO_INT: {
					if (ir.flags & IR_SIGNED_OP) {
						writeFloatPrefix(ir.opSize);
						if (ir.b == 8) {
							writeIntegerPrefix(8);
						}
						code->add1Unchecked(OPCODE_EXT);
						code->add1Unchecked(EXT_CVTTSf2SI_REG_MEM);
						writeRSPRegisterByte(function, RAX, ir.a);

						storeFromIntRegister(function, ir.b, ir.dest, RAX);
					}
					else {
						if (ir.b == 8) { // Aww sheet
							loadIntoFloatRegister(function, ir.opSize, 0, ir.a);

							writeFloatPrefix(ir.opSize);
							writeIntegerPrefix(8);
							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_CVTTSf2SI_REG_MEM);
							writeModRM(MODRM_MOD_DIRECT, RCX, 0);

							sizedIntInstruction(8, MOV_REG_MEM_BASE);
							writeModRM(MODRM_MOD_DIRECT, RDX, RCX);


							if (ir.opSize == 8) {
								if (f64ToU64ConstantSymbolIndex == -1) {
									f64ToU64ConstantSymbolIndex = symbols.count();

									rdata->allocateUnaligned(AlignPO2(rdata->totalSize, 8) - rdata->totalSize);

									Symbol f64ToU64Constant;
									setSymbolName(&f64ToU64Constant.name, "@f64ToU64Constant");
									f64ToU64Constant.value = static_cast<u32>(rdata->totalSize);
									f64ToU64Constant.sectionNumber = rdata->sectionNumber;
									f64ToU64Constant.type = 0;
									f64ToU64Constant.storageClass = IMAGE_SYM_CLASS_STATIC;
									f64ToU64Constant.numberOfAuxSymbols = 0;

									symbols.add(f64ToU64Constant);

									rdata->add8(0x43E0000000000000);
								}
							}
							else {
								if (f32ToU64ConstantSymbolIndex == -1) {
									f32ToU64ConstantSymbolIndex = symbols.count();

									rdata->allocateUnaligned(AlignPO2(rdata->totalSize, 4) - rdata->totalSize);

									Symbol f32ToU64Constant;
									setSymbolName(&f32ToU64Constant.name, "@f32ToU64Constant");
									f32ToU64Constant.value = static_cast<u32>(rdata->totalSize);
									f32ToU64Constant.sectionNumber = rdata->sectionNumber;
									f32ToU64Constant.type = 0;
									f32ToU64Constant.storageClass = IMAGE_SYM_CLASS_STATIC;
									f32ToU64Constant.numberOfAuxSymbols = 0;

									symbols.add(f32ToU64Constant);

									rdata->add4(0x5F000000);

								}
							}

							writeFloatPrefix(ir.opSize);
							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_SUBSf_REG_MEM);
							writeModRM(MODRM_MOD_INDIRECT, 0, MODRM_RM_RIP_OFFSET_32);
							code->addRel32Relocation(ir.opSize == 8 ? f64ToU64ConstantSymbolIndex : f32ToU64ConstantSymbolIndex);

							writeFloatPrefix(ir.opSize);
							writeIntegerPrefix(8);
							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_CVTTSf2SI_REG_MEM);
							writeModRM(MODRM_MOD_DIRECT, RAX, 0);

							sizedIntInstruction(8, SHIFT_IMM8_BASE_MODRM_X);
							writeModRM(MODRM_MOD_DIRECT, RDX, SHIFT_MODRM_SAR);
							code->add1Unchecked(63);

							sizedIntInstruction(8, AND_REG_MEM_BASE);
							writeModRM(MODRM_MOD_DIRECT, RAX, RDX);

							sizedIntInstruction(8, OR_REG_MEM_BASE);
							writeModRM(MODRM_MOD_DIRECT, RAX, RCX);
						}

						else {
							writeFloatPrefix(ir.opSize);
							if (ir.opSize >= 4) {
								writeIntegerPrefix(8);
							}

							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_CVTTSf2SI_REG_MEM);


							writeRSPRegisterByte(function, RAX, ir.a);
						}

						storeFromIntRegister(function, ir.b, ir.dest, RAX);
					}
				} break;
				case IrOp::INT_TO_FLOAT: {
					if (ir.flags & IR_SIGNED_OP) {
						if (ir.opSize >= 4) {
							writeFloatPrefix(ir.b);
							writeIntegerPrefix(ir.opSize);
							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_CVTSI2Sf_REG_MEM);
							writeRSPRegisterByte(function, 0, ir.a);

							storeFromFloatRegister(function, ir.b, ir.dest, 0);
						}
						else {
							code->add1Unchecked(OPCODE_EXT);
							sizedIntInstruction(ir.opSize, EXT_MOVSX_REG_MEM_BASE);
							writeRSPRegisterByte(function, RAX, ir.a);

							writeFloatPrefix(ir.b);
							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_CVTSI2Sf_REG_MEM);
							writeModRM(MODRM_MOD_DIRECT, RAX, 0);

							storeFromFloatRegister(function, ir.b, ir.dest, 0);
						}
					}
					else {
						if (ir.opSize == 8) {
							if (ir.b == 8) {
								if (u64ToF64ConstantSymbolIndex == -1) {
									u64ToF64ConstantSymbolIndex = symbols.count();

									rdata->allocateUnaligned(AlignPO2(rdata->totalSize, 8) - rdata->totalSize);

									Symbol u64ToF64Constant;
									setSymbolName(&u64ToF64Constant.name, "@u64ToF64Constant");
									u64ToF64Constant.value = static_cast<u32>(rdata->totalSize);
									u64ToF64Constant.sectionNumber = rdata->sectionNumber;
									u64ToF64Constant.type = 0;
									u64ToF64Constant.storageClass = IMAGE_SYM_CLASS_STATIC;
									u64ToF64Constant.numberOfAuxSymbols = 0;

									symbols.add(u64ToF64Constant);

									rdata->add8(0x43F0000000000000ULL);
								}
							}
							else {
								if (u64ToF32ConstantSymbolIndex == -1) {
									u64ToF32ConstantSymbolIndex = symbols.count();

									rdata->allocateUnaligned(AlignPO2(rdata->totalSize, 4) - rdata->totalSize);

									Symbol u64ToF32Constant;
									setSymbolName(&u64ToF32Constant.name, "@u64ToF32Constant");
									u64ToF32Constant.value = static_cast<u32>(rdata->totalSize);
									u64ToF32Constant.sectionNumber = rdata->sectionNumber;
									u64ToF32Constant.type = 0;
									u64ToF32Constant.storageClass = IMAGE_SYM_CLASS_STATIC;
									u64ToF32Constant.numberOfAuxSymbols = 0;

									symbols.add(u64ToF32Constant);

									rdata->add4(0x5F800000);

								}
							}

							loadIntoIntRegister(function, 8, RAX, ir.a);

							writeFloatPrefix(ir.b);
							writeIntegerPrefix(8);
							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_CVTSI2Sf_REG_MEM);
							writeModRM(MODRM_MOD_DIRECT, 0, RAX);

							writeIntegerPrefix(8);
							sizedIntInstruction(8, TEST_MEM_REG_BASE);
							writeModRM(MODRM_MOD_DIRECT, RAX, RAX);

							code->add1Unchecked(JCC_DIRECT_8_BASE | C_NS); // jns .done
							u8 *jumpPatch = code->add1Unchecked(0);
							u32 jumpRel = code->totalSize;

							writeFloatPrefix(ir.b);
							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_ADDSf_REG_MEM);
							writeModRM(MODRM_MOD_INDIRECT, 0, MODRM_RM_RIP_OFFSET_32);
							code->addRel32Relocation(ir.b == 8 ? u64ToF64ConstantSymbolIndex : u64ToF32ConstantSymbolIndex);

							*jumpPatch = code->totalSize - jumpRel;

							// .done
							storeFromFloatRegister(function, ir.b, ir.dest, 0);
						}
						else if (ir.opSize == 4) {
							loadIntoIntRegister(function, 4, RAX, ir.a);

							writeFloatPrefix(ir.b);
							writeIntegerPrefix(8);
							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_CVTSI2Sf_REG_MEM);
							writeModRM(MODRM_MOD_DIRECT, 0, RAX);

							storeFromFloatRegister(function, ir.b, ir.dest, 0);
						}
						else {
							code->add1Unchecked(OPCODE_EXT);
							sizedIntInstruction(ir.opSize, EXT_MOVZX_REG_MEM_BASE);
							writeRSPRegisterByte(function, RAX, ir.a);


							writeFloatPrefix(ir.b);
							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_CVTSI2Sf_REG_MEM);
							writeModRM(MODRM_MOD_DIRECT, 0, RAX);

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

					code->add1Unchecked(JMP_DIRECT);

					JumpPatch patch;
					patch.opToPatch = function->state.ir.count;
					patch.location = reinterpret_cast<s32 *>(code->add4Unchecked(0));
					patch.rip = code->totalSize;

					jumpPatches.add(patch);
				} break;
				case IrOp::CALL: {
					constexpr int intRegisters[4] = { RCX, RDX, 8, 9 };

					auto arguments = static_cast<FunctionCall *>(ir.data);

					code->ensure(128);

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
						

						writeIntegerPrefix(8);
						sizedIntInstruction(8, MOV_MEM_REG_BASE);
						writeRSPOffsetByte(RAX, getParameterSpaceForCallOffset(function) + i * 8);

						code->ensure(128);
					}

					assert(isStandardSize(arguments->returnType->size));

					code->add1Unchecked(CALL_INDIRECT_MODRM_2);
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
						code->add1Unchecked(OPCODE_EXT);
						code->add1Unchecked(EXT_XORPf_REG_MEM);
						writeModRM(MODRM_MOD_DIRECT, 0, 0);

						writeFloatPrefix(ir.opSize);
						code->add1Unchecked(OPCODE_EXT);
						code->add1Unchecked(EXT_SUBSf_REG_MEM);
						writeRSPRegisterByte(function, 0, ir.a);

						storeFromFloatRegister(function, ir.opSize, ir.dest, 0);
					}
					else {
						loadIntoIntRegister(function, ir.opSize, RAX, ir.a);
						
						writeIntegerPrefix(ir.opSize);
						sizedIntInstruction(ir.opSize, UNARY_INT_OP_BASE_MODRM_X);
						writeModRM(MODRM_MOD_DIRECT, UNARY_INT_OP_MODRM_NEG, RAX);

						storeFromIntRegister(function, ir.opSize, ir.dest, RAX);
					}
				} break;
				case IrOp::NOOP: {
					// we are done
				} break;
				case IrOp::FUNCTION: {
					writeIntegerPrefix(8);
					code->add1Unchecked(LEA);
					writeModRM(MODRM_MOD_INDIRECT, RAX, MODRM_RM_RIP_OFFSET_32);
					code->addRel32Relocation(createSymbolForFunction(ir.function));

					storeFromIntRegister(function, 8, ir.dest, RAX);
				} break;
				case IrOp::STRING: {
					writeIntegerPrefix(8);
					code->add1Unchecked(LEA);
					writeModRM(MODRM_MOD_INDIRECT, RAX, MODRM_RM_RIP_OFFSET_32);
					code->addRel32Relocation(createSymbolForString(static_cast<ExprStringLiteral *>(ir.data)));

					storeFromIntRegister(function, 8, ir.dest, RAX);
				} break;
				case IrOp::ARRAY_LITERAL: {
					auto array = static_cast<ExprArrayLiteral *>(ir.data);

					writeIntegerPrefix(8);
					code->add1Unchecked(LEA);
					writeModRM(MODRM_MOD_INDIRECT, RAX, MODRM_RM_RIP_OFFSET_32);

					if (!(array->flags & EXPR_HAS_STORAGE)) {
						array->flags |= EXPR_HAS_STORAGE;

						array->physicalStorage = static_cast<u32>(symbols.count());
						array->symbol = allocateSymbol();

						alignAllocator(rdata, array->type->alignment);

						setSymbolName(&array->symbol->name, symbols.count());
						array->symbol->storageClass = IMAGE_SYM_CLASS_STATIC;
						array->symbol->value = static_cast<u32>(rdata->totalSize);
						array->symbol->sectionNumber = rdata->sectionNumber;
						array->symbol->type = 0;
						array->symbol->numberOfAuxSymbols = 0;

						u32 offset = rdata->totalSize; // do this in a separate statement because within a c++ statement, subexpressions can execute in any order
						                              // which meant that allocateUnaligned happened _before_ rdata->totalSize was evaluated, causing the addresses 
						                              // for any patch applied to the literal to be wrong
						writeValue(offset, static_cast<u8 *>(rdata->allocateUnaligned(array->type->size)), rdata, array);
					}

					code->addRel32Relocation(array->physicalStorage);

					storeFromIntRegister(function, 8, ir.dest, RAX);
				} break;
				case IrOp::STRUCT_LITERAL: {
					auto literal = static_cast<ExprStructLiteral *>(ir.data);

					writeIntegerPrefix(8);
					code->add1Unchecked(LEA);
					writeModRM(MODRM_MOD_INDIRECT, RAX, MODRM_RM_RIP_OFFSET_32);

					if (!(literal->flags & EXPR_HAS_STORAGE)) {
						literal->flags |= EXPR_HAS_STORAGE;

						literal->physicalStorage = static_cast<u32>(symbols.count());
						literal->symbol = allocateSymbol();

						rdata->allocateUnaligned(AlignPO2(rdata->totalSize, literal->type->alignment) - rdata->totalSize);

						setSymbolName(&literal->symbol->name, symbols.count());
						literal->symbol->storageClass = IMAGE_SYM_CLASS_STATIC;
						literal->symbol->value = static_cast<u32>(rdata->totalSize);
						literal->symbol->sectionNumber = rdata->sectionNumber;
						literal->symbol->type = 0;
						literal->symbol->numberOfAuxSymbols = 0;

						u32 offset = rdata->totalSize; // do this in a separate statement because within a c++ statement, subexpressions can execute in any order
													  // which meant that allocateUnaligned happened _before_ rdata->totalSize was evaluated, causing the addresses 
													  // for any patch applied to the literal to be wrong
						writeValue(offset, static_cast<u8 *>(rdata->allocateUnaligned(literal->type->size)), rdata, literal);
					}

					code->addRel32Relocation(literal->physicalStorage);

					storeFromIntRegister(function, 8, ir.dest, RAX);
				} break;
				case IrOp::LINE_MARKER: {
					addLineInfo(&lineInfo, &columnInfo, code->totalSize - functionStart, ir.location.start, ir.location.end);
				} break;
				case IrOp::BLOCK: {
					auto block = static_cast<Block *>(ir.data);

					if (block) {
						debugSymbols->ensure(23);

						debugSymbols->add2Unchecked(21);
						debugSymbols->add2Unchecked(0x1103); // S_BLOCK32

						debugSymbols->add4Unchecked(0);
						debugSymbols->add4Unchecked(0);

						blockOffsetStack.add(debugSymbols->add4(0));

						*debugSymbols->addSectionRelocations(function->physicalStorage) = code->totalSize - functionStart;

						debugSymbols->add1Unchecked(0);

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

							debugSymbols->ensure(2 + sizeof(variableInfo));
							debugSymbols->add2Unchecked(static_cast<u16>(sizeof(variableInfo) + declaration->name.length + 1));
							REGREL32 *patch = (REGREL32 *) debugSymbols->addUnchecked(&variableInfo, sizeof(variableInfo));
							coffTypePatches.add({ &patch->typind, getDeclarationType(declaration) });
							debugSymbols->addNullTerminatedString(declaration->name);
						}
					}
					else {
						u32 *length = blockOffsetStack.pop();
						*length = code->totalSize - functionStart - length[1];

						debugSymbols->ensure(4);
						debugSymbols->add2Unchecked(2);
						debugSymbols->add2Unchecked(6); // S_END
					}
				} break;
				default: {
					assert(false);
				}
				}
			}
			
			*functionPostambleStartPatch = code->totalSize - functionStart;
			u32 functionPostambleStart = code->totalSize;

			code->ensure(64);

			// add rsp, spaceToAllocate
			if (spaceToAllocate < 0x80) {
				writeIntegerPrefix(8);
				code->add1Unchecked(INT_OP_MEM_IMM_8_MODRM_X);
				writeModRM(MODRM_MOD_DIRECT, INT_OP_MODRM_ADD, RSP);
				code->add1Unchecked(static_cast<u8>(spaceToAllocate));
			}
			else {
				writeIntegerPrefix(8);
				code->add1Unchecked(INT_OP_MEM_IMM_32_MODRM_X);
				writeModRM(MODRM_MOD_DIRECT, INT_OP_MODRM_ADD, RSP);
				code->add4Unchecked(static_cast<u32>(spaceToAllocate));
			}

			code->add1Unchecked(POP_BASE | RDI);
			code->add1Unchecked(POP_BASE | RSI);

			code->add1Unchecked(RET);

			*functionLengthPatch = code->totalSize - functionStart;

			instructionOffsets.add(functionPostambleStart);

			for (auto patch : jumpPatches) {
				*patch.location = static_cast<s32>(instructionOffsets[patch.opToPatch]) - static_cast<s32>(patch.rip);
			}

			{
				PROFILE_ZONE("Write Function Debug Symbols");
				debugSymbols->ensure(4);
				debugSymbols->add2Unchecked(2); // S_PROC_ID_END
				debugSymbols->add2Unchecked(0x114f);

				*subsectionSizePatch = debugSymbols->totalSize - subsectionOffset;

				alignAllocator(debugSymbols, 4);

				pdata->ensure(12);

				pdata->addAddr32NBRelocation(function->physicalStorage);
				*pdata->addAddr32NBRelocation(function->physicalStorage) = code->totalSize - functionStart;
				pdata->addAddr32NBRelocation(symbols.count());

				Symbol xdataSymbol;
				setSymbolName(&xdataSymbol.name, symbols.count());
				xdataSymbol.value = xdata->totalSize;
				xdataSymbol.type = 0;
				xdataSymbol.sectionNumber = xdata->sectionNumber;
				xdataSymbol.storageClass = IMAGE_SYM_CLASS_STATIC;
				xdataSymbol.numberOfAuxSymbols = 0;

				symbols.add(xdataSymbol);

				xdata->ensure(11);
				xdata->add1Unchecked(1);
				xdata->add1Unchecked(functionPreambleEnd);
				xdata->add1Unchecked(4);
				xdata->add1Unchecked(0);

				xdata->add1Unchecked(subRspOffset);
				xdata->add1Unchecked(0x01);
				xdata->add2Unchecked(spaceToAllocate / 8);
				xdata->add1Unchecked(pushRdiOffset);
				xdata->add1Unchecked(0x70);
				xdata->add1Unchecked(pushRsiOffset);
				xdata->add1Unchecked(0x60);
			}

			{
				PROFILE_ZONE("Write Function Debug Lines");
				debugSymbols->ensure(32 + lineInfo.count * 12);
				
				debugSymbols->add4Unchecked(0xF2);
				debugSymbols->add4Unchecked(24 + lineInfo.count * 12);

				debugSymbols->addSectionRelocations(function->physicalStorage);

				debugSymbols->add2Unchecked(1); // fHasColumns
				debugSymbols->add4Unchecked(code->totalSize - functionStart);

				debugSymbols->add4Unchecked(function->start.fileUid * 8);
				debugSymbols->add4Unchecked(lineInfo.count);
				debugSymbols->add4Unchecked(12 + lineInfo.count * 12);
				debugSymbols->addUnchecked(lineInfo.storage, lineInfo.count * sizeof(LineInfo));
				debugSymbols->addUnchecked(columnInfo.storage, columnInfo.count * sizeof(ColumnInfo));
			}

		}
		else if (job.flavor == CoffJobFlavor::GLOBAL_DECLARATION) {
			PROFILE_ZONE("Write Declaration");
			auto declaration = job.declaration;

			assert(declaration->enclosingScope->flavor == BlockFlavor::GLOBAL);
			assert(!(declaration->flags & DECLARATION_IS_CONSTANT));

			createSymbolForDeclaration(declaration);

			debugSymbols->ensure(22);

			debugSymbols->add4Unchecked(0xF1);
			auto subsectionSizePatch = debugSymbols->add4Unchecked(0);
			u32 subsectionOffset = debugSymbols->totalSize;

			debugSymbols->add2Unchecked(static_cast<u16>(sizeof(DATASYM32) + declaration->name.length - 1));
			debugSymbols->add2Unchecked(0x110d); // S_GDATA32

			u32 *patch = debugSymbols->add4Unchecked(0);
			coffTypePatches.add({ patch, getDeclarationType(declaration) });
			
			debugSymbols->addSectionRelocations(declaration->physicalStorage);

			debugSymbols->addNullTerminatedString(declaration->name);

			*subsectionSizePatch = debugSymbols->totalSize - subsectionOffset;

			alignAllocator(debugSymbols, 4);

			auto symbol = declaration->symbol;
			auto type = getDeclarationType(declaration);

			setSymbolName(&symbol->name, declaration->name);

			symbol->storageClass = IMAGE_SYM_CLASS_EXTERNAL;
			symbol->type = 0;

			if (placeValueInBSSSection(declaration)) {
				bss->totalSize = AlignPO2(bss->totalSize, type->alignment);

				symbol->value = bss->totalSize;
				symbol->sectionNumber = bss->sectionNumber;

				bss->totalSize += type->size;
			}
			else {
				data->allocateUnaligned(AlignPO2(data->totalSize, type->alignment) - data->totalSize);

				symbol->value = data->totalSize;
				symbol->sectionNumber = data->sectionNumber;

				u32 dataSize = data->totalSize;
				u8 *allocation = static_cast<u8 *>(data->allocateUnaligned(type->size));

				writeValue(dataSize, allocation, data, declaration->initialValue);
			}

			symbol->numberOfAuxSymbols = 0;
		}
	}

	{
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

		debugSymbols->ensure(8);

		debugSymbols->add4Unchecked(0xF1);
		u32 *subsectionSizePatch = debugSymbols->add4(0);

		u32 previousSize = debugSymbols->totalSize;

		exportTypeTableToDebugTSection(debugTypes);

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
						emitUDT(debugSymbols, type);
				}

				if (!(type->flags & TYPE_USED_IN_OUTPUT))
					continue;

				auto symbol = type->symbol;

				u32 name = createRdataPointer();
				rdata->addNullTerminatedString(type->name);

				assert(type->name.length);

				setSymbolName(&symbol->name, entry.value->physicalStorage);
				symbol->storageClass = IMAGE_SYM_CLASS_STATIC;
				symbol->type = 0;

				symbol->sectionNumber = rdata->sectionNumber;
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

					rdata->allocateUnaligned(AlignPO2(rdata->totalSize, 8) - rdata->totalSize);

					symbol->value = rdata->totalSize;

					info.tag = infoTag;
					info.size = type->size;
					info.alignment = type->alignment;
					info.name = { nullptr, type->name.length };

					if (name)
						rdata->addPointerRelocation(name, rdata->totalSize + offsetof(decltype(info), name));

					rdata->add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::INTEGER: {
					Type_Info_Integer info;

					rdata->allocateUnaligned(AlignPO2(rdata->totalSize, 8) - rdata->totalSize);

					symbol->value = rdata->totalSize;

					info.tag = infoTag;
					info.size = type->size;
					info.alignment = type->alignment;
					info.name = { nullptr, type->name.length };
					info.signed_ = type->flags & TYPE_INTEGER_IS_SIGNED;

					if (name)
						rdata->addPointerRelocation(name, rdata->totalSize + offsetof(decltype(info), name));

					rdata->add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::POINTER: {
					Type_Info_Pointer info;

					rdata->allocateUnaligned(AlignPO2(rdata->totalSize, 8) - rdata->totalSize);

					symbol->value = rdata->totalSize;

					info.tag = infoTag;
					info.size = type->size;
					info.alignment = type->alignment;
					info.name = { nullptr, type->name.length };
					info.value_type = nullptr;

					if (name)
						rdata->addPointerRelocation(name, rdata->totalSize + offsetof(decltype(info), name));

					rdata->addPointerRelocation(static_cast<TypePointer *>(type)->pointerTo->physicalStorage, rdata->totalSize + offsetof(decltype(info), value_type));

					rdata->add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::FUNCTION: {
					auto function = static_cast<TypeFunction *>(type);

					rdata->allocateUnaligned(AlignPO2(rdata->totalSize, 8) - rdata->totalSize);

					u32 arguments = createRdataPointer();

					for (u64 i = 0; i < function->argumentCount; i++) {
						rdata->addPointerRelocation(function->argumentTypes[i]->physicalStorage);
					}
					
					u32 returns = createRdataPointer();

					for (u64 i = 0; i < function->returnCount; i++) {
						rdata->addPointerRelocation(function->returnTypes[i]->physicalStorage);
					}

					Type_Info_Function info;

					symbol->value = rdata->totalSize;

					info.tag = infoTag;
					info.size = type->size;
					info.alignment = type->alignment;
					info.name = { nullptr, type->name.length };
					info.arguments.data = nullptr;
					info.arguments.count = function->argumentCount;
					info.returns.data = nullptr;
					info.returns.count = function->returnCount;

					if (name)
						rdata->addPointerRelocation(name, rdata->totalSize + offsetof(decltype(info), name));

					rdata->addPointerRelocation(arguments, rdata->totalSize + offsetof(decltype(info), arguments.data));
					rdata->addPointerRelocation(returns, rdata->totalSize + offsetof(decltype(info), returns.data));

					rdata->add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::ARRAY: {
					Type_Info_Array info;

					rdata->allocateUnaligned(AlignPO2(rdata->totalSize, 8) - rdata->totalSize);

					symbol->value = rdata->totalSize;

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
						rdata->addPointerRelocation(name, rdata->totalSize + offsetof(decltype(info), name));

					rdata->addPointerRelocation(static_cast<TypeArray *>(type)->arrayOf->physicalStorage, rdata->totalSize + offsetof(decltype(info), element_type));

					rdata->add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::STRUCT: {
					auto struct_ = static_cast<TypeStruct *>(type);

					u32 names = symbols.count();

					for (auto member : struct_->members.declarations) {
						if (member->flags & DECLARATION_IMPORTED_BY_USING) continue;


						createRdataPointer();
						rdata->addNullTerminatedString(member->name);
					}

					u32 values = symbols.count();

					for (auto member : struct_->members.declarations) {
						if (member->flags & DECLARATION_IMPORTED_BY_USING) continue;

						if (!member->initialValue) continue;

						auto type = getTypeForExpr(member->initialValue);
						
						if (member->initialValue->flavor == ExprFlavor::TYPE_LITERAL && static_cast<ExprLiteral *>(member->initialValue)->typeValue->flavor == TypeFlavor::MODULE)
							continue;

						rdata->allocateUnaligned(AlignPO2(rdata->totalSize, type->alignment) - rdata->totalSize);

						createRdataPointer();

						u32 dataSize = rdata->totalSize;
						u8 *allocation = static_cast<u8 *>(rdata->allocateUnaligned(type->size));

						writeValue(dataSize, allocation, rdata, member->initialValue);
					}

					rdata->allocateUnaligned(AlignPO2(rdata->totalSize, 8) - rdata->totalSize);
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


						rdata->addPointerRelocation(names + nameCount, rdata->totalSize + offsetof(decltype(data), name.data));

						if (member->initialValue) {
							rdata->addPointerRelocation(getTypeForExpr(member->initialValue)->physicalStorage, rdata->totalSize + offsetof(decltype(data), member_type));
						}
						else {
							rdata->addPointerRelocation(getDeclarationType(member)->physicalStorage, rdata->totalSize + offsetof(decltype(data), member_type));
						}

						if (member->initialValue) { // @Incomplete: Export info for namespaces
							if (member->initialValue->flavor != ExprFlavor::TYPE_LITERAL || static_cast<ExprLiteral *>(member->initialValue)->typeValue->flavor != TypeFlavor::MODULE) {
								rdata->addPointerRelocation(values + valueCount, rdata->totalSize + offsetof(decltype(data), initial_value));
								++valueCount;
							}
						}

						rdata->add(&data, sizeof(data));

						++nameCount;
					}

					Type_Info_Struct info;

					symbol->value = rdata->totalSize;

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
						rdata->addPointerRelocation(name, rdata->totalSize + offsetof(decltype(info), name));

					rdata->addPointerRelocation(members, rdata->totalSize + offsetof(decltype(info), members.data));

					rdata->add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::ENUM: {
					auto enum_ = static_cast<TypeEnum *>(type);

					u32 names = symbols.count();

					for (auto member : enum_->members.declarations) {
						if (!(member->flags & DECLARATION_IS_ENUM_VALUE))
							continue;
						createRdataPointer();
						rdata->addNullTerminatedString(member->name);
					}

					rdata->allocateUnaligned(AlignPO2(rdata->totalSize, 8) - rdata->totalSize);
					u32 values = createRdataPointer();

					for (u32 i = 0; i < enum_->members.declarations.count; i++) {
						auto member = enum_->members.declarations[i];
						if (!(member->flags & DECLARATION_IS_ENUM_VALUE))
							continue;

						Type_Info_Enum::Value data;

						data.name = { nullptr, member->name.length };
						data.value = static_cast<ExprLiteral *>(member->initialValue)->unsignedValue;

						rdata->addPointerRelocation(names + i, rdata->totalSize + offsetof(decltype(data), name.data));

						rdata->add(&data, sizeof(data));
					}

					Type_Info_Enum info;

					symbol->value = rdata->totalSize;

					info.tag = infoTag;
					info.size = type->size;
					info.alignment = type->alignment;
					info.name = { nullptr, type->name.length };
					info.base_type = nullptr;
					info.is_flags = enum_->flags & TYPE_ENUM_IS_FLAGS ? true : false;
				
					info.values.data = nullptr;
					info.values.count = enum_->members.declarations.count - ENUM_SPECIAL_MEMBER_COUNT;

					if (name)
						rdata->addPointerRelocation(name, rdata->totalSize + offsetof(decltype(info), name));

					rdata->addPointerRelocation(enum_->integerType->physicalStorage , rdata->totalSize + offsetof(decltype(info), base_type));
					rdata->addPointerRelocation(values, rdata->totalSize + offsetof(decltype(info), values.data));

					rdata->add(&info, sizeof(info));

					break;
				}
				default:
					assert(false);
				}
			}
		}

		*subsectionSizePatch = debugSymbols->totalSize - previousSize;
		alignAllocator(debugSymbols, 4);
	}

	{
		PROFILE_ZONE("Write output");
		debugSymbols->add4(0xF3);

		u32 *sizePointer = debugSymbols->add4(0);

		u32 totalSize = 0;

		for (auto file : compilerFiles) {
			file->offsetInStringTable = totalSize;

			char *filepath = fullPath(toCString(file->path) /* @Leak */);

			u32 len = static_cast<u32>(strlen(filepath));
			totalSize += len + 1;

			debugSymbols->addNullTerminatedString({ filepath, len });
		}

		*sizePointer = totalSize;

		alignAllocator(debugSymbols, 4);

		debugSymbols->add4(0xF4);
		debugSymbols->add4(8 * compilerFiles.count);

		for (auto &file : compilerFiles) {
			debugSymbols->add4(file->offsetInStringTable);
			debugSymbols->add4(0);
		}


		u32 stringTableSize = sizeof(u32) + stringTable.totalSize;

		Array<SectionHeader> sectionHeaders;


		FileHeader header = {};
		header.machine = IMAGE_FILE_MACHINE_AMD64;
		header.numberOfSections = sections.count;
		header.timestamp = (u32) time(0);
		header.pointerToSymbolTable = sizeof(FileHeader) + sizeof(SectionHeader) * sections.count;
		header.numberOfSymbols = symbols.count();

		u32 prefixSize = header.pointerToSymbolTable + sizeof(Symbol) * symbols.count() + stringTableSize;

		u32 sectionPointer = AlignPO2(prefixSize, 4);

		for (auto section : sections) {
			auto &header = sectionHeaders.add();
			setSectionName(header.name, sizeof(header.name), section->name);

			if (section->flags & SECTION_READ)
				header.characteristics |= IMAGE_SCN_MEM_READ;
			if (section->flags & SECTION_WRITE)
				header.characteristics |= IMAGE_SCN_MEM_WRITE;
			if (section->flags & SECTION_EXECUTE)
				header.characteristics |= IMAGE_SCN_MEM_EXECUTE;
			if (section->flags & SECTION_DISCARD)
				header.characteristics |= IMAGE_SCN_MEM_DISCARDABLE;

			switch (section->alignment) {
			case 1:
				header.characteristics |= IMAGE_SCN_ALIGN_1BYTES;
				break;
			case 2:
				header.characteristics |= IMAGE_SCN_ALIGN_2BYTES;
				break;
			case 4:
				header.characteristics |= IMAGE_SCN_ALIGN_4BYTES;
				break;
			case 8:
				header.characteristics |= IMAGE_SCN_ALIGN_8BYTES;
				break;
			case 16:
				header.characteristics |= IMAGE_SCN_ALIGN_16BYTES;
				break;
			case 32:
				header.characteristics |= IMAGE_SCN_ALIGN_32BYTES;
				break;
			case 64:
				header.characteristics |= IMAGE_SCN_ALIGN_64BYTES;
				break;
			case 128:
				header.characteristics |= IMAGE_SCN_ALIGN_128BYTES;
				break;
			case 256:
				header.characteristics |= IMAGE_SCN_ALIGN_256BYTES;
				break;
			case 512:
				header.characteristics |= IMAGE_SCN_ALIGN_512BYTES;
				break;
			case 1024:
				header.characteristics |= IMAGE_SCN_ALIGN_1024BYTES;
				break;
			case 2048:
				header.characteristics |= IMAGE_SCN_ALIGN_2048BYTES;
				break;
			case 4096:
				header.characteristics |= IMAGE_SCN_ALIGN_4096BYTES;
				break;
			case 8192:
				header.characteristics |= IMAGE_SCN_ALIGN_8192BYTES;
				break;
			default:
				assert(false);
			}

			if (section->flags & SECTION_INITIALIZED) {
				alignAllocator(section, 4);
				header.sizeOfRawData = section->totalSize;

				if (section->flags & SECTION_EXECUTE)
					header.characteristics |= IMAGE_SCN_CNT_CODE;
				else
					header.characteristics |= IMAGE_SCN_CNT_INITIALIZED_DATA;
			}
			else {
				header.characteristics |= IMAGE_SCN_CNT_UNINITIALIZED_DATA;

				header.sizeOfRawData = AlignPO2(section->totalSize, 4);
			}

			if (section->flags & SECTION_INITIALIZED) {
				header.pointerToRawData = sectionPointer;

				sectionPointer += header.sizeOfRawData;

				if (section->relocations.count()) {
					header.pointerToRelocations = sectionPointer;

					u32 relocationCount = section->relocations.count();

					if (relocationCount > UINT16_MAX) {
						header.characteristics |= IMAGE_SCN_LNK_NRELOC_OVFL;
						header.numberOfRelocations = UINT16_MAX;
					}
					else {
						header.numberOfRelocations = static_cast<u16>(relocationCount);
					}

					alignAllocator(&section->relocations.allocator, 4);

					if (relocationCount > UINT16_MAX) {
						sectionPointer += 10;

						section->relocations.allocator.add2(0); // @Hack We padded to what we though was a multiple of 4 bytes, but since an extra relocation is added at the beginning, 
						                              // we need to add 2 more bytes to ensure alignment
					}

					sectionPointer += section->relocations.allocator.totalSize;
				}
			}
		}

		if (hadError)
			return;
		
		FILE *out = fopen(objectFileName, "wb");
		if (!out) {
			reportError("Error: Could not open %s intermediate for writing", objectFileName);
			return;
		}

		#define doWrite(ptr, size) fwrite((ptr), (size), 1, out)
		
		const auto writeAllocator = [&](FILE *out, BucketedArenaAllocator allocator) {
			for (auto bucket = allocator.first; bucket; bucket = bucket->next) {
				u32 count = (bucket->size - bucket->remaining);

				doWrite(bucket->memory - count, count);
			}
		};

			
		doWrite(&header, sizeof(header));
		doWrite(sectionHeaders.storage, sectionHeaders.count * sizeof(SectionHeader));
			

		assert(ftell(out) == header.pointerToSymbolTable);
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
				reportInfo("%s size: %u", section->name, section->totalSize);
			}

			if (section->flags & SECTION_INITIALIZED) {
				writeAllocator(out, *section);
			}

			if (section->relocations.count()) {
				u32 relocationCount = section->relocations.count();

				if (printDiagnostics) {
					reportInfo("%s relocations: %u", section->name, relocationCount);
				}

				if (relocationCount > UINT16_MAX) {
					Relocation count;
					count.virtualAddress = relocationCount + 1;
					count.symbolTableIndex = 0;
					count.type = IMAGE_REL_AMD64_ABSOLUTE;

					doWrite(&count, sizeof(count));
				}
				writeAllocator(out, section->relocations.allocator);
			}
		}

		{
			PROFILE_ZONE("fclose");
			fclose(out);
		}
	}
}