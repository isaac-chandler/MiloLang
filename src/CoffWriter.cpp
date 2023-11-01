#include "Ast.h"
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

#include <elf.h>

#define IMAGE_REL_AMD64_ABSOLUTE 0
#define IMAGE_REL_AMD64_ADDR64   1
#define IMAGE_REL_AMD64_ADDR32NB 3
#define IMAGE_REL_AMD64_REL32    4
#define IMAGE_REL_AMD64_SECREL   11
#define IMAGE_REL_AMD64_SECTION  10

#endif


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
#pragma pack(pop)

#define SECTION_READ        0x01
#define SECTION_WRITE       0x02
#define SECTION_EXECUTE     0x04
#define SECTION_DISCARD     0x08
#define SECTION_INITIALIZED 0x10
#define SECTION_RELOCATIONS 0x20

#if BUILD_WINDOWS
void setSymbolAddress(void *symbol, u32 offset) {
	((Symbol *) symbol)->value = offset;
}

void setSymbolSection(void *symbol, u32 section) {
	((Symbol *) symbol)->sectionNumber = section;
}

void setSymbolSize(void *symbol, u32 size) {
	// Doesn't exist in COFF
}
#else
void setSymbolAddress(void *symbol, u32 offset) {
	((Symbol *) symbol)->st_value = offset;
}

void setSymbolSection(void *symbol, u32 section) {
	((Symbol *) symbol)->st_shndx = section;
}

void setSymbolSize(void *symbol, u32 size) {
	((Symbol *) symbol)->st_size = size;
}
#endif


#if BUILD_WINDOWS

void setSymbolName(Symbol *symbol, String name) {
	memset(symbol->name.name, 0, sizeof(symbol->name.name));
	if (name.length > sizeof(symbol->name.name)) {
		symbol->name.namePointer = stringTable.totalSize + 4;
		stringTable.addNullTerminatedString(name);
	}
	else {
		memcpy(symbol->name.name, name.characters, name.length);
	}
}

void setSymbolName(Symbol *symbol, String name, u32 id) {
	memset(symbol->name.name, 0, sizeof(symbol->name.name));

	char idBuffer[8];

	char *idName = idBuffer;

	while (id) {
		*idName = "0123456789ABCDEF"[id & 0xF];
		idName++;
		id >>= 4;
	}

	u32 idLen = idName - idBuffer;

	if (name.length + 1 + idLen > sizeof(symbol->name.name)) {
		symbol->name.namePointer = stringTable.totalSize + 4;
		stringTable.ensure(name.length + 2 + idLen);
		stringTable.addUnchecked(name.characters, name.length);
		stringTable.add1Unchecked('@');
		stringTable.addUnchecked(idBuffer, idLen);
		stringTable.add1Unchecked(0);
	}
	else {
		memcpy(symbol->name.name, name.characters, name.length);
		symbol->name.name[name.length] = '@';
		memcpy(symbol->name.name + name.length + 1, idBuffer, idLen);
	}
}

u32 createExternalSymbol(String name, void **symbolReturn) {
	u32 symbolIndex = symbols.count();

	Symbol symbol;
	setSymbolName(&symbol, name);
	symbol.value = 0;
	symbol.sectionNumber = 0;
	symbol.type = IMAGE_SYM_DTYPE_FUNCTION;
	symbol.storageClass = IMAGE_SYM_CLASS_EXTERNAL;
	symbol.numberOfAuxSymbols = 0;

	auto address = symbols.add(symbol);

	if (symbolReturn) *symbolReturn = address;

	return symbolIndex;
}

void createEntryPointSymbol() {
	Symbol *entryPointSymbol = (Symbol *) programStart->symbol;

	Symbol symbol;
	setSymbolName(&symbol, programStart->valueOfDeclaration->name);
	symbol.value = entryPointSymbol->value;
	symbol.sectionNumber = entryPointSymbol->sectionNumber;
	symbol.type = IMAGE_SYM_DTYPE_FUNCTION;
	symbol.storageClass = IMAGE_SYM_CLASS_EXTERNAL;
	symbol.numberOfAuxSymbols = 0;

	symbols.add(symbol);
}

u32 createSymbol(Section *section, String name = "", bool function = false, void **symbolReturn = nullptr) {
	u32 symbolIndex = symbols.count();

	Symbol symbol;
	setSymbolName(&symbol, name, symbolIndex);
	symbol.value = section->totalSize;
	symbol.sectionNumber = section->sectionNumber;
	symbol.type = function ? IMAGE_SYM_DTYPE_FUNCTION : IMAGE_SYM_DTYPE_NULL;
	symbol.storageClass = IMAGE_SYM_CLASS_STATIC;
	symbol.numberOfAuxSymbols = 0;

	auto address = symbols.add(symbol);

	if (symbolReturn) *symbolReturn = address;

	return symbolIndex;
}

#else
u32 createExternalSymbol(String name, void **symbolReturn) {
	u32 symbolIndex = symbols.count();
	
	Symbol symbol;
	symbol.st_name = stringTable.totalSize;
	symbol.st_value = 0;
	symbol.st_size = 0;
	symbol.st_info = ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE);
	symbol.st_other = STV_DEFAULT;
	symbol.st_shndx = SHN_UNDEF;

	stringTable.addNullTerminatedString(name);
	
	auto address = symbols.add(symbol);

	if (symbolReturn) *symbolReturn = address;

	return symbolIndex;
}

void createEntryPointSymbol() {
	Symbol *entryPointSymbol = (Symbol *)programStart->symbol;
	
	Symbol symbol;
	symbol.st_name = stringTable.totalSize;
	symbol.st_value = entryPointSymbol->st_value;
	symbol.st_size = entryPointSymbol->st_size;
	symbol.st_info = ELF64_ST_INFO(STB_GLOBAL, STT_FUNC);
	symbol.st_other = STV_DEFAULT;
	symbol.st_shndx = entryPointSymbol->st_shndx;

	stringTable.addNullTerminatedString(programStart->valueOfDeclaration->name);
	
	symbols.add(symbol);
}

u32 createSymbol(Section *section, String name = "", bool function = false, void **symbolReturn = nullptr) {
	u32 symbolIndex = symbols.count();
	
	Symbol symbol;
	symbol.st_name = stringTable.totalSize;
	symbol.st_value = section->totalSize;
	symbol.st_size = 0;
	symbol.st_info = ELF64_ST_INFO(STB_LOCAL, function ? STT_FUNC : STT_OBJECT);
	symbol.st_other = ELF64_ST_VISIBILITY(STV_DEFAULT);
	symbol.st_shndx = section->sectionNumber;

	stringTable.addString(name);
	stringTable.ensure(18);
	stringTable.add1Unchecked('@');
	
	u32 id = symbolIndex;

	while (id) {
		stringTable.add1Unchecked("0123456789ABCDEF"[id & 0xF]);
		id >>= 4;
	}

	stringTable.add1Unchecked(0);

	auto address = symbols.add(symbol);

	if (symbolReturn) *symbolReturn = address;

	return symbolIndex;
}
#endif

u32 createSymbolForExternalFunction(ExprFunction *function) {
	assert(function->flags & EXPR_FUNCTION_IS_EXTERNAL);
	if (!(function->flags & EXPR_HAS_STORAGE)) {
		function->flags |= EXPR_HAS_STORAGE;
 
		function->physicalStorage = createExternalSymbol(function->valueOfDeclaration->name, &function->symbol);
	}

	return function->physicalStorage;
}

u32 createSymbolForFunction(ExprFunction *function) {
	assert(!(function->flags & EXPR_FUNCTION_IS_EXTERNAL));

	if (!(function->flags & EXPR_HAS_STORAGE)) {
		function->flags |= EXPR_HAS_STORAGE;
		function->physicalStorage = createSymbol(code, function->valueOfDeclaration? function->valueOfDeclaration->name : "", true, &function->symbol);
		setSymbolSection(function->symbol, code->sectionNumber);
	}

	return function->physicalStorage;
}

#if BUILD_WINDOWS
void Section::addRel32Relocation(u32 symbolTableIndex, s64 addend) {
	relocations.add(Relocation{ totalSize, symbolTableIndex, IMAGE_REL_AMD64_REL32 });
	add4Unchecked(addend);
}

void Section::addFunctionRel32Relocation(ExprFunction *function) {
	if (function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
		addRel32Relocation(createSymbolForExternalFunction(function));
		
	}
	else {
		addRel32Relocation(createSymbolForFunction(function));
	}
}

void Section::addPointerRelocation(u32 symbolTableIndex) {
	relocations.add(Relocation{ totalSize, symbolTableIndex, IMAGE_REL_AMD64_ADDR64 });
	add8Unchecked(0);
}

void Section::addAddr32NBRelocation(u32 symbolTableIndex, s64 addend) {
	relocations.add(Relocation{ totalSize, symbolTableIndex, IMAGE_REL_AMD64_ADDR32NB });
	add4Unchecked(addend);
}

void Section::addSectionRelocations(u32 symbolTableInex, s64 addend) {
	relocations.add(Relocation{ totalSize, symbolTableInex, IMAGE_REL_AMD64_SECREL });
	add4Unchecked(addend);
	relocations.add(Relocation{ totalSize, symbolTableInex, IMAGE_REL_AMD64_SECTION });
	add2Unchecked(0);
}

void Section::addPointerRelocation(u32 symbolTableIndex, u32 offset, s64 addend) {
	// @Incomplete Need to walk the bucket array to find the location to poke the addend
	assert(!addend);
	relocations.add(Relocation{ offset, symbolTableIndex, IMAGE_REL_AMD64_ADDR64 });
}
#else

struct ExternalFunctionRelocation {
	ExprFunction *function;
	u32 offset;
	Section *section;
};

Array<ExternalFunctionRelocation> externalFunctionRelocations;
Array<ExternalFunctionRelocation> externalFunctionPointerRelocations;

void Section::addRel32Relocation(u32 symbolTableIndex, s64 addend) {
	relocations.add(Relocation{ totalSize, ELF64_R_INFO(symbolTableIndex, R_X86_64_PC32), addend - 4 });
	add4Unchecked(0);
}

void Section::addFunctionRel32Relocation(ExprFunction *function) {
	if (function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
		externalFunctionRelocations.add({ function, totalSize, this });
		add4Unchecked(0);
	}
	else {
		addRel32Relocation(createSymbolForFunction(function));
	}
}

void Section::addPointerRelocation(u32 symbolTableIndex) {
	relocations.add(Relocation{ totalSize, ELF64_R_INFO(symbolTableIndex, R_X86_64_64), 0 });
	add8Unchecked(0);
}

void Section::addPointerRelocation(u32 symbolTableIndex, u32 offset, s64 addend) {
	relocations.add(Relocation{ offset, ELF64_R_INFO(symbolTableIndex, R_X86_64_64), addend });
}
#endif

Array<Section *> sections;

Section *makeSection(String name, u64 alignment, u64 flags) {	
	Section *result = sections.add(new Section);
	result->name = name;
	result->sectionNumber = sections.count;
	result->alignment = alignment;
	result->flags = flags;

	return result;
}

Section *makeSection(BucketedArenaAllocator allocator, String name, u64 alignment, u64 flags) {	
	Section *result = sections.add(new Section(allocator));
	result->name = name;
	result->sectionNumber = sections.count;
	result->alignment = alignment;
	result->flags = flags;

	return result;
}

void makeRelocationSection(Section *section) {	
	Section *result = sections.add(new Section(section->relocations.allocator));
	result->name = msprintf(".rela%.*s", STRING_PRINTF(section->name));
	result->sectionNumber = sections.count;
	result->alignment = 16;
	result->relocationsApplyTo = section->sectionNumber;
	result->flags = SECTION_RELOCATIONS | SECTION_INITIALIZED | SECTION_DISCARD;
}


s64 emptyStringSymbolIndex = -1;

void setSectionName(char *header, u64 size, const char *name) {
	u64 len = strlen(name);
	assert(len <= size);

	memcpy(header, name, len);
	memset(header + len, 0, size - len);
}

u32 getParameterSpaceForCallOffset(ExprFunction *function) {
	(void)function;
	return 0;
}

u32 getStacktraceNodeOffset(ExprFunction *function) {
	return getParameterSpaceForCallOffset(function) + function->state.stackSpaceForCallingConvention;
}

u32 getStackSpaceOffset(ExprFunction *function) {
	return getStacktraceNodeOffset(function) + (buildOptions.enable_stack_trace ? 16 : 0);
}


u32 getRegisterOffset(ExprFunction *function) {
	return getStackSpaceOffset(function) + AlignPO2(function->state.stackSpace, 8);
}

u32 getSpaceToAllocate(ExprFunction *function) {
	u32 registerCount = function->state.nextRegister;

	return AlignPO2(getRegisterOffset(function) + registerCount * 8 - 8, 16) + 8;
}

u32 getRegisterOffset(ExprFunction *function, u32 regNo) {
	return getRegisterOffset(function) + regNo * 8;
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
#define REX_X 0x42
#define REX_R 0x44
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

void writeIntegerPrefixR(u64 size, u8 *rReg) {
	u8 rex = 0;

	if (size == 1 && (*rReg > 4))
		rex |= REX;
	else if (size == 8)
		rex |= REX_W;
	else if (size == 2)
		code->add1Unchecked(OPERAND_SIZE_OVERRIDE);
	
	if (*rReg >= 8) {
		rex |= REX_R;
		*rReg -= 8;
	}

	if (rex)
		code->add1Unchecked(rex);
}

void writeIntegerPrefixB(u64 size, u8 *bReg) {
	u8 rex = 0;

	if (size == 1 && (*bReg > 4))
		rex |= REX;
	else if (size == 8)
		rex |= REX_W;
	else if (size == 2)
		code->add1Unchecked(OPERAND_SIZE_OVERRIDE);

	if (*bReg >= 8) {
		rex |= REX_B;
		*bReg -= 8;
	}

	if (rex)
		code->add1Unchecked(rex);
}

void writeIntegerPrefixRB(u64 size, u8 *rReg, u8 *bReg) {
	u8 rex = 0;

	if (size == 1 && (*rReg > 4 || *bReg > 4))
		rex |= REX;
	else if (size == 8)
		rex |= REX_W;
	else if (size == 2)
		code->add1Unchecked(OPERAND_SIZE_OVERRIDE);

	if (*rReg >= 8)
		rex |= REX_R;
	if (*bReg >= 8)
		rex |= REX_B;

	// Separate these because rReg and bReg could be the same pointer

	if (*rReg >= 8)
		*rReg -= 8;
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

void writeFloatPrefixR(u64 size, u8 *rReg) {
	writeFloatPrefix(size);

	if (*rReg >= 8) {
		*rReg -= 8;
		code->add1Unchecked(REX_R);
	}
}

void writeFloatLegacyPrefix(u64 size) {
	if (size == 8) {
		code->add1Unchecked(OPERAND_SIZE_OVERRIDE);
	}
}

void loadIntoIntRegister(ExprFunction *function, u64 size, u8 loadInto, u32 regNo) {
	writeIntegerPrefixR(size, &loadInto);
	sizedIntInstruction(size, MOV_REG_MEM_BASE);
	writeRSPRegisterByte(function, loadInto, regNo);
}

void storeFromIntRegister(ExprFunction *function, u64 size, u32 regNo, u8 storeFrom) {
	writeIntegerPrefixR(size, &storeFrom);
	sizedIntInstruction(size, MOV_MEM_REG_BASE);
	writeRSPRegisterByte(function, storeFrom, regNo);
}

void loadIntoFloatRegister(ExprFunction *function, u64 size, u8 loadInto, u32 regNo) {
	writeFloatPrefixR(size, &loadInto);

	code->add1Unchecked(OPCODE_EXT);
	code->add1Unchecked(EXT_MOVSf_REG_MEM);

	writeRSPRegisterByte(function, loadInto, regNo);
}

void storeFromFloatRegister(ExprFunction *function, u64 size, u32 regNo, u8 storeFrom) {
	writeFloatPrefixR(size, &storeFrom);

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
		writeIntegerPrefixRB(4, &regNo, &regNo);
		sizedIntInstruction(4, XOR_REG_MEM_BASE);
		writeModRM(MODRM_MOD_DIRECT, regNo, regNo);
	} else if (immediate <= 0xFFFF'FFFFULL) {
		writeIntegerPrefixR(4, &regNo);
		code->add1Unchecked(MOV_REG_IMM_WORD_BASE | regNo);
		code->add4Unchecked(static_cast<u32>(immediate));
	}
	else if (static_cast<s64>(immediate) == static_cast<s64>(static_cast<s32>(immediate))) {
		writeIntegerPrefixR(8, &regNo);
		sizedIntInstruction(8, MOV_MEM_IMM_BASE_MODRM_0);
		writeModRM(MODRM_MOD_DIRECT, 0, regNo);
		code->add4Unchecked(static_cast<u32>(immediate));
	}
	else {
		writeIntegerPrefixR(8, &regNo);
		code->add1Unchecked(MOV_REG_IMM_WORD_BASE | regNo);
		code->add8Unchecked(immediate);
	}
}

void writeSet(ExprFunction *function, u64 size, u32 dest, u32 src) {
	assert(isStandardSize(size));
	loadIntoIntRegister(function, size, RAX, src);
	storeFromIntRegister(function, size, dest, RAX);
}

void loadWeirdSizeIntoRegister(u32 size, u8 destRegister, u8 addressRegister) {
	u8 destRegisterTemp, addressRegisterTemp;
	switch (size) {
		case 1:
		case 2:
		case 4:
		case 8: {
			destRegisterTemp = destRegister;
			addressRegisterTemp = addressRegister;
			writeIntegerPrefixRB(size, &destRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(size, MOV_REG_MEM_BASE);
			writeModRM(MODRM_MOD_INDIRECT, destRegisterTemp, addressRegisterTemp);
			break;
		}
		case 3: {
			destRegisterTemp = destRegister;
			addressRegisterTemp = addressRegister;
			writeIntegerPrefixRB(1, &destRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(1, MOV_REG_MEM_BASE);
			writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, destRegisterTemp, addressRegisterTemp);
			code->add1Unchecked(2);

			destRegisterTemp = destRegister;
			writeIntegerPrefixR(4, &destRegisterTemp);
			sizedIntInstruction(4, SHIFT_IMM8_BASE_MODRM_X);
			writeModRM(MODRM_MOD_DIRECT, destRegisterTemp, SHIFT_MODRM_SHL);
			code->add1Unchecked(16);
			
			addressRegisterTemp = addressRegister;
			writeIntegerPrefixRB(2, &addressRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(2, MOV_REG_MEM_BASE);
			writeModRM(MODRM_MOD_INDIRECT, addressRegisterTemp, addressRegisterTemp);

			destRegisterTemp = destRegister;
			addressRegisterTemp = addressRegister;
			writeIntegerPrefixRB(4, &destRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(4, OR_REG_MEM_BASE);
			writeModRM(MODRM_MOD_DIRECT, destRegisterTemp, addressRegisterTemp);
			code->add1Unchecked(2);
			break;
		}
		case 5: {
			destRegisterTemp = destRegister;
			addressRegisterTemp = addressRegister;
			writeIntegerPrefixRB(1, &destRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(1, MOV_REG_MEM_BASE);
			writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, destRegisterTemp, addressRegisterTemp);
			code->add1Unchecked(4);

			destRegisterTemp = destRegister;
			writeIntegerPrefixR(8, &destRegisterTemp);
			sizedIntInstruction(8, SHIFT_IMM8_BASE_MODRM_X);
			writeModRM(MODRM_MOD_DIRECT, destRegisterTemp, SHIFT_MODRM_SHL);
			code->add1Unchecked(32);
			
			addressRegisterTemp = addressRegister;
			writeIntegerPrefixRB(4, &addressRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(4, MOV_REG_MEM_BASE);
			writeModRM(MODRM_MOD_INDIRECT, addressRegisterTemp, addressRegisterTemp);

			destRegisterTemp = destRegister;
			addressRegisterTemp = addressRegister;
			writeIntegerPrefixRB(8, &destRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(8, OR_REG_MEM_BASE);
			writeModRM(MODRM_MOD_DIRECT, destRegisterTemp, addressRegisterTemp);
			code->add1Unchecked(2);
			break;
		}
		case 6: {
			destRegisterTemp = destRegister;
			addressRegisterTemp = addressRegister;
			writeIntegerPrefixRB(2, &destRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(2, MOV_REG_MEM_BASE);
			writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, destRegisterTemp, addressRegisterTemp);
			code->add1Unchecked(4);

			destRegisterTemp = destRegister;
			writeIntegerPrefixR(8, &destRegisterTemp);
			sizedIntInstruction(8, SHIFT_IMM8_BASE_MODRM_X);
			writeModRM(MODRM_MOD_DIRECT, destRegisterTemp, SHIFT_MODRM_SHL);
			code->add1Unchecked(32);
			
			addressRegisterTemp = addressRegister;
			writeIntegerPrefixRB(4, &addressRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(4, MOV_REG_MEM_BASE);
			writeModRM(MODRM_MOD_INDIRECT, addressRegisterTemp, addressRegisterTemp);

			destRegisterTemp = destRegister;
			addressRegisterTemp = addressRegister;
			writeIntegerPrefixRB(8, &destRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(8, OR_REG_MEM_BASE);
			writeModRM(MODRM_MOD_DIRECT, destRegisterTemp, addressRegisterTemp);
			code->add1Unchecked(2);
			break;
		}
		case 7: {
			destRegisterTemp = destRegister;
			addressRegisterTemp = addressRegister;
			writeIntegerPrefixRB(4, &destRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(4, MOV_REG_MEM_BASE);
			writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, destRegisterTemp, addressRegisterTemp);
			code->add1Unchecked(3);

			destRegisterTemp = destRegister;
			writeIntegerPrefixR(8, &destRegisterTemp);
			sizedIntInstruction(8, SHIFT_IMM8_BASE_MODRM_X);
			writeModRM(MODRM_MOD_DIRECT, destRegisterTemp, SHIFT_MODRM_SHL);
			code->add1Unchecked(32);
			
			addressRegisterTemp = addressRegister;
			writeIntegerPrefixRB(4, &addressRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(4, MOV_REG_MEM_BASE);
			writeModRM(MODRM_MOD_INDIRECT, addressRegisterTemp, addressRegisterTemp);

			destRegisterTemp = destRegister;
			addressRegisterTemp = addressRegister;
			writeIntegerPrefixRB(8, &destRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(8, OR_REG_MEM_BASE);
			writeModRM(MODRM_MOD_DIRECT, destRegisterTemp, addressRegisterTemp);
			code->add1Unchecked(2);
			break;
		}
		default:
			assert(false);
			break;
	}
}


void storeWeirdSizeFromRegister(u32 size, u8 addressRegister, u8 srcRegister) {
	u8 addressRegisterTemp, srcRegisterTemp;
	switch (size) {
		case 1:
		case 2:
		case 4:
		case 8: {
			addressRegisterTemp = addressRegister;
			srcRegisterTemp = srcRegister;
			writeIntegerPrefixRB(size, &srcRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(size, MOV_MEM_REG_BASE);
			writeModRM(MODRM_MOD_INDIRECT, srcRegisterTemp, addressRegisterTemp);
			break;
		}
		case 3: {
			addressRegisterTemp = addressRegister;
			srcRegisterTemp = srcRegister;
			writeIntegerPrefixRB(2, &srcRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(2, MOV_MEM_REG_BASE);
			writeModRM(MODRM_MOD_INDIRECT, srcRegisterTemp, addressRegisterTemp);

			srcRegisterTemp = srcRegister;
			writeIntegerPrefixR(4, &srcRegisterTemp);
			sizedIntInstruction(4, SHIFT_IMM8_BASE_MODRM_X);
			writeModRM(MODRM_MOD_DIRECT, srcRegisterTemp, SHIFT_MODRM_SHR);
			code->add1Unchecked(16);

			addressRegisterTemp = addressRegister;
			srcRegisterTemp = srcRegister;
			writeIntegerPrefixRB(1, &srcRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(1, MOV_MEM_REG_BASE);
			writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, srcRegisterTemp, addressRegisterTemp);
			code->add1Unchecked(2);
			break;
		}
		case 5: {
			addressRegisterTemp = addressRegister;
			srcRegisterTemp = srcRegister;
			writeIntegerPrefixRB(4, &srcRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(4, MOV_MEM_REG_BASE);
			writeModRM(MODRM_MOD_INDIRECT, srcRegisterTemp, addressRegisterTemp);

			srcRegisterTemp = srcRegister;
			writeIntegerPrefixR(8, &srcRegisterTemp);
			sizedIntInstruction(8, SHIFT_IMM8_BASE_MODRM_X);
			writeModRM(MODRM_MOD_DIRECT, srcRegisterTemp, SHIFT_MODRM_SHR);
			code->add1Unchecked(32);

			addressRegisterTemp = addressRegister;
			srcRegisterTemp = srcRegister;
			writeIntegerPrefixRB(1, &srcRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(1, MOV_MEM_REG_BASE);
			writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, srcRegisterTemp, addressRegisterTemp);
			code->add1Unchecked(4);
			break;
		}
		case 6: {
			addressRegisterTemp = addressRegister;
			srcRegisterTemp = srcRegister;
			writeIntegerPrefixRB(4, &srcRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(4, MOV_MEM_REG_BASE);
			writeModRM(MODRM_MOD_INDIRECT, srcRegisterTemp, addressRegisterTemp);

			srcRegisterTemp = srcRegister;
			writeIntegerPrefixR(8, &srcRegisterTemp);
			sizedIntInstruction(8, SHIFT_IMM8_BASE_MODRM_X);
			writeModRM(MODRM_MOD_DIRECT, srcRegisterTemp, SHIFT_MODRM_SHR);
			code->add1Unchecked(32);

			addressRegisterTemp = addressRegister;
			srcRegisterTemp = srcRegister;
			writeIntegerPrefixRB(2, &srcRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(2, MOV_MEM_REG_BASE);
			writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, srcRegisterTemp, addressRegisterTemp);
			code->add1Unchecked(4);
			break;
		}
		case 7: {
			addressRegisterTemp = addressRegister;
			srcRegisterTemp = srcRegister;
			writeIntegerPrefixRB(4, &srcRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(4, MOV_MEM_REG_BASE);
			writeModRM(MODRM_MOD_INDIRECT, srcRegisterTemp, addressRegisterTemp);

			srcRegisterTemp = srcRegister;
			writeIntegerPrefixR(8, &srcRegisterTemp);
			sizedIntInstruction(8, SHIFT_IMM8_BASE_MODRM_X);
			writeModRM(MODRM_MOD_DIRECT, srcRegisterTemp, SHIFT_MODRM_SHR);
			code->add1Unchecked(24);

			addressRegisterTemp = addressRegister;
			srcRegisterTemp = srcRegister;
			writeIntegerPrefixRB(4, &srcRegisterTemp, &addressRegisterTemp);
			sizedIntInstruction(4, MOV_MEM_REG_BASE);
			writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, srcRegisterTemp, addressRegisterTemp);
			code->add1Unchecked(3);
			break;
		}
		default:
			assert(false);
			break;
	}
}

u32 createSymbolForString(ExprStringLiteral *string) {
	if (string->string.length == 0) {
		if (emptyStringSymbolIndex == -1) {
			emptyStringSymbolIndex = createSymbol(rdata, "@emptyString");
			rdata->add1(0);
		}

		return static_cast<u32>(emptyStringSymbolIndex);
	}

	if (!(string->flags & EXPR_HAS_STORAGE)) {
		string->flags |= EXPR_HAS_STORAGE;

		string->physicalStorage = createSymbol(rdata);
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
	// Because Linux sucks we can't apply relocations to the .rodata section :NotUsingRdata
	auto section = BUILD_LINUX ? data : rdata;
	type->physicalStorage = createSymbol(section, type->name, false, &type->symbol);
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

		declaration->physicalStorage = createSymbol(data, declaration->name, false, &declaration->symbol);
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

		auto function = static_cast<ExprFunction *>(value);
#if BUILD_LINUX
		if (function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
			externalFunctionPointerRelocations.add({ function, dataSize, section });
		}
		else {
			section->addPointerRelocation(createSymbolForFunction(function), dataSize);
		}
#else
		section->addPointerRelocation(createSymbolForFunction(function), dataSize);
#endif
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

			
			section->align(arrayType->arrayOf->alignment);

			u32 symbolId = createSymbol(section);

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

bool placeValueInBSSSection(Declaration *declaration) {
	return (declaration->flags & DECLARATION_IS_UNINITIALIZED) ||
		((declaration->initialValue->flavor == ExprFlavor::INT_LITERAL || declaration->initialValue->flavor == ExprFlavor::FLOAT_LITERAL)
			&& static_cast<ExprLiteral *>(declaration->initialValue)->unsignedValue == 0);
}

void runCoffWriter() {
	PROFILE_FUNC();

#if BUILD_WINDOWS
	code = makeSection(".text", 16, SECTION_EXECUTE | SECTION_READ | SECTION_INITIALIZED);
	data = makeSection(".data", 16, SECTION_READ | SECTION_WRITE | SECTION_INITIALIZED);
	rdata = makeSection(".rdata", 16, SECTION_READ | SECTION_INITIALIZED);
	bss = makeSection(".bss", 16, SECTION_READ | SECTION_WRITE);
	debugSymbols = makeSection(".debug$S", 1, SECTION_READ | SECTION_INITIALIZED | SECTION_DISCARD);
	debugTypes = makeSection(".debug$T", 1, SECTION_READ | SECTION_INITIALIZED | SECTION_DISCARD);
	pdata = makeSection(".pdata", 4, SECTION_READ | SECTION_INITIALIZED);
	xdata = makeSection(".xdata", 4, SECTION_READ | SECTION_INITIALIZED);
#else
	code = makeSection(".text", 16, SECTION_EXECUTE | SECTION_READ | SECTION_INITIALIZED);
	data = makeSection(".data", 16, SECTION_READ | SECTION_WRITE | SECTION_INITIALIZED);
	rdata = makeSection(".rodata", 16, SECTION_READ | SECTION_INITIALIZED);
	bss = makeSection(".bss", 16, SECTION_READ | SECTION_WRITE);
	debugLines = makeSection(".debug_line", 16, SECTION_READ | SECTION_INITIALIZED | SECTION_DISCARD);
	debugInfo = makeSection(".debug_info", 16, SECTION_READ | SECTION_INITIALIZED | SECTION_DISCARD);
	debugAbbrev = makeSection(".debug_abbrev", 16, SECTION_READ | SECTION_INITIALIZED | SECTION_DISCARD);

	symbols.add({});
	stringTable.addNullTerminatedString("");
#endif
	if (hadError)
		return;

	s64 f32ToU64ConstantSymbolIndex = -1;
	s64 f64ToU64ConstantSymbolIndex = -1;
	s64 u64ToF32ConstantSymbolIndex = -1;
	s64 u64ToF64ConstantSymbolIndex = -1;

#if BUILD_WINDOWS
	// @Incomplete: Just emit code to do stack probing instead of calling a function?
	s64 chkstkSymbolIndex = -1;
#endif

	Array<u64> instructionOffsets;
	Array<JumpPatch> jumpPatches;


	emitCodeViewPrologue();

	while (true) {

		CoffJob job = coffWriterQueue.take();

		if (!job.function)
			break;
		if (job.flavor == CoffJobFlavor::FUNCTION) {
			PROFILE_ZONE("Write Function");
			auto function = job.function;

			if (function->flags & EXPR_FUNCTION_IS_EXTERNAL)
				continue;

			createSymbolForFunction(function);

			instructionOffsets.clear();

			jumpPatches.clear();

			u32 functionStart = code->totalSize;

			u32 spaceToAllocate = getSpaceToAllocate(function);

			EmitFunctionInfo emitInfo = emitFunctionBegin(function, spaceToAllocate);
			addLineInfo(&emitInfo, function->start, function->end);
			setSymbolAddress(function->symbol, functionStart);


			code->ensure(256);

#if BUILD_WINDOWS
			code->add1Unchecked(PUSH_BASE | RSI);
			emitFunctionPushRsi(&emitInfo);
			code->add1Unchecked(PUSH_BASE | RDI);
			emitFunctionPushRdi(&emitInfo);

			if (spaceToAllocate >= 4096) {
				loadImmediateIntoIntRegister64(RAX, spaceToAllocate);

				code->add1(CALL_DIRECT);

				if (chkstkSymbolIndex == -1) {
					chkstkSymbolIndex = symbols.count();

					Symbol chkstk;
					setSymbolName(&chkstk, "__chkstk");
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
			else 
#endif
			if (spaceToAllocate < 0x80) {
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
			emitFunctionPreambleEnd(&emitInfo);

#if BUILD_WINDOWS
			constexpr u8 intRegisters[4] = { RCX, RDX, 8, 9 };
			
			auto functionType = static_cast<TypeFunction *>(function->type);
			auto pointerReturn = returnsViaPointer(functionType);
			
			u32 argumentIndex = 0;

			if (pointerReturn) {
				storeFromIntRegister(function, 8, function->state.returnPointerRegister, intRegisters[argumentIndex++]);
			}

			if (!(functionType->flags & TYPE_FUNCTION_IS_C_CALL)) {
				storeFromIntRegister(function, 8, function->state.contextRegister, intRegisters[argumentIndex++]);
				emitLocalDeclaration("context", &TYPE_CONTEXT, getRegisterOffset(function, function->state.contextRegister));
			}
			for (u32 i = 0; i < function->arguments.declarations.count; i++) {
				code->ensure(16);
				auto argument = function->arguments.declarations[i];
				auto type = getDeclarationType(argument);

				u32 debugOffset;
				if (argumentIndex < 4) {
					if (type->flavor == TypeFlavor::FLOAT) {
						storeFromFloatRegister(function, type->size, argument->registerOfStorage, argumentIndex);
						debugOffset = getRegisterOffset(function, argument->registerOfStorage);
					}
					else if (isStoredByPointer(type)) {
						if (isStandardSize(type->size)) {
							writeIntegerPrefix(8);
							code->add1Unchecked(LEA);
							writeRSPOffsetByte(RAX, getStackSpaceOffset(function) + argument->physicalStorage);
							storeFromIntRegister(function, 8, argument->registerOfStorage, RAX);

							u8 reg = intRegisters[argumentIndex];
							writeIntegerPrefixR(type->size, &reg);
							sizedIntInstruction(type->size, MOV_MEM_REG_BASE);
							writeModRM(MODRM_MOD_INDIRECT, reg, RAX);
							debugOffset = getStackSpaceOffset(function) + argument->physicalStorage;
						}
						else {
							storeFromIntRegister(function, 8, argument->registerOfStorage, intRegisters[argumentIndex]);
							debugOffset = getRegisterOffset(function, argument->registerOfStorage);
						}
					}
					else {
						storeFromIntRegister(function, type->size, argument->registerOfStorage, intRegisters[argumentIndex]);
						debugOffset = getRegisterOffset(function, argument->registerOfStorage);
					}
				}
				else {
					u32 location = spaceToAllocate + 24 + 8 * argumentIndex;
					
					if (isStoredByPointer(type)) {
						if (isStandardSize(type->size)) {
							writeIntegerPrefix(8);
							code->add1Unchecked(LEA);
							writeRSPOffsetByte(RAX, location);
							storeFromIntRegister(function, 8, argument->registerOfStorage, RAX);
							debugOffset = location;
						}
						else {
							writeIntegerPrefix(8);
							sizedIntInstruction(8, MOV_REG_MEM_BASE);
							writeRSPOffsetByte(RAX, location);
							storeFromIntRegister(function, 8, argument->registerOfStorage, RAX);
							debugOffset = getRegisterOffset(function, argument->registerOfStorage);
						}
					}
					else {
						writeIntegerPrefix(type->size);
						sizedIntInstruction(type->size, MOV_REG_MEM_BASE);
						writeRSPOffsetByte(RAX, location);
						storeFromIntRegister(function, type->size, argument->registerOfStorage, RAX);
						debugOffset = getRegisterOffset(function, argument->registerOfStorage);
					}
				}

				argumentIndex++;
				emitLocalDeclaration(argument->name, getDeclarationType(argument), debugOffset);
			}
#else
			
			
			u32 intRegisterIndex = 0;
			u32 floatRegisterIndex = 8;

			constexpr u8 intRegisters[6] = { RDI, RSI, RDX, RCX, 8, 9 };
			
			auto functionType = static_cast<TypeFunction *>(function->type);
			auto pointerReturn = returnsViaPointer(functionType);

			if (pointerReturn) {
				storeFromIntRegister(function, 8, function->state.returnPointerRegister, intRegisters[intRegisterIndex++]);
			}

			if (!(functionType->flags & TYPE_FUNCTION_IS_C_CALL)) {
				storeFromIntRegister(function, 8, function->state.contextRegister, intRegisters[intRegisterIndex++]);
				//emitLocalDeclaration("context", &TYPE_CONTEXT, getRegisterOffset(function, function->state.contextRegister));
			}

			u32 memoryParameterOffset = 8 + spaceToAllocate;
			SystemVCallingState callingState = initSystemVCallingState(functionType);

			for (u32 i = 0; i < function->arguments.declarations.count; i++) {
				code->ensure(64);
				auto argument = function->arguments.declarations[i];
				auto type = getDeclarationType(argument);
				
				auto callingType = passSystemVParameter(&callingState, type);

				switch (callingType) {
					case SystemVCallingType::EMPTY:
						break;
					case SystemVCallingType::FLOAT:
						if (isStoredByPointer(type)) {
							writeIntegerPrefix(8);
							code->add1Unchecked(LEA);
							writeRSPOffsetByte(RAX, argument->physicalStorage);
							
							storeFromIntRegister(function, 8, argument->registerOfStorage, RAX);

							writeFloatPrefix(type->size);
							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_MOVSf_MEM_REG);
							writeModRM(MODRM_MOD_INDIRECT, floatRegisterIndex++, RAX);
						}
						else {
							storeFromFloatRegister(function, type->size, argument->registerOfStorage, floatRegisterIndex++);
						}
						break;
					case SystemVCallingType::INT:
						if (isStoredByPointer(type)) {
							writeIntegerPrefix(8);
							code->add1Unchecked(LEA);
							writeRSPOffsetByte(RAX, argument->physicalStorage);
							
							storeFromIntRegister(function, 8, argument->registerOfStorage, RAX);
							
							storeWeirdSizeFromRegister(type->size, RAX, intRegisters[intRegisterIndex++]);
						}
						else {
							storeFromIntRegister(function, type->size, argument->registerOfStorage, intRegisters[intRegisterIndex++]);
						}
						break;
					case SystemVCallingType::FLOAT_FLOAT:
						writeIntegerPrefix(8);
						code->add1Unchecked(LEA);
						writeRSPOffsetByte(RAX, argument->physicalStorage);
						
						storeFromIntRegister(function, 8, argument->registerOfStorage, RAX);

						writeFloatPrefix(8);
						code->add1Unchecked(OPCODE_EXT);
						code->add1Unchecked(EXT_MOVSf_MEM_REG);
						writeModRM(MODRM_MOD_INDIRECT, floatRegisterIndex++, RAX);

						writeFloatPrefix(type->size - 8);
						code->add1Unchecked(OPCODE_EXT);
						code->add1Unchecked(EXT_MOVSf_MEM_REG);
						writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, floatRegisterIndex++, RAX);
						code->add1Unchecked(8);
						break;
					case SystemVCallingType::INT_INT: {
						writeIntegerPrefix(8);
						code->add1Unchecked(LEA);
						writeRSPOffsetByte(RAX, argument->physicalStorage);
						
						storeFromIntRegister(function, 8, argument->registerOfStorage, RAX);

						u8 intRegister = intRegisters[intRegisterIndex++];

						writeIntegerPrefixR(8, &intRegister);
						sizedIntInstruction(8, MOV_MEM_REG_BASE);
						writeModRM(MODRM_MOD_INDIRECT, intRegister, RAX);

						writeIntegerPrefix(8);
						code->add1Unchecked(INT_OP_MEM_IMM_8_MODRM_X);
						writeModRM(MODRM_MOD_DIRECT, INT_OP_MODRM_ADD, RAX);
						code->add1Unchecked(8);

						storeWeirdSizeFromRegister(type->size - 8, RAX, intRegisters[intRegisterIndex++]);
						break;
					}
					case SystemVCallingType::INT_FLOAT:  {
						writeIntegerPrefix(8);
						code->add1Unchecked(LEA);
						writeRSPOffsetByte(RAX, argument->physicalStorage);
						
						storeFromIntRegister(function, 8, argument->registerOfStorage, RAX);

						u8 intRegister = intRegisters[intRegisterIndex++];

						writeIntegerPrefixR(8, &intRegister);
						sizedIntInstruction(8, MOV_MEM_REG_BASE);
						writeModRM(MODRM_MOD_INDIRECT, intRegister, RAX);

						writeFloatPrefix(type->size - 8);
						code->add1Unchecked(OPCODE_EXT);
						code->add1Unchecked(EXT_MOVSf_MEM_REG);
						writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, floatRegisterIndex++, RAX);
						code->add1Unchecked(8);
						break;
					}
					case SystemVCallingType::FLOAT_INT:
						writeIntegerPrefix(8);
						code->add1Unchecked(LEA);
						writeRSPOffsetByte(RAX, argument->physicalStorage);
						
						storeFromIntRegister(function, 8, argument->registerOfStorage, RAX);

						writeFloatPrefix(8);
						code->add1Unchecked(OPCODE_EXT);
						code->add1Unchecked(EXT_MOVSf_MEM_REG);
						writeModRM(MODRM_MOD_INDIRECT, floatRegisterIndex++, RAX);

						writeIntegerPrefix(8);
						code->add1Unchecked(INT_OP_MEM_IMM_8_MODRM_X);
						writeModRM(MODRM_MOD_DIRECT, INT_OP_MODRM_ADD, RAX);
						code->add1Unchecked(8);

						storeWeirdSizeFromRegister(type->size - 8, RAX, intRegisters[intRegisterIndex++]);
						break;
					case SystemVCallingType::MEMORY:
						break;
						if (isStoredByPointer(type)) {
							writeIntegerPrefix(8);
							code->add1Unchecked(LEA);
							writeRSPOffsetByte(RAX, memoryParameterOffset);
							storeFromIntRegister(function, 8, argument->registerOfStorage, RAX);
						}
						else {
							writeIntegerPrefix(type->size);
							sizedIntInstruction(type->size, MOV_REG_MEM_BASE);
							writeRSPOffsetByte(RAX, memoryParameterOffset);

							storeFromIntRegister(function, type->size, argument->registerOfStorage, RAX);
						}

						memoryParameterOffset = AlignPO2(memoryParameterOffset + type->size, 8);
						break;
					case SystemVCallingType::UNKNOWN: [[fallthrough]];
					default:
						assert(false);
						break;
				}

				//emitLocalDeclaration(argument->name, getDeclarationType(argument), debugOffset);
			}
#endif



			for (u32 index = 0; index < function->state.ir.count; index++) {
				auto &ir = function->state.ir[index];

				instructionOffsets.add(code->totalSize);

				code->ensure(128);

				switch (ir.op) {
				case IrOp::TYPE: {
					auto type = static_cast<Type *>(ir.data);
					markUsedTypeInfoInType(type);

					writeIntegerPrefix(8);
					code->add1Unchecked(LEA);
					writeModRM(MODRM_MOD_INDIRECT, RAX, MODRM_RM_RIP_OFFSET_32);
					code->addRel32Relocation(type->physicalStorage);

					storeFromIntRegister(function, 8, ir.dest, RAX);
				} break;
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
				case IrOp::COPY_SRC_OFFSET: {
					if (isStandardSize(ir.opSize)) {
						loadIntoIntRegister(function, 8, RAX, ir.a);
						loadIntoIntRegister(function, 8, RCX, ir.dest);

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

						writeIntegerPrefix(ir.opSize);
						sizedIntInstruction(ir.opSize, MOV_MEM_REG_BASE);
						writeModRM(MODRM_MOD_INDIRECT, RAX, RCX);
					}
					else {
						loadIntoIntRegister(function, 8, RDI, ir.dest);
						loadIntoIntRegister(function, 8, RSI, ir.a);

						if (ir.immediate) {
							writeIntegerPrefix(8);
							if (ir.immediate <= 0x7f) {
								sizedIntInstruction(8, INT_OP_MEM_IMM_8_MODRM_X);
								writeModRM(MODRM_MOD_DIRECT, INT_OP_MODRM_ADD, RSI);
								code->add1Unchecked(ir.immediate);
							}
							else {
								sizedIntInstruction(8, INT_OP_MEM_IMM_32_MODRM_X);
								writeModRM(MODRM_MOD_DIRECT, INT_OP_MODRM_ADD, RSI);
								code->add4Unchecked(ir.immediate);
							}
						}

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
					code->addRel32Relocation(createSymbolForDeclaration(declaration), ir.a);

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
									rdata->align(8);
									f64ToU64ConstantSymbolIndex = createSymbol(rdata, "@f64ToU64Constant");
									rdata->add8(0x43E0000000000000);
								}
							}
							else {
								if (f32ToU64ConstantSymbolIndex == -1) {
									rdata->align(4);
									f32ToU64ConstantSymbolIndex = createSymbol(rdata, "@f32ToU64Constant");
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
									rdata->align(8);
									u64ToF64ConstantSymbolIndex = createSymbol(rdata, "@u64ToF64Constant");
									rdata->add8(0x43F0000000000000ULL);
								}
							}
							else {
								if (u64ToF32ConstantSymbolIndex == -1) {
									rdata->align(4);
									u64ToF32ConstantSymbolIndex = createSymbol(rdata, "@u64ToF32Constant");
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
					auto returnType = static_cast<Type *>(ir.data);

					switch (static_cast<SystemVCallingType>(ir.opSize)) {
						case SystemVCallingType::UNKNOWN:
							assert(false);
							break;
						case SystemVCallingType::MEMORY: 
							loadIntoIntRegister(function, 8, RAX, function->state.returnPointerRegister);
							break;
						case SystemVCallingType::EMPTY:
							break;
						case SystemVCallingType::INT:
							if (isStoredByPointer(returnType)) {
								loadIntoIntRegister(function, 8, RCX, ir.a);
								loadWeirdSizeIntoRegister(returnType->size, RAX, RCX);
							}
							else {
								loadIntoIntRegister(function, returnType->size, RAX, ir.a);
							}
							break;
						case SystemVCallingType::FLOAT:
							if (isStoredByPointer(returnType)) {
								loadIntoIntRegister(function, 8, RCX, ir.a);

								writeFloatPrefix(returnType->size);
								code->add1Unchecked(OPCODE_EXT);
								code->add1Unchecked(EXT_MOVSf_REG_MEM);
								writeModRM(MODRM_MOD_INDIRECT, 0, RCX);
							}
							else {
								loadIntoFloatRegister(function, returnType->size, 0, ir.a);
							}
							break;
						case SystemVCallingType::INT_INT: {
							loadIntoIntRegister(function, returnType->size, RCX, ir.a);

							writeIntegerPrefix(8);
							sizedIntInstruction(8, MOV_REG_MEM_BASE);
							writeModRM(MODRM_MOD_INDIRECT, RAX, RCX);

							writeIntegerPrefix(8);
							code->add1Unchecked(INT_OP_MEM_IMM_8_MODRM_X);
							writeModRM(MODRM_MOD_DIRECT, INT_OP_MODRM_ADD, RCX);
							code->add1Unchecked(8);

							loadWeirdSizeIntoRegister(returnType->size - 8, RDX, RCX);
						} break;
						case SystemVCallingType::INT_FLOAT: {
							loadIntoIntRegister(function, returnType->size, RCX, ir.a);

							writeIntegerPrefix(8);
							sizedIntInstruction(8, MOV_REG_MEM_BASE);
							writeModRM(MODRM_MOD_INDIRECT, RAX, RCX);

							writeFloatPrefix(returnType->size - 8);
							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_MOVSf_REG_MEM);
							writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, 0, RCX);
							code->add1Unchecked(8);
						} break;
						case SystemVCallingType::FLOAT_INT: {
							loadIntoIntRegister(function, 8, RCX, ir.a);

							writeFloatPrefix(8);
							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_MOVSf_REG_MEM);
							writeModRM(MODRM_MOD_INDIRECT, 0, RCX);
						
							writeIntegerPrefix(8);
							code->add1Unchecked(INT_OP_MEM_IMM_8_MODRM_X);
							writeModRM(MODRM_MOD_DIRECT, INT_OP_MODRM_ADD, RCX);
							code->add1Unchecked(8);

							loadWeirdSizeIntoRegister(returnType->size - 8, RAX, RCX);
						} break;
						case SystemVCallingType::FLOAT_FLOAT: {
							loadIntoIntRegister(function, 8, RCX, ir.a);

							writeFloatPrefix(8);
							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_MOVSf_REG_MEM);
							writeModRM(MODRM_MOD_INDIRECT, 0, RCX);

							writeFloatPrefix(returnType->size - 8);
							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_MOVSf_REG_MEM);
							writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, 1, RCX);
							code->add1Unchecked(8);
						} break;
						default:
							assert(false);
							break;
					}

					code->add1Unchecked(JMP_DIRECT);

					JumpPatch patch;
					patch.opToPatch = function->state.ir.count;
					patch.location = reinterpret_cast<s32 *>(code->add4Unchecked(0));
					patch.rip = code->totalSize;

					jumpPatches.add(patch);
				} break;
				case IrOp::CALL: {
					auto arguments = static_cast<FunctionCall *>(ir.data);
					code->ensure(128);

					bool pointerReturn = returnsViaPointer(arguments->function);
					bool cCall = arguments->function->flags & TYPE_FUNCTION_IS_C_CALL;

					if (!cCall && buildOptions.enable_stack_trace) {
						writeIntegerPrefix(8);
						sizedIntInstruction(8, MOV_REG_MEM_BASE);
						writeRSPRegisterByte(function, RAX, arguments->arguments[0]);

						writeIntegerPrefix(8);
						sizedIntInstruction(8, MOV_REG_MEM_BASE);
						writeModRM(MODRM_MOD_INDIRECT, RCX, RAX);

						writeIntegerPrefix(8);
						code->add1Unchecked(LEA);
						writeRSPOffsetByte(RDX, getStacktraceNodeOffset(function));

						writeIntegerPrefix(8);
						sizedIntInstruction(8, MOV_MEM_REG_BASE);
						writeModRM(MODRM_MOD_INDIRECT, RDX, RAX);

						writeIntegerPrefix(8);
						sizedIntInstruction(8, MOV_MEM_REG_BASE);
						writeModRM(MODRM_MOD_INDIRECT, RCX, RDX);


						u32 functionSymbol = createSymbol(rdata);
						rdata->addNullTerminatedString(arguments->stackTrace.function);
						u32 filenameSymbol = createSymbol(rdata);
						rdata->addNullTerminatedString(arguments->stackTrace.filename);

						Section *section = BUILD_LINUX ? data : rdata;
						section->ensure(40);
						u32 symbolIndex = createSymbol(section, "");
						section->addPointerRelocation(functionSymbol);
						section->add8Unchecked(arguments->stackTrace.function.count);
						section->addPointerRelocation(filenameSymbol);
						section->add8Unchecked(arguments->stackTrace.filename.count);
						section->add8Unchecked(arguments->stackTrace.line);

						writeIntegerPrefix(8);
						code->add1Unchecked(LEA);
						writeModRM(MODRM_MOD_INDIRECT, RAX, MODRM_RM_RIP_OFFSET_32);
						code->addRel32Relocation(symbolIndex);

						writeIntegerPrefix(8);
						sizedIntInstruction(8, MOV_MEM_REG_BASE);
						writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, RAX, RDX);
						code->add1Unchecked(8);
					}

#if BUILD_WINDOWS
					constexpr int intRegisters[4] = { RCX, RDX, 8, 9 };
					u32 argumentIndex = 0;

					if (pointerReturn) {
						loadIntoIntRegister(function, 8, intRegisters[argumentIndex++], ir.dest);
					}

					if (!cCall) {
						loadIntoIntRegister(function, 8, intRegisters[argumentIndex++], arguments->arguments[0]);
					}

					u32 argumentCount = pointerReturn + !cCall + arguments->function->argumentCount;

					u32 parameterOffset = getParameterSpaceForCallOffset(function);
					u32 offsetForCCallLargeValueCopies = AlignPO2(parameterOffset + argumentCount * 8, 16);

					// Need to do this prior to other setup because it clobbers rcx
					if (cCall) {
						for (u8 i = 0; i < arguments->function->argumentCount; i++) {
							auto type = arguments->function->argumentTypes[i];

							if (isStandardSize(type->size))
								continue;

							code->ensure(32);

							auto number = arguments->arguments[i + !cCall];

							loadIntoIntRegister(function, 8, RSI, number);

							writeIntegerPrefix(8);
							code->add1Unchecked(LEA);
							writeRSPOffsetByte(RDI, offsetForCCallLargeValueCopies);

							loadImmediateIntoIntRegister64(RCX, type->size);

							code->add1Unchecked(REP_PREFIX);
							sizedIntInstruction(1, MOVS_BASE);

							offsetForCCallLargeValueCopies += AlignPO2(type->size, 16);
						}

						offsetForCCallLargeValueCopies = AlignPO2(parameterOffset + argumentCount * 8, 16);
					}

					for (u8 i = 0; i < arguments->function->argumentCount; i++) {
						code->ensure(64);
						auto type = arguments->function->argumentTypes[i];
						auto number = arguments->arguments[i + !cCall];

						if (argumentIndex < 4) {
							if (type->flavor == TypeFlavor::FLOAT) {
								loadIntoFloatRegister(function, type->size, argumentIndex, number);
							}
							else if (isStoredByPointer(type)) {
								if (isStandardSize(type->size)) {
									loadIntoIntRegister(function, 8, RAX, number);

									u8 intRegister = intRegisters[argumentIndex];
									writeIntegerPrefixR(type->size, &intRegister);
									sizedIntInstruction(type->size, MOV_REG_MEM_BASE);
									writeModRM(MODRM_MOD_INDIRECT, intRegister, RAX);
								}
								else {
									if (cCall) {
										u8 reg = intRegisters[argumentIndex];
										writeIntegerPrefixR(8, &reg);
										code->add1Unchecked(LEA);
										writeRSPOffsetByte(reg, offsetForCCallLargeValueCopies);
										
										offsetForCCallLargeValueCopies += AlignPO2(type->size, 16);
									}
									else {
										loadIntoIntRegister(function, 8, intRegisters[argumentIndex], number);
									}
								}
							}
							else {
								assert(isStandardSize(type->size));
								loadIntoIntRegister(function, type->size, intRegisters[argumentIndex], number);
							}
						}
						else {
							u32 location = parameterOffset + argumentIndex * 8;

							if (isStoredByPointer(type)) {
								if (isStandardSize(type->size)) {
									loadIntoIntRegister(function, 8, RAX, number);
									
									writeIntegerPrefix(type->size);
									sizedIntInstruction(type->size, MOV_REG_MEM_BASE);
									writeModRM(MODRM_MOD_INDIRECT, RAX, RAX);

									writeIntegerPrefix(type->size);
									sizedIntInstruction(type->size, MOV_MEM_REG_BASE);
									writeRSPOffsetByte(RAX, location);
								}
								else {
									if (cCall) {
										writeIntegerPrefix(8);
										code->add1Unchecked(LEA);
										writeRSPOffsetByte(RAX, offsetForCCallLargeValueCopies);

										sizedIntInstruction(8, MOV_REG_MEM_BASE);
										writeRSPOffsetByte(RAX, location);
										
										offsetForCCallLargeValueCopies += AlignPO2(type->size, 16);
									}
									else {
										loadIntoIntRegister(function, 8, RAX, number);

										writeIntegerPrefix(8);
										sizedIntInstruction(8, MOV_MEM_REG_BASE);
										writeRSPOffsetByte(RAX, location);
									}
								}
							}
							else {
								assert(isStandardSize(type->size));
								loadIntoIntRegister(function, type->size, RAX, number);

								writeIntegerPrefix(type->size);
								sizedIntInstruction(type->size, MOV_MEM_REG_BASE);
								writeRSPOffsetByte(RAX, location);
							}
						}

						argumentIndex++;				
					}
#else

					// First pass for memory arguments so we don't stomp RDI/RSI/RCX


					SystemVCallingState callingState = initSystemVCallingState(arguments->function);
					
					u32 memoryParameterOffset = getParameterSpaceForCallOffset(function);

					for (u32 i = 0; i < arguments->function->argumentCount; i++) {
						auto type = arguments->function->argumentTypes[i];
						auto number = arguments->arguments[!cCall + i];

						if (passSystemVParameter(&callingState, type) != SystemVCallingType::MEMORY)
							continue;
						code->ensure(32);

						if (isStoredByPointer(type)) {
							loadIntoIntRegister(function, 8, RSI, number);

							writeIntegerPrefix(8);
							code->add1Unchecked(LEA);
							writeRSPOffsetByte(RDI, memoryParameterOffset);

							loadImmediateIntoIntRegister64(RCX, type->size);

							code->add1Unchecked(REP_PREFIX);
							sizedIntInstruction(1, MOVS_BASE);
						}
						else {
							loadIntoIntRegister(function, type->size, RAX, number);

							writeIntegerPrefix(type->size);
							sizedIntInstruction(type->size, MOV_MEM_REG_BASE);
							writeRSPOffsetByte(RAX, memoryParameterOffset);
						}


						memoryParameterOffset = AlignPO2(memoryParameterOffset + type->size, 8);
					}


					callingState = initSystemVCallingState(arguments->function);

					u32 intRegisterIndex = 0;
					u32 floatRegisterIndex = 0;

					if (pointerReturn) {
						loadIntoIntRegister(function, 8, intRegisters[intRegisterIndex++], op.dest);
					}

					if (!cCall) {
						loadIntoIntRegister(function, 8, intRegisters[intRegisterIndex++], arguments->arguments[0]);
					}

					for (u32 i = 0; i < arguments->function->argumentCount; i++) {
						auto type = arguments->function->argumentTypes[i];
						auto number = arguments->arguments[!cCall + i];
						code->ensure(64);

						switch (passSystemVParameter(&callingState, type)) {
							case SystemVCallingType::EMPTY: [[fallthrough]];
							case SystemVCallingType::MEMORY: // Handled in previous pass
								break;
							case SystemVCallingType::INT:
								if (isStoredByPointer(type)) {
									loadIntoIntRegister(function, 8, RAX, number);

									loadWeirdSizeIntoRegister(type->size, intRegisters[intRegisterIndex++], RAX);
								}
								else {
									loadIntoIntRegister(function, type->size, intRegisters[intRegisterIndex++], number);
								}
								break;
							case SystemVCallingType::FLOAT:
								if (isStoredByPointer(type)) {
									loadIntoIntRegister(function, 8, RAX, number);

									writeFloatPrefix(type->size);
									code->add1Unchecked(OPCODE_EXT);
									code->add1Unchecked(EXT_MOVSf_REG_MEM);
									writeModRM(MODRM_MOD_INDIRECT, floatRegisterIndex++, RAX);
								}
								else {
									loadIntoFloatRegister(function, type->size, floatRegisterIndex++, RAX);
								}
								break;
							case SystemVCallingType::INT_INT: {
								loadIntoIntRegister(function, 8, RAX, number);

								u8 intRegister = intRegisters[intRegisterIndex++];
								writeIntegerPrefixR(8, &intRegister);
								sizedIntInstruction(8, MOV_REG_MEM_BASE);
								writeModRM(MODRM_MOD_INDIRECT, intRegister, RAX);

								writeIntegerPrefix(8);
								code->add1Unchecked(INT_OP_MEM_IMM_8_MODRM_X);
								writeModRM(MODRM_MOD_DIRECT, INT_OP_MODRM_ADD, RAX);
								code->add1Unchecked(8);

								loadWeirdSizeIntoRegister(type->size - 8, intRegisters[intRegisterIndex++], RAX);
								break;
							}
							case SystemVCallingType::FLOAT_FLOAT:
								loadIntoIntRegister(function, 8, RAX, number);

								writeFloatPrefix(8);
								code->add1Unchecked(OPCODE_EXT);
								code->add1Unchecked(EXT_MOVSf_REG_MEM);
								writeModRM(MODRM_MOD_INDIRECT, floatRegisterIndex++, RAX);

								writeFloatPrefix(type->size - 8);
								code->add1Unchecked(OPCODE_EXT);
								code->add1Unchecked(EXT_MOVSf_REG_MEM);
								writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, floatRegisterIndex++, RAX);
								code->add1Unchecked(8);
								break;
							case SystemVCallingType::INT_FLOAT: {
								loadIntoIntRegister(function, 8, RAX, number);

								u8 intRegister = intRegisters[intRegisterIndex++];
								writeIntegerPrefixR(8, &intRegister);
								sizedIntInstruction(8, MOV_REG_MEM_BASE);
								writeModRM(MODRM_MOD_INDIRECT, intRegister, RAX);

								writeFloatPrefix(type->size - 8);
								code->add1Unchecked(OPCODE_EXT);
								code->add1Unchecked(EXT_MOVSf_REG_MEM);
								writeModRM(MODRM_MOD_INDIRECT_OFFSET_8, floatRegisterIndex++, RAX);
								code->add1Unchecked(8);
								break;
							}
							case SystemVCallingType::FLOAT_INT:
								loadIntoIntRegister(function, 8, RAX, number);

								writeFloatPrefix(8);
								code->add1Unchecked(OPCODE_EXT);
								code->add1Unchecked(EXT_MOVSf_REG_MEM);
								writeModRM(MODRM_MOD_INDIRECT, floatRegisterIndex++, RAX);

								writeIntegerPrefix(8);
								code->add1Unchecked(INT_OP_MEM_IMM_8_MODRM_X);
								writeModRM(MODRM_MOD_DIRECT, INT_OP_MODRM_ADD, RAX);
								code->add1Unchecked(8);

								loadWeirdSizeIntoRegister(type->size - 8, intRegisters[intRegisterIndex++], RAX);
								break;
							case SystemVCallingType::UNKNOWN: [[fallthrough]];
							default:
								assert(false);
								break;
						}
					}

#endif

					code->ensure(64);
					code->add1Unchecked(CALL_INDIRECT_MODRM_2);
					writeRSPRegisterByte(function, 2, ir.a);

					auto returnType = arguments->function->returnTypes[0];
					if (!pointerReturn && returnType != &TYPE_VOID) {
						loadIntoIntRegister(function, 8, RCX, ir.dest);

						if (returnType->flavor == TypeFlavor::FLOAT) {
							writeFloatPrefix(returnType->size);
							code->add1Unchecked(OPCODE_EXT);
							code->add1Unchecked(EXT_MOVSf_MEM_REG);
							writeModRM(MODRM_MOD_INDIRECT, 0, RCX);
						}
						else {
							writeIntegerPrefix(returnType->size);
							sizedIntInstruction(returnType->size, MOV_MEM_REG_BASE);
							writeModRM(MODRM_MOD_INDIRECT, RAX, RCX);
						}
					}

					if (!cCall && buildOptions.enable_stack_trace) {
						writeIntegerPrefix(8);
						sizedIntInstruction(8, MOV_REG_MEM_BASE);
						writeRSPRegisterByte(function, RAX, arguments->arguments[0]);

						writeIntegerPrefix(8);
						sizedIntInstruction(8, MOV_REG_MEM_BASE);
						writeRSPOffsetByte(RCX, getStacktraceNodeOffset(function));

						writeIntegerPrefix(8);
						sizedIntInstruction(8, MOV_MEM_REG_BASE);
						writeModRM(MODRM_MOD_INDIRECT, RCX, RAX);
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
					
					code->addFunctionRel32Relocation(static_cast<ExprFunction *>(ir.data));

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

						// Because Linux sucks we can't apply relocations to the .rodata section :NotUsingRdata
						auto section = BUILD_LINUX ? data : rdata;
						array->physicalStorage = createSymbol(section);
						u32 offset = section->totalSize; // do this in a separate statement because within a c++ statement, subexpressions can execute in any order
						                              // which meant that allocateUnaligned happened _before_ section->totalSize was evaluated, causing the addresses 
						                              // for any patch applied to the literal to be wrong
						writeValue(offset, static_cast<u8 *>(section->allocateUnaligned(array->type->size)), section, array);
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

						// Because Linux sucks we can't apply relocations to the .rodata section :NotUsingRdata
						auto section = BUILD_LINUX ? data : rdata;

						literal->physicalStorage = createSymbol(section);
						u32 offset = section->totalSize; // do this in a separate statement because within a c++ statement, subexpressions can execute in any order
													  // which meant that allocateUnaligned happened _before_ section->totalSize was evaluated, causing the addresses 
													  // for any patch applied to the literal to be wrong
						writeValue(offset, static_cast<u8 *>(section->allocateUnaligned(literal->type->size)), section, literal);
					}

					code->addRel32Relocation(literal->physicalStorage);

					storeFromIntRegister(function, 8, ir.dest, RAX);
				} break;
				case IrOp::LINE_MARKER: {
					addLineInfo(&emitInfo, ir.location.start, ir.location.end);
				} break;
				case IrOp::BLOCK: {
					auto block = static_cast<Block *>(ir.data);

					if (block) {
						emitBlockStart(&emitInfo);

						for (auto declaration : block->declarations) {
							if (declaration->flags & (DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_CONSTANT)) continue;

							u32 offset;

							if (declarationIsStoredByPointer(declaration)) {
								offset = getStackSpaceOffset(function) + declaration->physicalStorage;
							}
							else {
								offset = getRegisterOffset(function, declaration->registerOfStorage);
							}

							emitLocalDeclaration(declaration->name, getDeclarationType(declaration), offset);
						}
					}
					else {
						emitBlockEnd(&emitInfo);
					}
				} break;
				default: {
					assert(false);
				}
				}
			}
			
			instructionOffsets.add(code->totalSize);
			emitFunctionPostambleStart(&emitInfo);

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

#if BUILD_WINDOWS
			code->add1Unchecked(POP_BASE | RDI);
			code->add1Unchecked(POP_BASE | RSI);
#endif

			code->add1Unchecked(RET);

			for (auto patch : jumpPatches) {
				*patch.location = static_cast<s32>(instructionOffsets[patch.opToPatch]) - static_cast<s32>(patch.rip);
			}

			setSymbolSize(function->symbol, code->totalSize - functionStart);
			emitFunctionEnd(&emitInfo);
		}
		else if (job.flavor == CoffJobFlavor::GLOBAL_DECLARATION) {
			PROFILE_ZONE("Write Declaration");
			auto declaration = job.declaration;

			assert(declaration->enclosingScope->flavor == BlockFlavor::GLOBAL);
			assert(!(declaration->flags & DECLARATION_IS_CONSTANT));

			createSymbolForDeclaration(declaration);

			emitGlobalDeclaration(declaration);

			auto type = getDeclarationType(declaration);

			if (placeValueInBSSSection(declaration)) {
				bss->totalSize = AlignPO2(bss->totalSize, type->alignment);

				setSymbolAddress(declaration->symbol, bss->totalSize);
				setSymbolSection(declaration->symbol, bss->sectionNumber);

				bss->totalSize += type->size;
			}
			else {
				data->align(type->alignment);

				setSymbolAddress(declaration->symbol, data->totalSize);
				setSymbolSection(declaration->symbol, data->sectionNumber);

				u32 dataSize = data->totalSize;
				u8 *allocation = static_cast<u8 *>(data->allocateUnaligned(type->size));

				writeValue(dataSize, allocation, data, declaration->initialValue);
			}
		}
	}
	
	emitCodeViewEpilogue();

	{
		PROFILE_ZONE("Write types");

		for (u64 i = 0; i < typeTableCapacity; i++) {
			auto entry = typeTableEntries[i];

			if (entry.hash) {
				auto type = entry.value;

				if (!(type->flags & TYPE_USED_IN_OUTPUT))
					continue;

				u32 name = createSymbol(rdata);
				rdata->addNullTerminatedString(type->name);

				assert(type->name.length);

				Type_Info::Tag infoTag = Type_Info::Tag::VOID;

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

					// :NotUsingRdata
					auto section = BUILD_LINUX ? data : rdata;
					section->align(8);

					setSymbolAddress(type->symbol, section->totalSize);

					info.tag = infoTag;
					info.size = type->size;
					info.alignment = type->alignment;
					info.name = { nullptr, type->name.length };

					if (name)
						section->addPointerRelocation(name, section->totalSize + offsetof(decltype(info), name));

					section->add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::INTEGER: {
					Type_Info_Integer info;

					// Because Linux sucks we can't apply relocations to the .rodata section :NotUsingRdata
					auto section = BUILD_LINUX ? data : rdata;
					section->align(8);

					setSymbolAddress(type->symbol, section->totalSize);

					info.tag = infoTag;
					info.size = type->size;
					info.alignment = type->alignment;
					info.name = { nullptr, type->name.length };
					info.signed_ = type->flags & TYPE_INTEGER_IS_SIGNED;

					if (name)
						section->addPointerRelocation(name, section->totalSize + offsetof(decltype(info), name));

					section->add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::POINTER: {
					Type_Info_Pointer info;

					// :NotUsingRdata
					auto section = BUILD_LINUX ? data : rdata;
					section->align(8);

					setSymbolAddress(type->symbol, section->totalSize);

					info.tag = infoTag;
					info.size = type->size;
					info.alignment = type->alignment;
					info.name = { nullptr, type->name.length };
					info.value_type = nullptr;

					if (name)
						section->addPointerRelocation(name, section->totalSize + offsetof(decltype(info), name));

					section->addPointerRelocation(static_cast<TypePointer *>(type)->pointerTo->physicalStorage, section->totalSize + offsetof(decltype(info), value_type));

					section->add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::FUNCTION: {
					auto function = static_cast<TypeFunction *>(type);

					// :NotUsingRdata
					auto section = BUILD_LINUX ? data : rdata;
					section->align(8);

					u32 arguments = createSymbol(section);

					for (u64 i = 0; i < function->argumentCount; i++) {
						section->addPointerRelocation(function->argumentTypes[i]->physicalStorage);
					}
					
					u32 returns = createSymbol(section);

					for (u64 i = 0; i < function->returnCount; i++) {
						section->addPointerRelocation(function->returnTypes[i]->physicalStorage);
					}

					Type_Info_Function info;

					setSymbolAddress(type->symbol, section->totalSize);

					info.tag = infoTag;
					info.size = type->size;
					info.alignment = type->alignment;
					info.name = { nullptr, type->name.length };
					info.arguments.data = nullptr;
					info.arguments.count = function->argumentCount;
					info.returns.data = nullptr;
					info.returns.count = function->returnCount;

					if (name)
						section->addPointerRelocation(name, section->totalSize + offsetof(decltype(info), name));

					section->addPointerRelocation(arguments, section->totalSize + offsetof(decltype(info), arguments.data));
					section->addPointerRelocation(returns, section->totalSize + offsetof(decltype(info), returns.data));

					section->add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::ARRAY: {
					Type_Info_Array info;

					// :NotUsingRdata
					auto section = BUILD_LINUX ? data : rdata;
					section->align(8);

					setSymbolAddress(type->symbol, section->totalSize);

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
						section->addPointerRelocation(name, section->totalSize + offsetof(decltype(info), name));

					section->addPointerRelocation(static_cast<TypeArray *>(type)->arrayOf->physicalStorage, section->totalSize + offsetof(decltype(info), element_type));

					section->add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::STRUCT: {
					auto struct_ = static_cast<TypeStruct *>(type);

					u32 names = symbols.count();

					for (auto member : struct_->members.declarations) {
						if (member->flags & DECLARATION_IMPORTED_BY_USING) continue;


						createSymbol(rdata);
						rdata->addNullTerminatedString(member->name);
					}

					// :NotUsingRdata
					auto section = BUILD_LINUX ? data : rdata;
					u32 values = symbols.count();

					for (auto member : struct_->members.declarations) {
						if (member->flags & DECLARATION_IMPORTED_BY_USING) continue;

						if (!member->initialValue) continue;

						auto type = getTypeForExpr(member->initialValue);
						
						if (member->initialValue->flavor == ExprFlavor::TYPE_LITERAL && static_cast<ExprLiteral *>(member->initialValue)->typeValue->flavor == TypeFlavor::MODULE)
							continue;

						section->align(type->alignment);

						createSymbol(section);

						u32 dataSize = section->totalSize;
						u8 *allocation = static_cast<u8 *>(section->allocateUnaligned(type->size));

						writeValue(dataSize, allocation, section, member->initialValue);
					}

					section->align(8);
					u32 members = createSymbol(section);

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


						section->addPointerRelocation(names + nameCount, section->totalSize + offsetof(decltype(data), name.data));

						if (member->initialValue) {
							section->addPointerRelocation(getTypeForExpr(member->initialValue)->physicalStorage, section->totalSize + offsetof(decltype(data), member_type));
						}
						else {
							section->addPointerRelocation(getDeclarationType(member)->physicalStorage, section->totalSize + offsetof(decltype(data), member_type));
						}

						if (member->initialValue) { // @Incomplete: Export info for namespaces
							if (member->initialValue->flavor != ExprFlavor::TYPE_LITERAL || static_cast<ExprLiteral *>(member->initialValue)->typeValue->flavor != TypeFlavor::MODULE) {
								section->addPointerRelocation(values + valueCount, section->totalSize + offsetof(decltype(data), initial_value));
								++valueCount;
							}
						}

						section->add(&data, sizeof(data));

						++nameCount;
					}

					Type_Info_Struct info;

					setSymbolAddress(type->symbol, section->totalSize);

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
						section->addPointerRelocation(name, section->totalSize + offsetof(decltype(info), name));

					section->addPointerRelocation(members, section->totalSize + offsetof(decltype(info), members.data));

					section->add(&info, sizeof(info));

					break;
				}
				case Type_Info::Tag::ENUM: {
					auto enum_ = static_cast<TypeEnum *>(type);

					u32 names = symbols.count();

					for (auto member : enum_->members.declarations) {
						if (!(member->flags & DECLARATION_IS_ENUM_VALUE))
							continue;
						createSymbol(rdata);
						rdata->addNullTerminatedString(member->name);
					}

					// :NotUsingRdata
					auto section = BUILD_LINUX ? data : rdata;
					section->align(8);
					u32 values = createSymbol(section);

					for (u32 i = 0; i < enum_->members.declarations.count; i++) {
						auto member = enum_->members.declarations[i];
						if (!(member->flags & DECLARATION_IS_ENUM_VALUE))
							continue;

						Type_Info_Enum::Value data;

						data.name = { nullptr, member->name.length };
						data.value = static_cast<ExprLiteral *>(member->initialValue)->unsignedValue;

						section->addPointerRelocation(names + i, section->totalSize + offsetof(decltype(data), name.data));

						section->add(&data, sizeof(data));
					}

					Type_Info_Enum info;

					setSymbolAddress(type->symbol, section->totalSize);

					info.tag = infoTag;
					info.size = type->size;
					info.alignment = type->alignment;
					info.name = { nullptr, type->name.length };
					info.base_type = nullptr;
					info.is_flags = enum_->flags & TYPE_ENUM_IS_FLAGS ? true : false;
				
					info.values.data = nullptr;
					info.values.count = enum_->members.declarations.count - ENUM_SPECIAL_MEMBER_COUNT;

					if (name)
						section->addPointerRelocation(name, section->totalSize + offsetof(decltype(info), name));

					section->addPointerRelocation(enum_->integerType->physicalStorage , section->totalSize + offsetof(decltype(info), base_type));
					section->addPointerRelocation(values, section->totalSize + offsetof(decltype(info), values.data));

					section->add(&info, sizeof(info));

					break;
				}
				default:
					assert(false);
				}
			}
		}
	}

	#if BUILD_LINUX
	u32 localSymbolCount = symbols.count();

	for (auto relocation : externalFunctionRelocations) {
		relocation.section->relocations.add({ relocation.offset, ELF64_R_INFO(createSymbolForExternalFunction(relocation.function), R_X86_64_PLT32), -4 });
	}

	for (auto relocation : externalFunctionPointerRelocations) {
		relocation.section->relocations.add({ relocation.offset, ELF64_R_INFO(createSymbolForExternalFunction(relocation.function), R_X86_64_64), 0 });
	}

	#endif
	createEntryPointSymbol();

	{
		PROFILE_ZONE("Write output");

		if (hadError)
			return;
		
		FILE *out = fopen(objectFileName, "wb");
		if (!out) {
			reportError("Error: Could not open %s intermediate for writing", objectFileName);
			return;
		}

		u64 bytesWritten = 0;
		#define doWrite(ptr, size) do {\
			u32 tempSize = (size);\
			fwrite((ptr), tempSize, 1, out);\
			bytesWritten += tempSize;\
			assert(ftell(out) == (s64)bytesWritten);\
		} while (false)
		
		const auto writeAllocator = [&](FILE *out, BucketedArenaAllocator allocator) {
			for (auto bucket = allocator.first; bucket; bucket = bucket->next) {
				u32 count = (bucket->size - bucket->remaining);

				doWrite(bucket->memory - count, count);
			}
		};

#if BUILD_WINDOWS
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
			setSectionName(header.name, sizeof(header.name), toCString(section->name));

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
				section->align(4);
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

					section->relocations.allocator.align(4);

					if (relocationCount > UINT16_MAX) {
						sectionPointer += 10;

						section->relocations.allocator.add2(0); // @Hack We padded to what we though was a multiple of 4 bytes, but since an extra relocation is added at the beginning, 
						                              // we need to add 2 more bytes to ensure alignment
					}

					sectionPointer += section->relocations.allocator.totalSize;
				}
			}
		}
			
		doWrite(&header, sizeof(header));
		doWrite(sectionHeaders.storage, sectionHeaders.count * sizeof(SectionHeader));
			

		assert(ftell(out) == header.pointerToSymbolTable);
		writeAllocator(out, symbols.allocator);

		if (printDiagnostics) {
			reportInfo("%u COFF symbols", symbols.count());
		}

		doWrite(&stringTableSize, sizeof(stringTableSize));
		writeAllocator(out, stringTable);

		u64 alignmentPadding = 0;
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
#else
		Section *sectionHeaderStringTable = makeSection(".shstrtab", 16, SECTION_READ | SECTION_INITIALIZED | SECTION_DISCARD);
		Section *stringTableSection = makeSection(stringTable, ".strtab", 16, SECTION_READ | SECTION_INITIALIZED | SECTION_DISCARD);
		Section *symbolTableSection = makeSection(symbols.allocator, ".symtab", 16, SECTION_READ | SECTION_INITIALIZED | SECTION_DISCARD);

		u32 sectionCount = sections.count;
		for (u32 i = 0; i < sectionCount; i++) {
			if (sections[i]->relocations.count()) {
				makeRelocationSection(sections[i]);
			}
		}


		Elf64_Ehdr elfHeader = {};
		u32 sectionHeaderOffset = AlignPO2(sizeof(elfHeader), alignof(Elf64_Shdr));
		u32 currentSectionOffset = sectionHeaderOffset + sizeof(Elf64_Shdr) * (sections.count + 1);

		elfHeader.e_ident[EI_MAG0] = ELFMAG0;
		elfHeader.e_ident[EI_MAG1] = ELFMAG1;
		elfHeader.e_ident[EI_MAG2] = ELFMAG2;
		elfHeader.e_ident[EI_MAG3] = ELFMAG3;
		elfHeader.e_ident[EI_CLASS] = ELFCLASS64;
		elfHeader.e_ident[EI_DATA] = ELFDATA2LSB;
		elfHeader.e_ident[EI_VERSION] = 1;
		elfHeader.e_ident[EI_OSABI] = ELFOSABI_SYSV;
		elfHeader.e_ident[EI_ABIVERSION] = 0;
		
		elfHeader.e_type = ET_REL;
		elfHeader.e_machine = EM_X86_64;
		elfHeader.e_version = 1;
		elfHeader.e_entry = 0;
		elfHeader.e_phoff = 0;
		elfHeader.e_shoff = sectionHeaderOffset;
		elfHeader.e_ehsize = sizeof(elfHeader);
		elfHeader.e_phentsize = 0;
		elfHeader.e_phnum = 0;
		elfHeader.e_shentsize = sizeof(Elf64_Shdr);
		elfHeader.e_shnum = sections.count + 1;
		elfHeader.e_shstrndx = sectionHeaderStringTable->sectionNumber;

		doWrite(&elfHeader, sizeof(elfHeader));

		u64 alignmentPadding[2] = {};
		doWrite(alignmentPadding, sectionHeaderOffset - bytesWritten);

		sectionHeaderStringTable->addNullTerminatedString("");
		for (auto section : sections) {
			section->sectionNameOffset = sectionHeaderStringTable->totalSize;
			sectionHeaderStringTable->addNullTerminatedString(section->name);
		}

		Elf64_Shdr initialSectionHeader = {};
		doWrite(&initialSectionHeader, sizeof(initialSectionHeader));

		for (auto section : sections) {
			currentSectionOffset = AlignPO2(currentSectionOffset, 16);

			section->offsetInFile = currentSectionOffset;

			Elf64_Shdr header = {};
			header.sh_name = section->sectionNameOffset;
			header.sh_offset = currentSectionOffset;
			header.sh_size = section->totalSize;
			header.sh_addralign = 16;

			if (section->flags & SECTION_INITIALIZED)
				currentSectionOffset += section->totalSize;
			
			if (section->name == ".bss") {
				header.sh_type = SHT_NOBITS;
				header.sh_flags = SHF_ALLOC | SHF_WRITE;
			}
			else if (section->name == ".data") {
				header.sh_type = SHT_PROGBITS;
				header.sh_flags = SHF_ALLOC | SHF_WRITE;
			}
			else if (section->name == ".rodata") {
				header.sh_type = SHT_PROGBITS;
				header.sh_flags = SHF_ALLOC;
			}
			else if (section->name == ".shstrtab") {
				header.sh_type = SHT_STRTAB;
			}
			else if (section->name == ".strtab") {
				header.sh_type = SHT_STRTAB;
			}
			else if (section->name == ".symtab") {
				header.sh_type = SHT_SYMTAB;
				header.sh_entsize = sizeof(Elf64_Sym);
				header.sh_link = stringTableSection->sectionNumber;
				header.sh_info = localSymbolCount;
			}
			else if (section->name == ".text") {
				header.sh_type = SHT_PROGBITS;
				header.sh_flags = SHF_ALLOC | SHF_EXECINSTR;
			}
			else if (section->name.length >= 6 && memcmp(section->name.characters, ".debug", 6) == 0) {
				header.sh_type = SHT_PROGBITS;
			}
			else if (section->name.length >= 5 && memcmp(section->name.characters, ".rela", 5) == 0) {
				header.sh_type = SHT_RELA;
				header.sh_entsize = sizeof(Elf64_Rela);
				header.sh_link = symbolTableSection->sectionNumber;
				header.sh_info = section->relocationsApplyTo;
			}
			else {
				reportError("Internal Compiler Error: Unknown section name %.*s", STRING_PRINTF(section->name));
				fclose(out);
				exit(1);
			}

			doWrite(&header, sizeof(header));
		}
		
		for (auto section : sections) {
			if (!(section->flags & SECTION_INITIALIZED))
				continue;
			
			assert(bytesWritten <= section->offsetInFile);
			doWrite(alignmentPadding, section->offsetInFile - bytesWritten);
			writeAllocator(out, *section);
		}
#endif

		{
			PROFILE_ZONE("fclose");
			fclose(out);
		}
	}
}