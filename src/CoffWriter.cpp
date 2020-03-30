#include "Basic.h"
#include "CoffWriter.h"
#include "BucketedArenaAllocator.h"
#include "Infer.h"
#include "Parser.h"

WorkQueue<CoffJob> coffWriterQueue;

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

	struct {
		u32 tagIndex;
		u32 totalSize;
		u32 pointerToLinenumber;
		u32 pointerToNextFunction;
		u16 unused;
	} functionDefinition;

	struct {
		u32 unused1;
		u16 linenumber;
		char unused2[6];
		u32 pointerToNextFunction;
		u16 unused3;
	} bf;
};

struct Relocation {
	u32 virtualAddress;
	u32 symbolTableIndex;
	u16 type;
};
#pragma pack(pop)

void writeAllocator(FILE *out, BucketedArenaAllocator allocator) {
	for (auto bucket = allocator.first; bucket; bucket = bucket->next) {
		u64 count = (allocator.bucketSize - bucket->remaining);

		fwrite(bucket->memory - count, count, 1, out);
	}
}

u64 getRegisterOffset(ExprFunction *function, u64 regNo) {
	assert(regNo != 0);

	u64 argCount = function->arguments.declarations.count;

	u64 callSpace = 0;

	if (function->state.maxCallRegisters > 4) {
		callSpace = static_cast<u64>(function->state.maxCallRegisters);
	}
	else if (function->state.maxCallRegisters >= 0) {
		callSpace = 4;
	}

	if (regNo > argCount) {
		return (regNo + callSpace - argCount - 1) * 8;
	}

	u64 registerCount = function->state.nextRegister + callSpace - argCount - 1;

	u64 spaceToAllocate = (registerCount >> 1) * 16 + 8;

	return spaceToAllocate + regNo * 8;
}

// eax 01
// ecx 09
// edx 11
// ebx 19
// esp 21
// ebp 29
// esi 31
// edi 39

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

void writeRSPRegisterByte(BucketedArenaAllocator *code, ExprFunction *function, u8 physicalRegister, u64 stackRegister) {
	u64 offset = getRegisterOffset(function, stackRegister);

	if (offset >= 0x80) {
		code->add1(0x84 | (physicalRegister << 3));
		code->add1(0x24);
		code->add4(static_cast<u32>(offset));
	}
	else if (offset != 0) {
		code->add1(0x44 | (physicalRegister << 3));
		code->add1(0x24);
		code->add1(static_cast<u8>(offset));
	}
	else {
		code->add1(0x04 | (physicalRegister << 3));
		code->add1(0x24);
	}
}

void loadIntoIntRegister(BucketedArenaAllocator *code, ExprFunction *function, u64 size, u8 loadInto, u64 regNo) {
	if (regNo == 0) {
		if (loadInto >= 8) {
			code->add1(0x45);
			loadInto -= 8;
		}

		code->add1(0x31);
		code->add1(0xC0 | loadInto | (loadInto << 3));
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
		code->add1(0x66);
	}

	if (rex != 0x40) {
		code->add1(rex);
	}

	if (size == 1) {
		code->add1(0x8a);
	}
	else {
		code->add1(0x8b);
	}

	writeRSPRegisterByte(code, function, loadInto, regNo);
}

void storeFromIntRegister(BucketedArenaAllocator *code, ExprFunction *function, u64 size, u64 regNo, u8 storeFrom) {
	u8 rex = 0x40;

	if (size == 8) {
		rex |= 8;
	}

	if (storeFrom >= 8) {
		rex |= 4;
		storeFrom -= 8;
	}

	if (size == 2) {
		code->add1(0x66);
	}

	if (rex != 0x40) {
		code->add1(rex);
	}

	if (size == 1) {
		code->add1(0x88);
	}
	else {
		code->add1(0x89);
	}

	writeRSPRegisterByte(code, function, storeFrom, regNo);
}

void loadIntoFloatRegister(BucketedArenaAllocator *code, ExprFunction *function, u64 size, u8 loadInto, u64 regNo) {
	if (regNo == 0) {
		if (regNo >= 8) {
			code->add1(0x45);
			regNo -= 8;
		}

		code->add1(0x0F);
		code->add1(0x57);
		code->add1(0xC0 | loadInto | (loadInto << 3));
		return;
	}

	if (size == 8) {
		code->add1(0xF3);
	}
	else {
		code->add1(0xF2);
	}

	if (loadInto >= 8) {
		code->add1(0x44);
		loadInto -= 8;
	}

	code->add1(0x0F);
	code->add1(0x10);

	writeRSPRegisterByte(code, function, loadInto, regNo);
}

void storeFromFloatRegister(BucketedArenaAllocator *code, ExprFunction *function, u64 size, u64 regNo, u8 storeFrom) {
	if (size == 8) {
		code->add1(0xF3);
	}
	else {
		code->add1(0xF2);
	}

	if (storeFrom >= 8) {
		code->add1(0x44);
		storeFrom -= 8;
	}

	code->add1(0x0F);
	code->add1(0x11);

	writeRSPRegisterByte(code, function, storeFrom, regNo);
}

void storeImmediate(BucketedArenaAllocator *code, ExprFunction *function, u64 size, u64 regNo, u64 immediate) {
	if (size == 8 && static_cast<s64>(immediate) != static_cast<s64>(static_cast<s32>(immediate))) {
		code->add1(0x48); // mov rax, ir.a
		code->add1(0xB8);
		code->add8(immediate);

		storeFromIntRegister(code, function, 8, regNo, RAX);
	}
	else {
		if (size == 2) {
			code->add1(0x66);
		}
		else if (size == 8) {
			code->add1(0x48);
		}

		if (size == 1) {
			code->add1(0xC6);
		}
		else {
			code->add1(0xC7);
		}

		writeRSPRegisterByte(code, function, 0, regNo);

		if (size == 1) {
			code->add1(static_cast<u8>(immediate));
		}
		else if (size == 2) {
			code->add2(static_cast<u16>(immediate));
		}
		else if (size == 4 || size == 8) {
			code->add4(static_cast<u32>(immediate));
		}
	}
}

void setCondition(BucketedArenaAllocator *code, ExprFunction *function, u64 dest, u8 condition) {
	code->add1(0x0F);
	code->add1(0x90 | condition);
	writeRSPRegisterByte(code, function, 0, dest);
}

void setConditionInt(BucketedArenaAllocator *code, ExprFunction *function, u64 size, u64 dest, u64 a, u64 b, u8 condition) {
	loadIntoIntRegister(code, function, size, RAX, a);
	loadIntoIntRegister(code, function, size, RCX, b);

	if (size == 8) {
		code->add1(0x48);
	}
	else if (size == 2) {
		code->add1(0x66);
	}

	if (size == 1) {
		code->add1(0x38);
	}
	else {
		code->add1(0x39);
	}

	code->add1(0xC8);

	setCondition(code, function, dest, condition);
}


#define AlignPO2(a, alignment) ((static_cast<u32>(a) + (static_cast<u32>(alignment) - 1)) & ~(static_cast<u32>(alignment) - 1))

void setConditionFloat(BucketedArenaAllocator *code, ExprFunction *function, u64 size, u64 dest, u64 a, u64 b, u8 condition) {
	loadIntoFloatRegister(code, function, size, 0, a);
	loadIntoFloatRegister(code, function, size, 1, b);

	if (size == 8) {
		code->add1(0x66);
	}

	code->add1(0x0F);
	code->add1(0x2F);
	code->add1(0xC1);

	setCondition(code, function, dest, condition);
}

void writeSet(BucketedArenaAllocator *code, ExprFunction *function, u64 size, u64 dest, u64 src) {
	if (src == 0) {
		storeImmediate(code, function, size, dest, 0);
	}
	else {
		loadIntoIntRegister(code, function, size, RAX, src);
		storeFromIntRegister(code, function, size, dest, RAX);
	}
}

#define RDATA_SECTION_NUMBER 1
#define BSS_SECTION_NUMBER 2
#define DATA_SECTION_NUMBER 3
#define TEXT_SECTION_NUMBER 4

u32 *addRelocationToUnkownSymbol(BucketedArenaAllocator *allocator, u32 virtualAddress, u16 type) {
	allocator->add4(virtualAddress);
	u32 *value = allocator->add4(0);
	allocator->add2(type);

	return value;
}

void loadImmediateIntoRAX(BucketedArenaAllocator *code, u64 immediate) {
	if (static_cast<s64>(immediate) != static_cast<s64>(static_cast<s32>(immediate))) {
		code->add1(0x48);
		code->add1(0xB8);
		code->add8(immediate);
	}
	else {
		code->add1(0x48);
		code->add1(0xC7);
		code->add1(0xC0);
		code->add4(static_cast<u32>(immediate));
	}
}

Block externalsBlock;

u32 createSymbolForFunction(BucketArray<Symbol> *symbols, ExprFunction *function) {
	if (!(function->flags & EXPR_FUNCTION_HAS_STORAGE)) {
		function->flags |= EXPR_FUNCTION_HAS_STORAGE;

		if (function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
			if (Declaration *declaration = findDeclaration(&externalsBlock, function->declaration->name)) {
				return static_cast<ExprFunction *>(declaration->initialValue)->physicalStorage;
			}
			else {
				externalsBlock.declarations.add(function->declaration);
			}
		}

		function->physicalStorage = static_cast<u32>(symbols->count());
		function->symbol = reinterpret_cast<Symbol *>(symbols->allocator.allocateUnaligned(sizeof(Symbol)));
	}

	return function->physicalStorage;
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
	u64 opToPatch;
	s32 *location;
	u64 rip;
};

void runCoffWriter() {
	BucketedArenaAllocator code(4096);
	BucketedArenaAllocator codeRelocations(4096);
	BucketedArenaAllocator data(4096);
	BucketedArenaAllocator dataRelocations(4096);
	BucketedArenaAllocator rdata(4096);
	BucketedArenaAllocator stringTable(4096);
	SectionHeader bssSection = {};
	bssSection.virtualSize = 0;
	BucketArray<Symbol> symbols;

	s64 f32ToU64ConstantSymbolIndex = -1;
	s64 f64ToU64ConstantSymbolIndex = -1;

	u64 alignmentPadding = 0;

	Array<u64> instructionOffsets;
	Array<JumpPatch> jumpPatches;

	while (true) {
		CoffJob job = coffWriterQueue.take();

		if (!job.function)
			break;
		if (job.isFunction) {
			auto function = job.function;

			createSymbolForFunction(&symbols, function);

			if (function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
				assert(function->declaration);
				auto symbol = function->symbol;

				setSymbolName(&stringTable, &symbol->name, function->declaration->name);

				symbol->storageClass = IMAGE_SYM_CLASS_EXTERNAL;
				symbol->value = 0;
				symbol->sectionNumber = 0;
				symbol->type = 0x20;


				symbol->numberOfAuxSymbols = 0;
				continue;
			}

			instructionOffsets.clear();

			jumpPatches.clear();

			{
				auto symbol = function->symbol;

				if (function->declaration) {
					setSymbolName(&stringTable, &symbol->name, function->declaration->name);

					if (function->declaration->enclosingScope == &globalBlock) {
						symbol->storageClass = IMAGE_SYM_CLASS_EXTERNAL;
					}
					else {
						symbol->storageClass = IMAGE_SYM_CLASS_STATIC;
					}
				}
				else {
					symbol->name.zeroes = 0;
					symbol->name.namePointer = 4 + static_cast<u32>(stringTable.totalSize);

					char *memory = static_cast<char *>(stringTable.allocateUnaligned(22));

					memcpy(memory, "@func", 5);

					memory += 5;

					u64 value = reinterpret_cast<u64>(function);

					for (u64 i = 0; i < 16; i++) {
						memory[15 - i] = "0123456789ABCDEF"[value & 0xF];
						value >>= 4;
					}

					memory[16] = 0;
					symbol->storageClass = IMAGE_SYM_CLASS_STATIC;
				}

				symbol->value = static_cast<u32>(code.totalSize);
				symbol->sectionNumber = TEXT_SECTION_NUMBER;
				symbol->type = 0x20;


				symbol->numberOfAuxSymbols = 0;
			}

			u64 argCount = function->arguments.declarations.count;

			u64 callSpace = 0;

			if (function->state.maxCallRegisters > 4) {
				callSpace = static_cast<u64>(function->state.maxCallRegisters);
			}
			else if (function->state.maxCallRegisters >= 0) {
				callSpace = 4;
			}

			u64 registerCount = function->state.nextRegister + callSpace - argCount - 1;

			u64 spaceToAllocate = (registerCount >> 1) * 16 + 8;

			constexpr u8 intRegisters[4] = { 0x4C, 0x54, 0x44, 0x4C };

			for (u32 i = 0; i < (argCount > 4 ? 4 : argCount); i++) {
				auto type = static_cast<ExprLiteral *>(function->arguments.declarations[i]->type)->typeValue;

				if (type->flavor == TypeFlavor::FLOAT) {
					if (type->size == 4) {
						code.add2(0xF3);
					}
					else if (type->size == 8) {
						code.add1(0xF2);
					}

					code.add1(0x0F);
					code.add1(0x11);
					code.add1(0x44 | (static_cast<u8>(i) << 3));
				}
				else {
					if (type->size == 2) {
						code.add1(0x66);
					}

					if (type->size == 8 || i >= 2) {
						u8 rex = 0x40;

						if (type->size == 8) rex |= 0x08;
						if (i >= 2) rex |= 0x04;

						code.add1(rex);
					}

					if (type->size == 1) {
						code.add1(0x88);
					}
					else {
						code.add1(0x89);
					}

					code.add1(intRegisters[i]);
				}

				code.add1(0x24);
				code.add1((static_cast<u8>(i) + 1) * 8);
			}

			// sub rsp, spaceToAllocate
			if (spaceToAllocate < 0x80) {
				code.add1(0x48);
				code.add1(0x83);
				code.add1(0xEC);
				code.add1(static_cast<u8>(spaceToAllocate));
			}
			else {
				code.add1(0x48);
				code.add1(0x81);
				code.add1(0xEC);
				code.add4(static_cast<u32>(spaceToAllocate));
			}

			for (u64 index = 0; index < function->state.ir.count; index++) {
				auto &ir = function->state.ir[index];

				instructionOffsets.add(code.totalSize);

				switch (ir.op) {
					case IrOp::ADD: {
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
									code.add1(0xF2);
								}
								else {
									code.add1(0xF3);
								}

								code.add1(0x0F);
								code.add1(0x58);
								writeRSPRegisterByte(&code, function, 0, ir.b);

								storeFromFloatRegister(&code, function, ir.opSize, ir.dest, 0);
							}
							else {
								loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);

								if (ir.opSize == 8) {
									code.add1(0x48);
								}
								else if (ir.opSize == 2) {
									code.add1(0x66);
								}

								if (ir.opSize == 1) {
									code.add1(0x02);
								}
								else {
									code.add1(0x03);
								}

								writeRSPRegisterByte(&code, function, RAX, ir.b);

								storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
							}
						}
					} break;
					case IrOp::ADD_CONSTANT: {
						assert(ir.opSize == 8);

						if (ir.a == 0) {
							storeImmediate(&code, function, 8, ir.dest, ir.b);
						}
						else if (ir.b == 0) {
							writeSet(&code, function, 8, ir.dest, ir.a);
						}
						else {
							loadImmediateIntoRAX(&code, ir.b);

							code.add1(0x48);
							code.add1(0x03);
							writeRSPRegisterByte(&code, function, RAX, ir.a);

							storeFromIntRegister(&code, function, 8, ir.dest, RAX);
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
									code.add1(0xF2);
								}
								else {
									code.add1(0xF3);
								}

								code.add1(0x0F);
								code.add1(0x5C);

								writeRSPRegisterByte(&code, function, 0, ir.b);

								storeFromFloatRegister(&code, function, ir.opSize, ir.dest, 0);
							}
							else {
								loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);

								if (ir.opSize == 8) {
									code.add1(0x48);
								}
								else if (ir.opSize == 2) {
									code.add1(0x66);
								}

								if (ir.opSize == 1) {
									code.add1(0x2A);
								}
								else {
									code.add1(0x2B);
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
									code.add1(0xF2);
								}
								else {
									code.add1(0xF3);
								}

								code.add1(0x0F);
								code.add1(0x59);

								writeRSPRegisterByte(&code, function, 0, ir.b);

								storeFromFloatRegister(&code, function, ir.opSize, ir.dest, 0);
							}
							else {
								if (ir.opSize == 1) {
									loadIntoIntRegister(&code, function, 1, RAX, ir.a);

									code.add1(0xF6);
									writeRSPRegisterByte(&code, function, 5, ir.b);

									storeFromIntRegister(&code, function, 1, ir.dest, RAX);
								}
								else {
									loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);

									if (ir.opSize == 8) {
										code.add1(0x48);
									}
									else if (ir.opSize == 2) {
										code.add1(0x66);
									}

									code.add1(0x0F);
									code.add1(0xAF);
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
							loadImmediateIntoRAX(&code, ir.b);

							code.add1(0x48);
							code.add1(0x0F);
							code.add1(0xAF);
							writeRSPRegisterByte(&code, function, RAX, ir.a);

							storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
						}
					} break;
					case IrOp::DIV: {
						if (ir.flags & IR_FLOAT_OP) {
							loadIntoFloatRegister(&code, function, ir.opSize, 0, ir.a);
							loadIntoFloatRegister(&code, function, ir.opSize, 1, ir.b);

							if (ir.opSize == 8) {
								code.add1(0xF2);
							}
							else {
								code.add1(0xF3);
							}
							code.add1(0x0F);
							code.add1(0x5E);
							code.add1(0xC1);

							storeFromFloatRegister(&code, function, ir.opSize, ir.dest, 0);
						}
						else {
							loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);
							loadIntoIntRegister(&code, function, ir.opSize, RCX, ir.b);

							if (ir.flags & IR_SIGNED_OP) {
								if (ir.opSize == 1) {
									code.add1(0x66); // cbw
									code.add1(0x98);

									code.add1(0xF6); // idiv cl
									code.add1(0xF9);

								}
								else {
									if (ir.opSize == 2) {
										code.add1(0x66);
									}
									else if (ir.opSize == 8) {
										code.add1(0x48);
									}

									code.add1(0x99);

									if (ir.opSize == 2) {
										code.add1(0x66);
									}
									else if (ir.opSize == 8) {
										code.add1(0x48);
									}
									code.add1(0xF7);
									code.add1(0xF9);
								}
							}
							else {
								if (ir.opSize == 1) {
									code.add1(0x66); // movzx ax, al
									code.add1(0x0F);
									code.add1(0xB6);
									code.add1(0xC0);

									code.add1(0xF6); // div cl
									code.add1(0xF1);

								}
								else {
									if (ir.opSize == 2) {
										code.add1(0x66);
									}
									else if (ir.opSize == 8) {
										code.add1(0x48);
									}

									code.add1(0x31);
									code.add1(0xD2);

									if (ir.opSize == 2) {
										code.add1(0x66);
									}
									else if (ir.opSize == 8) {
										code.add1(0x48);
									}
									code.add1(0xF7);
									code.add1(0xF1);
								}
							}

							storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
						}
					} break;
					case IrOp::DIVIDE_BY_CONSTANT: {
						loadImmediateIntoRAX(&code, ir.b);

						code.add1(0x48); // mov rcx, rax
						code.add1(0x89);
						code.add1(0xC1);

						loadIntoIntRegister(&code, function, 8, RAX, ir.a);

						code.add1(0x48); // cqo
						code.add1(0x99);

						code.add1(0x48); // div rcx
						code.add1(0xF7);
						code.add1(0xF1);

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
									code.add1(0x66); // cbw
									code.add1(0x98);

									code.add1(0xF6); // idiv cl
									code.add1(0xF9);

								}
								else {
									if (ir.opSize == 2) {
										code.add1(0x66);
									}
									else if (ir.opSize == 8) {
										code.add1(0x48);
									}

									code.add1(0x99);

									if (ir.opSize == 2) {
										code.add1(0x66);
									}
									else if (ir.opSize == 8) {
										code.add1(0x48);
									}
									code.add1(0xF7);
									code.add1(0xF9);
								}
							}
							else {
								if (ir.opSize == 1) {
									code.add1(0x66); // movzx ax, al
									code.add1(0x0F);
									code.add1(0xB6);
									code.add1(0xC0);

									code.add1(0xF6); // div cl
									code.add1(0xF1);

								}
								else {
									if (ir.opSize == 2) {
										code.add1(0x66);
									}
									else if (ir.opSize == 8) {
										code.add1(0x48);
									}

									code.add1(0x31);
									code.add1(0xD2);

									if (ir.opSize == 2) {
										code.add1(0x66);
									}
									else if (ir.opSize == 8) {
										code.add1(0x48);
									}
									code.add1(0xF7);
									code.add1(0xF1);
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
								code.add1(0x48);
							}
							else if (ir.opSize == 2) {
								code.add1(0x66);
							}

							if (ir.opSize == 1) {
								code.add1(0x22);
							}
							else {
								code.add1(0x23);
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
								code.add1(0x48);
							}
							else if (ir.opSize == 2) {
								code.add1(0x66);
							}

							if (ir.opSize == 1) {
								code.add1(0x0A);
							}
							else {
								code.add1(0x0B);
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
								code.add1(0x48);
							}
							else if (ir.opSize == 2) {
								code.add1(0x66);
							}

							if (ir.opSize == 1) {
								code.add1(0x32);
							}
							else {
								code.add1(0x33);
							}
							writeRSPRegisterByte(&code, function, RAX, ir.b);

							storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
						}
					} break;
					case IrOp::NOT: {
						loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);
						if (ir.opSize == 8) {
							code.add1(0x48);
						}
						else if (ir.opSize == 2) {
							code.add1(0x66);
						}

						if (ir.opSize == 1) {
							code.add1(0xF6);
						}
						else {
							code.add1(0xF7);
						}

						code.add1(0xD0);

						storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
					} break;
					case IrOp::SHIFT_LEFT: {
						loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);
						loadIntoIntRegister(&code, function, ir.opSize, RCX, ir.b);

						if (ir.opSize == 8) {
							code.add1(0x48);
						}
						else if (ir.opSize == 2) {
							code.add1(0x66);
						}

						if (ir.opSize == 1) {
							code.add1(0xD2);
						}
						else {
							code.add1(0xD3);
						}

						code.add1(0xE0);

						storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
					} break;
					case IrOp::SHIFT_RIGHT: {

						loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);
						loadIntoIntRegister(&code, function, ir.opSize, RCX, ir.b);

						if (ir.opSize == 8) {
							code.add1(0x48);
						}
						else if (ir.opSize == 2) {
							code.add1(0x66);
						}

						if (ir.opSize == 1) {
							code.add1(0xD2);
						}
						else {
							code.add1(0xD3);
						}

						if (ir.flags & IR_SIGNED_OP) {
							code.add1(0xF8);
						}
						else {
							code.add1(0xE8);
						}

						storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
					} break;
					case IrOp::READ: {
						assert(ir.opSize == 8);

						loadIntoIntRegister(&code, function, 8, RAX, ir.a);

						if (ir.destSize == 8) {
							code.add1(0x48);
						}
						else if (ir.destSize == 2) {
							code.add1(0x66);
						}

						if (ir.destSize == 1) {
							code.add1(0x8A);
						}
						else {
							code.add1(0x8B);
						}

						code.add1(0x00);

						storeFromIntRegister(&code, function, ir.destSize, ir.dest, RAX);
					} break;
					case IrOp::WRITE: {
						loadIntoIntRegister(&code, function, 8, RAX, ir.a);
						loadIntoIntRegister(&code, function, ir.opSize, RCX, ir.b);

						if (ir.opSize == 1) {
							code.add1(0x88);
						}
						else {
							code.add1(0x89);
						}

						code.add1(0x08);
					} break;
					case IrOp::SET: {
						if (ir.opSize == ir.destSize || ir.a == 0) {
							writeSet(&code, function, ir.destSize, ir.dest, ir.a);
						}
						else {
							if (ir.flags & IR_FLOAT_OP) {
								if (ir.opSize == 8) {
									assert(ir.destSize == 4);

									code.add1(0xF2);
								}
								else {
									assert(ir.opSize == 4);
									assert(ir.destSize == 8);

									code.add1(0xF3);
								}

								code.add1(0x0F);
								code.add1(0x5A);

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
												code.add1(0x66);
											}
											else if (ir.destSize == 8) {
												code.add1(0x48);
											}

											code.add1(0x0F);
											code.add1(0xBE);
											writeRSPRegisterByte(&code, function, RAX, ir.a);
										}
										else if (ir.opSize == 2) {
											if (ir.destSize == 8) {
												code.add1(0x48);
											}

											code.add1(0x0F);
											code.add1(0xBF);
											writeRSPRegisterByte(&code, function, RAX, ir.a);
										}
										else if (ir.opSize == 4) {
											code.add1(0x48);
											code.add1(0x63);
											writeRSPRegisterByte(&code, function, RAX, ir.a);
										}
									}
									else {
										if (ir.opSize == 1) {
											if (ir.destSize == 2) {
												code.add1(0x66);
											}

											code.add1(0x0F);
											code.add1(0xB6);
											writeRSPRegisterByte(&code, function, RAX, ir.a);


										}
										else if (ir.opSize == 2) {
											code.add1(0x0F);
											code.add1(0xB7);
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
						code.add1(0xE9);
						
						JumpPatch patch;
						patch.opToPatch = ir.b;
						patch.location = reinterpret_cast<s32 *>(code.add4(0));
						patch.rip = code.totalSize;
						
						jumpPatches.add(patch);
					} break;
					case IrOp::IF_Z_GOTO: {
						loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);
						
						if (ir.opSize == 8) {
							code.add1(0x48);
						}
						else if (ir.opSize == 2) {
							code.add1(0x66);
						}

						if (ir.opSize == 1) {
							code.add1(0x84);
						}
						else {
							code.add1(0x85);
						}
						code.add1(0xC0);

						code.add1(0x0F);
						code.add1(0x80 | C_Z);

						JumpPatch patch;
						patch.opToPatch = ir.b;
						patch.location = reinterpret_cast<s32 *>(code.add4(0));
						patch.rip = code.totalSize;

						jumpPatches.add(patch);
					} break;
					case IrOp::IF_NZ_GOTO: {
						loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);

						if (ir.opSize == 8) {
							code.add1(0x48);
						}
						else if (ir.opSize == 2) {
							code.add1(0x66);
						}

						if (ir.opSize == 1) {
							code.add1(0x84);
						}
						else {
							code.add1(0x85);
						}
						code.add1(0xC0);

						code.add1(0x0F);
						code.add1(0x80 | C_NZ);

						JumpPatch patch;
						patch.opToPatch = ir.b;
						patch.location = reinterpret_cast<s32 *>(code.add4(0));
						patch.rip = code.totalSize;

						jumpPatches.add(patch);
					} break;
					case IrOp::LESS: {
						if (ir.flags & IR_FLOAT_OP) {
							setConditionFloat(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_L);
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
							setConditionFloat(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_G);
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
							setConditionFloat(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_LE);
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
							setConditionFloat(&code, function, ir.opSize, ir.dest, ir.a, ir.b, C_GE);
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
						assert(ir.declaration->enclosingScope == &globalBlock);
						assert(!(ir.declaration->flags & DECLARATION_IS_CONSTANT));

						code.add1(0x48);
						code.add1(0x8D);
						code.add1(0x05);
						
						codeRelocations.add4(static_cast<u32>(code.totalSize));
						codeRelocations.add4(createSymbolForDeclaration(&symbols, ir.declaration));
						codeRelocations.add2(IMAGE_REL_AMD64_REL32);

						code.add4(0);

						storeFromIntRegister(&code, function, 8, ir.dest, RAX);
					} break;
					case IrOp::ADDRESS_OF_LOCAL: {
						code.add1(0x48);
						code.add1(0x8D);

						writeRSPRegisterByte(&code, function, RAX, ir.a);
						storeFromIntRegister(&code, function, 8, ir.dest, RAX);
					} break;
					case IrOp::IMMEDIATE: {
						storeImmediate(&code, function, ir.opSize, ir.dest, ir.a);
					} break;
					case IrOp::FLOAT_TO_INT: {
						if (ir.a == 0) {
							writeSet(&code, function, ir.destSize, ir.dest, 0);
							break;
						}

						if (ir.flags & IR_SIGNED_OP) {
							if (ir.opSize == 8) {
								code.add1(0xF2);
							}
							else {
								code.add1(0xF3);
							}

							if (ir.destSize == 8) {
								code.add1(0x48);
							}

							code.add1(0x2C);
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

									code.add1(0xF2); // movsd xmm1, f32ToU64Constant
									code.add1(0x0F);
									code.add1(0x10);
									code.add1(0x0D);

									codeRelocations.add4(static_cast<u32>(code.totalSize));
									codeRelocations.add4(static_cast<u32>(f64ToU64ConstantSymbolIndex));
									codeRelocations.add2(IMAGE_REL_AMD64_REL32);

									code.add4(0);

									code.add1(0x31); // xor eax, eax
									code.add1(0xC0);

									code.add1(0x66); // comisd xmm0, xmm1
									code.add1(0x0F);
									code.add1(0x2F);
									code.add1(0xC1);

									code.add1(0x70 | C_B); // jb .cvt
									u8 *firstJumpPatch = code.add1(0);
									u64 firstJumpRel = code.totalSize;

									code.add1(0xF2); // subsd xmm0, xmm1
									code.add1(0x0F);
									code.add1(0x5C);
									code.add1(0xC1);

									code.add1(0x66); // comisd xmm0, xmm1
									code.add1(0x0F);
									code.add1(0x2F);
									code.add1(0xC1);

									code.add1(0x70 | C_AE); // jae .cvt
									u8 *secondJumpPatch = code.add1(0);
									u64 secondJumpRel = code.totalSize;

									code.add1(0x48); // mov rax, 0x8000'0000'0000'0000
									code.add1(0xB8);
									code.add8(0x8000'0000'0000'0000);

									*firstJumpPatch = static_cast<u8>(code.totalSize - firstJumpRel);
									*secondJumpPatch = static_cast<u8>(code.totalSize - secondJumpRel);

									// .cvt
									code.add1(0xF2); // cvttsd2si rcx, xmm0
									code.add1(0x48);
									code.add1(0x0F);
									code.add1(0x2C);
									code.add1(0xC8);

									code.add1(0x48); // add rax, rcx
									code.add1(0x01);
									code.add1(0xC8);
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

									code.add1(0xF3); // movss xmm1, f32ToU64Constant
									code.add1(0x0F);
									code.add1(0x10);
									code.add1(0x0D);

									codeRelocations.add4(code.totalSize);
									codeRelocations.add4(f32ToU64ConstantSymbolIndex);
									codeRelocations.add2(IMAGE_REL_AMD64_REL32);

									code.add4(0);

									code.add1(0x31); // xor eax, eax
									code.add1(0xC0);

									code.add1(0x0F); // comiss xmm0, xmm1
									code.add1(0x2F);
									code.add1(0xC1);

									code.add1(0x70 | C_B); // jb .cvt
									u8 *firstJumpPatch = code.add1(0);
									u64 firstJumpRel = code.totalSize;

									code.add1(0xF3); // subss xmm0, xmm1
									code.add1(0x0F);
									code.add1(0x5C);
									code.add1(0xC1);

									code.add1(0x0F); // comiss xmm0, xmm1
									code.add1(0x2F);
									code.add1(0xC1);

									code.add1(0x70 | C_AE); // jae .cvt
									u8 *secondJumpPatch = code.add1(0);
									u64 secondJumpRel = code.totalSize;

									code.add1(0x48); // mov rax, 0x8000'0000'0000'0000
									code.add1(0xB8);
									code.add8(0x8000'0000'0000'0000);

									*firstJumpPatch = static_cast<u8>(code.totalSize - firstJumpRel);
									*secondJumpPatch = static_cast<u8>(code.totalSize - secondJumpRel);

									// .cvt
									code.add1(0xF3); // cvttss2si rcx, xmm0
									code.add1(0x48);
									code.add1(0x0F);
									code.add1(0x2C);
									code.add1(0xC8);

									code.add1(0x48); // add rax, rcx
									code.add1(0x01);
									code.add1(0xC8);
								}

								storeFromIntRegister(&code, function, ir.destSize, ir.dest, RAX);
							}

							else {
								if (ir.opSize == 8) {
									code.add1(0xF2);
								}
								else {
									code.add1(0xF3);
								}

								if (ir.destSize == 4) {
									code.add1(0x48);
								}


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
									code.add1(0xF2);
								}
								else {
									code.add1(0xF3);
								}

								if (ir.opSize == 8) {
									code.add1(0x48);
								}

								code.add1(0x0F);
								code.add1(0x2A);
								writeRSPRegisterByte(&code, function, 0, ir.a);
								storeFromFloatRegister(&code, function, ir.destSize, ir.dest, 0);
							}
							else {
								code.add1(0x0F);
								if (ir.opSize == 2) {
									code.add1(0xBF);
								}
								else {
									code.add1(0xBE);
								}
								writeRSPRegisterByte(&code, function, RAX, ir.a);

								if (ir.destSize == 8) {
									code.add1(0xF2);
								}
								else {
									code.add1(0xF3);
								}

								code.add1(0x0F);
								code.add1(0x2A);
								code.add1(0xC0);

								storeFromFloatRegister(&code, function, ir.destSize, ir.dest, 0);
							}
						}
						else {
							if (ir.opSize == 8) { // Aww sheet
								loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);

								code.add1(0x48); // test rax, rax
								code.add1(0x85);
								code.add1(0xC0);

								code.add1(0x70 | C_S); // js .large

								u8 *firstJumpPatch = code.add1(0);
								u64 firstJumpRel = code.totalSize;

								code.add1(0x0F);

								if (ir.destSize == 8) {
									code.add1(0xF2);
								}
								else {
									code.add1(0xF3);
								}

								code.add1(0x2a);
								code.add1(0xC0);

								code.add1(0xEB); // jmp .done

								u8 *secondJumpPatch = code.add1(0);
								u64 secondJumpRel = code.totalSize;


								*firstJumpPatch = code.totalSize - firstJumpRel;

								// .large
								code.add1(0x48); // mov rcx, rax
								code.add1(0x89);
								code.add1(0xC1);

								code.add1(0x83); // and ecx, 1
								code.add1(0E1);
								code.add1(0x01);

								code.add1(0x48); // shr rax, 1
								code.add1(0xD1);
								code.add1(0xE8);

								code.add1(0x48); // or rax, rcx
								code.add1(0x09);
								code.add1(0xC8);

								code.add1(0x0F);

								if (ir.destSize == 8) {
									code.add1(0xF2);
								}
								else {
									code.add1(0xF3);
								}

								code.add1(0x2a);
								code.add1(0xC0);

								if (ir.destSize == 8) {
									code.add1(0xF2);
								}
								else {
									code.add1(0xF3);
								}

								code.add1(0x0F);
								code.add1(0x58);
								code.add1(0xC0);

								*secondJumpPatch = code.totalSize - secondJumpRel;

								// .done
								storeFromFloatRegister(&code, function, ir.destSize, ir.dest, 0);
							}
							else if (ir.opSize == 4) {
								if (ir.destSize == 8) {
									code.add1(0xF2);
								}
								else {
									code.add1(0xF3);
								}

								code.add1(0x48);

								code.add1(0x0F);
								code.add1(0x2A);
								writeRSPRegisterByte(&code, function, 0, ir.a);
								storeFromFloatRegister(&code, function, ir.destSize, ir.dest, 0);
							}
							else {
								code.add1(0x0F);
								if (ir.opSize == 2) {
									code.add1(0xB7);
								}
								else {
									code.add1(0xB6);
								}
								writeRSPRegisterByte(&code, function, RAX, ir.a);

								if (ir.destSize == 8) {
									code.add1(0xF2);
								}
								else {
									code.add1(0xF3);
								}

								code.add1(0x0F);
								code.add1(0x2A);
								code.add1(0xC0);

								storeFromFloatRegister(&code, function, ir.destSize, ir.dest, 0);
							}
						}
					} break;
					case IrOp::RETURN: {
						if (ir.a) {
							if (ir.flags & IR_FLOAT_OP) {
								loadIntoFloatRegister(&code, function, ir.opSize, 0, ir.a);
							}
							else {
								loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);
							}
						}

						// add rsp, spaceToAllocate
						if (spaceToAllocate < 0x80) {
							code.add1(0x48);
							code.add1(0x83);
							code.add1(0xC4);
							code.add1(static_cast<u8>(spaceToAllocate));
						}
						else {
							code.add1(0x48);
							code.add1(0x81);
							code.add1(0xC4);
							code.add4(static_cast<u32>(spaceToAllocate));
						}

						code.add1(0xC3);
					} break;
					case IrOp::CALL: {
						constexpr int intRegisters[4] = { RCX, RDX, 8, 9 };

						for (u8 i = 0; i < (ir.arguments->argCount > 4 ? 4 : ir.arguments->argCount); i++) {
							auto type = ir.arguments->args[i].type;

							if (type->flavor == TypeFlavor::FLOAT) {
								loadIntoFloatRegister(&code, function, type->size, i, ir.arguments->args[i].number);
							}
							else {
								loadIntoIntRegister(&code, function, type->size, intRegisters[i], ir.arguments->args[i].number);
							}
						}

						for (u32 i = 4; i < ir.arguments->argCount; i++) {
							loadIntoIntRegister(&code, function, ir.arguments->args[i].type->size, RAX, ir.arguments->args[i].number);

							code.add1(0x48);
							code.add1(0x89);
							code.add1(0x84);
							code.add1(0x24);
							code.add4(4 * i);
						}

						code.add1(0xFF);
						writeRSPRegisterByte(&code, function, 2, ir.a);

						if (ir.dest != 0) {
							if (ir.arguments->returnType->flavor == TypeFlavor::FLOAT) {
								storeFromFloatRegister(&code, function, ir.arguments->returnType->size, ir.dest, 0);
							}
							else {
								storeFromIntRegister(&code, function, ir.arguments->returnType->size, ir.dest, RAX);
							}
						}
					} break;
					case IrOp::NEG: {
						if (ir.a == 0) {
							storeImmediate(&code, function, ir.opSize, ir.dest, 0);
						}
						else if (ir.flags & IR_FLOAT_OP) {
							code.add1(0x0F); // xorps xmm0, xmm0
							code.add1(0x57);
							code.add1(0xC0);

							if (ir.opSize == 8) {
								code.add1(0xF2);
							}
							else {
								code.add1(0xF3);
							}

							code.add1(0x0F);
							code.add1(0x5C);
							writeRSPRegisterByte(&code, function, 0, ir.a);

							storeFromFloatRegister(&code, function, ir.opSize, ir.dest, 0);
						}
						else {
							loadIntoIntRegister(&code, function, ir.opSize, RAX, ir.a);
							if (ir.opSize == 8) {
								code.add1(0x48);
							}
							else if (ir.opSize == 2) {
								code.add1(0x66);
							}

							if (ir.opSize == 1) {
								code.add1(0xF6);
							}
							else {
								code.add1(0xF7);
							}

							code.add1(0xD8);

							storeFromIntRegister(&code, function, ir.opSize, ir.dest, RAX);
						}
					} break;
					case IrOp::NOOP: {
						// we are done
					} break;
					case IrOp::FUNCTION: {
						code.add1(0x48);
						code.add1(0x8D);
						code.add1(0x05);

						codeRelocations.add4(code.totalSize);
						codeRelocations.add4(createSymbolForFunction(&symbols, ir.function));
						codeRelocations.add2(IMAGE_REL_AMD64_REL32);

						code.add4(0);

						storeFromIntRegister(&code, function, 8, ir.dest, RAX);
					} break;
				}
			}

			for (auto patch : jumpPatches) {
				*patch.location = static_cast<s32>(static_cast<s64>(instructionOffsets[patch.opToPatch]) - static_cast<s64>(patch.rip));
			}

			function->state.allocator.free();
			function->state.loopStack.free();
			function->state.ir.free();
		}
		else {
			auto declaration = job.declaration;

			assert(declaration->enclosingScope == &globalBlock);
			assert(!(declaration->flags & DECLARATION_IS_CONSTANT));

			createSymbolForDeclaration(&symbols, declaration);

			auto symbol = declaration->symbol;
			auto type = static_cast<ExprLiteral *>(declaration->type)->typeValue;

			setSymbolName(&stringTable, &symbol->name, declaration->name);

			symbol->storageClass = IMAGE_SYM_CLASS_EXTERNAL;
			symbol->type = 0;

			if (declaration->flags & DECLARATION_IS_UNINITIALIZED) {
				bssSection.virtualSize = AlignPO2(bssSection.virtualSize, type->alignment);

				symbol->value = bssSection.virtualSize;
				symbol->sectionNumber = BSS_SECTION_NUMBER;

				bssSection.virtualSize += type->size;
			}
			else {
				data.allocateUnaligned(AlignPO2(data.totalSize, type->alignment) - data.totalSize);

				symbol->value = data.totalSize;
				symbol->sectionNumber = DATA_SECTION_NUMBER;

				assert(declaration->initialValue->flavor == ExprFlavor::FLOAT_LITERAL ||
					declaration->initialValue->flavor == ExprFlavor::INT_LITERAL ||
					declaration->initialValue->flavor == ExprFlavor::FUNCTION);

				if (declaration->initialValue->flavor == ExprFlavor::FUNCTION) {
					assert(type->size == 8);
					dataRelocations.add4(data.totalSize);
					dataRelocations.add4(createSymbolForFunction(&symbols, static_cast<ExprFunction *>(declaration->initialValue)));
					dataRelocations.add2(IMAGE_REL_AMD64_ADDR64);

					data.add8(0);
				}
				else {
					data.add(&static_cast<ExprLiteral *>(declaration->initialValue)->unsignedValue, type->size);
				}
			}

			symbol->numberOfAuxSymbols = 0;
		}
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
	rdataSection.characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_ALIGN_4096BYTES;

	setSectionName(bssSection.name, sizeof(bssSection.name), ".bss");
	bssSection.characteristics = IMAGE_SCN_CNT_UNINITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE | IMAGE_SCN_ALIGN_4096BYTES;

	SectionHeader dataSection = {};
	setSectionName(dataSection.name, sizeof(dataSection.name), ".data");
	dataSection.characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE | IMAGE_SCN_ALIGN_4096BYTES;

	SectionHeader textSection = {};
	setSectionName(textSection.name, sizeof(textSection.name), ".text");
	textSection.characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_ALIGN_4096BYTES;

	sections.add({ &rdataSection, &rdata });
	sections.add({ &bssSection });
	sections.add({ &dataSection, &data, &dataRelocations });
	sections.add({ &textSection, &code, &codeRelocations });



	FileHeader header = {};
	header.machine = IMAGE_FILE_MACHINE_AMD64;
	header.numberOfSections = sections.count;
	header.timestamp = (DWORD) time(0);
	header.pointerToSymbolTable = sizeof(FileHeader) + sizeof(SectionHeader) * sections.count;
	header.numberOfSymbols = symbols.count();
	header.sizeOfOptionalHeader = 0;
	header.characteristics = 0;

	u32 prefixSize = header.pointerToSymbolTable + sizeof(Symbol) * symbols.count() + stringTableSize;


	{
		u32 sectionPointer = AlignPO2(prefixSize, 4);

		for (auto section : sections) {
			section.header->virtualAddress = 0;

			if (section.data) {
				section.header->virtualSize = section.data->totalSize;

				section.header->sizeOfRawData = section.data->totalSize;
				section.header->pointerToRawData = sectionPointer;

				sectionPointer += AlignPO2(section.header->sizeOfRawData, 4);

				if (section.relocations) {
					section.header->pointerToRelocations = AlignPO2(sectionPointer, 4);
					section.header->numberOfRelocations = section.relocations->totalSize / sizeof(Relocation);
					sectionPointer += AlignPO2(section.relocations->totalSize, 4);
				}
				else {
					section.header->pointerToRelocations = 0;
					section.header->numberOfRelocations = 0;
				}
			}
			else {
				section.header->sizeOfRawData = 0;
				section.header->pointerToRawData = 0;
				section.header->pointerToRelocations = 0;
				section.header->numberOfRelocations = 0;
			}


			section.header->pointerToLinenumbers = 0;
			section.header->numberOfLinenumbers = 0;
		}
	}
	FILE *out = fopen("out.obj", "wb");

	fwrite(&header, sizeof(header), 1, out);

	for (auto section : sections) {
		fwrite(section.header, sizeof(SectionHeader), 1, out);
	}


	assert(ftell(out) == header.pointerToSymbolTable);
	writeAllocator(out, symbols.allocator);

	fwrite(&stringTableSize, sizeof(stringTableSize), 1, out);
	writeAllocator(out, stringTable);

	fwrite(&alignmentPadding, AlignPO2(prefixSize, 4) - prefixSize, 1, out);

	for (u64 i = 0; i < sections.count; i++) {
		auto section = sections[i];

		if (section.data) {
			assert(section.header->pointerToRawData == ftell(out));
			writeAllocator(out, *section.data);

			fwrite(&alignmentPadding, AlignPO2(section.data->totalSize, 4) - section.data->totalSize, 1, out);
		}

		if (section.relocations) {
			assert(section.header->pointerToRelocations == ftell(out));

			writeAllocator(out, *section.relocations);

			fwrite(&alignmentPadding, AlignPO2(section.relocations->totalSize, 4) - section.relocations->totalSize, 1, out);
		}
	}

	fclose(out);
}