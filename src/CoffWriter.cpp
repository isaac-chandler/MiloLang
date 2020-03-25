#include "Basic.h"
#include "CoffWriter.h"
#include "BucketedArenaAllocator.h"
#include "Infer.h"

WorkQueue<ExprFunction *> coffWriterQueue;

union SymbolName {
	char name[8];
	struct {
		u32 unused;
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
		memset(header->name + name.length , 0, sizeof(header->name) - name.length);
	}
	else {
		header->unused = 0;
		header->namePointer = stringTable->totalSize + 4;
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
#pragma pack(pop)

void writeAllocator(FILE *out, BucketedArenaAllocator allocator) {
	for (auto bucket = allocator.first; bucket; bucket = bucket->next) {
		u64 count = (allocator.bucketSize - bucket->remaining);

		fwrite(bucket->memory - count, count, 1, out);
	}
}

u64 getRegisterOffset(ExprFunction *function, u64 regNo) {
	u64 argCount = function->arguments.declarations.count;


	if (regNo > argCount) {
		return (regNo - argCount - 1) * 8;
	}

	u64 registerCount = function->state.nextRegister - argCount - 1;

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

void loadIntoIntRegister(BucketedArenaAllocator *code, ExprFunction *function, u8 size, u8 loadInto, u64 regNo) {
	u64 rex = 0x40;

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

	u64 offset = getRegisterOffset(function, regNo);

	if (offset >= 0x80) {
		code->add1(0x84 | (loadInto << 3));
		code->add1(0x24);
		code->add4(static_cast<u32>(offset));
	}
	else {
		code->add1(0x44 | (loadInto << 3));
		code->add1(0x24);
		code->add1(static_cast<u8>(offset));
	}
	
}

void storeFromIntRegister(BucketedArenaAllocator *code, ExprFunction *function, u8 size, u64 regNo, u8 storeFrom) {
	u64 rex = 0x40;

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

	u64 offset = getRegisterOffset(function, regNo);

	if (offset >= 0x80) {
		code->add1(0x84 | (storeFrom << 3));
		code->add1(0x24);
		code->add4(static_cast<u32>(offset));
	}
	else {
		code->add1(0x44 | (storeFrom << 3));
		code->add1(0x24);
		code->add1(static_cast<u8>(offset));
	}
}

void loadIntoFloatRegister(BucketedArenaAllocator *code, ExprFunction *function, u8 size, u8 loadInto, u64 regNo) {
	if (size == 8) {
		code->add1(0xF3);
	}
	else {
		code->add1(0xF2);
	}

	u64 rex = 0x40;

	if (loadInto >= 8) {
		rex |= 4;
		loadInto -= 8;
	}

	if (rex != 0x40) {
		code->add1(rex);
	}

	code->add1(0x0F);
	code->add1(0x10);

	u64 offset = getRegisterOffset(function, regNo);

	if (offset >= 0x80) {
		code->add1(0x84 | (loadInto << 3));
		code->add1(0x24);
		code->add4(static_cast<u32>(offset));
	}
	else {
		code->add1(0x44 | (loadInto << 3));
		code->add1(0x24);
		code->add1(static_cast<u8>(offset));
	}

}

void storeFromFloatRegister(BucketedArenaAllocator *code, ExprFunction *function, u8 size, u64 regNo, u8 storeFrom) {
	if (size == 8) {
		code->add1(0xF3);
	}
	else {
		code->add1(0xF2);
	}

	u64 rex = 0x40;

	if (storeFrom >= 8) {
		rex |= 4;
		storeFrom -= 8;
	}

	if (rex != 0x40) {
		code->add1(rex);
	}

	code->add1(0x0F);
	code->add1(0x11);

	u64 offset = getRegisterOffset(function, regNo);

	if (offset >= 0x80) {
		code->add1(0x84 | (storeFrom << 3));
		code->add1(0x24);
		code->add4(static_cast<u32>(offset));
	}
	else {
		code->add1(0x44 | (storeFrom << 3));
		code->add1(0x24);
		code->add1(static_cast<u8>(offset));
	}

}

void storeImmediate(BucketedArenaAllocator *code, ExprFunction *function, u8 size, u64 regNo, u64 immediate) {
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

	u64 offset = getRegisterOffset(function, regNo);

	if (offset == 0) {
		code->add1(0x04);
		code->add1(0x24);
	}
	else if (offset >= 0x80) {
		code->add1(0x84);
		code->add1(0x24);
		code->add4(static_cast<u32>(offset));
	}
	else {
		code->add1(0x44);
		code->add1(0x24);
		code->add1(static_cast<u8>(offset));
	}

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

void runCoffWriter() {
	FILE *out = fopen("out.obj", "wb");

	BucketedArenaAllocator code(4096);
	BucketedArenaAllocator data(4096);
	BucketedArenaAllocator stringTable(4096);
	Array<Symbol> symbols;

	while (true) {
		ExprFunction *function = coffWriterQueue.take();

		if (!function)
			break;

		u64 start = code.totalSize;

		u64 argCount = function->arguments.declarations.count;

		u64 registerCount = function->state.nextRegister - argCount - 1;

		u64 spaceToAllocate = (registerCount >> 1) * 16 + 8;

		constexpr u8 intRegisters[4] = { 0x4C, 0x54, 0x44, 0x4C };

		for (u32 i = 0; i < (argCount > 4 ? 4 : argCount); i++) {
			auto type = static_cast<ExprLiteral *>(function->arguments.declarations[i]->type)->type;

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
					u64 rex = 0x40;

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

		for (auto &ir : function->state.ir) {
			switch (ir.op) {
				case IrOp::ADD: {
					
				} break;
				case IrOp::ADD_CONSTANT: {
				
				} break;
				case IrOp::SUB: {
				
				} break;
				case IrOp::MUL: {
				
				} break;
				case IrOp::MUL_BY_CONSTANT: {
				
				} break;
				case IrOp::DIV: {
				
				} break;
				case IrOp::DIVIDE_BY_CONSTANT: {
				
				} break;
				case IrOp::MOD: {
				
				} break;
				case IrOp::AND: {
				
				} break;
				case IrOp::OR: {
				
				} break;
				case IrOp::XOR: {
				
				} break;
				case IrOp::NOT: {
				
				} break;
				case IrOp::SHIFT_LEFT: {
				
				} break;
				case IrOp::SHIFT_RIGHT: {
				
				} break;
				case IrOp::READ: {
				
				} break;
				case IrOp::WRITE: {
				
				} break;
				case IrOp::SET: {
				
				} break;
				case IrOp::GOTO: {
				
				} break;
				case IrOp::IF_Z_GOTO: {
				
				} break;
				case IrOp::IF_NZ_GOTO: {
				
				} break;
				case IrOp::LESS: {
				
				} break;
				case IrOp::GREATER: {
				
				} break;
				case IrOp::LESS_EQUAL: {
				
				} break;
				case IrOp::GREATER_EQUAL: {
				
				} break;
				case IrOp::NOT_EQUAL: {
				
				} break;
				case IrOp::EQUAL: {
				
				} break;
				case IrOp::ADDRESS_OF_GLOBAL: {
				
				} break;
				case IrOp::ADDRESS_OF_LOCAL: {
				
				} break;
				case IrOp::IMMEDIATE: {
					if (ir.opSize == 8 && static_cast<s64>(ir.a) != static_cast<s64>(static_cast<s32>(ir.a))) {
						code.add1(0x48); // mov rax, ir.a
						code.add1(0xB8);
						code.add8(ir.a);

						storeFromIntRegister(&code, function, 8, ir.dest, RAX);
					}
					else {
						storeImmediate(&code, function, ir.opSize, ir.dest, ir.a);
					}
				} break;
				case IrOp::EXTEND: {
				
				} break;
				case IrOp::FLOAT_TO_INT: {
				
				} break;
				case IrOp::INT_TO_FLOAT: {
				
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
				
				} break;
				case IrOp::NEG: {
				
				} break;
				case IrOp::NOOP: {
				
				} break;
				case IrOp::FUNCTION: {
				
				} break;
			}
		}

		if (function->declaration) {
			Symbol symbol;
			setSymbolName(&stringTable, &symbol.name, function->declaration->name);
			symbol.value = start;
			symbol.sectionNumber = 1;
			symbol.type = 0x20;

			

			if (function->declaration->enclosingScope == &globalBlock) {
				symbol.storageClass = IMAGE_SYM_CLASS_EXTERNAL;
			}
			else {
				symbol.storageClass = IMAGE_SYM_CLASS_STATIC;
			}
			symbol.numberOfAuxSymbols = 0;

			symbols.add(symbol);
		}

		function->state.allocator.free();
		function->state.loopStack.free();
		function->state.ir.free();

	}

	u32 stringTableSize = sizeof(u32) + stringTable.totalSize;

	FileHeader header = {};
	header.machine = IMAGE_FILE_MACHINE_AMD64;
	header.numberOfSections = 1;
	header.timestamp = (DWORD) time(0);
	header.pointerToSymbolTable = sizeof(FileHeader) + sizeof(SectionHeader);
	header.numberOfSymbols = symbols.count;
	header.sizeOfOptionalHeader = 0;
	header.characteristics = 0;

	SectionHeader textSection = {};
	setSectionName(textSection.name, sizeof(textSection.name), ".text");
	textSection.virtualSize = code.totalSize;
	textSection.virtualAddress = 0;
	textSection.sizeOfRawData = code.totalSize;
	textSection.pointerToRawData = sizeof(FileHeader) + sizeof(SectionHeader) + sizeof(Symbol) * symbols.count + stringTableSize;
	textSection.pointerToRelocations = 0;
	textSection.pointerToLinenumbers = 0;
	textSection.numberOfRelocations = 0;
	textSection.numberOfLinenumbers = 0;
	textSection.characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_ALIGN_4096BYTES;

	fwrite(&header, sizeof(header), 1, out);
	fwrite(&textSection, sizeof(textSection), 1, out);
	fwrite(symbols.begin(), sizeof(Symbol), symbols.count, out);

	fwrite(&stringTableSize, sizeof(stringTableSize), 1, out);
	writeAllocator(out, stringTable);

	writeAllocator(out, code);

	fclose(out);
}