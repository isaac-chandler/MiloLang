#include "Basic.h"
#include "CoffWriter.h"

WorkQueue<ExprFunction *> coffWriterQueue;

void setSectionName(char *header, u64 size, const char *name) {
	u64 len = strlen(name);
	assert(len <= size);

	memcpy(header, name, len);
	memset(header + len, 0, size - len);
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
		char name[8];
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

void runCoffWriter() {
	FILE *out = fopen("out.obj", "wb");

	char code[10] = {
		0xCC,
		0xB8, 0x3C, 0x00, 0x00, 0x00,
		0x31, 0xff,
		0x0f, 0x05
	};

	static_assert(sizeof(FileHeader) == sizeof(IMAGE_FILE_HEADER));

	u32 stringTableSize = sizeof(u32);

	Symbol symbols[1] = {};

	setSectionName(symbols[0].name, sizeof(symbols[0].name), "main");
	symbols[0].value = 0;
	symbols[0].sectionNumber = 1;
	symbols[0].type = 0x20;
	symbols[0].storageClass = IMAGE_SYM_CLASS_EXTERNAL;
	symbols[0].numberOfAuxSymbols = 0;

	FileHeader header = {};
	header.machine = IMAGE_FILE_MACHINE_AMD64;
	header.numberOfSections = 1;
	header.timestamp = (DWORD) time(0);
	header.pointerToSymbolTable = sizeof(FileHeader) + sizeof(SectionHeader);
	header.numberOfSymbols = ARRAY_COUNT(symbols);
	header.sizeOfOptionalHeader = 0;
	header.characteristics = 0;

	SectionHeader textSection = {};
	setSectionName(textSection.name, sizeof(textSection.name), ".text");
	textSection.virtualSize = sizeof(code);
	textSection.virtualAddress = 0;
	textSection.sizeOfRawData = sizeof(code);
	textSection.pointerToRawData = sizeof(FileHeader) + sizeof(SectionHeader) + sizeof(symbols) + stringTableSize;
	textSection.pointerToRelocations = 0;
	textSection.pointerToLinenumbers = 0;
	textSection.numberOfRelocations = 0;
	textSection.numberOfLinenumbers = 0;
	textSection.characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_ALIGN_4096BYTES;

	fwrite(&header, sizeof(header), 1, out);
	fwrite(&textSection, sizeof(textSection), 1, out);
	fwrite(symbols, sizeof(symbols), 1, out);
	fwrite(&stringTableSize, sizeof(stringTableSize), 1, out);
	fwrite(code, sizeof(code), 1, out);
	
	fclose(out);



	while (true) {
		ExprFunction *function = coffWriterQueue.take();


		if (!function)
			break;
	}
}