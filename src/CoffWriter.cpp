#include "Basic.h"
#include "CoffWriter.h"

WorkQueue<ExprFunction *> coffWriterQueue;

void setSectionName(BYTE *header, u64 size, const char *name) {
	u64 len = strlen(name);
	assert(len <= size);

	memcpy(header, name, len);
	memset(header + len, 0, size - len);
}

void runCoffWriter() {
	FILE *out = fopen("out.obj", "wb");

	char code[10] = {
		0xCC,
		0xB8, 0x3C, 0x00, 0x00, 0x00,
		0x31, 0xff,
		0x0f, 0x05
	};

	IMAGE_SYMBOL entryPoint = {};

	setSectionName(entryPoint.N.ShortName, sizeof(entryPoint.N.ShortName), "start");
	entryPoint.Value = 0;
	entryPoint.SectionNumber = 1;
	entryPoint.Type = 0x20;
	entryPoint.StorageClass = 3;
	entryPoint.NumberOfAuxSymbols = 0;


	IMAGE_FILE_HEADER header = {};
	header.Machine = IMAGE_FILE_MACHINE_AMD64;
	header.NumberOfSections = 1;
	header.TimeDateStamp = (DWORD) time(0);
	header.PointerToSymbolTable = sizeof(IMAGE_FILE_HEADER) + sizeof(IMAGE_SECTION_HEADER) + sizeof(code);
	header.NumberOfSymbols = 0;
	header.SizeOfOptionalHeader = 0;
	header.Characteristics = 0;

	fwrite(&header, sizeof(header), 1, out);

	IMAGE_SECTION_HEADER textSection = {};
	setSectionName(textSection.Name, sizeof(textSection.Name), ".text");
	textSection.Misc.VirtualSize = sizeof(code);
	textSection.VirtualAddress = 0;
	textSection.SizeOfRawData = sizeof(code);
	textSection.PointerToRawData = sizeof(IMAGE_FILE_HEADER) + sizeof(IMAGE_SECTION_HEADER);
	textSection.PointerToRelocations = 0;
	textSection.PointerToLinenumbers = 0;
	textSection.NumberOfRelocations = 0;
	textSection.NumberOfLinenumbers = 0;
	textSection.Characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_ALIGN_4096BYTES;

	fwrite(&textSection, sizeof(textSection), 1, out);

	fwrite(code, sizeof(code), 1, out);

	fwrite(&entryPoint, sizeof(entryPoint), 1, out);
	
	fclose(out);



	while (true) {
		ExprFunction *function = coffWriterQueue.take();


		if (!function)
			break;
	}
}