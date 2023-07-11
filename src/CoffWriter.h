#pragma once

#include "Ast.h"
#include "WorkQueue.h"
#include <elf.h>

#if !BUILD_WINDOWS
#define IMAGE_SYM_TYPE_NULL      0
#define IMAGE_SYM_CLASS_EXTERNAL 2
#define IMAGE_SYM_CLASS_STATIC   3
#endif

#if BUILD_WINDOWS
union SymbolName {
	char name[8];
	struct {
		u32 zeroes;
		u32 namePointer;
	};
};

#pragma pack(push, 1)
struct Relocation {
	u32 virtualAddress;
	u32 symbolTableIndex;
	u16 type;
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

#pragma pack(pop)
#else
using Symbol = Elf64_Sym;
using Relocation = Elf64_Rela;
#endif

struct Section : public BucketedArenaAllocator {
	Section() : BucketedArenaAllocator(65536) {}
	Section(BucketedArenaAllocator allocator) : BucketedArenaAllocator(allocator) {}

	String name;
	u64 sectionNumber;
	u64 relocationsApplyTo; 
	u64 alignment;
	u64 flags;
	u32 sectionNameOffset;
	u32 offsetInFile;
	BucketArray<Relocation> relocations;

	void addRel32Relocation(u32 symbolTableIndex, s64 addend = 0);
	void addFunctionRel32Relocation(ExprFunction *function);
	void addPointerRelocation(u32 symbolTableIndex);
	void addPointerRelocation(u32 symbolTableIndex, u32 offset, s64 addend = 0);

#if BUILD_WINDOWS
	void addAddr32NBRelocation(u32 symbolTableIndex, s64 addend = 0);
	void addSectionRelocations(u32 symbolTableInex, s64 addend = 0);
#endif
};

inline MPMCWorkQueue<CoffJob> coffWriterQueue;

void runCoffWriter();

inline BucketArray<Symbol> symbols;
inline BucketedArenaAllocator stringTable(65536);

#if BUILD_WINDOWS
inline Section *code;
inline Section *data;
inline Section *rdata;
inline Section *bss;
inline Section *debugSymbols;
inline Section *debugTypes;
inline Section *pdata;
inline Section *xdata;
#else
inline Section *code;
inline Section *data;
inline Section *rdata;
inline Section *bss;
inline Section *debugLines;
inline Section *debugInfo;
inline Section *debugAbbrev;
#endif