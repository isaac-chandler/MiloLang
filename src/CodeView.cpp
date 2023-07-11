#include "Basic.h"

#include "CodeView.h"

#include "Ast.h"
#include "CompilerMain.h"
#include "CoffWriter.h"

#if BUILD_WINDOWS

constexpr u16 LF_POINTER   = 0x1002;
constexpr u16 LF_PROCEDURE = 0x1008;
constexpr u16 LF_ARGLIST   = 0x1201;
constexpr u16 LF_FIELDLIST = 0x1203;
constexpr u16 LF_BITFIELD  = 0x1205;
constexpr u16 LF_ENUMERATE = 0x1502;
constexpr u16 LF_ARRAY     = 0x1503;
constexpr u16 LF_STRUCTURE = 0x1505;
constexpr u16 LF_UNION     = 0x1506;
constexpr u16 LF_ENUM      = 0x1507;
constexpr u16 LF_MEMBER    = 0x150D;
constexpr u16 LF_NESTTYPE  = 0x1510;
constexpr u16 LF_FUNC_ID   = 0x1601;
constexpr u16 LF_ULONG     = 0x8004;
constexpr u16 LF_UQUADWORD = 0x800A;

struct CodeViewTypeInfo {
	u32 id : 31 = 0;
	bool forwardDeclaration : 1 = false;
};

struct CoffTypeIndexPatch {
	u32 *location;
	Type *type;
};

struct CoffFunctionIDTypeIndexPatch {
	u32 *location;
	ExprFunction *function;
};

Array<CoffTypeIndexPatch> coffTypePatches;
Array<CoffFunctionIDTypeIndexPatch> coffFunctionIdTypePatches;


#pragma pack(push, 1)
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

struct DATASYM32 {
	u16 reclen;
	u16 rectyp;
	u32 typind;
	u32 off;
	u16 seg;
};

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

struct FRAMEPROCSYM {
	u16 reclen = sizeof(FRAMEPROCSYM) - 2;     // Record length
	u16 rectyp = 0x1012;     // S_FRAMEPROC
	u32 cbFrame;    // count of bytes of total frame of procedure
	u32 cbPad = 0;      // count of bytes of padding in the frame
	u32 offPad = 0;     // offset (relative to frame poniter) to where   padding starts
	u32 cbSaveRegs = 0; // count of bytes of callee save registers
	u32 offExHdlr = 0;  // offset of exception handler
	u16 sectExHdlr = 0; // section id of exception handler

	struct {
		unsigned long   unused : 14;   // function uses _alloca()
		unsigned long   encodedLocalBasePointer : 2;  // record function's local pointer explicitly.
		unsigned long   encodedParamBasePointer : 2;  // record function's parameter pointer explicitly.
		unsigned long   pad : 14;   // must be zero
	} flags;
};
#pragma pack(pop)


void emitBasicType(BucketedArenaAllocator *debugSymbols, u32 type, const char *name) {
	debugSymbols->ensure(8);
	debugSymbols->add2Unchecked(static_cast<u16>(7 + strlen(name)));
	debugSymbols->add2Unchecked(S_UDT);
	debugSymbols->add4Unchecked(type);
	debugSymbols->addNullTerminatedString(name);
}

void emitCodeViewPrologue() {
	debugSymbols->add4(4);
	debugTypes->add4(4);

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

	debugSymbols->align(4);
	debugSymbols->add4(0xF1);
	subsectionSizePatch = debugSymbols->add4(0);
	subsectionOffset = debugSymbols->totalSize;

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

	*subsectionSizePatch = debugSymbols->totalSize - subsectionOffset;
	debugSymbols->align(4);
}

static u32 debugTypeId = 0x1000;

struct DebugLeaf {
	template<typename T>
	u32 operator+(T t) {
		u16 *patch = debugTypes->add2(0);
		u32 start = debugTypes->totalSize;
		t();
		alignDebugTypes();
		*patch = static_cast<u16>(debugTypes->totalSize - start);
		return debugTypeId++;
	}
};

void alignDebugTypes() {
	u32 padding = -debugTypes->totalSize & 3;

	debugTypes->ensure(padding);
	for (u32 i = 0; i < padding; i++) {
		debugTypes->add1Unchecked(0xF0 + padding - i);
	}

	assert(!(debugTypes->totalSize & 3));
}

#define DEBUG_LEAF DebugLeaf() + [&]()

u32 createFunctionIDType(ExprFunction *function) {
	auto name = function->valueOfDeclaration ? function->valueOfDeclaration->name : "__unnamed";

	// @Volatile: This relies on the LF_PROCEDURE node being directly before the LF_POINTER node that defines the function pointer type
	u32 funcType = getCoffTypeIndex(function->type) - 1;

	return DEBUG_LEAF{
		debugTypes->ensure(10);
		debugTypes->add2Unchecked(LF_FUNC_ID);
		debugTypes->add4Unchecked(0);
		debugTypes->add4Unchecked(funcType);
		debugTypes->addNullTerminatedString(name);
	};
}


void addStructUniqueNumber(u32 name = debugTypeId) {
	char buffer[32];

	snprintf(buffer, sizeof(buffer), "%x", static_cast<int>(name - 0x1000));

	debugTypes->addNullTerminatedString(buffer);
}

bool appendCoffName(Type *type) {
	if (type->flavor == TypeFlavor::STRUCT || type->flavor == TypeFlavor::ENUM) {
		auto structure = static_cast<TypeStruct *>(type);

		if (structure->enclosingScope->flavor == BlockFlavor::STRUCT) {
			appendCoffName(CAST_FROM_SUBSTRUCT(TypeStruct, members, structure->enclosingScope));
			debugSymbols->addString("::");
		}
	}

	if (type->flavor == TypeFlavor::ARRAY) {
		auto array = static_cast<TypeArray *>(type);

		if (array->flags & TYPE_ARRAY_IS_FIXED) {
			appendCoffName(array->arrayOf);
			debugSymbols->addString(" [");

			char buffer[32];
			snprintf(buffer, sizeof(buffer), "%u", array->count);

			debugSymbols->addString(buffer);
			debugSymbols->add1(']');
		}
		else if (array->flags & TYPE_ARRAY_IS_DYNAMIC) {
			debugSymbols->addString("Dynamic_Array<");
			if (appendCoffName(array->arrayOf)) {
				debugSymbols->add1(' ');
			}

			debugSymbols->add1('>');
			return true;
		}
		else {
			debugSymbols->addString("Array<");
			if (appendCoffName(array->arrayOf)) {
				debugSymbols->add1(' ');
			}

			debugSymbols->add1('>');
			return true;
		}
	}
	else if (type->flags & TYPE_IS_ANONYMOUS) {
		debugSymbols->addString("<unnamed-");
		debugSymbols->addString(type->name);
		debugSymbols->add1('-');
		addStructUniqueNumber(getCoffTypeIndex(type));
		debugSymbols->add1('>');
	}
	else {
		debugSymbols->addString(type->name);
	}

	return false;
}

u32 createType(Type *type, CodeViewTypeInfo *codeView = nullptr);
u32 createTypeOrForwardDeclaration(Type *type, CodeViewTypeInfo *codeView = nullptr);

u32 createTypeImpl(Type *type, CodeViewTypeInfo *codeView) {

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

		auto indexOfType = createTypeOrForwardDeclaration(pointer->pointerTo);


		// @Incomplete change 0x100 to a more concrete value
		if (indexOfType < 0x100) {
			return indexOfType | 0x600;
		}

		return DEBUG_LEAF{
			debugTypes->ensure(2);
			debugTypes->add2Unchecked(LF_POINTER);
			debugTypes->add4Unchecked(indexOfType);
			debugTypes->add4Unchecked(0x1000c); // 64 bit normal pointer
		};
	}
	else if (type->flavor == TypeFlavor::STRING) {
		auto fieldList = DEBUG_LEAF{
			debugTypes->ensure(12);
			debugTypes->add2Unchecked(LF_FIELDLIST);

			debugTypes->add2Unchecked(LF_MEMBER);
			debugTypes->add2Unchecked(0x3); // public
			debugTypes->add4Unchecked(T_64PUCHAR);
			debugTypes->add2Unchecked(0); // offset 0
			debugTypes->addNullTerminatedString("data");
			alignDebugTypes();

			debugTypes->ensure(10);
			debugTypes->add2Unchecked(LF_MEMBER); 
			debugTypes->add2Unchecked(0x3); // public
			debugTypes->add4Unchecked(T_UINT8);
			debugTypes->add2Unchecked(8); // offset 8
			debugTypes->addNullTerminatedString("count");
		};

		return DEBUG_LEAF{
			debugTypes->ensure(20);
			debugTypes->add2Unchecked(LF_STRUCTURE); 
			debugTypes->add2Unchecked(2); // 2 members
			debugTypes->add2Unchecked(0x200); // Has a unique name
			debugTypes->add4Unchecked(fieldList); // field list
			debugTypes->add4Unchecked(0); // super class
			debugTypes->add4Unchecked(0); // vtable
			debugTypes->add2Unchecked(16);
			debugTypes->addNullTerminatedString("string");
			debugTypes->add1('@');
			addStructUniqueNumber();
		};
	}
	else if (type->flavor == TypeFlavor::ARRAY) {
		auto array = static_cast<TypeArray *>(type);

		if (array->flags & TYPE_ARRAY_IS_FIXED) {
			u32 arrayOf = createType(array->arrayOf);

			return DEBUG_LEAF{
				debugTypes->ensure(17);
				debugTypes->add2Unchecked(LF_ARRAY);
				debugTypes->add4Unchecked(arrayOf);
				debugTypes->add4Unchecked(T_INT8);
				debugTypes->add2Unchecked(LF_ULONG);
				debugTypes->add4Unchecked(array->size);
				debugTypes->add1Unchecked(0); // No name
			};
		}
		else {
			u32 dataType = createType(getDeclarationType(array->members.declarations[0]));

			u32 fieldList = DEBUG_LEAF{
				debugTypes->ensure(12);
				debugTypes->add2Unchecked(LF_FIELDLIST);

				debugTypes->add2Unchecked(LF_MEMBER);
				debugTypes->add2Unchecked(0x3); // public
				debugTypes->add4Unchecked(dataType);
				debugTypes->add2Unchecked(0); // offset 0
				debugTypes->addNullTerminatedString("data");
				alignDebugTypes();

				debugTypes->ensure(10);
				debugTypes->add2Unchecked(LF_MEMBER);
				debugTypes->add2Unchecked(0x3); // public
				debugTypes->add4Unchecked(T_UINT8);
				debugTypes->add2Unchecked(8); // offset 8
				debugTypes->addNullTerminatedString("count");

				if (array->flags & TYPE_ARRAY_IS_DYNAMIC) {
					alignDebugTypes();

					debugTypes->ensure(10);
					debugTypes->add2Unchecked(LF_MEMBER);
					debugTypes->add2Unchecked(0x3); // public
					debugTypes->add4Unchecked(T_UINT8);
					debugTypes->add2Unchecked(16); // offset 8
					debugTypes->addNullTerminatedString("capacity");
				}
			};

			return DEBUG_LEAF{
				debugTypes->ensure(20);
				debugTypes->add2Unchecked(LF_STRUCTURE);
				debugTypes->add2Unchecked(array->flags & TYPE_ARRAY_IS_DYNAMIC ? 3 : 2); // 2 members
				debugTypes->add2Unchecked(0x200); // Has a unique name
				debugTypes->add4Unchecked(fieldList); // field list
				debugTypes->add4Unchecked(0); // super class
				debugTypes->add4Unchecked(0); // vtable
				debugTypes->add2Unchecked(array->size);
				appendCoffName(array);
				debugTypes->add1(0);
				debugTypes->add1('@');
				addStructUniqueNumber();
			};
		}
	}
	else if (type->flavor == TypeFlavor::FUNCTION) {
		auto function = static_cast<TypeFunction *>(type);

		u32 extraParams = 0;

		if (!(function->flags & TYPE_FUNCTION_IS_C_CALL)) {
			createTypeOrForwardDeclaration(&TYPE_CONTEXT);
			extraParams++;
		}

		for (u32 i = 0; i < function->argumentCount; i++) {
			createTypeOrForwardDeclaration(function->argumentTypes[i]);
		}

		for (u32 i = 0; i < function->returnCount; i++) {
			createTypeOrForwardDeclaration(function->returnTypes[i]);
		}

		u32 firstReturn = debugTypeId;

		for (u32 i = 1; i < function->returnCount; i++) {
			DEBUG_LEAF{
				debugTypes->ensure(10);
				debugTypes->add2Unchecked(LF_POINTER);
				debugTypes->add4Unchecked(getCoffTypeIndex(function->returnTypes[i]));
				debugTypes->add4Unchecked(0x1000c); // 64 bit normal pointer
			};
		}

		u32 argCount = extraParams + function->argumentCount + function->returnCount - 1;

		u32 argList = DEBUG_LEAF{
			debugTypes->ensure(6 + argCount * 4);
			debugTypes->add2Unchecked(LF_ARGLIST);
			debugTypes->add4Unchecked(argCount);

			if (!(function->flags & TYPE_FUNCTION_IS_C_CALL)) {
				debugTypes->add4Unchecked(getCoffTypeIndex(&TYPE_CONTEXT));
			}

			for (u32 i = 0; i < function->argumentCount; i++) {
				debugTypes->add4Unchecked(getCoffTypeIndex(function->argumentTypes[i]));
			}

			for (u32 i = 1; i < function->returnCount; i++) {
				debugTypes->add4Unchecked(firstReturn + i - 1);
			}
		};

		u32 functionType = DEBUG_LEAF{
			debugTypes->ensure(14);
			debugTypes->add2Unchecked(LF_PROCEDURE);
			debugTypes->add4Unchecked(getCoffTypeIndex(function->returnTypes[0]));
			debugTypes->add1Unchecked(0); // C near call
			debugTypes->add1Unchecked(0);
			debugTypes->add2Unchecked(argCount);
			debugTypes->add4Unchecked(argList);
		};

		return DEBUG_LEAF{
			debugTypes->ensure(10);
			debugTypes->add2Unchecked(LF_POINTER);
			debugTypes->add4Unchecked(functionType);
			debugTypes->add4Unchecked(0x1000c); // 64 bit normal pointer
		};
	}
	else if (type->flavor == TypeFlavor::ENUM) {
		auto enumeration = static_cast<TypeEnum *>(type);

		u32 integerType = createType(enumeration->integerType);

		// Use a struct with bitfields to represent a flags enum
		if (enumeration->flags & TYPE_ENUM_IS_FLAGS) {
			u32 firstFlag = debugTypeId;

			u16 nested = enumeration->enclosingScope->flavor == BlockFlavor::STRUCT ? 8 : 0;

			for (auto declaration : enumeration->members.declarations) {
				if (!(declaration->flags & DECLARATION_IS_ENUM_VALUE))
					continue;

				assert(declaration->initialValue->flavor == ExprFlavor::INT_LITERAL);
				assert(!(declaration->initialValue->type->flags & TYPE_INTEGER_IS_SIGNED));

				u64 value = static_cast<ExprLiteral *>(declaration->initialValue)->unsignedValue;

				if (value && !(value & value - 1)) { // If exactly one bit is set
					u32 bit = bitScanForward64(value);

					DEBUG_LEAF{
						debugTypes->ensure(8);
						debugTypes->add2Unchecked(LF_BITFIELD);
						debugTypes->add4Unchecked(integerType);
						debugTypes->add1Unchecked(1); // 1 bit
						debugTypes->add1Unchecked(static_cast<u8>(bit));
					};
				}
			}

			u32 flagCount = 0;

			u32 fieldList = DEBUG_LEAF{
				debugTypes->add2(LF_FIELDLIST);

				for (auto declaration : enumeration->members.declarations) {
					if (!(declaration->flags & DECLARATION_IS_ENUM_VALUE))
						continue;

					u64 value = static_cast<ExprLiteral *>(declaration->initialValue)->unsignedValue;

					if (value && !(value & value - 1)) { // If exactly one bit is set
						alignDebugTypes();

						debugTypes->ensure(10);
						debugTypes->add2Unchecked(LF_MEMBER);
						debugTypes->add2Unchecked(0x3); // public
						debugTypes->add4Unchecked(firstFlag + flagCount++);
						debugTypes->add2Unchecked(0); // offset 0
						debugTypes->addNullTerminatedString(declaration->name);
					}
				}
			};

			// Enum naming for anonymous enums needs the type id to be set
			codeView->id = debugTypeId;

			return DEBUG_LEAF{
				debugTypes->ensure(20);
				debugTypes->add2Unchecked(LF_STRUCTURE);
				debugTypes->add2Unchecked(flagCount);
				debugTypes->add2Unchecked(0x200 | nested); // Has a unique name
				debugTypes->add4Unchecked(fieldList); // field list
				debugTypes->add4Unchecked(0); // super class
				debugTypes->add4Unchecked(0); // vtable
				debugTypes->add2Unchecked(enumeration->size);
				appendCoffName(enumeration);
				debugTypes->add1(0);
				debugTypes->add1('@');
				addStructUniqueNumber();
			};
		}
		else {
			u32 fieldList = DEBUG_LEAF{
				debugTypes->add4(LF_FIELDLIST);

				for (auto declaration : enumeration->members.declarations) {
					if (!(declaration->flags & DECLARATION_IS_ENUM_VALUE))
						continue;

					assert(declaration->initialValue->flavor == ExprFlavor::INT_LITERAL);
					assert(!(declaration->initialValue->type->flags & TYPE_INTEGER_IS_SIGNED));

					alignDebugTypes();
					debugTypes->ensure(14);
					debugTypes->add2Unchecked(LF_ENUMERATE);
					debugTypes->add2Unchecked(0x3); // Public
					debugTypes->add2Unchecked(LF_UQUADWORD);
					debugTypes->add8Unchecked(static_cast<ExprLiteral *>(declaration->initialValue)->unsignedValue);
					debugTypes->addNullTerminatedString(declaration->name);
				}
			};

			// Enum naming for anonymous enums needs the type id to be set
			codeView->id = debugTypeId;

			return DEBUG_LEAF{
				debugTypes->ensure(14);
				debugTypes->add2Unchecked(LF_ENUM);
				debugTypes->add2Unchecked(static_cast<u16>(enumeration->members.declarations.count - ENUM_SPECIAL_MEMBER_COUNT));
				debugTypes->add2Unchecked(0x200); // Has a unique name
				debugTypes->add4Unchecked(integerType);
				debugTypes->add4Unchecked(fieldList);
				appendCoffName(enumeration);
				debugTypes->add1(0);
				debugTypes->add1('@');
				addStructUniqueNumber();
			};
		}
	}
	else if (type->flavor == TypeFlavor::TYPE) {
		u32 fieldList = DEBUG_LEAF{
			debugTypes->ensure(12);
			debugTypes->add2Unchecked(LF_FIELDLIST);

			debugTypes->add2Unchecked(LF_MEMBER);
			debugTypes->add2Unchecked(0x3); // public
			debugTypes->add4Unchecked(T_64PVOID);
			debugTypes->add2Unchecked(0); // offset 0
			debugTypes->addNullTerminatedString("value");
		};

		return DEBUG_LEAF{
			debugTypes->ensure(20);
			debugTypes->add2Unchecked(LF_STRUCTURE);
			debugTypes->add2Unchecked(1); // 1 member
			debugTypes->add2Unchecked(0x200); // Has a unique name
			debugTypes->add4Unchecked(fieldList); // field list
			debugTypes->add4Unchecked(0); // super class
			debugTypes->add4Unchecked(0); // vtable
			debugTypes->add2Unchecked(8);
			debugTypes->addNullTerminatedString("type");
			debugTypes->add1('@');
			addStructUniqueNumber();
		};
	}
	else if (type->flavor == TypeFlavor::STRUCT) {
		auto structure = static_cast<TypeStruct *>(type);

		u32 memberCount = 0;

		for (auto member : structure->members.declarations) {
			if (member->flags & DECLARATION_IMPORTED_BY_USING) continue;

			auto type = getDeclarationType(member);

			if (member->flags & DECLARATION_IS_CONSTANT) {
				if (type != &TYPE_TYPE)
					continue;

				auto nestType = static_cast<ExprLiteral *>(member->initialValue)->typeValue;

				createTypeOrForwardDeclaration(nestType);
			}
			else {
				createType(type);
			}
		}

		u32 fieldList = DEBUG_LEAF{
			debugTypes->add2(LF_FIELDLIST);

			auto struct_ = static_cast<TypeStruct *>(type);

			for (auto member : struct_->members.declarations) {
				if (member->flags & DECLARATION_IMPORTED_BY_USING) continue;

				auto type = getDeclarationType(member);

				if (member->flags & DECLARATION_IS_CONSTANT) {
					if (type != &TYPE_TYPE)
						continue;

					auto nestType = static_cast<ExprLiteral *>(member->initialValue)->typeValue;

					alignDebugTypes();
					debugTypes->ensure(6);
					debugTypes->add2(LF_NESTTYPE);
					debugTypes->add2(0);
					debugTypes->add4(getCoffTypeIndex(nestType));
					debugTypes->addNullTerminatedString(member->name);
					memberCount++;
				}
				else {
					alignDebugTypes();
					debugTypes->ensure(10);
					debugTypes->add2Unchecked(LF_MEMBER);
					debugTypes->add2Unchecked(0x3); // public
					debugTypes->add4Unchecked(getCoffTypeIndex(type));
					debugTypes->add2Unchecked(member->physicalStorage); // offset
					debugTypes->addNullTerminatedString(member->name);
					memberCount++;

				}
			}
		};

		bool isUnion = structure->flags & TYPE_STRUCT_IS_UNION ? true : false;

		u16 packed = structure->flags & TYPE_STRUCT_IS_PACKED ? 1 : 0;

		// Struct naming uses the type id, so if we don't have one created by a forward declaration, assign the type id
		if (!codeView->id) {
			codeView->id = debugTypeId;
		}

		return DEBUG_LEAF{
			debugTypes->ensure(20);
			debugTypes->add2Unchecked(isUnion ? LF_UNION : LF_STRUCTURE);
			debugTypes->add2Unchecked(memberCount);
			debugTypes->add2Unchecked(0x200 | packed); // Has a unique name
			debugTypes->add4Unchecked(fieldList); // field list
			debugTypes->add4Unchecked(0); // super class
			debugTypes->add4Unchecked(0); // vtable
			debugTypes->add2Unchecked(structure->size);
			appendCoffName(structure);
			debugTypes->add1(0);
			debugTypes->add1('@');
			addStructUniqueNumber(codeView->id);
		};
	}

	assert(false);
	return 0;
}

CodeViewTypeInfo *codeViewTypeTable;

u32 createType(Type *type, CodeViewTypeInfo *codeView) {
	if (!codeView) {
		codeView = &codeViewTypeTable[findInTypeTable(type)];
	}

	if (codeView->id && !codeView->forwardDeclaration) {
		return codeView->id;
	}

	codeView->id = createTypeImpl(type, codeView);
	return codeView->id;
}

u32 createTypeOrForwardDeclaration(Type *type, CodeViewTypeInfo *codeView) {
	if (!codeView) {
		codeView = &codeViewTypeTable[findInTypeTable(type)];
	}

	if (codeView->id) {
		return codeView->id;
	}

	if (type->flavor != TypeFlavor::STRUCT) {
		return createType(type, codeView);
	}

	auto structure = static_cast<TypeStruct *>(type);

	u16 packed = structure->flags & TYPE_STRUCT_IS_PACKED ? 1 : 0;
	u16 nested = structure->enclosingScope->flavor == BlockFlavor::STRUCT ? 8 : 0;

	codeView->id = debugTypeId;

	DEBUG_LEAF{
		debugTypes->ensure(20);
		debugTypes->add2Unchecked(LF_STRUCTURE);
		debugTypes->add2Unchecked(0);

		debugTypes->add2Unchecked(0x280 | packed | nested); // Has a unique name and is a forward declaration
		debugTypes->add4Unchecked(0); // field list
		debugTypes->add4Unchecked(0); // super class
		debugTypes->add4Unchecked(0); // vtable
		debugTypes->add2Unchecked(0); // size
		appendCoffName(structure);
		debugTypes->add1(0);
		debugTypes->add1('@');
		addStructUniqueNumber();
	};

	codeView->forwardDeclaration = true;

	return codeView->id;
}

u32 getCoffTypeIndex(Type *type) {
	auto id = codeViewTypeTable[findInTypeTable(type)].id;
	assert(id);
	return id;
}


void exportTypeTableToDebugTSection() {
	PROFILE_FUNC();
	codeViewTypeTable = new CodeViewTypeInfo[typeTableCapacity]{};


	for (u64 i = 0; i < typeTableCapacity; i++) {
		auto entry = typeTableEntries[i];

		if (entry.hash) {
			auto type = entry.value;

			auto codeView = &codeViewTypeTable[i];

			createType(type, codeView);
		}
	}
}

void emitUDT(Type *type) {
	debugSymbols->ensure(8);
	u16 *size = debugSymbols->add2Unchecked(0);
	u32 start = debugSymbols->totalSize;
	debugSymbols->add2Unchecked(S_UDT);
	auto patch = debugSymbols->add4Unchecked(0);
	coffTypePatches.add({ patch, type });
	appendCoffName(type);
	debugSymbols->add1(0);
	*size = static_cast<u16>(debugSymbols->totalSize - start);
}

void emitLocalDeclaration(String name, Type *type, u64 offset) {
	REGREL32 argumentInfo;
	argumentInfo.off = offset;
	argumentInfo.typind = 0;

	debugSymbols->ensure(2 + sizeof(argumentInfo));
	debugSymbols->add2Unchecked(static_cast<u16>(sizeof(argumentInfo) + 1 + name.length));
	REGREL32 *patch = (REGREL32 *)debugSymbols->addUnchecked(&argumentInfo, sizeof(argumentInfo));
	coffTypePatches.add({ &patch->typind, type });

	debugSymbols->addNullTerminatedString(name);
}

void emitGlobalDeclaration(Declaration *declaration) {
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

	debugSymbols->align(4);
}

struct LineInfo {
	u32 offset;
	u32 line;
};

struct ColumnInfo {
	u16 start;
	u16 end;
};

Array<u32 *> blockOffsetStack;
Array<LineInfo> lineInfo;
Array<ColumnInfo> columnInfo;

EmitFunctionInfo emitFunctionBegin(ExprFunction *function, u32 stackSpace) {
	lineInfo.clear();
	columnInfo.clear();
	
	EmitFunctionInfo result;
	result.function = function;
	result.functionStart = code->totalSize;
	result.stackSpace = stackSpace;
	
	debugSymbols->ensure(64);
	debugSymbols->add4Unchecked(0xF1);
	result.subsectionSizePatch = debugSymbols->add4Unchecked(0);
	result.subsectionOffset = debugSymbols->totalSize;

	auto name = function->valueOfDeclaration ? function->valueOfDeclaration->name : "__unnamed";

	debugSymbols->add2Unchecked(static_cast<u16>(sizeof(PROCSYM32) + name.length - 1));
	debugSymbols->add2Unchecked(0x1147); // S_GPROC32_ID
	debugSymbols->add4Unchecked(0);
	debugSymbols->add4Unchecked(0);
	debugSymbols->add4Unchecked(0);
	result.lengthPatch = debugSymbols->add4Unchecked(0);
	result.preambleEndPatch = debugSymbols->add4Unchecked(0);
	result.postambleStartPatch = debugSymbols->add4Unchecked(0);
	u32 *patch = debugSymbols->add4Unchecked(0);
	coffFunctionIdTypePatches.add({ patch, function });
	
	debugSymbols->addSectionRelocations(function->physicalStorage);

	debugSymbols->add1Unchecked(0);
	debugSymbols->addNullTerminatedString(name);

	FRAMEPROCSYM frame;
	frame.cbFrame = stackSpace;
	frame.flags.unused = 0;
	frame.flags.encodedLocalBasePointer = 1; // RSP
	frame.flags.encodedParamBasePointer = 1; // RSP
	frame.flags.pad = 0;

	debugSymbols->add(&frame, sizeof(frame));
	return result;
}

void emitFunctionPushRdi(EmitFunctionInfo *info) {
	info->pushRdiOffset = code->totalSize - info->functionStart;
}

void emitFunctionPushRsi(EmitFunctionInfo *info) {
	info->pushRsiOffset = code->totalSize - info->functionStart;
}

void emitFunctionPreambleEnd(EmitFunctionInfo *info) {
	*info->preambleEndPatch = code->totalSize - info->functionStart;
}

void emitFunctionPostambleStart(EmitFunctionInfo *info) {
	*info->postambleStartPatch = code->totalSize - info->functionStart;
}

void emitFunctionEnd(EmitFunctionInfo *info) {
	*info->lengthPatch = code->totalSize - info->functionStart;

	debugSymbols->ensure(4);
	debugSymbols->add2Unchecked(2); // S_PROC_ID_END
	debugSymbols->add2Unchecked(0x114f);

	*info->subsectionSizePatch = debugSymbols->totalSize - info->subsectionOffset;

	debugSymbols->align(4);

	{
		PROFILE_ZONE("Write Function Debug Lines");
		debugSymbols->ensure(32 + lineInfo.count * 12);
		
		debugSymbols->add4Unchecked(0xF2);
		debugSymbols->add4Unchecked(24 + lineInfo.count * 12);

		debugSymbols->addSectionRelocations(info->function->physicalStorage);

		debugSymbols->add2Unchecked(1); // fHasColumns
		debugSymbols->add4Unchecked(code->totalSize - info->functionStart);

		debugSymbols->add4Unchecked(info->function->start.fileUid * 8);
		debugSymbols->add4Unchecked(lineInfo.count);
		debugSymbols->add4Unchecked(12 + lineInfo.count * 12);
		debugSymbols->addUnchecked(lineInfo.storage, lineInfo.count * sizeof(LineInfo));
		debugSymbols->addUnchecked(columnInfo.storage, columnInfo.count * sizeof(ColumnInfo));
	}

	
	pdata->ensure(12);

	pdata->addAddr32NBRelocation(info->function->physicalStorage, code->totalSize - info->functionStart);
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
	xdata->add1Unchecked(*info->preambleEndPatch);
	xdata->add1Unchecked(4);
	xdata->add1Unchecked(0);

	xdata->add1Unchecked(*info->preambleEndPatch);
	xdata->add1Unchecked(0x01);
	xdata->add2Unchecked(info->stackSpace);
	xdata->add1Unchecked(info->pushRdiOffset);
	xdata->add1Unchecked(0x70);
	xdata->add1Unchecked(info->pushRsiOffset);
	xdata->add1Unchecked(0x60);
}

void emitBlockStart(EmitFunctionInfo *info) {
	debugSymbols->ensure(23);

	debugSymbols->add2Unchecked(21);
	debugSymbols->add2Unchecked(0x1103); // S_BLOCK32

	debugSymbols->add4Unchecked(0);
	debugSymbols->add4Unchecked(0);

	blockOffsetStack.add(debugSymbols->add4(0));

	debugSymbols->addSectionRelocations(info->function->physicalStorage, code->totalSize - info->functionStart);

	debugSymbols->add1Unchecked(0);
}

void emitBlockEnd(EmitFunctionInfo *info) {
	u32 *length = blockOffsetStack.pop();
	*length = code->totalSize - info->functionStart - length[1];

	debugSymbols->ensure(4);
	debugSymbols->add2Unchecked(2);
	debugSymbols->add2Unchecked(6); // S_END
}


void addLineInfo(CodeLocation start, EndLocation end) {
	s64 delta = end.line - start.line;

	if (delta < 0) {
		delta = 0;
	}

	auto &line = lineInfo.add();

	line.offset = code->totalSize;
	line.line = (start.line & 0xFFF) | ((delta & 0x7F) << 24) | (1 << 31);


	auto &column = columnInfo.add();

	column.start = start.column;
	column.end = end.column + 1;
}

void emitCodeViewEpilogue() {

	exportTypeTableToDebugTSection();

	{
		PROFILE_ZONE("Patch debug types");
		for (auto patch : coffTypePatches) {
			*patch.location = getCoffTypeIndex(patch.type);
		}

		for (auto patch : coffFunctionIdTypePatches) {
			*patch.location = createFunctionIDType(patch.function);
		}
	}

	debugSymbols->ensure(8);

	debugSymbols->add4Unchecked(0xF1);
	u32 *subsectionSizePatch = debugSymbols->add4(0);
	u32 previousSize = debugSymbols->totalSize;

	for (u64 i = 0; i < typeTableCapacity; i++) {
		auto entry = typeTableEntries[i];

		if (entry.hash) {
			auto type = entry.value;



			if (type->flavor == TypeFlavor::STRUCT || type->flavor == TypeFlavor::ARRAY || type->flavor == TypeFlavor::ENUM) {
				if (!(type->flags & (TYPE_ARRAY_IS_FIXED | TYPE_ENUM_IS_FLAGS)))
					emitUDT(type);
			}
		}
	}

	*subsectionSizePatch = debugSymbols->totalSize - previousSize;
	debugSymbols->align(4);

	
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

	debugSymbols->align(4);

	debugSymbols->add4(0xF4);
	debugSymbols->add4(8 * compilerFiles.count);

	for (auto &file : compilerFiles) {
		debugSymbols->add4(file->offsetInStringTable);
		debugSymbols->add4(0);
	}
}
#else

u32 getCoffTypeIndex(Type *type) {
	(void)type;
	return 0;
}

void emitUDT(Type *type) {
	(void)type;
}

void alignDebugTypes() {

}

u32 createFunctionIDType(ExprFunction *function) {
	(void)function;
	return 0;
}

u32 textSectionSymbolIndex;

u32 *debugLinesLength;
u32 debugLinesLengthOffset;
u32 lastLineAddress = 0;
CodeLocation lastLineLocation = { { 0, 1, 0 }, 0};

u32 *debugInfoLength;
u32 debugInfoLengthOffset;

#define DWARF_VERSION 5
#define ADDRESS_SIZE 8
#define SEGMENT_SELECTOR_SIZE 0
#define MIN_INSTRUCTION_LENGTH 1
#define MAX_OPS_PER_INSTRUCTION 1 // VLIW ISA's only
#define DEFAULT_IS_STMT 1
#define LINE_BASE -5
#define LINE_RANGE 14
#define OPCODE_BASE 13

static constexpr u8 standard_opcode_lengths[OPCODE_BASE - 1] = {
	1, 
	0, 
	1, 
	1, 
	1, 
	0, 
	0, 
	0, 
	1, 
	0, 
	0, 
	1, 
};

#define DIRECTORY_ENTRY_FORMAT_COUNT 1

#define FILE_ENTRY_FORMAT_COUNT 2
#define DW_LNCT_path 1
#define DW_LNCT_directory_index 2

#define DW_FORM_addr 1
#define DW_FORM_data2 5
#define DW_FORM_data8 7
#define DW_FORM_string 8
#define DW_FORM_sec_offset 0x17

#define DW_LNS_extension 0
#define DW_LNS_copy 1
#define DW_LNS_advance_line 3
#define DW_LNS_set_file 4
#define DW_LNS_fixed_advance_pc 9

#define DW_LNE_set_address 2

#define DW_UT_compile 1

void addUleb(Section *section, u64 value) {
	section->ensure(10);
	do {
		u8 byte = value & 0x7f;
		value >>= 7;
		if (value)
			byte |= 0x80;
		section->add1Unchecked(byte);
	} while (value);
}

void addSleb(Section *section, s64 value) {
	section->ensure(10);
	
	auto more = true;
	do {
		u8 byte = value & 0x7f;
		value >>= 7;
		bool sign_bit = byte & 0x40;

		if ((value == 0 && !sign_bit) || (value == -1 && sign_bit)) 
			more = false;
		else
			byte |= 0x80;

		section->add1Unchecked(byte);
	} while (more);
}

#define DW_CHILDREN_yes 1

#define DW_TAG_compile_unit 0x11

#define ABBREV_NULL 0
#define ABBREV_COMPILE_UNIT 1

#define DW_AT_name 0x3
#define DW_AT_stmt_list 0x10
#define DW_AT_low_pc 0x11
#define DW_AT_high_pc 0x12

u32 compileUnitHighPcOffset;

void emitCodeViewPrologue() {
	Symbol textSectionSymbol;
	textSectionSymbol.st_name = stringTable.totalSize;
	textSectionSymbol.st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
	textSectionSymbol.st_other = STV_DEFAULT;
	textSectionSymbol.st_shndx = code->sectionNumber;
	textSectionSymbol.st_value = 0;
	textSectionSymbol.st_size = 0;
	stringTable.addNullTerminatedString(code->name);

	textSectionSymbolIndex = symbols.count();
	symbols.add(textSectionSymbol);
	
	debugLines->ensure(256);

	u32 debugLineOffset = debugLines->totalSize;

	debugLinesLength = debugLines->add4Unchecked(0);
	debugLinesLengthOffset = debugLines->totalSize;

	debugLines->add2Unchecked(DWARF_VERSION);
	debugLines->add1Unchecked(ADDRESS_SIZE);
	debugLines->add1Unchecked(SEGMENT_SELECTOR_SIZE);

	u32 *headerLength = debugLines->add4Unchecked(0);
	u32 headerLengthOffset = debugLines->totalSize;
	
	debugLines->add1Unchecked(MIN_INSTRUCTION_LENGTH);
	debugLines->add1Unchecked(MAX_OPS_PER_INSTRUCTION);
	debugLines->add1Unchecked(DEFAULT_IS_STMT);
	debugLines->add1Unchecked(LINE_BASE);
	debugLines->add1Unchecked(LINE_RANGE);
	debugLines->add1Unchecked(OPCODE_BASE);

	debugLines->add(standard_opcode_lengths, sizeof(standard_opcode_lengths));

	debugLines->add1Unchecked(DIRECTORY_ENTRY_FORMAT_COUNT);

	addUleb(debugLines, DW_LNCT_path);
	addUleb(debugLines, DW_FORM_string);

	debugLines->add1(compilerDirectories.count);

	for (auto directory : compilerDirectories) {
		debugLines->addNullTerminatedString(directory);
	}

	debugLines->add1(FILE_ENTRY_FORMAT_COUNT);
	addUleb(debugLines, DW_LNCT_directory_index);
	addUleb(debugLines, DW_FORM_data2);
	addUleb(debugLines, DW_LNCT_path);
	addUleb(debugLines, DW_FORM_string);

	addUleb(debugLines, compilerFiles.count);

	for (u32 i = 0; i < compilerFiles.count; i++) {
		u32 emitId = i;

		if (emitId < 2) {
			emitId = !emitId;
		}

		debugLines->add2(compilerFiles[emitId]->directoryId);
		debugLines->addNullTerminatedString(compilerFiles[emitId]->name);
	}

	*headerLength = debugLines->totalSize - headerLengthOffset;

	debugLines->ensure(32);
	debugLines->add1Unchecked(DW_LNS_extension);
	addUleb(debugLines, 9);
	debugLines->add1Unchecked(DW_LNE_set_address);
	debugLines->addPointerRelocation(textSectionSymbolIndex);

	u32 debugAbbrevOffset = debugAbbrev->totalSize;
	debugAbbrev->ensure(256);


	addUleb(debugAbbrev, ABBREV_COMPILE_UNIT);
	addUleb(debugAbbrev, DW_TAG_compile_unit);
	debugAbbrev->add1Unchecked(DW_CHILDREN_yes);

	addUleb(debugAbbrev, DW_AT_name);
	addUleb(debugAbbrev, DW_FORM_string);
	addUleb(debugAbbrev, DW_AT_low_pc);
	addUleb(debugAbbrev, DW_FORM_addr);
	// GCC emits low pc and high pc with different forms for some reason 
	// not sure if this is actually required
	addUleb(debugAbbrev, DW_AT_high_pc);
	addUleb(debugAbbrev, DW_FORM_data8); 
	addUleb(debugAbbrev, DW_AT_stmt_list);
	addUleb(debugAbbrev, DW_FORM_sec_offset);
	addUleb(debugAbbrev, 0);
	addUleb(debugAbbrev, 0);

	addUleb(debugAbbrev, ABBREV_NULL);


	debugInfo->ensure(256);
	debugInfoLength = debugInfo->add4Unchecked(0);
	debugInfoLengthOffset = debugInfo->totalSize;
	debugInfo->add2Unchecked(DWARF_VERSION);
	debugInfo->add1Unchecked(DW_UT_compile);
	debugInfo->add1Unchecked(ADDRESS_SIZE);
	debugInfo->add4Unchecked(debugAbbrevOffset);

	addUleb(debugInfo, ABBREV_COMPILE_UNIT);
	debugInfo->addNullTerminatedString(compilerFiles[1]->name);

	debugInfo->ensure(256);
	debugInfo->addPointerRelocation(textSectionSymbolIndex);
	compileUnitHighPcOffset = debugInfo->totalSize;
	debugInfo->add8Unchecked(0);
	debugInfo->add4Unchecked(debugLineOffset);

	addUleb(debugInfo, ABBREV_NULL);
}

void emitCodeViewEpilogue() {
	*debugLinesLength = debugLines->totalSize - debugLinesLengthOffset;
	*debugInfoLength = debugInfo->totalSize - debugInfoLengthOffset;

	debugInfo->addPointerRelocation(textSectionSymbolIndex, compileUnitHighPcOffset, code->totalSize);
}

void emitLocalDeclaration(String name, Type *type, u64 offset) {
	(void)name;
	(void)type;
	(void)offset;
}

void emitGlobalDeclaration(Declaration *declaration) {
	(void)declaration;
}

EmitFunctionInfo emitFunctionBegin(ExprFunction *function, u32 stackSpace) {
	(void)function;
	(void)stackSpace;
	return {};
}

void emitFunctionPushRdi(EmitFunctionInfo *info) {
	(void)info;
}

void emitFunctionPushRsi(EmitFunctionInfo *info) {
	(void)info;
}

void emitFunctionPreambleEnd(EmitFunctionInfo *info) {
	(void)info;
}

void emitFunctionPostambleStart(EmitFunctionInfo *info) {
	(void)info;
}

void emitFunctionEnd(EmitFunctionInfo *info) {
	(void)info;
}

void addLineInfo(CodeLocation start, EndLocation end) {
	(void)start;
	(void)end;

	debugLines->ensure(32);

	if (start.fileUid != lastLineLocation.fileUid) {
		debugLines->add1Unchecked(DW_LNS_set_file);

		u32 emitId = start.fileUid;

		if (emitId < 2) {
			emitId = !emitId;
		}

		addUleb(debugLines, emitId);
	}

	if (start.line != lastLineLocation.line) {
		debugLines->add1Unchecked(DW_LNS_advance_line);
		addSleb(debugLines, (s64)start.line - (s64)lastLineLocation.line);
	}

	if (code->totalSize != lastLineAddress) {
		debugLines->add1Unchecked(DW_LNS_fixed_advance_pc);
		debugLines->add2Unchecked(code->totalSize - lastLineAddress);
	}

	debugLines->add1Unchecked(DW_LNS_copy);

	lastLineLocation = start;
	lastLineAddress = code->totalSize;
}

void emitBlockStart(EmitFunctionInfo *info) {
	(void)info;
}

void emitBlockEnd(EmitFunctionInfo *info) {
	(void)info;
}

#endif