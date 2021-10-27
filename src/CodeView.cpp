#include "Basic.h"

#include "Ast.h"
#include "CodeView.h"

struct CodeViewTypeInfo {
	u32 id : 31 = 0;
	bool forwardDeclaration : 1 = false;
};

BucketedArenaAllocator *debugTypes;

static u32 debugTypeId = 0x1000;

struct DebugLeaf {
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

void alignDebugTypes(BucketedArenaAllocator *debugTypes) {
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
		debugTypes->add2Unchecked(0x1601); // LF_FUNC_ID
		debugTypes->add4Unchecked(0);
		debugTypes->add4Unchecked(funcType);
		debugTypes->addNullTerminatedString(name);
	};
}


void addStructUniqueNumber(BucketedArenaAllocator *debugTypes, u32 name = debugTypeId) {
	char buffer[32];

	_itoa(static_cast<int>(name - 0x1000), buffer, 16);

	debugTypes->addNullTerminatedString(buffer);
}

bool appendCoffName(BucketedArenaAllocator *debugSymbols, Type *type) {
	if (type->flavor == TypeFlavor::STRUCT || type->flavor == TypeFlavor::ENUM) {
		auto structure = static_cast<TypeStruct *>(type);

		if (structure->enclosingScope->flavor == BlockFlavor::STRUCT) {
			appendCoffName(debugSymbols, CAST_FROM_SUBSTRUCT(TypeStruct, members, structure->enclosingScope));
			debugSymbols->addString("::");
		}
	}

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
	else if (type->flags & TYPE_IS_ANONYMOUS) {
		debugSymbols->addString("<unnamed-");
		debugSymbols->addString(type->name);
		debugSymbols->add1('-');
		addStructUniqueNumber(debugSymbols, getCoffTypeIndex(type));
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
			debugTypes->add1('@');
			addStructUniqueNumber(debugTypes);
		};
	}
	else if (type->flavor == TypeFlavor::ARRAY) {
		auto array = static_cast<TypeArray *>(type);

		if (array->flags & TYPE_ARRAY_IS_FIXED) {
			u32 arrayOf = createType(array->arrayOf);

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
			u32 dataType = createType(getDeclarationType(array->members.declarations[0]));

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
				debugTypes->add1('@');
				addStructUniqueNumber(debugTypes);
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
				debugTypes->add2Unchecked(0x1002); // LF_POINTER
				debugTypes->add4Unchecked(getCoffTypeIndex(function->returnTypes[i]));
				debugTypes->add4Unchecked(0x1000c); // 64 bit normal pointer
			};
		}

		u32 argCount = extraParams + function->argumentCount + function->returnCount - 1;

		u32 argList = DEBUG_LEAF{
			debugTypes->ensure(6 + argCount * 4);
			debugTypes->add2Unchecked(0x1201); // LF_ARGLIST
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
			debugTypes->add2Unchecked(0x1008); // LF_PROCEDURE
			debugTypes->add4Unchecked(getCoffTypeIndex(function->returnTypes[0]));
			debugTypes->add1Unchecked(0); // C near call
			debugTypes->add1Unchecked(0);
			debugTypes->add2Unchecked(argCount);
			debugTypes->add4Unchecked(argList);
		};

		return DEBUG_LEAF{
			debugTypes->ensure(10);
			debugTypes->add2Unchecked(0x1002); // LF_POINTER
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

				for (auto declaration : enumeration->members.declarations) {
					if (!(declaration->flags & DECLARATION_IS_ENUM_VALUE))
						continue;

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

			// Enum naming for anonymous enums needs the type id to be set
			codeView->id = debugTypeId;

			return DEBUG_LEAF{
				debugTypes->ensure(20);
				debugTypes->add2Unchecked(0x1505); // LF_STRUCTURE
				debugTypes->add2Unchecked(flagCount);
				debugTypes->add2Unchecked(0x200 | nested); // Has a unique name
				debugTypes->add4Unchecked(fieldList); // field list
				debugTypes->add4Unchecked(0); // super class
				debugTypes->add4Unchecked(0); // vtable
				debugTypes->add2Unchecked(enumeration->size);
				appendCoffName(debugTypes, enumeration);
				debugTypes->add1(0);
				debugTypes->add1('@');
				addStructUniqueNumber(debugTypes);
			};
		}
		else {
			u32 fieldList = DEBUG_LEAF{
				debugTypes->add4(0x1203); // LF_FIELDLIST

				for (auto declaration : enumeration->members.declarations) {
					if (!(declaration->flags & DECLARATION_IS_ENUM_VALUE))
						continue;

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

			// Enum naming for anonymous enums needs the type id to be set
			codeView->id = debugTypeId;

			return DEBUG_LEAF{
				debugTypes->ensure(14);
				debugTypes->add2Unchecked(0x1507); // LF_ENUM
				debugTypes->add2Unchecked(static_cast<u16>(enumeration->members.declarations.count - ENUM_SPECIAL_MEMBER_COUNT));
				debugTypes->add2Unchecked(0x200); // Has a unique name
				debugTypes->add4Unchecked(integerType);
				debugTypes->add4Unchecked(fieldList);
				appendCoffName(debugTypes, enumeration);
				debugTypes->add1(0);
				debugTypes->add1('@');
				addStructUniqueNumber(debugTypes);
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
			debugTypes->add1('@');
			addStructUniqueNumber(debugTypes);
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
			debugTypes->add2(0x1203); // LF_FIELDLIST

			auto struct_ = static_cast<TypeStruct *>(type);

			u16 hasNested = 0;

			for (auto member : struct_->members.declarations) {
				if (member->flags & DECLARATION_IMPORTED_BY_USING) continue;

				auto type = getDeclarationType(member);

				if (member->flags & DECLARATION_IS_CONSTANT) {
					if (type != &TYPE_TYPE)
						continue;

					auto nestType = static_cast<ExprLiteral *>(member->initialValue)->typeValue;

					alignDebugTypes(debugTypes);
					debugTypes->ensure(6);
					debugTypes->add2(0x1510); // LF_NESTTYPE
					debugTypes->add2(0);
					debugTypes->add4(getCoffTypeIndex(nestType));
					debugTypes->addNullTerminatedString(member->name);
					hasNested = 0x10;
					memberCount++;
				}
				else {
					alignDebugTypes(debugTypes);
					debugTypes->ensure(10);
					debugTypes->add2Unchecked(0x150D); // LF_MEMBER
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
		u16 nested = structure->enclosingScope->flavor == BlockFlavor::STRUCT ? 8 : 0;

		// Struct naming uses the type id, so if we don't have one created by a forward declaration, assign the type id
		if (!codeView->id) {
			codeView->id = debugTypeId;
		}

		return DEBUG_LEAF{
			debugTypes->ensure(20);
			debugTypes->add2Unchecked(isUnion ? 0x1506 : 0x1505); //LF_UNION : LF_STRUCTURE
			debugTypes->add2Unchecked(memberCount);
			debugTypes->add2Unchecked(0x200 | packed); // Has a unique name
			debugTypes->add4Unchecked(fieldList); // field list
			debugTypes->add4Unchecked(0); // super class
			debugTypes->add4Unchecked(0); // vtable
			debugTypes->add2Unchecked(structure->size);
			appendCoffName(debugTypes, structure);
			debugTypes->add1(0);
			debugTypes->add1('@');
			addStructUniqueNumber(debugTypes, codeView->id);
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
		debugTypes->add2Unchecked(0x1505); // LF_STRUCTURE
		debugTypes->add2Unchecked(0);

		debugTypes->add2Unchecked(0x280 | packed | nested); // Has a unique name and is a forward declaration
		debugTypes->add4Unchecked(0); // field list
		debugTypes->add4Unchecked(0); // super class
		debugTypes->add4Unchecked(0); // vtable
		debugTypes->add2Unchecked(0); // size
		appendCoffName(debugTypes, structure);
		debugTypes->add1(0);
		debugTypes->add1('@');
		addStructUniqueNumber(debugTypes);
	};

	codeView->forwardDeclaration = true;

	return codeView->id;
}

u32 getCoffTypeIndex(Type *type) {
	auto id = codeViewTypeTable[findInTypeTable(type)].id;
	assert(id);
	return id;
}


void exportTypeTableToDebugTSection(BucketedArenaAllocator *debugTypes_) {
	PROFILE_FUNC();
	debugTypes = debugTypes_;
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