#include "Basic.h"

#ifndef BUILD_NO_LLVM

#include "CoffWriter.h"
#include "Lexer.h"
#include "IrGenerator.h"
#include "Error.h"

static llvm::Type *createLlvmType(llvm::LLVMContext &context, Type *type);

static llvm::Type *getLlvmType(llvm::LLVMContext &context, Type *type) {
	if (!type->llvmType)
		type->llvmType = createLlvmType(context, type);

	return type->llvmType;
}


static llvm::DIFile **files;
static llvm::DICompileUnit *compileUnit;
static llvm::DIBuilder *dib;


extern bool isStandardSize(u64 size);

llvm::StringRef stringRef(const String &string) {
	return llvm::StringRef(string.characters, string.length);
}

template<typename T>
llvm::ArrayRef<T> arrayRef(Array<T> array) {
	return { array.begin(), array.end() };
}

template<typename T>
llvm::ArrayRef<T> arrayRef(std::initializer_list<T> array) {
	auto data = new T[array.end() - array.begin()];
	std::copy(array.begin(), array.end(), data);

	return { data, (size_t) (array.end() - array.begin()) };
}

static llvm::Type *createLlvmType(llvm::LLVMContext &context, Type *type) {
	if (type == &TYPE_VOID) {
		return llvm::Type::getVoidTy(context);
	}
	else if (type == &TYPE_BOOL) {
		return llvm::Type::getInt1Ty(context);
	}
	else if (type == &TYPE_U8 || type == &TYPE_S8) {
		return llvm::Type::getInt8Ty(context);
	}
	else if (type == &TYPE_U16 || type == &TYPE_S16) {
		return llvm::Type::getInt16Ty(context);
	}
	else if (type == &TYPE_U32 || type == &TYPE_S32) {
		return llvm::Type::getInt32Ty(context);
	}
	else if (type == &TYPE_U64 || type == &TYPE_S64 || type == &TYPE_UNSIGNED_INT_LITERAL || type == &TYPE_SIGNED_INT_LITERAL) {
		return llvm::Type::getInt64Ty(context);
	}
	else if (type == &TYPE_F32) {
		return llvm::Type::getFloatTy(context);
	}
	else if (type == &TYPE_F64) {
		return llvm::Type::getDoubleTy(context);
	}
	else if (type == TYPE_VOID_POINTER || type == &TYPE_TYPE) {
		return llvm::Type::getInt8PtrTy(context);
	}
	else if (type == &TYPE_STRING) {
		return llvm::StructType::get(llvm::Type::getInt8PtrTy(context), llvm::Type::getInt64Ty(context));
	}
	else if (type->flavor == TypeFlavor::POINTER) {
		return llvm::PointerType::getUnqual(getLlvmType(context, static_cast<TypePointer *>(type)->pointerTo));
	}
	else if (type->flavor == TypeFlavor::ENUM) {
		return getLlvmType(context, static_cast<TypeEnum *>(type)->integerType);
	}
	else if (type->flavor == TypeFlavor::ARRAY) {
		auto array = static_cast<TypeArray *>(type);

		if (array->flags & TYPE_ARRAY_IS_FIXED) {
			return llvm::ArrayType::get(getLlvmType(context, array->arrayOf), array->count);
		}
		else {
			auto pointer = llvm::PointerType::getUnqual(getLlvmType(context, array->arrayOf));
			auto int64 = llvm::Type::getInt64Ty(context);


			if (array->flags & TYPE_ARRAY_IS_DYNAMIC) {
				return llvm::StructType::get(pointer, int64, int64);
			}
			else {
				return llvm::StructType::get(pointer, int64);
			}
		}
	}
	else if (type->flavor == TypeFlavor::STRUCT) {
		auto struct_ = static_cast<TypeStruct *>(type);
		llvm::StructType *llvmType = llvm::StructType::create(context, stringRef(struct_->name));

		type->llvmType = llvmType; // Put the empty struct on the type so that if we have a struct that points to itself we don't infinitely recurse

		if (type->flags & TYPE_STRUCT_IS_UNION) {
			return llvm::StructType::create(stringRef(struct_->name), llvm::ArrayType::get(llvm::IntegerType::get(context, type->alignment * 8), type->size / type->alignment));
		}
		else {

			u32 count = 0;
			for (auto member : struct_->members.declarations) {
				if (member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING))
					continue;

				++count;
			}

			Array<llvm::Type *> body(count);

			for (auto member : struct_->members.declarations) {
				if (member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING))
					continue;

				body.add(getLlvmType(context, getDeclarationType(member)));
			}

			llvmType->setBody(arrayRef(body), struct_->flags & TYPE_STRUCT_IS_PACKED ? true : false);

			return llvmType;
		}
	}
	else if (type->flavor == TypeFlavor::FUNCTION) {
		auto function = static_cast<TypeFunction *>(type);

		// LLVM doesn't support multiple return values, so the extra return values are passed by pointer after the other arguments


		u32 paramOffset = 0;
		auto return_ = function->returnTypes[0];

		if (!isStandardSize(return_->size)) {
			paramOffset++;
		}

		if (!(function->flags & TYPE_FUNCTION_IS_C_CALL)) {
			paramOffset++;
		}

		u32 count = function->argumentCount + function->returnCount + paramOffset - 1;

		llvm::Type **arguments = new llvm::Type * [count];

		paramOffset = 0;

		if (!isStandardSize(return_->size)) {
			arguments[paramOffset++] = llvm::PointerType::getUnqual(getLlvmType(context, return_));
		}

		if (!(function->flags & TYPE_FUNCTION_IS_C_CALL)) {
			arguments[paramOffset++] = llvm::PointerType::getUnqual(getLlvmType(context, &TYPE_CONTEXT));
		}

		for (u64 i = 0; i < function->argumentCount; i++) {
			auto argument = function->argumentTypes[i];

			if (!isStandardSize(argument->size)) {
				arguments[i + paramOffset] = llvm::PointerType::getUnqual(getLlvmType(context, argument));
			}
			else if (argument->flavor == TypeFlavor::STRUCT || argument->flavor == TypeFlavor::ARRAY) {
				arguments[i + paramOffset] = llvm::IntegerType::get(context, argument->size * 8);
			}
			else {
				arguments[i + paramOffset] = getLlvmType(context, argument);
			}
		}

		for (u64 i = 1; i < function->returnCount; i++) {
			arguments[function->argumentCount + paramOffset + i - 1] = llvm::PointerType::getUnqual(getLlvmType(context, function->returnTypes[i]));
		}

		llvm::Type *retType;

		if (return_ == &TYPE_VOID || !isStandardSize(return_->size)) {
			retType = llvm::Type::getVoidTy(context);
		}
		else if (return_->flavor == TypeFlavor::STRUCT || return_->flavor == TypeFlavor::ARRAY) {
			retType = llvm::IntegerType::get(context, return_->size * 8);
		}
		else {
			retType = getLlvmType(context, return_);
		}


		return llvm::PointerType::getUnqual(llvm::FunctionType::get(retType, llvm::ArrayRef(arguments, arguments + count), false));
	}
	else {
		assert(false);
		return nullptr;
	}
}

struct State {
	llvm::LLVMContext &context;
	llvm::IRBuilder<> &builder;
	llvm::Module &module;
	llvm::Function *function;
	llvm::BasicBlock *entryBlock;
	llvm::Value *contextValue;
	Array<llvm::Metadata *> debugScopeStack;
};


static llvm::DIType *createLlvmDebugType(Type *type);

static llvm::DIType *getLlvmDebugType(Type *type) {
	if (!type->llvmDebugType) {
		type->llvmDebugType = createLlvmDebugType(type);
	}

	return type->llvmDebugType;
}

static llvm::DIType *createMemberType(Declaration *declaration, llvm::DIScope *enclosing) {
	auto type = getDeclarationType(declaration);

	return dib->createMemberType(enclosing, stringRef(declaration->name), files[declaration->start.fileUid], declaration->start.line,
		type->size * 8, type->alignment * 8, declaration->physicalStorage * 8, llvm::DINode::DIFlags::FlagZero, getLlvmDebugType(type));
}

static llvm::DIType *createLlvmDebugType(Type *type) {
	
	if (type == &TYPE_VOID) {
		return nullptr; // ??? LLVM debug metadata represents void as null !!!
	}
	else if (type == &TYPE_BOOL) {
		return dib->createBasicType("bool", 8, llvm::dwarf::DW_ATE_boolean);
	}
	else if (type->flavor == TypeFlavor::INTEGER) {
		return dib->createBasicType(stringRef(type->name), type->size * 8, type->flags & TYPE_INTEGER_IS_SIGNED ? llvm::dwarf::DW_ATE_signed : llvm::dwarf::DW_ATE_unsigned);
	}
	else if (type->flavor == TypeFlavor::FLOAT) {
		return dib->createBasicType(stringRef(type->name), type->size * 8, llvm::dwarf::DW_ATE_float);
	}
	else if (type == &TYPE_TYPE) {
		auto structType = dib->createStructType(nullptr, "type", files[0], 0, type->size * 8, type->alignment * 8, llvm::DINode::DIFlags::FlagTypePassByValue, nullptr,
			llvm::DINodeArray());
		type->llvmDebugType = structType;

		auto memberType = dib->createMemberType(type->llvmDebugType, "value", files[0], 0, type->size * 8, type->alignment * 8, 0, llvm::DINode::DIFlags::FlagZero, 
			getLlvmDebugType(TYPE_VOID_POINTER));

		dib->replaceArrays(structType, dib->getOrCreateArray(arrayRef<llvm::Metadata *>({ memberType })));

		return structType;
    }
	else if (type == &TYPE_STRING) {
		auto structType = dib->createStructType(nullptr, "string", files[0], 0, type->size * 8, type->alignment * 8, llvm::DINode::DIFlags::FlagTypePassByValue, nullptr,
			llvm::DINodeArray());
		type->llvmDebugType = structType;

		auto dataType = createMemberType(TYPE_STRING.members.declarations[0], structType);
		auto countType = createMemberType(TYPE_STRING.members.declarations[1], structType);

		dib->replaceArrays(structType, dib->getOrCreateArray(arrayRef<llvm::Metadata *>({dataType, countType })));

		return structType;
	}
	else if (type->flavor == TypeFlavor::POINTER) {
		return dib->createPointerType(getLlvmDebugType(static_cast<TypePointer *>(type)->pointerTo), type->size * 8);
	}
	else if (type->flavor == TypeFlavor::ENUM) {
		auto enum_ = static_cast<TypeEnum *>(type);
		auto integerType = getLlvmDebugType(enum_->integerType);

		if (enum_->flags & TYPE_ENUM_IS_FLAGS) {
			auto structType = dib->createStructType(nullptr, stringRef(enum_->name), files[0], 0,
				enum_->size * 8, enum_->alignment * 8, llvm::DINode::DIFlags::FlagTypePassByValue, nullptr,
				llvm::DINodeArray());
			type->llvmDebugType = structType;

			Array<llvm::Metadata *> members;

			for (auto declaration : enum_->members.declarations) {
				if (!(declaration->flags & DECLARATION_IS_ENUM_VALUE))
					continue;

				assert(declaration->initialValue->flavor == ExprFlavor::INT_LITERAL);
				assert(!(declaration->initialValue->type->flags & TYPE_INTEGER_IS_SIGNED));

				u64 value = static_cast<ExprLiteral *>(declaration->initialValue)->unsignedValue;

				if (value && !(value & value - 1)) { // If exactly one bit is set
					unsigned long bit;

					_BitScanForward64(&bit, value);

					members.add(dib->createMemberType(structType, stringRef(declaration->name), files[declaration->start.fileUid], declaration->start.line,
						1, 1, bit, llvm::DINode::DIFlags::FlagBitField, integerType));
				}
			}

			dib->replaceArrays(structType, dib->getOrCreateArray(arrayRef(members)));
			return structType;
		}
		else {
			Array<llvm::Metadata *> members;


			for (auto declaration : enum_->members.declarations) {
				if (!(declaration->flags & DECLARATION_IS_ENUM_VALUE))
					continue;

				assert(declaration->initialValue->flavor == ExprFlavor::INT_LITERAL);
				assert(!(declaration->initialValue->type->flags & TYPE_INTEGER_IS_SIGNED));

				auto value = static_cast<ExprLiteral *>(declaration->initialValue)->unsignedValue;

				members.add(dib->createEnumerator(stringRef(declaration->name), value, true));
			}

			auto enumType = dib->createEnumerationType(nullptr, stringRef(enum_->name), files[0], 0, enum_->size * 8, enum_->alignment * 8,
				dib->getOrCreateArray(arrayRef(members)), integerType);

			return enumType;
		}
	}
	else if (type->flavor == TypeFlavor::ARRAY) {
		auto array = static_cast<TypeArray *>(type);

		if (array->flags & TYPE_ARRAY_IS_FIXED) {
			auto arrayOf = getLlvmDebugType(array->arrayOf);

			return dib->createArrayType(array->size * 8, array->alignment * 8, arrayOf,
				dib->getOrCreateArray(arrayRef<llvm::Metadata *>({ dib->getOrCreateSubrange(0, array->count) })));
		}
		else {
			auto structType = dib->createStructType(nullptr, stringRef(type->name), files[0], 0,
				type->size * 8, type->alignment * 8, llvm::DINode::DIFlags::FlagTypePassByValue, nullptr,
				llvm::DINodeArray());
			type->llvmDebugType = structType;

			auto dataType = createMemberType(array->members.declarations[0], structType);
			auto countType = createMemberType(array->members.declarations[1], structType);

			if (array->flags & TYPE_ARRAY_IS_DYNAMIC) {
				auto capacityType = createMemberType(array->members.declarations[2], structType);
				dib->replaceArrays(structType, dib->getOrCreateArray(arrayRef<llvm::Metadata *>({ dataType, countType, capacityType })));
			}
			else {
				dib->replaceArrays(structType, dib->getOrCreateArray(arrayRef<llvm::Metadata *>({ dataType, countType })));
			}

			return structType;
		}
	}
	else if (type->flavor == TypeFlavor::STRUCT) {
		auto struct_ = static_cast<TypeStruct *>(type);

		auto structType = dib->createStructType(nullptr, stringRef(type->name), files[0], 0,
			type->size * 8, type->alignment * 8, llvm::DINode::DIFlags::FlagTypePassByValue, nullptr,
			llvm::DINodeArray());
		type->llvmDebugType = structType;

		Array<llvm::Metadata *> members;

		for (auto member : struct_->members.declarations) {
			if (member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING))
				continue;

			members.add(createMemberType(member, structType));
		}

		dib->replaceArrays(structType, dib->getOrCreateArray(arrayRef(members)));

		return structType;
	}
	else if (type->flavor == TypeFlavor::FUNCTION) {
		auto function = static_cast<TypeFunction *>(type);

		// LLVM doesn't support multiple return values, so the extra return values are passed by pointer after the other arguments


		u32 paramOffset = 1;
		auto return_ = function->returnTypes[0];

		if (!(function->flags & TYPE_FUNCTION_IS_C_CALL)) {
			paramOffset++;
		}

		u32 count = function->argumentCount + function->returnCount + paramOffset - 1;

		llvm::Metadata **arguments = new llvm::Metadata * [count];

		arguments[0] = getLlvmDebugType(function->returnTypes[0]);

		paramOffset = 1;

		if (!(function->flags & TYPE_FUNCTION_IS_C_CALL)) {
			//arguments[paramOffset++] = getLlvmDebugType(&TYPE_CONTEXT);
		}

		for (u64 i = 0; i < function->argumentCount; i++) {
			auto argument = function->argumentTypes[i];

			//arguments[i + paramOffset] = getLlvmDebugType(argument);
		}

		for (u64 i = 1; i < function->returnCount; i++) {
			//arguments[function->argumentCount + paramOffset + i - 1] = getLlvmDebugType(function->returnTypes[i]);
		}

		//auto functionType = dib->createSubroutineType(dib->getOrCreateTypeArray(llvm::ArrayRef(arguments, count)));
		auto functionType = dib->createSubroutineType(dib->getOrCreateTypeArray(arrayRef<llvm::Metadata *>({ nullptr })));

		return dib->createPointerType(functionType, function->size * 8);
	}
	

	assert(false);
	return nullptr;
}

llvm::Constant *createLlvmString(State *state, String string) {
	return llvm::ConstantStruct::get(static_cast<llvm::StructType *>(getLlvmType(state->context, &TYPE_STRING)),
		state->builder.CreateGlobalStringPtr(stringRef(string)), llvm::ConstantInt::get(llvm::Type::getInt64Ty(state->context), string.length));
}

llvm::Value *generateLlvmIr(State *state, Expr *expr);

static llvm::GlobalVariable *createUnnnamedConstant(State *state, llvm::Type *type) {
	auto constant = new llvm::GlobalVariable(state->module, type, true, llvm::GlobalValue::PrivateLinkage, nullptr);

	constant->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

	return constant;
}

static llvm::GlobalVariable *createTypeInfoVariable(State *state, Type *type) {
	if (type == &TYPE_UNSIGNED_INT_LITERAL) {
		return createTypeInfoVariable(state, &TYPE_U64);
	}

	if (type == &TYPE_SIGNED_INT_LITERAL) {
		return createTypeInfoVariable(state, &TYPE_S64);
	}

	if (type == &TYPE_FLOAT_LITERAL) {
		return createTypeInfoVariable(state, &TYPE_F64);
	}

	if (type->llvmStorage) {
		return type->llvmStorage;
	}

	Type *typeInfoType = nullptr;

	switch (type->flavor) {
	case TypeFlavor::ARRAY:
		typeInfoType = TYPE_TYPE_INFO_ARRAY;
		break;
	case TypeFlavor::ENUM:
		typeInfoType = TYPE_TYPE_INFO_ENUM;
		break;
	case TypeFlavor::FUNCTION:
		typeInfoType = TYPE_TYPE_INFO_FUNCTION;
		break;
	case TypeFlavor::INTEGER:
		typeInfoType = TYPE_TYPE_INFO_INTEGER;
		break;
	case TypeFlavor::POINTER:
		typeInfoType = TYPE_TYPE_INFO_POINTER;
		break;
	case TypeFlavor::STRUCT:
		typeInfoType = TYPE_TYPE_INFO_STRUCT;
		break;
	case TypeFlavor::TYPE:
	case TypeFlavor::VOID:
	case TypeFlavor::STRING:
	case TypeFlavor::FLOAT:
	case TypeFlavor::BOOL:
		typeInfoType = TYPE_TYPE_INFO;
		break;
	default:
		assert(false);
	}
	
	type->llvmStorage = createUnnnamedConstant(state, getLlvmType(state->context, typeInfoType));

	return type->llvmStorage;
}

static llvm::Constant *createIntLiteral(State *state, ExprLiteral *literal) {
	if (literal->unsignedValue == 0) {
		return llvm::Constant::getNullValue(getLlvmType(state->context, literal->type));
	}

	if (literal->type->flavor == TypeFlavor::POINTER) {
		return llvm::ConstantExpr::getIntToPtr(
			llvm::ConstantInt::get(llvm::Type::getInt64Ty(state->context), literal->unsignedValue),
			getLlvmType(state->context, literal->type));
	}

	return llvm::ConstantInt::get(getLlvmType(state->context, literal->type), literal->unsignedValue);
}

static llvm::Constant *createConstant(State *state, Expr *expr) {

	switch (expr->flavor) {
	case ExprFlavor::INT_LITERAL: {
		auto literal = static_cast<ExprLiteral *>(expr);

		return createIntLiteral(state, literal);
	}
	case ExprFlavor::STRING_LITERAL: {
		return createLlvmString(state, static_cast<ExprStringLiteral *>(expr)->string);
	}
	case ExprFlavor::FLOAT_LITERAL:
	case ExprFlavor::FUNCTION:
	case ExprFlavor::TYPE_LITERAL: {
		return static_cast<llvm::Constant *>(generateLlvmIr(state, expr));
	}
	case ExprFlavor::STRUCT_LITERAL: {
		auto literal = static_cast<ExprStructLiteral *>(expr);
		auto struct_ = static_cast<TypeStruct *>(literal->type);

		auto llvmType = static_cast<llvm::StructType *>(getLlvmType(state->context, literal->type));

		if (literal->type->flags & TYPE_STRUCT_IS_UNION) {
			if (literal->initializers.count) {
				assert(literal->initializers.count == 1);
				auto value = literal->initializers.values[0];

				if (value->type->size == literal->type->size) {
					return createConstant(state, value);

				} 
				else {
					auto memberType = getLlvmType(state->context, value->type);
					auto arrayType = llvm::ArrayType::get(llvm::Type::getInt8Ty(state->context), struct_->size - value->type->size);

					auto tempType = llvm::StructType::get(memberType, arrayType);

					// :Sadge LLVM doesn't allow bitcasts for aggregate types so to return a constant union
					// we may not be able to return the correct type. Now everywhere that calls createConstant has to handle this :(
					return llvm::ConstantStruct::get(tempType, createConstant(state, value), llvm::UndefValue::get(arrayType));
				}

			}
			else {
				return llvm::UndefValue::get(llvmType);
			}
		}
		else {
			u64 memberCount = 0;

			for (auto decl : struct_->members.declarations) {
				if (decl->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING)) continue;

				++memberCount;
			}

			llvm::Constant **values = new llvm::Constant * [memberCount] {};

			u32 memberIndex = 0;

			bool typesAreCorrect = true;

			for (u32 i = 0; i < literal->initializers.count; i++) {
				u32 memberIndex = getDeclarationIndex(&struct_->members, literal->initializers.declarations[i], DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING);

				values[memberIndex] = createConstant(state, literal->initializers.values[i]);

				// :Sadge
				if (values[memberIndex]->getType() != getLlvmType(state->context, literal->initializers.values[i]->type)) {
					typesAreCorrect = false;
				}
			}

			for (u32 i = 0; i < memberCount; i++) {
				if (!values[i]) {
					values[i] = llvm::UndefValue::get(llvmType->getStructElementType(i));
				}
			}

			if (!typesAreCorrect) {
				auto types = new llvm::Type * [memberCount];

				for (u32 i = 0; i < memberCount; i++) {
					types[i] = values[i]->getType();
				}

				llvmType = llvm::StructType::get(state->context, llvm::ArrayRef(types, memberCount));
			}

			return llvm::ConstantStruct::get(llvmType, llvm::ArrayRef(values, memberCount));
		}
	}
	case ExprFlavor::ARRAY_LITERAL: {

		auto array = static_cast<ExprArrayLiteral *>(expr);

		auto arrayType = static_cast<TypeArray *>(array->type);
		assert(arrayType->flags &TYPE_ARRAY_IS_FIXED);

		u32 arrayCount = (arrayType->flags & TYPE_ARRAY_IS_FIXED) ? arrayType->count : array->count;

		llvm::Constant **values = new llvm::Constant * [arrayType->count];

		bool typesAreCorrect = true;

		auto elementType = getLlvmType(state->context, arrayType->arrayOf);
		for (u64 i = 0; i < arrayCount; i++) {
			auto value = createConstant(state, array->values[i]);
			values[i] = value;

			// :Sadge
			if (value->getType() != elementType) {
				typesAreCorrect = false;
			}

			if (i + 1 == array->count && arrayCount > array->count) {
				for (u64 j = i + 1; j < arrayCount; j++) {
					values[j] = value;
				}

				break;
			}
		}

		llvm::Constant *data;
		
		if (typesAreCorrect) {
			data = llvm::ConstantArray::get(llvm::ArrayType::get(elementType, arrayCount), llvm::ArrayRef(values, arrayCount));
		}
		else {
			// :Sadge

			auto types = new llvm::Type * [arrayCount];

			for (u64 i = 0; i < arrayCount; i++) {
				types[i] = values[i]->getType();
			}

			auto structType = llvm::StructType::get(state->context, llvm::ArrayRef(types, arrayCount));

			data = llvm::ConstantStruct::get(structType, llvm::ArrayRef(values, arrayCount));
		}

		if (arrayType->flags & TYPE_ARRAY_IS_FIXED) {
			return data;
		}
		else {
			auto dataPointer = createUnnnamedConstant(state, data->getType());
			dataPointer->setInitializer(data);

			auto correctType = static_cast<llvm::StructType *>(getLlvmType(state->context, arrayType));

			llvm::Constant *dataPointerCasted = dataPointer;
			if (!typesAreCorrect) {
				dataPointerCasted = llvm::ConstantExpr::getBitCast(dataPointer->getInitializer(), correctType->getStructElementType(0));
			}

			return llvm::ConstantStruct::get(correctType, 
				llvm::ConstantExpr::getGetElementPtr(correctType->getStructElementType(0)->getPointerTo(), dataPointer, arrayRef<llvm::Value *>({
						llvm::ConstantInt::get(llvm::Type::getInt32Ty(state->context), 0), 
						llvm::ConstantInt::get(llvm::Type::getInt32Ty(state->context), 0)
					})), 
				llvm::ConstantInt::get(llvm::Type::getInt64Ty(state->context), arrayCount));
		}
	}
	}

	assert(false);
	return nullptr;
}

static llvm::GlobalVariable *createLlvmGlobal(State *state, Declaration *declaration) {
	if (!declaration->llvmStorage) {
		auto llvmType = getLlvmType(state->context, getDeclarationType(declaration));

		auto global = static_cast<llvm::GlobalVariable *>(state->module.getOrInsertGlobal(stringRef(declaration->name), 
			llvmType));

		declaration->llvmStorage = global;
	}

	return static_cast<llvm::GlobalVariable *>(declaration->llvmStorage);
}

struct Loop {
	struct ExprLoop *loop;
	llvm::BasicBlock *start;

	Array<llvm::BasicBlock *> endPatches;
};

static Array<Expr *> deferStack;
static Block *currentBlock;

static Array<Loop> loopStack;
static u32 loopCount;

static llvm::Value *allocateType(State *state, Type *type, String name = "") {
	auto old = state->builder.GetInsertBlock();

	state->builder.SetInsertPoint(state->entryBlock);
	auto value = state->builder.CreateAlloca(getLlvmType(state->context, type), nullptr, stringRef(name));

	state->builder.SetInsertPoint(old);

	return value;
}


static llvm::Value *allocateType(State *state, llvm::Type *type, String name = "") {
	auto old = state->builder.GetInsertBlock();

	state->builder.SetInsertPoint(state->entryBlock);
	auto value = state->builder.CreateAlloca(type, nullptr, stringRef(name));

	state->builder.SetInsertPoint(old);

	return value;
}

static llvm::Value *allocateAndStore(State *state, llvm::Value *value) {
	auto old = state->builder.GetInsertBlock();

	state->builder.SetInsertPoint(state->entryBlock);
	auto allocation = state->builder.CreateAlloca(value->getType());

	state->builder.SetInsertPoint(old);

	state->builder.CreateStore(value, allocation);

	return allocation;

}

static void pushLoop(State *state, ExprLoop *loop) {
	if (loopCount >= loopStack.count) {
		loopStack.add();
	}

	loopStack[loopCount].start = state->builder.GetInsertBlock();
	loopStack[loopCount].loop = loop;
	loopStack[loopCount].endPatches.clear();

	++loopCount;
}

static void popLoop(State *state) {
	--loopCount;

	Loop loop = loopStack[loopCount];

	auto end = state->builder.GetInsertBlock();

	for (auto patch : loop.endPatches) {
		state->builder.SetInsertPoint(patch);
		state->builder.CreateBr(end);
	}

	state->builder.SetInsertPoint(end);
}

llvm::Value *generateIrAndLoadIfStoredByPointer(State *state, Expr *expr) {
	auto value = generateLlvmIr(state, expr);

	if (isStoredByPointer(expr->type)) {
		return state->builder.CreateLoad(value);
	}

	return value;
}

llvm::Value *storeIfPointerType(State *state, Type *type, llvm::Value *value) {
	if (isStoredByPointer(type)) {
		return allocateAndStore(state, value);
	}

	return value;
}


static void generateIncrement(State *state, ExprLoop *loop) {
	auto it = loop->iteratorBlock.declarations[0];
	auto it_index = loop->iteratorBlock.declarations[1];

	state->builder.CreateStore(
		state->builder.CreateAdd(
			state->builder.CreateLoad(it_index->llvmStorage),
			llvm::ConstantInt::get(llvm::Type::getInt64Ty(state->context), 1)),
		it_index->llvmStorage);

	if (loop->forBegin->type->flavor != TypeFlavor::INTEGER) {
		state->builder.CreateStore(
			state->builder.CreateConstGEP1_64(state->builder.CreateLoad(loop->llvmPointer), 1), 
			loop->llvmPointer);
	}
	else {
		state->builder.CreateStore(
			state->builder.CreateAdd(
				state->builder.CreateLoad(loop->llvmPointer), 
				llvm::ConstantInt::get(loop->llvmPointer->getType()->getPointerElementType(), 1)), 
			loop->llvmPointer);
	}
}

static void addLineMarker(State *state, Expr *expr) {
	state->builder.SetCurrentDebugLocation(llvm::DILocation::get(state->context, expr->start.line, expr->start.column, state->debugScopeStack.peek()));
}


static void exitBlock(State *state, Block *block, bool isBreak) {
	for (u32 i = deferStack.count; i-- != 0;) {
		auto expr = deferStack[i];

		if (expr->flavor == ExprFlavor::FOR) {
			auto loop = static_cast<ExprLoop *>(expr);

			auto current = &loop->iteratorBlock;
			bool found = false;

			if (block) {
				while (current) {
					if (current == block) {
						found = true;
						break;
					}

					current = current->parentBlock;
				}

				if (!found)
					break;
			}

			if (!isBreak) // Break statements shouldn't execute the increment
				generateIncrement(state, loop);
		}
		else {
			assert(expr->flavor == ExprFlavor::DEFER);

			auto defer = static_cast<ExprDefer *>(expr);

			if (block) {
				auto current = defer->enclosingScope;
				bool found = false;

				while (current) {
					if (current == block) {
						found = true;
						break;
					}

					current = current->parentBlock;
				}

				if (!found)
					break;
			}

			addLineMarker(state, defer);
			generateLlvmIr(state, defer->expr);
		}
	}
}

static Block externalsBlock;

static llvm::Function *createLlvmFunction(State *state, ExprFunction *function) {
	if (!function->llvmStorage) {
		auto type = getLlvmType(state->context, function->type);

		assert(type->isPointerTy());
		assert(type->getPointerElementType()->isFunctionTy());

		auto functionType = static_cast<llvm::FunctionType *>(type->getPointerElementType());

		auto linkage = llvm::Function::PrivateLinkage;
		auto name = llvm::StringRef("");

		if (function->valueOfDeclaration) {
			name = stringRef(function->valueOfDeclaration->name);
		}

		if (function == programStart || function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
			linkage = llvm::Function::ExternalLinkage;
		}

		function->llvmStorage = llvm::Function::Create(functionType, linkage, name, state->module);

		{
			auto type = static_cast<TypeFunction *>(function->type);
			u64 paramOffset = 0;

			if (!isStandardSize(type->returnTypes[0]->size)) {
				auto paramType = functionType->getParamType(paramOffset);
				function->llvmStorage->addParamAttrs(paramOffset, llvm::AttrBuilder()
					.addStructRetAttr(paramType->getPointerElementType())
					.addAttribute(llvm::Attribute::NoCapture)
					.addAttribute(llvm::Attribute::NoAlias));
				paramOffset++;
			}

			if (!(function->flags & EXPR_FUNCTION_IS_C_CALL)) {
				function->llvmStorage->addParamAttrs(paramOffset, llvm::AttrBuilder()
					.addAttribute(llvm::Attribute::NoCapture));
				paramOffset++;
			}

			for (u32 i = 0; i < type->argumentCount; i++) {
				if (!isStandardSize(type->argumentTypes[i]->size)) {
					function->llvmStorage->addParamAttrs(i + paramOffset, llvm::AttrBuilder()
						.addAttribute(llvm::Attribute::NoCapture)
						.addByRefAttr(functionType->getParamType(i + paramOffset)->getPointerElementType()));
				}
			}

			for (u32 i = 1; i < type->returnCount; i++) {
				if (!isStandardSize(type->returnTypes[i]->size)) {
					function->llvmStorage->addParamAttrs(i + paramOffset + type->argumentCount - 1, llvm::AttrBuilder()
						.addAttribute(llvm::Attribute::NoCapture));
				}
			}
		}

		function->llvmStorage->setCallingConv(llvm::CallingConv::Win64);
	}

	return function->llvmStorage;
}


llvm::CmpInst::Predicate getFCmp(TokenT op) {
	using namespace llvm;

	switch (op) {
	case TokenT::EQUAL:
		return CmpInst::Predicate::FCMP_OEQ;
	case TokenT::NOT_EQUAL:
		return CmpInst::Predicate::FCMP_ONE;
	case TOKEN('>'):
		return CmpInst::Predicate::FCMP_OGT;
	case TOKEN('<'):
		return CmpInst::Predicate::FCMP_OLT;
	case TokenT::GREATER_EQUAL:
		return CmpInst::Predicate::FCMP_OGE;
	case TokenT::LESS_EQUAL:
		return CmpInst::Predicate::FCMP_OLE;
	default:
		assert(false);
		return CmpInst::Predicate::FCMP_FALSE;
	}
}


llvm::CmpInst::Predicate getSCmp(TokenT op) {
	using namespace llvm;

	switch (op) {
	case TokenT::EQUAL:
		return ICmpInst::Predicate::ICMP_EQ;
	case TokenT::NOT_EQUAL:
		return ICmpInst::Predicate::ICMP_NE;
	case TOKEN('>'):
		return CmpInst::Predicate::ICMP_SGT;
	case TOKEN('<'):
		return CmpInst::Predicate::ICMP_SLT;
	case TokenT::GREATER_EQUAL:
		return CmpInst::Predicate::ICMP_SGE;
	case TokenT::LESS_EQUAL:
		return CmpInst::Predicate::ICMP_SLE;
	default:
		assert(false);
		return CmpInst::Predicate::ICMP_EQ;
	}
}


llvm::CmpInst::Predicate getUCmp(TokenT op) {
	using namespace llvm;

	switch (op) {
	case TokenT::EQUAL:
		return ICmpInst::Predicate::ICMP_EQ;
	case TokenT::NOT_EQUAL:
		return ICmpInst::Predicate::ICMP_NE;
	case TOKEN('>'):
		return CmpInst::Predicate::ICMP_UGT;
	case TOKEN('<'):
		return CmpInst::Predicate::ICMP_ULT;
	case TokenT::GREATER_EQUAL:
		return CmpInst::Predicate::ICMP_UGE;
	case TokenT::LESS_EQUAL:
		return CmpInst::Predicate::ICMP_ULE;
	default:
		assert(false);
		return CmpInst::Predicate::ICMP_EQ;
	}
}

llvm::Value *createGEPForStruct(State *state, llvm::Value *structPointer, TypeStruct *struct_, Declaration *member) {
	if (struct_->flags & TYPE_STRUCT_IS_UNION) {
		u64 offset = member->physicalStorage; // In the case of struct members, physicalStorage is the offset within the struct
		assert(offset == 0);

		return state->builder.CreatePointerCast(
			structPointer,
			llvm::PointerType::getUnqual(getLlvmType(state->context, getDeclarationType(member)))
		);
	}
	else {
		// @Speed currently we do a linear search through struct members to find 
		u32 memberIndex = getDeclarationIndex(&struct_->members, member, DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING);

		return state->builder.CreateStructGEP(structPointer, memberIndex);
	}
}

llvm::Value *loadAddressOf(State *state, Expr *expr) {
	if (expr->flavor == ExprFlavor::BINARY_OPERATOR && static_cast<ExprBinaryOperator *>(expr)->op == TOKEN('[')) {
		auto binary = static_cast<ExprBinaryOperator *>(expr);

		assert(binary->op == TOKEN('['));

		if (binary->left->type->flavor == TypeFlavor::POINTER) {
			return state->builder.CreateGEP(generateLlvmIr(state, binary->left), generateLlvmIr(state, binary->right));
		}
		else if (binary->left->type->flags & TYPE_ARRAY_IS_FIXED) {
			auto values = new llvm::Value * [2] {
				llvm::ConstantInt::get(llvm::Type::getInt64Ty(state->context), 0),
				generateLlvmIr(state, binary->right)
			};


			return state->builder.CreateGEP(
				loadAddressOf(state, binary->left),
				llvm::ArrayRef( values, 2));
		}
		else {
			assert(binary->left->type->flavor == TypeFlavor::ARRAY || binary->left->type->flavor == TypeFlavor::STRING);

			llvm::Value *dataPointer;

			auto value = generateLlvmIr(state, binary->left);

			dataPointer = state->builder.CreateLoad(state->builder.CreateStructGEP(value, 0));

			auto right = generateLlvmIr(state, binary->right);

			return state->builder.CreateGEP(dataPointer, right);
		}
	}
	else if (expr->flavor == ExprFlavor::UNARY_OPERATOR) {
		auto unary = static_cast<ExprUnaryOperator *>(expr);

		assert(unary->op == TokenT::SHIFT_LEFT);

		return generateLlvmIr(state, unary->value);
	}
	else if (expr->flavor == ExprFlavor::IDENTIFIER) {
		auto identifier = static_cast<ExprIdentifier *>(expr);

		if (identifier->structAccess) {
			auto type = identifier->structAccess->type;


			llvm::Value *store;

			if (identifier->structAccess->type->flavor == TypeFlavor::POINTER) {
				store = generateLlvmIr(state, identifier->structAccess);
			}
			else {
				store = loadAddressOf(state, identifier->structAccess);
			}

			assert(!(type->flags & TYPE_ARRAY_IS_FIXED));

			if (type->flavor == TypeFlavor::POINTER) {
				type = static_cast<TypePointer *>(type)->pointerTo;
			}
			
			return createGEPForStruct(state, store, static_cast<TypeStruct *>(type), identifier->declaration);
		}
		else {
			if (identifier->declaration->enclosingScope->flavor == BlockFlavor::GLOBAL) {
				createLlvmGlobal(state, identifier->declaration);
			}

			return identifier->declaration->llvmStorage;
		}
	}
	else {
		assert(expr->type->flavor == TypeFlavor::ARRAY || expr->type->flavor == TypeFlavor::STRING || expr->type->flavor == TypeFlavor::STRUCT);

		return generateLlvmIr(state, expr);
	}
}

static llvm::Value *generateLlvmCall(State *state, ExprFunctionCall *call, ExprCommaAssignment *comma) {
	auto function = static_cast<TypeFunction *>(call->function->type);

	if (call->function->flags & EXPR_FUNCTION_IS_INSTRINSIC) {
		if (call->function->valueOfDeclaration->name == "pop_count") {
			auto argumentType = function->argumentTypes[0];

			if (argumentType->flavor != TypeFlavor::INTEGER && argumentType->flavor != TypeFlavor::ENUM) {
				reportError(call->arguments.values[0], "Error: pop_count can only operate on integers or enums");
				return 0;
			}

			auto argument = generateLlvmIr(state, call->arguments.values[0]);

			return state->builder.CreateIntrinsic(llvm::Intrinsic::ctpop, arrayRef({ getLlvmType(state->context, argumentType) }), arrayRef({ argument }));
		}
		else if (call->function->valueOfDeclaration->name == "bit_scan_forward") {
			auto argumentType = function->argumentTypes[0];

			if (argumentType->flavor != TypeFlavor::INTEGER && argumentType->flavor != TypeFlavor::ENUM) {
				reportError(call->arguments.values[0], "Error: bit_scan_forward can only operate on integers or enums");
				return 0;
			}

			auto argument = generateLlvmIr(state, call->arguments.values[0]);

			auto resultIndex = state->builder.CreateIntrinsic(llvm::Intrinsic::cttz, arrayRef({ getLlvmType(state->context, argumentType) }), 
				arrayRef<llvm::Value *>({ argument, llvm::ConstantInt::get(llvm::Type::getInt1Ty(state->context), 1) }));

			if (comma && comma->exprCount >= 2) {
				state->builder.CreateStore(state->builder.CreateICmpEQ(argument, llvm::ConstantInt::get(argument->getType(), 0)), loadAddressOf(state, comma->left[1]));
			}

			return resultIndex;
		}
		else if (call->function->valueOfDeclaration->name == "bit_scan_reverse") {
			auto argumentType = function->argumentTypes[0];

			if (argumentType->flavor != TypeFlavor::INTEGER && argumentType->flavor != TypeFlavor::ENUM) {
				reportError(call->arguments.values[0], "Error: bit_scan_reverse can only operate on integers or enums");
				return 0;
			}

			auto argument = generateLlvmIr(state, call->arguments.values[0]);

			auto integerType = getLlvmType(state->context, argumentType);
			auto ctlz = state->builder.CreateIntrinsic(llvm::Intrinsic::ctlz, arrayRef({ integerType }),
				arrayRef<llvm::Value *>({ argument, llvm::ConstantInt::get(llvm::Type::getInt1Ty(state->context), 1) }));

			auto resultIndex = state->builder.CreateSub(llvm::ConstantInt::get(integerType, integerType->getIntegerBitWidth()  - 1), ctlz);

			if (comma && comma->exprCount >= 2) {
				state->builder.CreateStore(state->builder.CreateICmpEQ(argument, llvm::ConstantInt::get(argument->getType(), 0)), loadAddressOf(state, comma->left[1]));
			}

			return resultIndex;
		}
		else {
			reportError(call, "Error: Call to unknown intrinsic function: %.*s", STRING_PRINTF(call->function->valueOfDeclaration->name));
			return 0;
		}
	}

	u64 paramOffset = 0;
	auto return_ = function->returnTypes[0];

	if (!isStandardSize(return_->size)) {
		paramOffset++;
	}

	if (!(function->flags & TYPE_FUNCTION_IS_C_CALL)) {
		paramOffset++;
	}

	u64 count = function->argumentCount + function->returnCount + paramOffset - 1;

	auto arguments = new llvm::Value * [count];

	u64 returnIndex = 1;
	for (; returnIndex < (comma ? comma->exprCount : 1); returnIndex++) {
		arguments[function->argumentCount + paramOffset + returnIndex - 1] = loadAddressOf(state, comma->left[returnIndex]);
	}

	for (; returnIndex < function->returnCount; returnIndex++) {
		arguments[function->argumentCount + paramOffset + returnIndex - 1] = allocateType(state, function->returnTypes[returnIndex]);
	}

	auto functionIr = generateLlvmIr(state, call->function);


	for (u64 i = 0; i < function->argumentCount; i++) {
		auto argument = function->argumentTypes[i];

		if (!isStandardSize(argument->size)) {
			assert(isStoredByPointer(argument));
			arguments[i + paramOffset] = generateLlvmIr(state, call->arguments.values[i]);

		}
		else if (argument->flavor == TypeFlavor::STRUCT || argument->flavor == TypeFlavor::ARRAY) {
			arguments[i + paramOffset] = state->builder.CreateAlignedLoad(state->builder.CreateBitCast(generateLlvmIr(state, call->arguments.values[i]),
				llvm::PointerType::getUnqual(llvm::IntegerType::get(state->context, argument->size * 8))), 
				llvm::MaybeAlign(function->argumentTypes[i]->alignment));
		}
		else {
			arguments[i + paramOffset] = generateLlvmIr(state, call->arguments.values[i]);
		}
	}

	paramOffset = 0;

	if (!isStandardSize(return_->size)) {
		arguments[paramOffset++] = allocateType(state, return_);
	}

	if (!(function->flags & TYPE_FUNCTION_IS_C_CALL)) {
		arguments[paramOffset++] = state->contextValue;
	}

	auto ir = state->builder.CreateCall(static_cast<llvm::FunctionType *>(functionIr->getType()->getPointerElementType()), 
		functionIr, llvm::ArrayRef(arguments, count));
	ir->setCallingConv(llvm::CallingConv::Win64);

	if (!isStandardSize(return_->size)) {
		return arguments[0];
	}
	else if (return_->flavor == TypeFlavor::ARRAY || return_->flavor == TypeFlavor::STRUCT) {
		auto store = allocateType(state, ir->getFunctionType()->getReturnType());
		state->builder.CreateStore(ir, store);

		return state->builder.CreateBitCast(store, llvm::PointerType::getUnqual(getLlvmType(state->context, return_)));
	}
	else {
		return ir;
	}
}

static llvm::Value *generateLlvmEqual(State *state, bool equal, llvm::Value *l, Expr *right) {
	if (right->type->flavor == TypeFlavor::FLOAT) {
		return state->builder.CreateFCmp(getFCmp(equal ? TokenT::EQUAL : TokenT::NOT_EQUAL),
			l, generateLlvmIr(state, right));
	}
	else if (right->type->flavor == TypeFlavor::STRING) {
		if (!removeFunction) {
			reportError(right, "Internal Compiler Error: Removing something before __remove is declared");
			assert(false);
			exit(1); // @Cleanup Forceful exit since we don't have good error handling here and its an internal compiler error
		}

		auto result = state->builder.CreateCall(createLlvmFunction(state, stringsEqualFunction), arrayRef({ l, generateLlvmIr(state, right) }));

		result->setCallingConv(llvm::CallingConv::Win64);

		if (!equal) {
			return state->builder.CreateICmp(llvm::CmpInst::Predicate::ICMP_EQ, result, llvm::ConstantInt::get(llvm::Type::getInt1Ty(state->context), 0));
		}

		
		return result;
	}
	else {
		return state->builder.CreateICmp(getSCmp(equal ? TokenT::EQUAL : TokenT::NOT_EQUAL),
			l, generateLlvmIr(state, right));
	}
}

llvm::Value *generateLlvmArrayLiteral(State *state, ExprArrayLiteral *array) {
	auto arrayType = static_cast<TypeArray *>(array->type);

	auto arrayCount = arrayType->flags & TYPE_ARRAY_IS_FIXED ? arrayType->count : array->count;

	auto elementType = getLlvmType(state->context, arrayType->arrayOf);
	auto storage = allocateType(state, llvm::ArrayType::get(elementType, arrayCount));	


	for (u64 i = 0; i < arrayCount; i++) {
		auto value = generateIrAndLoadIfStoredByPointer(state, array->values[i]);

		if (i + 1 == array->count && arrayCount > array->count) {
			auto preBlock = state->builder.GetInsertBlock();

			auto loopBlock = llvm::BasicBlock::Create(state->context, "array.fill.loop", state->function);
			auto postBlock = llvm::BasicBlock::Create(state->context, "array.fill.post", state->function);

			state->builder.CreateBr(loopBlock);

			auto i64 = llvm::Type::getInt64Ty(state->context);

			state->builder.SetInsertPoint(loopBlock);
			auto phi = state->builder.CreatePHI(llvm::Type::getInt64Ty(state->context), 2);
			phi->addIncoming(llvm::ConstantInt::get(i64, arrayCount - i), preBlock);

			state->builder.CreateStore(value, state->builder.CreateGEP(storage, arrayRef<llvm::Value *>({ llvm::ConstantInt::get(i64, 0), phi })));

			auto sub = state->builder.CreateSub(phi, llvm::ConstantInt::get(i64, 1));
			phi->addIncoming(sub, loopBlock);

			state->builder.CreateCondBr(state->builder.CreateICmpNE(sub, llvm::ConstantInt::get(i64, 0)), loopBlock, postBlock);

			state->builder.SetInsertPoint(postBlock);
			break;
		}
		else {
			state->builder.CreateStore(value, state->builder.CreateConstGEP2_64(storage, 0, i));
		}
	}

	return storage;
}

llvm::Value *generateLlvmIr(State *state, Expr *expr) {
	PROFILE_FUNC();
	switch (expr->flavor) {
	case ExprFlavor::BINARY_OPERATOR: {

		auto binary = static_cast<ExprBinaryOperator *>(expr);

		auto left = binary->left;
		auto right = binary->right;

		switch (binary->op) {
		case TokenT::CAST: {
			assert(binary->left->flavor == ExprFlavor::TYPE_LITERAL);

			Type *castTo = binary->type;

			if (castTo == right->type) {
				return generateLlvmIr(state, right);
			}
			else if (binary->flags & EXPR_CAST_IS_BITWISE) {
				llvm::Value *value = generateLlvmIr(state, right);

				if (isStoredByPointer(right->type)) {
					value = state->builder.CreateLoad(state->builder.CreatePointerCast(value, llvm::PointerType::getUnqual(getLlvmType(state->context, castTo))));
				}
				else {
					value = state->builder.CreateBitCast(value, getLlvmType(state->context, castTo));
				}

				return storeIfPointerType(state, castTo, value);
			}
			else if (right->type == TYPE_ANY) {
				auto any = generateLlvmIr(state, right);

				auto pointer = state->builder.CreatePointerCast(state->builder.CreateLoad(state->builder.CreateStructGEP(any, 0)), llvm::PointerType::getUnqual(getLlvmType(state->context, castTo)));

				return storeIfPointerType(state, castTo, state->builder.CreateLoad(pointer));
			}

			switch (castTo->flavor) {
			case TypeFlavor::STRUCT: {
				assert(castTo == TYPE_ANY);

				auto any = allocateType(state, TYPE_ANY);

				auto int8p = llvm::Type::getInt8PtrTy(state->context);

				auto value = generateIrAndLoadIfStoredByPointer(state, right);

				state->builder.CreateStore(state->builder.CreateBitCast(allocateAndStore(state, value), int8p),
					state->builder.CreateStructGEP(any, 0));

				state->builder.CreateStore(llvm::ConstantExpr::getBitCast(createTypeInfoVariable(state, right->type), llvm::PointerType::getUnqual(getLlvmType(state->context, TYPE_TYPE_INFO))), 
					state->builder.CreateStructGEP(any, 1));

				return any;
			}
			case TypeFlavor::BOOL: {
				if (right->type->flavor == TypeFlavor::ARRAY || right->type == &TYPE_STRING) {
					if (right->type->flags & TYPE_ARRAY_IS_FIXED) {
						return llvm::ConstantInt::get(llvm::Type::getInt1Ty(state->context), 1); // We don't allow 0 size constant arrays so this is always true
					}
					else {
						return state->builder.CreateICmpNE(state->builder.CreateLoad(state->builder.CreateStructGEP(generateLlvmIr(state, right), 1)),
							llvm::ConstantInt::get(llvm::Type::getInt64Ty(state->context), 0));
					}
				}
				else if (right->type->flavor == TypeFlavor::FLOAT) {
					auto value = generateLlvmIr(state, right);

					return state->builder.CreateFCmpONE(value, llvm::ConstantFP::get(value->getType(), 0));
				}
				else if (right->type->flavor == TypeFlavor::INTEGER || right->type->flavor == TypeFlavor::ENUM) {
					auto value = generateLlvmIr(state, right);

					return state->builder.CreateICmpNE(value, llvm::ConstantInt::get(value->getType(), 0));
				}
				else {
					auto value = generateLlvmIr(state, right);
					assert(value->getType()->isPointerTy());

					return state->builder.CreateICmpNE(value, llvm::ConstantPointerNull::get(static_cast<llvm::PointerType *>(value->getType())));
				}
			}
			case TypeFlavor::FLOAT: {
				auto value = generateLlvmIr(state, right);

				if (right->type->flavor == TypeFlavor::FLOAT) {
					return state->builder.CreateFPCast(value, getLlvmType(state->context, castTo));
				}
				else {
					assert(right->type->flavor == TypeFlavor::INTEGER);

					if (right->type->flags & TYPE_INTEGER_IS_SIGNED) {
						return state->builder.CreateSIToFP(value, getLlvmType(state->context, castTo));
					}
					else {
						return state->builder.CreateUIToFP(value, getLlvmType(state->context, castTo));
					}
				}
			}
			case TypeFlavor::ENUM:
			case TypeFlavor::INTEGER: {
				auto value = generateLlvmIr(state, right);

				if (right->type->flavor == TypeFlavor::POINTER || right->type->flavor == TypeFlavor::FUNCTION) {
					return state->builder.CreatePtrToInt(value, getLlvmType(state->context, castTo));
				}
				else if (right->type->flavor == TypeFlavor::FLOAT) {
					if (castTo->flags & TYPE_INTEGER_IS_SIGNED) {
						return state->builder.CreateFPToSI(value, getLlvmType(state->context, castTo));
					}
					else {
						return state->builder.CreateFPToUI(value, getLlvmType(state->context, castTo));
					}
				}
				else if (right->type->flavor == TypeFlavor::BOOL) {
					return state->builder.CreateIntCast(value, getLlvmType(state->context, castTo), false);
				}
				else {
					return state->builder.CreateIntCast(value, getLlvmType(state->context, castTo), (castTo->flags & TYPE_INTEGER_IS_SIGNED) && (right->type->flags & TYPE_INTEGER_IS_SIGNED));
				}
			}
			case TypeFlavor::ARRAY: {
				auto value = generateLlvmIr(state, right);

				if (right->type->flavor == TypeFlavor::STRING) {
					return value;
				}
				else if (right->type->flags & TYPE_ARRAY_IS_DYNAMIC) {
					assert(!(castTo->flags & TYPE_ARRAY_IS_FIXED));

					auto load = state->builder.CreateLoad(state->builder.CreateBitCast(value, llvm::PointerType::getUnqual(getLlvmType(state->context, castTo))));

					return allocateAndStore(state, load);
				}
				else {
					assert(right->type->flags & TYPE_ARRAY_IS_FIXED);
					assert(!(castTo->flags & TYPE_ARRAY_IS_DYNAMIC));


					auto array = allocateType(state, castTo);

					auto elementType = getLlvmType(state->context, right->type)->getArrayElementType();

					state->builder.CreateStore(state->builder.CreatePointerCast(value, llvm::PointerType::getUnqual(elementType)),
						state->builder.CreateStructGEP(array, 0));

					state->builder.CreateStore(llvm::ConstantInt::get(llvm::Type::getInt64Ty(state->context), static_cast<TypeArray *>(right->type)->count),
						state->builder.CreateStructGEP(array, 1));
					

					return array;
				}
			}
			case TypeFlavor::STRING: {
				return generateLlvmIr(state, right);
			}
			case TypeFlavor::POINTER:
			case TypeFlavor::FUNCTION: {
				if (right->type->flavor == TypeFlavor::ARRAY && (right->type->flags & TYPE_ARRAY_IS_FIXED)) {
					return state->builder.CreatePointerCast(loadAddressOf(state, right), getLlvmType(state->context, castTo));
				}
				else if (right->type->flavor == TypeFlavor::INTEGER) {
					return state->builder.CreateIntToPtr(generateLlvmIr(state, right), getLlvmType(state->context, castTo));
				}
				else {
					return state->builder.CreatePointerCast(generateLlvmIr(state, right), getLlvmType(state->context, castTo));
				}
			}
			case TypeFlavor::TYPE:
			case TypeFlavor::VOID:
				assert(false);
				return nullptr;
			}

			assert(false);
			return nullptr;
		}
		case TOKEN('['): {
			auto value = state->builder.CreateLoad(loadAddressOf(state, expr));

			return storeIfPointerType(state, expr->type, value);
		}
		case TokenT::EQUAL:
		case TokenT::NOT_EQUAL: {
			assert(left->type == right->type);

			return generateLlvmEqual(state, binary->op == TokenT::EQUAL, generateLlvmIr(state, left), right);
		}
		case TokenT::GREATER_EQUAL:
		case TokenT::LESS_EQUAL:
		case TOKEN('>'):
		case TOKEN('<'): {
			if (left->type->flavor == TypeFlavor::FLOAT) {
				return state->builder.CreateFCmp(getFCmp(binary->op),
					generateLlvmIr(state, left), generateLlvmIr(state, right));
			}
			else if (left->type->flags & TYPE_INTEGER_IS_SIGNED) {
				return state->builder.CreateICmp(getSCmp(binary->op),
					generateLlvmIr(state, left), generateLlvmIr(state, right));
			}
			else {
				return state->builder.CreateICmp(getUCmp(binary->op),
					generateLlvmIr(state, left), generateLlvmIr(state, right));
			}
		}
		case TOKEN('+'):
		case TOKEN('-'): {
			if (left->type->flavor == TypeFlavor::POINTER) {
				auto int64 = llvm::Type::getInt64Ty(state->context);

				if (right->type->flavor == TypeFlavor::POINTER) {
					assert(binary->op == TOKEN('-'));
					return state->builder.CreateSDiv(
						state->builder.CreateSub(
							state->builder.CreatePtrToInt(generateLlvmIr(state, left), int64),
							state->builder.CreatePtrToInt(generateLlvmIr(state, right), int64)), 
						llvm::ConstantInt::get(int64, static_cast<TypePointer *>(left->type)->pointerTo->size));
				}
				else {
					auto pointer = generateLlvmIr(state, left);

					auto offset = generateLlvmIr(state, right);

					if (binary->op == TOKEN('-')) {
						offset = state->builder.CreateNeg(offset);
					}

					return state->builder.CreateGEP(pointer, offset);
				}
			}
			else if (left->type->flavor == TypeFlavor::FLOAT) {
				if (binary->op == TOKEN('+')) {
					return state->builder.CreateFAdd(generateLlvmIr(state, left), generateLlvmIr(state, right));
				}
				else {
					return state->builder.CreateFSub(generateLlvmIr(state, left), generateLlvmIr(state, right));
				}
			}
			else {
				if (binary->op == TOKEN('+')) {
					return state->builder.CreateAdd(generateLlvmIr(state, left), generateLlvmIr(state, right));
				}
				else {
					return state->builder.CreateSub(generateLlvmIr(state, left), generateLlvmIr(state, right));
				}
			}
		}
		case TOKEN('&'):
		case TOKEN('|'):
		case TOKEN('^'):
		case TokenT::SHIFT_LEFT:
		case TokenT::SHIFT_RIGHT: {
			auto l = generateLlvmIr(state, left);
			auto r = generateLlvmIr(state, right);

			if (binary->op == TOKEN('&')) {
				return state->builder.CreateAnd(l, r);
			}
			else if (binary->op == TOKEN('|')) {
				return state->builder.CreateOr(l, r);
			}
			else if (binary->op == TOKEN('^')) {
				return state->builder.CreateXor(l, r);
			}
			else if (binary->op == TokenT::SHIFT_LEFT) {
				return state->builder.CreateShl(l, r);
			}
			else {
				assert(binary->op == TokenT::SHIFT_RIGHT);
				if (left->type->flags & TYPE_INTEGER_IS_SIGNED) {
					return state->builder.CreateAShr(l, r);
				}
				else {
					return state->builder.CreateLShr(l, r);
				}
			}
		}
		case TOKEN('*'):
		case TOKEN('/'):
		case TOKEN('%'): {
			auto l = generateLlvmIr(state, left);
			auto r = generateLlvmIr(state, right);

			if (left->type->flavor == TypeFlavor::FLOAT) {
				if (binary->op == TOKEN('*')) {
					return state->builder.CreateFMul(l, r);
				}
				else if (binary->op == TOKEN('/')) {
					return state->builder.CreateFDiv(l, r);
				}
				else {
					assert(binary->op == TOKEN('%'));
					return state->builder.CreateFRem(l, r);
				}
			}
			else {
				if (binary->op == TOKEN('*')) {
					return state->builder.CreateMul(l, r);
				}
				else {
					if (left->type->flags & TYPE_INTEGER_IS_SIGNED) {
						if (binary->op == TOKEN('/')) {
							return state->builder.CreateSDiv(l, r);
						}
						else {
							assert(binary->op == TOKEN('%'));
							return state->builder.CreateSRem(l, r);
						}
					}
					else {
						if (binary->op == TOKEN('/')) {
							return state->builder.CreateUDiv(l, r);
						}
						else {
							assert(binary->op == TOKEN('%'));
							return state->builder.CreateURem(l, r);
						}
					}
				}
			}
		}
		case TOKEN('='): {
			auto store = loadAddressOf(state, binary->left);

			state->builder.CreateStore(generateIrAndLoadIfStoredByPointer(state, binary->right), store);

			return nullptr;
		}
		case TokenT::PLUS_EQUALS:
		case TokenT::MINUS_EQUALS: {
			auto address = loadAddressOf(state, binary->left);

			llvm::Value *l = state->builder.CreateLoad(address);
			auto r = generateLlvmIr(state, right);

			if (left->type->flavor == TypeFlavor::POINTER) {
				if (binary->op == TokenT::MINUS_EQUALS) {
					r = state->builder.CreateNeg(r);
				}

				l = state->builder.CreateGEP(l, r);
			}
			else if (left->type->flavor == TypeFlavor::FLOAT) {
				if (binary->op == TokenT::PLUS_EQUALS) {
					l = state->builder.CreateFAdd(l, r);
				}
				else {
					l = state->builder.CreateFSub(l, r);					
				}
			}
			else {
				if (binary->op == TokenT::PLUS_EQUALS) {
					l = state->builder.CreateAdd(l, r);
				}
				else {
					l = state->builder.CreateSub(l, r);
				}
			}

			state->builder.CreateStore(l, address);
			
			return nullptr;
		}
		case TokenT::LOGIC_AND:
		case TokenT::LOGIC_OR: {
			auto rightBlock = llvm::BasicBlock::Create(state->context, "short.circuit.right", state->function);
			auto exitBlock = llvm::BasicBlock::Create(state->context, "short.circuit.post", state->function);

			auto left = generateLlvmIr(state, binary->left);
			auto leftBlock = state->builder.GetInsertBlock();

			state->builder.CreateCondBr(left, binary->op == TokenT::LOGIC_AND ? rightBlock : exitBlock, binary->op == TokenT::LOGIC_AND ? exitBlock : rightBlock);

			state->builder.SetInsertPoint(rightBlock);

			auto right = generateLlvmIr(state, binary->right);
			rightBlock = state->builder.GetInsertBlock();
			state->builder.CreateBr(exitBlock);

			
			state->builder.SetInsertPoint(exitBlock);
			auto phi = state->builder.CreatePHI(llvm::Type::getInt1Ty(state->context), 2);
			
			phi->addIncoming(left, leftBlock);
			phi->addIncoming(right, rightBlock);

			return phi;
		}
		case TokenT::AND_EQUALS:
		case TokenT::OR_EQUALS:
		case TokenT::XOR_EQUALS:
		case TokenT::SHIFT_LEFT_EQUALS:
		case TokenT::SHIFT_RIGHT_EQUALS: {
			auto address = loadAddressOf(state, binary->left);

			llvm::Value *l = state->builder.CreateLoad(address);
			auto r = generateLlvmIr(state, right);

			if (binary->op == TokenT::AND_EQUALS) {
				l = state->builder.CreateAnd(l, r);
			}
			else if (binary->op == TokenT::OR_EQUALS) {
				l = state->builder.CreateOr(l, r);
			}
			else if (binary->op == TokenT::XOR_EQUALS) {
				l = state->builder.CreateXor(l, r);
			}
			else if (binary->op == TokenT::SHIFT_LEFT_EQUALS) {
				l = state->builder.CreateShl(l, r);
			}
			else {
				if (left->type->flags & TYPE_INTEGER_IS_SIGNED) {
					l = state->builder.CreateAShr(l, r);
				}
				else {
					l = state->builder.CreateLShr(l, r);
				}
			}

			state->builder.CreateStore(l, address);

			return nullptr;
		}
		case TokenT::TIMES_EQUALS:
		case TokenT::DIVIDE_EQUALS:
		case TokenT::MOD_EQUALS: {
			auto address = loadAddressOf(state, binary->left);

			llvm::Value *l = state->builder.CreateLoad(address);
			auto r = generateLlvmIr(state, right);

			if (left->type->flavor == TypeFlavor::FLOAT) {
				if (binary->op == TokenT::TIMES_EQUALS) {
					l = state->builder.CreateFMul(l, r);
				}
				else if (binary->op == TokenT::DIVIDE_EQUALS) {
					l = state->builder.CreateFDiv(l, r);
				}
				else {
					l = state->builder.CreateFRem(l, r);
				}
			}
			else {
				if (binary->op == TokenT::TIMES_EQUALS) {
					l = state->builder.CreateMul(l, r);
				}
				else if (binary->op == TokenT::DIVIDE_EQUALS) {
					if (left->type->flags & TYPE_INTEGER_IS_SIGNED) {
						l = state->builder.CreateSDiv(l, r);
					}
					else {
						l = state->builder.CreateUDiv(l, r);
					}
				}
				else {
					if (left->type->flags & TYPE_INTEGER_IS_SIGNED) {
						l = state->builder.CreateSRem(l, r);
					}
					else {
						l = state->builder.CreateURem(l, r);
					}
				}
			}

			state->builder.CreateStore(l, address);

			return nullptr;
		}
		default:
			assert(false);
			return nullptr;
		}
		break;
	}
	case ExprFlavor::SLICE: {
		
		auto slice = static_cast<ExprSlice *>(expr);

		auto result = allocateType(state, slice->type);

		llvm::Value *pointer = nullptr;
		llvm::Value *count = nullptr;

		if (slice->array->type->flavor == TypeFlavor::POINTER) {
			pointer = generateLlvmIr(state, slice->array);
		}
		else if (slice->array->type->flags & TYPE_ARRAY_IS_FIXED) {
			pointer = loadAddressOf(state, slice->array);
			count = llvm::ConstantInt::get(llvm::Type::getInt64Ty(state->context), static_cast<TypeArray *>(slice->array->type)->count);
		}
		else {
			auto array = generateLlvmIr(state, slice->array);

			pointer = state->builder.CreateLoad(state->builder.CreateStructGEP(array, 0));
			count = state->builder.CreateLoad(state->builder.CreateStructGEP(array, 1));
		}
		
		if (slice->sliceStart && slice->sliceEnd) {
			auto offset = generateLlvmIr(state, slice->sliceStart);

			auto end = generateLlvmIr(state, slice->sliceEnd);

			state->builder.CreateStore(state->builder.CreateGEP(pointer, offset), state->builder.CreateStructGEP(result, 0));
			state->builder.CreateStore(state->builder.CreateSub(end, offset), state->builder.CreateStructGEP(result, 1));
		}
		else if (slice->sliceStart) {
			auto offset = generateLlvmIr(state, slice->sliceStart);

			state->builder.CreateStore(state->builder.CreateGEP(pointer, offset), state->builder.CreateStructGEP(result, 0));
			state->builder.CreateStore(state->builder.CreateSub(count, offset), state->builder.CreateStructGEP(result, 1));
		}
		else {
			assert(slice->sliceEnd);

			auto end = generateLlvmIr(state, slice->sliceEnd);

			state->builder.CreateStore(pointer, state->builder.CreateStructGEP(result, 0));
			state->builder.CreateStore(end, state->builder.CreateStructGEP(result, 1));
		}

		return result;
	}
	case ExprFlavor::BLOCK: {
		auto block = static_cast<ExprBlock *>(expr);

		for (auto declaration : block->declarations.declarations) {
			if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING))) {				
				declaration->llvmStorage = allocateType(state, getDeclarationType(declaration), declaration->name);
			}
		}

		for (auto subExpr : block->exprs) {
			addLineMarker(state, subExpr);
			generateLlvmIr(state, subExpr);
		}

		exitBlock(state, &block->declarations, false);

		while (deferStack.count) {
			auto expr = deferStack[deferStack.count - 1];

			if (expr->flavor != ExprFlavor::DEFER)
				break;

			auto defer = static_cast<ExprDefer *>(expr);

			if (defer->enclosingScope != &block->declarations)
				break;

			deferStack.pop();
		}

		return nullptr;
	}
	case ExprFlavor::DEFER: {
		deferStack.add(expr);

		return nullptr;
	}
	case ExprFlavor::BREAK: {
		auto break_ = static_cast<ExprBreakOrContinue *>(expr);


		for (u32 i = loopCount; i-- != 0;) {

			if (loopStack[i].loop == break_->refersTo) {
				exitBlock(state, &loopStack[i].loop->iteratorBlock, true);
				loopStack[i].endPatches.add(state->builder.GetInsertBlock());

				state->builder.SetInsertPoint(llvm::BasicBlock::Create(state->context, "break.post", state->function));
				break;
			}
		}

		return nullptr;
	}
	case ExprFlavor::CONTINUE: {
		auto continue_ = static_cast<ExprBreakOrContinue *>(expr);


		for (u32 i = loopCount; i-- != 0;) {
			if (loopStack[i].loop == continue_->refersTo) {
				exitBlock(state, &loopStack[i].loop->iteratorBlock, false);
				
				state->builder.CreateBr(loopStack[i].start);

				state->builder.SetInsertPoint(llvm::BasicBlock::Create(state->context, "continue.post", state->function));
				break;
			}
		}
	
		return nullptr;
	}
	case ExprFlavor::REMOVE: {
		auto remove = static_cast<ExprBreakOrContinue *>(expr);

		if (!removeFunction) {
			reportError(expr, "Internal Compiler Error: Removing something before __remove is declared");
			assert(false);
			exit(1); // @Cleanup Forceful exit since we don't have good error handling here and its an internal compiler error
		}

		assert(remove->refersTo->forBegin->type->flavor == TypeFlavor::ARRAY);
		assert(!(remove->refersTo->forBegin->type->flags & TYPE_ARRAY_IS_FIXED));

		auto function = createLlvmFunction(state, removeFunction);

		auto argumentType = function->getFunctionType()->getParamType(0);

		auto args = new llvm::Value * [3]{
			state->builder.CreateBitCast(remove->refersTo->llvmArrayPointer, argumentType),
			state->builder.CreateBitCast(state->builder.CreateLoad(remove->refersTo->llvmPointer), llvm::Type::getInt8PtrTy(state->context)),
			llvm::ConstantInt::get(llvm::Type::getInt64Ty(state->context), static_cast<TypeArray *>(remove->refersTo->forBegin->type)->arrayOf->size)
		};

		auto call = state->builder.CreateCall(function, llvm::ArrayRef(args, 3));

		call->setCallingConv(llvm::CallingConv::Win64);

		state->builder.CreateStore(state->builder.CreateBitCast(call, remove->refersTo->llvmPointer->getType()->getPointerElementType()), remove->refersTo->llvmPointer);
		
		auto it_index = remove->refersTo->iteratorBlock.declarations[1]->llvmStorage;

		state->builder.CreateStore(
			state->builder.CreateSub(state->builder.CreateLoad(it_index), llvm::ConstantInt::get(llvm::Type::getInt64Ty(state->context), 1)),
			it_index
		);

		return nullptr;
	}
	case ExprFlavor::INT_LITERAL: {
		auto literal = static_cast<ExprLiteral *>(expr);

		return storeIfPointerType(state, literal->type, createIntLiteral(state, literal));
	}
	case ExprFlavor::FLOAT_LITERAL: {
		return llvm::ConstantFP::get(getLlvmType(state->context, expr->type), static_cast<ExprLiteral *>(expr)->floatValue);
	}
	case ExprFlavor::FOR: {
		auto loop = static_cast<ExprLoop *>(expr);

		auto it = loop->iteratorBlock.declarations[0];
		auto it_index = loop->iteratorBlock.declarations[1];

		it->llvmStorage = allocateType(state, getDeclarationType(it), it->name);

		it_index->llvmStorage = allocateType(state, getDeclarationType(it_index), it_index->name);

		state->builder.CreateStore(llvm::ConstantInt::get(llvm::Type::getInt64Ty(state->context), 0), it_index->llvmStorage);

		if (loop->forBegin->type->flavor != TypeFlavor::INTEGER && !(loop->flags & EXPR_FOR_BY_POINTER)) {
			loop->llvmPointer = allocateType(state, it->llvmStorage->getType(), "for.pointer");
		}
		else {
			loop->llvmPointer = it->llvmStorage;
		}

		if (loop->forBegin->type->flavor == TypeFlavor::ARRAY || loop->forBegin->type->flavor == TypeFlavor::STRING) {
			auto begin = loop->forBegin;

			loop->llvmArrayPointer = loadAddressOf(state, begin);

		
			if (!(begin->type->flags & TYPE_ARRAY_IS_FIXED)) {
				state->builder.CreateStore(state->builder.CreateLoad(state->builder.CreateStructGEP(loop->llvmArrayPointer, 0)), loop->llvmPointer);
			}
			else {
				state->builder.CreateStore(state->builder.CreateBitCast(loop->llvmArrayPointer, loop->llvmPointer->getType()->getPointerElementType()), loop->llvmPointer);
			}
		}
		else {
			state->builder.CreateStore(generateLlvmIr(state, loop->forBegin), loop->llvmPointer);
		}

		llvm::Value *end = nullptr;

		if (loop->forEnd) {
			end = generateLlvmIr(state, loop->forEnd);
		}

		auto testBlock = llvm::BasicBlock::Create(state->context, "for.test", state->function);
		auto bodyBlock = llvm::BasicBlock::Create(state->context, "for.body", state->function);
		auto postBlock = llvm::BasicBlock::Create(state->context, "for.post", state->function);
		auto completedBlock = loop->completedBody ? llvm::BasicBlock::Create(state->context, "for.completed", state->function) : postBlock;

		state->builder.CreateBr(testBlock);
		state->builder.SetInsertPoint(testBlock);

		pushLoop(state, loop);
		addLineMarker(state, loop);

		llvm::Value *compare;

		if (loop->forBegin->type->flavor == TypeFlavor::ARRAY || loop->forBegin->type == &TYPE_STRING) {
			llvm::Value *count;

			if (loop->forBegin->type->flags & TYPE_ARRAY_IS_FIXED) {
				count = llvm::ConstantInt::get(llvm::Type::getInt64Ty(state->context), static_cast<TypeArray *>(loop->forBegin->type)->count);
			}
			else {
				count = state->builder.CreateLoad(state->builder.CreateStructGEP(loop->llvmArrayPointer, 1));
			}

			compare = state->builder.CreateICmpULT(state->builder.CreateLoad(it_index->llvmStorage), count);
		}
		else {
			if (loop->forBegin->type->flags & TYPE_INTEGER_IS_SIGNED) {
				compare = state->builder.CreateICmpSLT(state->builder.CreateLoad(loop->llvmPointer), end);
			}
			else {
				compare = state->builder.CreateICmpULT(state->builder.CreateLoad(loop->llvmPointer), end);
			}
		}

		auto branch = state->builder.CreateCondBr(compare, bodyBlock, completedBlock);

		state->builder.SetInsertPoint(bodyBlock);

		if (loop->forBegin->type->flavor != TypeFlavor::INTEGER && !(loop->flags & EXPR_FOR_BY_POINTER)) {
			state->builder.CreateStore(state->builder.CreateLoad(state->builder.CreateLoad(loop->llvmPointer)), it->llvmStorage);
		}

		deferStack.add(loop);

		if (loop->body) {
			addLineMarker(state, loop->body);
			generateLlvmIr(state, loop->body);
		}

		exitBlock(state, &loop->iteratorBlock, false);

		Expr *inc = deferStack.pop();
		assert(inc == loop);

		state->builder.CreateBr(testBlock);


		if (loop->completedBody) {
			state->builder.SetInsertPoint(completedBlock);
			addLineMarker(state, loop->completedBody);
			generateLlvmIr(state, loop->completedBody);
			state->builder.CreateBr(postBlock);
		}

		state->builder.SetInsertPoint(postBlock);

		popLoop(state);

		return nullptr;
	}
	case ExprFlavor::FUNCTION: {
		auto function = static_cast<ExprFunction *>(expr);

		return createLlvmFunction(state, function);
	}
	case ExprFlavor::STRING_LITERAL: {
		auto string = static_cast<ExprStringLiteral *>(expr);

		return storeIfPointerType(state, &TYPE_STRING, createLlvmString(state, string->string));
	}
	case ExprFlavor::FUNCTION_CALL: {
		return generateLlvmCall(state, static_cast<ExprFunctionCall *>(expr), nullptr);
	}
	case ExprFlavor::IDENTIFIER: {
		auto identifier = static_cast<ExprIdentifier *>(expr);

		auto address = loadAddressOf(state, expr);

		if (isStoredByPointer(identifier->type)) {
			return address;
		}

		return state->builder.CreateLoad(address);
	}
	case ExprFlavor::SWITCH: {
		auto switch_ = static_cast<ExprSwitch *>(expr);
		
		auto value = generateLlvmIr(state, switch_->condition);

		ExprSwitch::Case *else_ = nullptr;

		auto postBlock = llvm::BasicBlock::Create(state->context, "switch.post", state->function);

		for (auto &case_ : switch_->cases) {
			case_.llvmCaseBlock = llvm::BasicBlock::Create(state->context, "switch.case", state->function);
		}

		for (u32 i = 0; i < switch_->cases.count; i++) {
			auto &case_ = switch_->cases[i];


			if (case_.condition) {
				auto condition = case_.condition;

				assert(condition->type == switch_->condition->type);

				auto result = generateLlvmEqual(state, true, value, condition);


				auto nextCheck = llvm::BasicBlock::Create(state->context, "switch.test", state->function);

				state->builder.CreateCondBr(result, case_.llvmCaseBlock, nextCheck);

				state->builder.SetInsertPoint(nextCheck);
			}
			else {
				else_ = &case_;
			}

			auto insert = state->builder.GetInsertBlock();

			state->builder.SetInsertPoint(case_.llvmCaseBlock);

			generateLlvmIr(state, case_.block);

			if (!case_.fallsThrough || &case_ + 1 == switch_->cases.end()) {
				state->builder.CreateBr(postBlock);
			}
			else {
				state->builder.CreateBr(switch_->cases[i + 1].llvmCaseBlock);
			}

			state->builder.SetInsertPoint(insert);
		}


		if (else_) {
			state->builder.CreateBr(else_->llvmCaseBlock);
		}
		else {
			state->builder.CreateBr(postBlock);
		}

		
		state->builder.SetInsertPoint(postBlock);

		return nullptr;
	}
	case ExprFlavor::IF: {
		auto ifElse = static_cast<ExprIf *>(expr);

		llvm::Value *condition = generateLlvmIr(state, ifElse->condition);

		auto trueBlock = llvm::BasicBlock::Create(state->context, "if.true", state->function);
		auto falseBlock = llvm::BasicBlock::Create(state->context, "if.false", state->function);

		state->builder.CreateCondBr(condition, trueBlock, falseBlock);

		if (ifElse->ifBody && ifElse->elseBody) {
			auto postBlock = llvm::BasicBlock::Create(state->context, "if.post", state->function);

			state->builder.SetInsertPoint(trueBlock);
			addLineMarker(state, ifElse->ifBody);
			generateLlvmIr(state, ifElse->ifBody);
			state->builder.CreateBr(postBlock);

			state->builder.SetInsertPoint(falseBlock);
			addLineMarker(state, ifElse->elseBody);
			generateLlvmIr(state, ifElse->elseBody);
			state->builder.CreateBr(postBlock);

			state->builder.SetInsertPoint(postBlock);
		}
		else if (ifElse->ifBody) {
			state->builder.SetInsertPoint(trueBlock);
			addLineMarker(state, ifElse->ifBody);
			generateLlvmIr(state, ifElse->ifBody);
			state->builder.CreateBr(falseBlock);

			state->builder.SetInsertPoint(falseBlock);
		}
		else if (ifElse->elseBody) {
			state->builder.SetInsertPoint(falseBlock);
			addLineMarker(state, ifElse->elseBody);
			generateLlvmIr(state, ifElse->elseBody);
			state->builder.CreateBr(trueBlock);

			state->builder.SetInsertPoint(trueBlock);
		}

		return nullptr;
	}
	case ExprFlavor::COMMA_ASSIGNMENT: {
		auto comma = static_cast<ExprCommaAssignment *>(expr);

		auto store = loadAddressOf(state, comma->left[0]);

		auto value = generateLlvmCall(state, static_cast<ExprFunctionCall *>(comma->call), comma);

		if (isStoredByPointer(comma->left[0]->type)) {
			value = state->builder.CreateLoad(value);
		}

		state->builder.CreateStore(value, store);

		return nullptr;
	}
	case ExprFlavor::RETURN: {
		auto return_ = static_cast<ExprReturn *>(expr);

		u64 result = 0;

		if (return_->returns.count) {
			auto result = generateLlvmIr(state, return_->returns.values[0]);

			auto retType = return_->returns.values[0]->type;

			for (u32 i = 1; i < return_->returns.count; i++) {
				auto store = generateIrAndLoadIfStoredByPointer(state, return_->returns.values[i]);

				state->builder.CreateStore(store, return_->returnsFrom->returns.declarations[i]->llvmStorage);
			}

			exitBlock(state, nullptr, true);


			if (!isStandardSize(retType->size)) {
				state->builder.CreateStore(state->builder.CreateLoad(result), return_->returnsFrom->returns.declarations[0]->llvmStorage);

				state->builder.CreateRetVoid();
			}
			else if (retType->flavor == TypeFlavor::STRUCT || retType->flavor == TypeFlavor::ARRAY) {
				state->builder.CreateRet(state->builder.CreateAlignedLoad(
					state->builder.CreateBitCast(
						result,
						llvm::PointerType::getUnqual(llvm::IntegerType::get(state->context, retType->size * 8))
					),
					llvm::MaybeAlign(retType->alignment)
				));
			}
			else {
				state->builder.CreateRet(result);
			}
		}
		else {
			exitBlock(state, nullptr, true);
			
			state->builder.CreateRetVoid();
		}

		auto postBlock = llvm::BasicBlock::Create(state->context, "return.post", state->function);
		state->builder.SetInsertPoint(postBlock);


		return nullptr;
	}
	case ExprFlavor::STRUCT_LITERAL: {
		auto literal = static_cast<ExprStructLiteral *>(expr);

		if (literal->llvmStorage) {
			return literal->llvmStorage;
		}
		else if (structIsLiteral(literal)) {
			auto constant = createConstant(state, literal);

			literal->llvmStorage = createUnnnamedConstant(state, constant->getType());
			literal->llvmStorage->setInitializer(constant);
			

			// :Sadge
			if (literal->llvmStorage->getInitializer()->getType() != literal->type->llvmType) {
				return state->builder.CreatePointerCast(literal->llvmStorage, llvm::PointerType::getUnqual(getLlvmType(state->context, literal->type)));
			}

			 return literal->llvmStorage;
		}
		else {

			auto store = allocateType(state, literal->type);

			for (u32 i = 0; i < literal->initializers.count; i++) {
				auto value = generateIrAndLoadIfStoredByPointer(state, literal->initializers.values[i]);

				state->builder.CreateStore(value, createGEPForStruct(state, store, static_cast<TypeStruct *>(literal->type), literal->initializers.declarations[i]));
			}

			return store;
		}
	}
	case ExprFlavor::TYPE_LITERAL: {
		auto type = static_cast<ExprLiteral *>(expr)->typeValue;

		if (type->flavor == TypeFlavor::MODULE) {
			reportError(expr, "Error: Cannot operate on a module");
			return llvm::UndefValue::get(llvm::Type::getInt8PtrTy(state->context));
		}

		return llvm::ConstantExpr::getBitCast(createTypeInfoVariable(state, type), llvm::Type::getInt8PtrTy(state->context));
	}
	case ExprFlavor::UNARY_OPERATOR: {
		ExprUnaryOperator *unary = static_cast<ExprUnaryOperator *>(expr);

		switch (unary->op) {
		case TOKEN('*'): {
			return loadAddressOf(state, unary->value);
		}
		case TOKEN('-'): {
			auto toNegate = generateLlvmIr(state, unary->value);

			if (unary->value->type->flavor == TypeFlavor::FLOAT) {
				return state->builder.CreateFNeg(toNegate);
			}
			else {
				return state->builder.CreateNeg(toNegate);
			}
		}
		case TOKEN('~'): {
			auto toInvert = generateLlvmIr(state, unary->value);

			return state->builder.CreateNot(toInvert);
		}
		case TOKEN('!'): {
			auto toInvert = generateLlvmIr(state, unary->value);

			return state->builder.CreateICmpEQ(toInvert, llvm::ConstantInt::get(llvm::Type::getInt1Ty(state->context), 0));
		}
		case TokenT::SHIFT_LEFT: {
			auto addressReg = generateLlvmIr(state, unary->value);

			return storeIfPointerType(state, unary->type, state->builder.CreateLoad(addressReg));
		}
		case TokenT::TYPE_INFO: {
			auto type = generateLlvmIr(state, unary->value);

			return state->builder.CreateBitCast(type, getLlvmType(state->context, unary->type));
		}
		default:
			assert(false);
			return nullptr;
		}
	}
	case ExprFlavor::WHILE: {
		ExprLoop *loop = static_cast<ExprLoop *>(expr);

		auto testBlock = llvm::BasicBlock::Create(state->context, "while.test", state->function);
		auto bodyBlock = llvm::BasicBlock::Create(state->context, "while.body", state->function);
		auto postBlock = llvm::BasicBlock::Create(state->context, "while.post", state->function);
		auto completedBlock = loop->completedBody ? llvm::BasicBlock::Create(state->context, "while.completed", state->function) : postBlock;

		state->builder.CreateBr(testBlock);
		state->builder.SetInsertPoint(testBlock);

		pushLoop(state, loop);

		auto conditionReg = generateLlvmIr(state, loop->whileCondition);

		state->builder.CreateCondBr(conditionReg, bodyBlock, completedBlock);

		state->builder.SetInsertPoint(bodyBlock);
		if (loop->body) {
			addLineMarker(state, loop->body);
			generateLlvmIr(state, loop->body);
		}

		state->builder.CreateBr(testBlock);

		if (loop->completedBody) {
			state->builder.SetInsertPoint(completedBlock);
			addLineMarker(state, loop->completedBody);
			generateLlvmIr(state, loop->completedBody);
			state->builder.CreateBr(postBlock);
		}

		state->builder.SetInsertPoint(postBlock);

		popLoop(state);

		return nullptr;
	}
	case ExprFlavor::ARRAY_LITERAL: {
		auto array = static_cast<ExprArrayLiteral *>(expr);

		if (array->llvmStorage) {
			return array->llvmStorage;
		}
		else if (arrayIsLiteral(array)) {
			auto constant = createConstant(state, array);
			array->llvmStorage = createUnnnamedConstant(state, constant->getType());

			array->llvmStorage->setInitializer(constant);

			// :Sadge
			if (array->llvmStorage->getInitializer()->getType() != array->type->llvmType) {
				return state->builder.CreatePointerCast(array->llvmStorage, llvm::PointerType::getUnqual(getLlvmType(state->context, array->type)));
			}

			return array->llvmStorage;
		}
		else {
			auto data = generateLlvmArrayLiteral(state, array);

			if (array->type->flags & TYPE_ARRAY_IS_FIXED) {
				return data;
			}
			else {
				auto value = allocateType(state, array->type);

				state->builder.CreateStore(
					state->builder.CreateConstGEP2_32(data->getType()->getPointerElementType(), data, 0, 0),
					state->builder.CreateStructGEP(value, 0));
				state->builder.CreateStore(
					llvm::ConstantInt::get(llvm::Type::getInt64Ty(state->context), array->count),
					state->builder.CreateStructGEP(value, 1));

				return value;
			}
		}
	}

	case ExprFlavor::CONTEXT: {
		return state->contextValue;
	}
	case ExprFlavor::PUSH_CONTEXT: {
		auto pushContext = static_cast<ExprBinaryOperator *>(expr);

		auto oldContext = state->contextValue;
		addLineMarker(state, pushContext->right);
		state->contextValue = generateLlvmIr(state, pushContext->left);
		generateLlvmIr(state, pushContext->right);
		state->contextValue = oldContext;
		return nullptr;
	}
	case ExprFlavor::STATIC_IF: {
		return nullptr; // In the event that the static if returns false and there is no else block, we just leave the static if expression in the tree, 
			   // so when we see a static if here we should just generate no code
	}
	default:
		assert(false);
		return nullptr;
	}
}

static void addDiscriminatorsPass(const llvm::PassManagerBuilder &Builder, llvm::legacy::PassManagerBase &PM) {
	PM.add(llvm::createAddDiscriminatorsPass());
}

llvm::codegen::RegisterCodeGenFlags CGF;

// General options for llc.  Other pass-specific options are specified
// within the corresponding llc passes, and target-specific options
// and back-end code generation options are specified with the target machine.
//

enum OptLevel {
	O0, 
	O1, 
	O2, 
	O3, 
	Os, 
	Oz
};

// Determine optimization level.
static llvm::cl::opt<OptLevel>
OptLevel(llvm::cl::desc("Optimization level. [-O0, -O1, -O2, -O3, -Os or -Oz] "
		"(default = '-O2')"),
	llvm::cl::values(
		clEnumVal(O0, "No optimization"),
		clEnumVal(O1, "Enable simple optimizations"), 
		clEnumVal(O2, "Enable default optimizations"), 
		clEnumVal(O3, "Enable expensive optimizations"), 
		clEnumVal(Os, "Optimize for size"), 
		clEnumVal(Oz, "Agressively optimize for size")

	), 
	llvm::cl::init(O2));

static llvm::cl::opt<std::string>
TargetTriple("mtriple", llvm::cl::desc("Override target triple for module"));

static llvm::DIFile *getDebugFile(CodeLocation location) {
	return files[location.fileUid];
}

void runLlvm() {

	llvm::LLVMContext context;

	llvm::InitializeAllTargets();
	llvm::InitializeAllTargetMCs();
	llvm::InitializeAllAsmPrinters();
	llvm::InitializeAllAsmParsers();

	llvm::PassRegistry &passRegistry = *llvm::PassRegistry::getPassRegistry();

	llvm::initializeCore(passRegistry);
	llvm::initializeCodeGen(passRegistry);
	llvm::initializeLoopStrengthReducePass(passRegistry);
	llvm::initializeLowerIntrinsicsPass(passRegistry);
	llvm::initializeEntryExitInstrumenterPass(passRegistry);
	llvm::initializePostInlineEntryExitInstrumenterPass(passRegistry);
	llvm::initializeUnreachableBlockElimLegacyPassPass(passRegistry);
	llvm::initializeConstantHoistingLegacyPassPass(passRegistry);
	llvm::initializeScalarOpts(passRegistry);
	llvm::initializeVectorization(passRegistry);
	llvm::initializeScalarizeMaskedMemIntrinPass(passRegistry);
	llvm::initializeExpandReductionsPass(passRegistry);
	llvm::initializeHardwareLoopsPass(passRegistry);
	llvm::initializeTransformUtils(passRegistry);
	llvm::initializeScavengerTestPass(passRegistry);

	llvm::cl::AddExtraVersionPrinter(llvm::TargetRegistry::printRegisteredTargetsForVersion);


	const char **commandLineOptions = new const char *[1 + buildOptions.llvm_options.count];

	commandLineOptions[0] = ""; // The first argument is ignored by llvm since normally it would be the command used to invoke the compiler
	
	for (u64 i = 0; i < buildOptions.llvm_options.count; i++) {
		MiloString option = buildOptions.llvm_options.data[i];
		char *cString = (char *) malloc(option.count + 1);
		memcpy(cString, option.data, option.count);
		cString[option.count] = 0;

		commandLineOptions[i + 1] = cString;
	}

	
	if (!llvm::cl::ParseCommandLineOptions(1 + buildOptions.llvm_options.count, commandLineOptions)) {
		reportError("Error: Failed to parse LLVM options");
		return;
	}

#if BUILD_DEBUG
#else
	context.setDiscardValueNames(true);
#endif

	llvm::Triple targetTriple;
	std::string cpuStr = llvm::codegen::getCPUStr();
	std::string featuresStr = llvm::codegen::getFeaturesStr();

	auto mAttrs = llvm::codegen::getMAttrs();

	// @Incomplete: Support Os, Oz
	llvm::CodeGenOpt::Level optLevel = llvm::CodeGenOpt::Default;
	u32 sizeLevel = 0;

	switch (OptLevel) {
	default:
		reportError("Error: Invalid LLVM optimization level O%c", OptLevel.getValue());
		return;
	case O0: optLevel = llvm::CodeGenOpt::None; break;
	case O1: optLevel = llvm::CodeGenOpt::Less; break;
	case O2: optLevel = llvm::CodeGenOpt::Default; break;
	case O3: optLevel = llvm::CodeGenOpt::Aggressive; break;
	case Os: sizeLevel = 1;
	case Oz: sizeLevel = 2;
	}

	bool optimized = optLevel != llvm::CodeGenOpt::None || sizeLevel > 0;

	auto relocModel = llvm::codegen::getExplicitRelocModel();
	auto codeModel = llvm::codegen::getExplicitCodeModel();
	

	const llvm::Target *target = nullptr;
	llvm::TargetMachine *targetMachine = nullptr;

	llvm::Module llvmModule("llvm_out", context);

	llvm::Triple triple;

	if (TargetTriple.empty()) {
		triple.setTriple(llvm::sys::getDefaultTargetTriple());
	}
	else {
		triple = llvm::Triple(llvm::Triple::normalize(TargetTriple));
	}

	llvmModule.setTargetTriple(triple.getTriple());
	
	std::string errorString;
	target = llvm::TargetRegistry::lookupTarget(llvm::codegen::getMArch(), triple, errorString);
	if (!target) {
		reportError("LLVM Error: %s", errorString.c_str());
		return;
	}

	llvm::TargetOptions options = llvm::codegen::InitTargetOptionsFromCodeGenFlags(triple);

	if (llvm::codegen::getFloatABIForCalls() != llvm::FloatABI::Default) {
		options.FloatABIType = llvm::codegen::getFloatABIForCalls();
	}

	targetMachine = target->createTargetMachine(triple.getTriple(), cpuStr, featuresStr, options, relocModel, codeModel, optLevel);

	llvmModule.addModuleFlag(llvm::Module::Warning, "CodeView", 1); // I hate LLVM

	if (!targetMachine) {
		reportError("LLVM Error: Failed to allocate target machine", errorString.c_str());
		return;
	}

	llvm::IRBuilder<> builder(context);
	
	State state{ context, builder, llvmModule };

	std::error_code errorCode;
	std::string verifyOutput;

	llvm::raw_string_ostream verifyStream(verifyOutput);

	files = new llvm::DIFile * [compilerFiles.count];

	dib = new llvm::DIBuilder(llvmModule);

	for (u32 i = 0; i < compilerFiles.count; i++) {
		auto file = compilerFiles[i];

		char buffer[1024]; // @Robustness
		char *name;
		GetFullPathNameA(toCString(file->path) /* @Leak */, sizeof(buffer), buffer, &name);

		if (name != buffer) {
			name[-1] = 0;
		}
#if BUILD_WINDOWS
		const char *compilerName = "Milo Compiler 0.1.1 (Windows-x64)";
#endif

		if (i == 0) {
			compileUnit = dib->createCompileUnit(llvm::dwarf::DW_LANG_C_plus_plus_11, dib->createFile(name, buffer), compilerName, optimized, "", 0);
		}

		files[i] = dib->createFile(name, buffer);
	}

	{
		llvmModule.setDataLayout(targetMachine->createDataLayout());

	
		while (true) {
			auto job = coffWriterQueue.take();

			if (job.flavor == CoffJobFlavor::FUNCTION) {
				auto function = job.function;

				if (!function) {
					break;
				}

				if (!(function->flags & EXPR_FUNCTION_IS_EXTERNAL)) {

					auto llvmFile = getDebugFile(function->start);

					auto llvmFunction = createLlvmFunction(&state, function);
					auto functionType = static_cast<llvm::FunctionType *>(llvmFunction->getType()->getPointerElementType());
					auto debugType = static_cast<llvm::DISubroutineType *>(static_cast<llvm::DIDerivedType *>(getLlvmDebugType(function->type))->getBaseType());

					auto debugFunction = dib->createFunction(nullptr, function->valueOfDeclaration ? stringRef(function->valueOfDeclaration->name) : "(function)",
						llvmFunction->getName(), llvmFile, function->start.line, debugType, 
						function->start.line, llvm::DINode::DIFlags::FlagPrototyped, llvm::DISubprogram::DISPFlags::SPFlagDefinition);

					llvmFunction->setSubprogram(debugFunction);

					state.debugScopeStack.add(debugFunction);
					addLineMarker(&state, function);

					

					auto entry = llvm::BasicBlock::Create(context, "entry", llvmFunction);

					auto code = llvm::BasicBlock::Create(context, "code", llvmFunction);

					builder.SetInsertPoint(code);

					state.function = llvmFunction;
					state.entryBlock = entry;

					u32 paramOffset = 0; 
					
					if (!isStandardSize(getDeclarationType(function->returns.declarations[0])->size)) {
						function->returns.declarations[0]->llvmStorage = llvmFunction->getArg(paramOffset++);
					}

					if (!(function->flags & EXPR_FUNCTION_IS_C_CALL)) {
						state.contextValue = llvmFunction->getArg(paramOffset++);
					}
					else {
						state.contextValue = nullptr;
					}

					for (u32 i = 0; i < function->arguments.declarations.count; i++) {
						auto argument = function->arguments.declarations[i];
						auto argType = getDeclarationType(function->arguments.declarations[i]);


						llvm::Value *llvmArg = llvmFunction->getArg(i + paramOffset);
						if (!isStandardSize(argType->size)) {
							argument->llvmStorage = llvmArg;
						}
						else if (argType->flavor == TypeFlavor::ARRAY || argType->flavor == TypeFlavor::STRUCT) {
							auto alloc = allocateType(&state, functionType->getFunctionParamType(i + paramOffset));

							builder.CreateStore(llvmArg, alloc);

							argument->llvmStorage = builder.CreateBitCast(alloc, llvm::PointerType::getUnqual(getLlvmType(context, argType)), stringRef(argument->name));
						}
						else {
							argument->llvmStorage = allocateType(&state, getDeclarationType(argument), argument->name);

							

							builder.CreateStore(llvmArg, argument->llvmStorage);
						}
					}

					for (u32 i = 1; i < function->returns.declarations.count; i++) {
						function->returns.declarations[i]->llvmStorage = llvmFunction->getArg(paramOffset + function->arguments.declarations.count - 1 + i);
					}

					generateLlvmIr(&state, function->body);

					if (!builder.GetInsertBlock()->getTerminator()) {
						if (functionType->getReturnType()->isVoidTy()) {
							builder.CreateRetVoid();
						}
						else {
							builder.CreateRet(llvm::Constant::getNullValue(functionType->getReturnType()));
						}
					}


					builder.SetInsertPoint(entry);

					builder.CreateBr(code);

					state.debugScopeStack.pop();
					dib->finalizeSubprogram(debugFunction);
#if BUILD_DEBUG
					if (llvm::verifyFunction(*llvmFunction, &verifyStream)) {
						verifyStream.flush();

						reportError("LLVM Error in %s: %s", function->valueOfDeclaration ? toCString(function->valueOfDeclaration->name) : "(function)", verifyOutput.c_str());
						llvmModule.dump();

						return;
					}
#endif
				}

			}
			else if (job.flavor == CoffJobFlavor::GLOBAL_DECLARATION) {

				auto declaration = job.declaration;

				auto global = createLlvmGlobal(&state, declaration);

				auto llvmType = getLlvmType(state.context, getDeclarationType(declaration));

				if (!declaration->initialValue) {
					global->setInitializer(llvm::UndefValue::get(llvmType));
				}
				else {
					global->setInitializer(createConstant(&state, declaration->initialValue));
				}
			}
		}

		if (!hadError) {
			for (u64 i = 0; i < typeTableCapacity; i++) {
				auto entry = typeTableEntries[i];

				if (entry.hash) {
					auto type = entry.value;

					auto variable = createTypeInfoVariable(&state, type);

					auto name = createLlvmString(&state, type->name);

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

					auto int1 = llvm::Type::getInt1Ty(context);
					auto int64 = llvm::Type::getInt64Ty(context);

					auto tag = llvm::ConstantInt::get(int64, static_cast<u64>(infoTag));
					auto size = llvm::ConstantInt::get(int64, type->size);
					auto alignment = llvm::ConstantInt::get(int64, type->alignment);


#define GET_STRUCT(t) static_cast<llvm::StructType *>(getLlvmType(context, (t)))
#define TO_TYPE_INFO(t) llvm::ConstantExpr::getBitCast(createTypeInfoVariable(&state, (t)), llvmTypeInfoPtr)

					auto llvmTypeInfo = GET_STRUCT(TYPE_TYPE_INFO);
					auto llvmTypeInfoPtr = llvm::PointerType::getUnqual(llvmTypeInfo);

					auto base = llvm::ConstantStruct::get(llvmTypeInfo, tag, size, alignment, name);



					switch (infoTag) {
					case Type_Info::Tag::VOID:
					case Type_Info::Tag::FLOAT:
					case Type_Info::Tag::BOOL:
					case Type_Info::Tag::TYPE:
					case Type_Info::Tag::STRING: {
						variable->setInitializer(base);

						break;
					}
					case Type_Info::Tag::INTEGER: {
						auto signed_ = llvm::ConstantInt::get(int1, type->flags & TYPE_INTEGER_IS_SIGNED ? 1 : 0);

						variable->setInitializer(llvm::ConstantStruct::get(GET_STRUCT(TYPE_TYPE_INFO_INTEGER), base, signed_));

						break;
					}
					case Type_Info::Tag::POINTER: {
						auto function = static_cast<TypeFunction *>(type);

						auto value_type = TO_TYPE_INFO(static_cast<TypePointer *>(type)->pointerTo);

						variable->setInitializer(llvm::ConstantStruct::get(GET_STRUCT(TYPE_TYPE_INFO_POINTER), base, value_type));

						break;
					}
					case Type_Info::Tag::FUNCTION: {
						auto function = static_cast<TypeFunction *>(type);

						auto argumentsVariable = createUnnnamedConstant(&state, llvm::ArrayType::get(llvmTypeInfoPtr, function->argumentCount));

						llvm::Constant **arguments = new llvm::Constant * [function->argumentCount];

						for (u64 i = 0; i < function->argumentCount; i++) {
							arguments[i] = TO_TYPE_INFO(function->argumentTypes[i]);
						}

						argumentsVariable->setInitializer(llvm::ConstantArray::get(static_cast<llvm::ArrayType *>(argumentsVariable->getValueType()), 
							llvm::ArrayRef(arguments, function->argumentCount)));

						auto returnsVariable = createUnnnamedConstant(&state, llvm::ArrayType::get(llvmTypeInfoPtr, function->argumentCount));

						llvm::Constant **returns = new llvm::Constant * [function->returnCount];

						for (u64 i = 0; i < function->returnCount; i++) {
							returns[i] = TO_TYPE_INFO(function->returnTypes[i]);
						}

						returnsVariable->setInitializer(llvm::ConstantArray::get(static_cast<llvm::ArrayType *>(argumentsVariable->getValueType()),
							llvm::ArrayRef(arguments, function->argumentCount)));

						auto typeInfoPtrPtr = llvm::PointerType::getUnqual(llvmTypeInfoPtr);
						auto typeInfoArray = llvm::StructType::get(typeInfoPtrPtr, int64);

						auto argumentsArray = llvm::ConstantStruct::get(typeInfoArray, 
							llvm::ConstantExpr::getBitCast(argumentsVariable, typeInfoPtrPtr), 
							llvm::ConstantInt::get(int64, function->argumentCount));

						auto returnsArray = llvm::ConstantStruct::get(typeInfoArray,
							llvm::ConstantExpr::getBitCast(returnsVariable, typeInfoPtrPtr),
							llvm::ConstantInt::get(int64, function->returnCount));

						auto cCall = llvm::ConstantInt::get(llvm::IntegerType::getInt1Ty(context), function->flags & TYPE_FUNCTION_IS_C_CALL ? 1 : 0);
						auto isVarargs = llvm::ConstantInt::get(llvm::IntegerType::getInt1Ty(context), function->isVarargs ? 1 : 0);

						variable->setInitializer(llvm::ConstantStruct::get(GET_STRUCT(TYPE_TYPE_INFO_FUNCTION), base, argumentsArray, returnsArray, cCall, isVarargs));

						break;
					}
					case Type_Info::Tag::ARRAY: {

						auto flavor = llvm::ConstantInt::get(int64, static_cast<u64>(type->flags & TYPE_ARRAY_IS_FIXED ?
							Type_Info_Array::Flavor::FIXED : type->flags & TYPE_ARRAY_IS_DYNAMIC ?
							Type_Info_Array::Flavor::DYNMAIC : Type_Info_Array::Flavor::NORMAL));
						auto count = llvm::ConstantInt::get(int64, type->flags &TYPE_ARRAY_IS_FIXED ? static_cast<TypeArray *>(type)->count : 0);
						auto element_type = TO_TYPE_INFO(static_cast<TypeArray *>(type)->arrayOf);


						variable->setInitializer(llvm::ConstantStruct::get(GET_STRUCT(TYPE_TYPE_INFO_ARRAY), base, flavor, element_type, count));

						break;
					}
					case Type_Info::Tag::STRUCT: {
						auto struct_ = static_cast<TypeStruct *>(type);

						u64 count = 0;

						for (auto member : struct_->members.declarations) {
							if (member->flags & DECLARATION_IMPORTED_BY_USING) continue;

							++count;
						}

						auto structMember = GET_STRUCT(TYPE_TYPE_INFO_STRUCT_MEMBER);
						auto structMemberPtr = llvm::PointerType::getUnqual(structMember);


						auto membersVariable = createUnnnamedConstant(&state, llvm::ArrayType::get(structMember, count));
						auto members = new llvm::Constant * [count];

						count = 0;

						for (auto member : struct_->members.declarations) {
							if (member->flags & DECLARATION_IMPORTED_BY_USING) continue;

							llvm::Constant *initial_value;

							if (member->initialValue && (member->initialValue->flavor != ExprFlavor::TYPE_LITERAL || static_cast<ExprLiteral *>(member->initialValue)->typeValue->flavor != TypeFlavor::MODULE)) { 
								auto constant = createConstant(&state, member->initialValue);
								auto variable = createUnnnamedConstant(&state, constant->getType());
								variable->setInitializer(constant);
								initial_value = llvm::ConstantExpr::getBitCast(variable, llvm::Type::getInt8PtrTy(context));
							}
							else {
								initial_value = llvm::ConstantPointerNull::get(llvm::Type::getInt8PtrTy(context));
							}

							auto memberName = createLlvmString(&state, member->name);
							auto offset = llvm::ConstantInt::get(int64, member->physicalStorage);
							auto member_type = TO_TYPE_INFO(getDeclarationType(member));

							u64 flags_int = 0;

							if (member->flags & DECLARATION_IS_UNINITIALIZED) flags_int |= Type_Info_Struct::Member::Flags::UNINITIALIZED;
							if (member->flags & DECLARATION_IS_CONSTANT) flags_int |= Type_Info_Struct::Member::Flags::CONSTANT;
							if (member->flags & DECLARATION_MARKED_AS_USING) flags_int |= Type_Info_Struct::Member::Flags::USING;

							auto flags = llvm::ConstantInt::get(int64, flags_int);

							members[count++] = llvm::ConstantStruct::get(structMember, memberName, offset, member_type, initial_value, flags);
						}

						auto typeMembersArray = llvm::StructType::get(structMemberPtr, int64);

						membersVariable->setInitializer(llvm::ConstantArray::get(static_cast<llvm::ArrayType *>(membersVariable->getValueType()),
							llvm::ArrayRef(members, count)));


						auto membersArray = llvm::ConstantStruct::get(typeMembersArray,
							llvm::ConstantExpr::getBitCast(membersVariable, structMemberPtr),
							llvm::ConstantInt::get(int64, count));

						u64 flags_int = 0;


						if (struct_->flags & TYPE_STRUCT_IS_UNION) flags_int |= Type_Info_Struct::Flags::UNION;
						if (struct_->flags & TYPE_STRUCT_IS_PACKED) flags_int |= Type_Info_Struct::Flags::PACKED;

						auto flags = llvm::ConstantInt::get(int64, flags_int);

						variable->setInitializer(llvm::ConstantStruct::get(GET_STRUCT(TYPE_TYPE_INFO_STRUCT), base, flags, membersArray));

						break;
					}
					case Type_Info::Tag::ENUM: {
						auto enum_ = static_cast<TypeEnum *>(type);

						auto enumValue = GET_STRUCT(TYPE_TYPE_INFO_ENUM_VALUE);
						auto enumValuePtr = llvm::PointerType::getUnqual(enumValue);

						const auto count = enum_->members.declarations.count - ENUM_SPECIAL_MEMBER_COUNT;

						auto valuesVariable = createUnnnamedConstant(&state, llvm::ArrayType::get(enumValue, count));
						auto values = new llvm::Constant * [count];

						for (u32 i = 0; i < count; i++) {
							auto value = enum_->members.declarations[i];
							assert(value->flags & DECLARATION_IS_ENUM_VALUE);
								
							auto valueName = createLlvmString(&state, value->name);
							auto valueValue = llvm::ConstantInt::get(int64, static_cast<ExprLiteral *>(value->initialValue)->unsignedValue);

							values[i] = llvm::ConstantStruct::get(enumValue, valueName, valueValue);
						}

						auto typeValuesArray = llvm::StructType::get(enumValuePtr, int64);

						valuesVariable->setInitializer(llvm::ConstantArray::get(static_cast<llvm::ArrayType *>(valuesVariable->getValueType()),
							llvm::ArrayRef(values, count)));


						auto valuesArray = llvm::ConstantStruct::get(typeValuesArray,
							llvm::ConstantExpr::getBitCast(valuesVariable, enumValuePtr),
							llvm::ConstantInt::get(int64, count));

						auto is_flags = llvm::ConstantInt::get(int1, enum_->flags & TYPE_ENUM_IS_FLAGS ? 1 : 0);

						auto base_type = llvm::ConstantExpr::getBitCast(createTypeInfoVariable(&state, enum_->integerType), llvm::PointerType::getUnqual(getLlvmType(context, TYPE_TYPE_INFO_INTEGER)));

						variable->setInitializer(llvm::ConstantStruct::get(GET_STRUCT(TYPE_TYPE_INFO_ENUM), base, base_type, is_flags, valuesArray));
						break;
					}
					default:
						assert(false);
					}
				}
			}

			auto int32 = llvm::Type::getInt32Ty(context);
			auto fltused = new llvm::GlobalVariable(llvmModule, int32, true, llvm::GlobalValue::ExternalLinkage, llvm::ConstantInt::get(int32, 0), "_fltused");

			//llvmModule.dump();

			// llvmModule.print(llvm::errs(), nullptr);
			std::error_code err;

			llvm::raw_fd_ostream irOut("out2.ir", err);


			llvm::raw_fd_ostream output(objectFileName, err);

			llvm::legacy::PassManager pass;

			llvm::TargetLibraryInfoImpl tlii(llvm::Triple(llvmModule.getTargetTriple()));

			if (!linkLibC)
				tlii.disableAllFunctions();

			pass.add(new llvm::TargetLibraryInfoWrapperPass(tlii));

#if BUILD_DEBUG
			if (llvm::verifyModule(llvmModule, &llvm::errs())) {
				reportError("LLVM Error: Module verification failed");
				return;
			}
#endif

			llvm::codegen::setFunctionAttributes(cpuStr, featuresStr, llvmModule);


			llvm::PassManagerBuilder *pmbuilder = new llvm::PassManagerBuilder;
			pmbuilder->OptLevel = targetMachine->getOptLevel();
			pmbuilder->SizeLevel = pmbuilder->SizeLevel;
			pmbuilder->DisableTailCalls = llvm::codegen::getDisableTailCalls();
			pmbuilder->DisableUnrollLoops = pmbuilder->OptLevel == 0;
			pmbuilder->SLPVectorize = pmbuilder->OptLevel > 1 && pmbuilder->SizeLevel < 2;
			pmbuilder->LoopVectorize = pmbuilder->SLPVectorize;
			pmbuilder->LoopsInterleaved = pmbuilder->LoopVectorize;
			pmbuilder->DisableGVNLoadPRE = pmbuilder->OptLevel == 0;
			pmbuilder->MergeFunctions = pmbuilder->OptLevel > 2 || pmbuilder->SizeLevel > 1;
			pmbuilder->PrepareForLTO = false;
			pmbuilder->PrepareForLTO = false;
			pmbuilder->PerformThinLTO = false;
#if BUILD_DEBUG
			pmbuilder->VerifyInput = true;
			pmbuilder->VerifyOutput = true;
#else
			pmbuilder->VerifyInput = false;
			pmbuilder->VerifyOutput = false;
#endif
			


			pmbuilder->LibraryInfo = &tlii;

			if (pmbuilder->OptLevel < 2) {
				pmbuilder->Inliner = llvm::createAlwaysInlinerLegacyPass(false);
			}
			else {
				targetMachine->adjustPassManager(*pmbuilder);

				pmbuilder->addExtension(llvm::PassManagerBuilder::EP_EarlyAsPossible, addDiscriminatorsPass);
				pmbuilder->Inliner = llvm::createFunctionInliningPass(pmbuilder->OptLevel, pmbuilder->SizeLevel, false);
			}

			llvm::legacy::FunctionPassManager fpm(&llvmModule);
			fpm.add(new llvm::TargetLibraryInfoWrapperPass(tlii));
			fpm.add(llvm::createTargetTransformInfoWrapperPass(targetMachine->getTargetIRAnalysis()));
			
#if BUILD_DEBUG
			fpm.add(llvm::createVerifierPass());
#endif
			
			pmbuilder->populateFunctionPassManager(fpm);

			pass.add(llvm::createTargetTransformInfoWrapperPass(targetMachine->getTargetIRAnalysis()));
			pmbuilder->populateModulePassManager(pass);


			auto fileType = llvm::CGFT_ObjectFile;

			if (targetMachine->addPassesToEmitFile(pass, output, nullptr, fileType)) {
				reportError("LLVM Error: can't emit a file of this type");
			}

			fpm.doInitialization();

			for (auto &function : llvmModule) {
				if (!function.isDeclaration())
					fpm.run(function);
			}

			fpm.doFinalization();

			pass.run(llvmModule);
			dib->finalize();

			llvmModule.print(irOut, nullptr);

			output.flush();
		}
	}
error:;
}

#else

void runLlvm() {
	reportInfo("LLVM is not supported in this build.");
}

#endif