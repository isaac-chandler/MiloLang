#include "Basic.h"

#ifndef BUILD_NO_LLVM

#include "CoffWriter.h"
#include "Lexer.h"
#include "IrGenerator.h"

static llvm::FunctionCallee llvmDebugDeclare;
static llvm::Type *createLlvmType(llvm::LLVMContext &context, Type *type);

static llvm::Type *getLlvmType(llvm::LLVMContext &context, Type *type) {
	if (!type->llvmType)
		type->llvmType = createLlvmType(context, type);

	return type->llvmType;
}

extern bool isStandardSize(u64 size);

llvm::StringRef stringRef(const String &string) {
	return llvm::StringRef(string.characters, string.length);
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

		if (type->flags & TYPE_STRUCT_IS_UNION) {
			auto struct_ = static_cast<TypeStruct *>(type);

			Type *mainMember = nullptr;

			for (auto member : struct_->members.declarations) {
				if (member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IS_IMPLICIT_IMPORT | DECLARATION_IMPORTED_BY_USING))
					continue;

				auto memberType = static_cast<ExprLiteral *>(member->type)->typeValue;

				if ((type->flags & TYPE_STRUCT_IS_PACKED) || memberType->alignment == struct_->alignment) {
					if (!mainMember || (memberType->size > mainMember->size)) {
						mainMember = memberType;
					}
				}
			}

			if (!mainMember) {
				return llvm::StructType::create(stringRef(struct_->name), llvm::ArrayType::get(llvm::IntegerType::get(context, type->alignment * 8), type->size / type->alignment));
			}
			else {
				llvm::ArrayRef<llvm::Type *> members;

				if (struct_->size == mainMember->size) {
					members = llvm::ArrayRef(new llvm::Type * [1]{ getLlvmType(context, mainMember)}, 1);

				}
				else {
					members = llvm::ArrayRef(new llvm::Type * [2]{ getLlvmType(context, mainMember), 
						llvm::ArrayType::get(llvm::Type::getInt8Ty(context), struct_->size - mainMember->size) }, 2);
				}

				return llvm::StructType::create(members, stringRef(struct_->name), struct_->flags & TYPE_STRUCT_IS_PACKED ? true : false);
			}
		}
		else {
			auto struct_ = static_cast<TypeStruct *>(type);

			llvm::StructType *llvmType = llvm::StructType::create(context, stringRef(struct_->name));

			type->llvmType = llvmType; // Put the empty struct on the type so that if we have a struct that points to itself we don't infinitely recurse

			u32 count = 0;
			for (auto member : struct_->members.declarations) {
				if (member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IS_IMPLICIT_IMPORT | DECLARATION_IMPORTED_BY_USING))
					continue;

				++count;
			}

			Array<llvm::Type *> body(count);

			for (auto member : struct_->members.declarations) {
				if (member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IS_IMPLICIT_IMPORT | DECLARATION_IMPORTED_BY_USING))
					continue;

				body.add(getLlvmType(context, static_cast<ExprLiteral *>(member->type)->typeValue));
			}

			llvmType->setBody(llvm::ArrayRef(body.begin(), body.end()), struct_->flags & TYPE_STRUCT_IS_PACKED ? true : false);

			return llvmType;
		}
	}
	else if (type->flavor == TypeFlavor::FUNCTION) {
		auto function = static_cast<TypeFunction *>(type);

		// LLVM doesn't support multiple return values, so the extra return values are passed by pointer after the other arguments


		u64 paramOffset = 0;
		auto return_ = function->returnTypes[0];

		if (!isStandardSize(return_->size)) {
			paramOffset = 1;
		}

		u64 count = function->argumentCount + function->returnCount + paramOffset - 1;

		llvm::Type **arguments = new llvm::Type * [count];

		if (paramOffset) {
			arguments[0] = llvm::PointerType::getUnqual(getLlvmType(context, return_));
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

		if (return_ == &TYPE_VOID || paramOffset) {
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
	llvm::DIBuilder &debug;
	llvm::Function *function;
	llvm::BasicBlock *entryBlock;
	llvm::DIFile *diFile; // @Incomplete: Dummy file info
};

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
	case ExprFlavor::FLOAT_LITERAL:
	case ExprFlavor::STRING_LITERAL:
	case ExprFlavor::FUNCTION:
	case ExprFlavor::TYPE_LITERAL: {
		return static_cast<llvm::Constant *>(generateLlvmIr(state, expr));
	}
	case ExprFlavor::STRUCT_DEFAULT: {
		auto struct_ = static_cast<TypeStruct *>(expr->type);

		auto llvmType = static_cast<llvm::StructType *>(getLlvmType(state->context, struct_));

		if (struct_->flags & TYPE_STRUCT_IS_UNION) {
			Expr *default_ = nullptr;

			for (auto decl : struct_->members.declarations) {
				if (decl->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_IMPLICIT_IMPORT)) continue;

				if (decl->initialValue) {
					default_ = decl->initialValue;
					break;
				}
			}

			if (!default_) {
				return llvm::UndefValue::get(llvmType);
			}
			else {
				if (getLlvmType(state->context, default_->type) == llvmType->getStructElementType(0)) {
					if (llvmType->getStructNumElements() == 1) {
						return llvm::ConstantStruct::get(llvmType, createConstant(state, default_));
					}
					else {
						return llvm::ConstantStruct::get(llvmType, createConstant(state, default_), llvm::UndefValue::get(llvmType->getStructElementType(1)));
					}
				}
				else {
					auto memberType = getLlvmType(state->context, default_->type);
					auto arrayType = llvm::ArrayType::get(llvm::Type::getInt8Ty(state->context), struct_->size - default_->type->size);

					auto tempType = llvm::StructType::get(memberType, arrayType);

					return llvm::ConstantExpr::getBitCast(llvm::ConstantStruct::get(tempType, createConstant(state, default_), llvm::UndefValue::get(arrayType)), llvmType);
				}
			}
		}
		else {
			u64 memberCount = 0;

			for (auto decl : struct_->members.declarations) {
				if (decl->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_IMPLICIT_IMPORT)) continue;

				++memberCount;
			}

			llvm::Constant **values = new llvm::Constant * [memberCount];

			u32 memberIndex = 0;


			for (auto decl : struct_->members.declarations) {
				if (decl->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_IMPLICIT_IMPORT)) continue;

				assert(memberIndex < memberCount);

				if (decl->flags & DECLARATION_IS_UNINITIALIZED) {
					values[memberIndex] = llvm::UndefValue::get(llvmType->getStructElementType(memberIndex));
				}
				else {
					values[memberIndex] = createConstant(state, decl->initialValue);
				}

				++memberIndex;
			}

			return llvm::ConstantStruct::get(llvmType, llvm::ArrayRef(values, memberCount));
		}
	}
	case ExprFlavor::ARRAY: {
		assert(expr->type->flags & TYPE_ARRAY_IS_FIXED);

		auto literal = static_cast<ExprArray *>(expr);

		auto array = static_cast<TypeArray *>(expr->type);

		llvm::Constant **values = new llvm::Constant * [array->count];

		for (u64 i = 0; i < array->count; i++) {
			auto value = createConstant(state, literal->storage[i]);
			values[i] = value;
			
			if (i + 1 < array->count && literal->storage[i + 1] == nullptr) {
				for (u64 j = i + 1; j < array->count; j++) {
					values[j] = value;
				}

				break;
			}
		}

		return llvm::ConstantArray::get(static_cast<llvm::ArrayType *>(getLlvmType(state->context, array)), llvm::ArrayRef(values, array->count));
	}
	}

	assert(false);
	return nullptr;
}

static llvm::GlobalVariable *createLlvmGlobal(State *state, Declaration *declaration) {
	if (!declaration->llvmStorage) {
		auto llvmType = getLlvmType(state->context, static_cast<ExprLiteral *>(declaration->type)->typeValue);

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


bool isStoredByPointer(Type *type) {
	return type->flavor == TypeFlavor::ARRAY || type->flavor == TypeFlavor::STRUCT || type->flavor == TypeFlavor::STRING;
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


static void exitBlock(State *state, Block *block, bool isBreak) {
	for (u32 i = deferStack.count; i-- != 0;) {
		auto expr = deferStack[i];

		if (expr->flavor == ExprFlavor::FOR) {
			auto loop = static_cast<ExprLoop *>(expr);

			auto current = &loop->iteratorBlock;
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

			generateLlvmIr(state, defer->expr);
		}
	}
}

static Block externalsBlock;

static llvm::Function *createLlvmFunction(State *state, ExprFunction *function) {
	if (!function->llvmStorage) {
		if (function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
			if (Declaration *declaration = findDeclarationNoYield(&externalsBlock, function->valueOfDeclaration->name)) {
				function->llvmStorage = static_cast<ExprFunction *>(declaration->initialValue)->llvmStorage;

				return function->llvmStorage;
			}
			else {
				putDeclarationInBlock(&externalsBlock, function->valueOfDeclaration);
			}
		}

		auto type = getLlvmType(state->context, function->type);

		assert(type->isPointerTy());
		assert(type->getPointerElementType()->isFunctionTy());

		auto functionType = static_cast<llvm::FunctionType *>(type->getPointerElementType());

		if (function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
			function->llvmStorage = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, stringRef(function->valueOfDeclaration->name), state->module);
		}
		else if (function->valueOfDeclaration) {
			function->llvmStorage = llvm::Function::Create(functionType, function->valueOfDeclaration->enclosingScope == &globalBlock ? llvm::Function::ExternalLinkage : llvm::Function::PrivateLinkage, stringRef(function->valueOfDeclaration->name), state->module);


		}
		else {
			function->llvmStorage = llvm::Function::Create(functionType, llvm::Function::PrivateLinkage, "", state->module);
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

			if (type->flags & TYPE_ARRAY_IS_FIXED) { // The only struct access we will generate for fixed arrays are .data, which is just the address of the array
				assert(identifier->name == "data");
				return state->builder.CreatePointerCast(store, 
					llvm::PointerType::getUnqual(getLlvmType(state->context, static_cast<TypeArray *>(type)->arrayOf)));
			}

			if (type->flavor == TypeFlavor::POINTER) {
				type = static_cast<TypePointer *>(type)->pointerTo;
			}
			
			if (type->flags & TYPE_STRUCT_IS_UNION) {
				u64 offset = identifier->declaration->physicalStorage; // In the case of struct members, physicalStorage is the offset within the struct
				assert(offset == 0);

				/*
				return state->builder.CreatePointerCast(
					state->builder.CreateConstGEP1_64(
						state->builder.CreatePointerCast(
							store,
							llvm::Type::getInt8PtrTy(state->context)
						),
						offset
					),
					llvm::PointerType::getUnqual(getLlvmType(state->context, expr->type))
				);
				*/

				return state->builder.CreatePointerCast(
					store,
					llvm::PointerType::getUnqual(getLlvmType(state->context, expr->type))
				);
			}
			else {
				auto struct_ = static_cast<TypeStruct *>(type);

				u32 memberIndex = 0;

				// @Speed currently we do a linear search through struct members to find 
				for (auto member : struct_->members.declarations) {
					if (member->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IS_IMPLICIT_IMPORT | DECLARATION_IMPORTED_BY_USING))
						continue;

					if (member == identifier->declaration) {
						break;
					}

					memberIndex++;
				}

				return state->builder.CreateStructGEP(store, memberIndex);
			}
		}
		else {
			if (identifier->declaration->enclosingScope == &globalBlock) {
				createLlvmGlobal(state, identifier->declaration);
			}

			return identifier->declaration->llvmStorage;
		}
	}
	else {
		assert(expr->type->flavor == TypeFlavor::ARRAY || expr->type->flavor == TypeFlavor::STRING);

		return generateLlvmIr(state, expr);
	}
}

static llvm::Value *generateLlvmCall(State *state, ExprFunctionCall *call, ExprCommaAssignment *comma) {
	auto function = static_cast<TypeFunction *>(call->function->type);

	u64 paramOffset = 0;
	auto return_ = function->returnTypes[0];

	if (!isStandardSize(return_->size)) {
		paramOffset = 1;
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

	if (paramOffset) {
		arguments[0] = allocateType(state, return_);
	}

	auto ir = state->builder.CreateCall(static_cast<llvm::FunctionType *>(functionIr->getType()->getPointerElementType()), 
		functionIr, llvm::ArrayRef(arguments, count));
	ir->setCallingConv(llvm::CallingConv::Win64);

	if (paramOffset) {
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

		auto args = new llvm::Value * [2]{ l, generateLlvmIr(state, right) };

		auto result = state->builder.CreateCall(createLlvmFunction(state, stringsEqualFunction), llvm::ArrayRef(args, 2));

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
				else {
					return state->builder.CreateIntCast(value, getLlvmType(state->context, castTo), castTo->flags & TYPE_INTEGER_IS_SIGNED ? true : false);
				}
			}
			case TypeFlavor::ARRAY: {
				auto value = generateLlvmIr(state, right);

				if (right->type->flavor == TypeFlavor::STRING) {
					return value;
				}
				else if (right->type->flags & TYPE_ARRAY_IS_DYNAMIC) {
					assert(!(castTo->flags & TYPE_ARRAY_IS_FIXED));

					auto load = state->builder.CreateLoad(state->builder.CreateBitCast(value, getLlvmType(state->context, castTo)));

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
			if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_IMPLICIT_IMPORT))) {				
				declaration->llvmStorage = allocateType(state, static_cast<ExprLiteral *>(declaration->type)->typeValue, declaration->name);
			}
		}

		for (auto subExpr : block->exprs) {
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

		it->llvmStorage = allocateType(state, static_cast<ExprLiteral *>(it->type)->typeValue, it->name);

		it_index->llvmStorage = allocateType(state, static_cast<ExprLiteral *>(it_index->type)->typeValue, it_index->name);

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
			generateLlvmIr(state, loop->body);
		}

		exitBlock(state, &loop->iteratorBlock, false);

		Expr *inc = deferStack.pop();
		assert(inc == loop);

		state->builder.CreateBr(testBlock);


		if (loop->completedBody) {
			state->builder.SetInsertPoint(completedBlock);
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
				auto condition = static_cast<ExprBinaryOperator *>(case_.condition);

				assert(condition->flavor == ExprFlavor::BINARY_OPERATOR);

				assert(condition->right->type == switch_->condition->type);

				auto result = generateLlvmEqual(state, true, value, condition->right);


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
			generateLlvmIr(state, ifElse->ifBody);
			state->builder.CreateBr(postBlock);

			state->builder.SetInsertPoint(falseBlock);
			generateLlvmIr(state, ifElse->elseBody);
			state->builder.CreateBr(postBlock);

			state->builder.SetInsertPoint(postBlock);
		}
		else if (ifElse->ifBody) {
			state->builder.SetInsertPoint(trueBlock);
			generateLlvmIr(state, ifElse->ifBody);
			state->builder.CreateBr(falseBlock);

			state->builder.SetInsertPoint(falseBlock);
		}
		else if (ifElse->elseBody) {
			state->builder.SetInsertPoint(falseBlock);
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
			u32 paramOffset = isStandardSize(retType->size) ? 0 : 1;

			for (u32 i = 1; i < return_->returns.count; i++) {
				auto store = generateIrAndLoadIfStoredByPointer(state, return_->returns.values[i]);

				state->builder.CreateStore(store, state->function->getArg(return_->returnsFrom->arguments.declarations.count + paramOffset + i - 1));
			}

			exitBlock(state, nullptr, true);


			if (paramOffset) {
				state->builder.CreateStore(state->builder.CreateLoad(result), return_->returnsFrom->llvmStorage->getArg(0));

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
	case ExprFlavor::STRUCT_DEFAULT: {
		auto store = allocateType(state, expr->type);

		state->builder.CreateStore(createConstant(state, expr), store);

		return store;
	}
	case ExprFlavor::TYPE_LITERAL: {
		auto type = static_cast<ExprLiteral *>(expr)->typeValue;

		if (type->flavor == TypeFlavor::NAMESPACE) {
			reportError(expr, "Error: Cannot operate on a namespace");
			return nullptr;
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
			generateLlvmIr(state, loop->body);
		}

		state->builder.CreateBr(testBlock);

		if (loop->completedBody) {
			state->builder.SetInsertPoint(completedBlock);
			generateLlvmIr(state, loop->completedBody);
			state->builder.CreateBr(postBlock);
		}

		state->builder.SetInsertPoint(postBlock);

		popLoop(state);

		return nullptr;
	}
	case ExprFlavor::ARRAY: {
		auto array = static_cast<ExprArray *>(expr);

		assert(array->type->flags & TYPE_ARRAY_IS_FIXED);

		auto storage = allocateType(state, array->type);

		for (u64 i = 0; i < array->count; i++) {
			auto value = generateIrAndLoadIfStoredByPointer(state, array->storage[i]);

			state->builder.CreateStore(value, state->builder.CreateConstGEP2_64(storage, 0, i));

			if (i + 1 < array->count && array->storage[i + 1] == nullptr) {
				for (; i < array->count; i++) {
					state->builder.CreateStore(value, state->builder.CreateConstGEP2_64(storage, 0, i));
				}

				break;
			}
		}

		return storage;
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

void runLlvm() {

	{
		std::unique_lock<std::mutex> lock = std::unique_lock(startLlvmLock);

		startLlvm.wait(lock);
	}

	//Sleep(1000);

	llvm::LLVMContext context;

	llvm::Module llvmModule("llvm_out", context);
	llvm::IRBuilder<> builder(context);
	llvm::DIBuilder debug(llvmModule);

	State state{ context, builder, llvmModule, debug };

	state.diFile = debug.createFile("incomplete", "incomplete");

	std::error_code errorCode;
	std::string verifyOutput;

	llvm::raw_string_ostream verifyStream(verifyOutput);

	llvm::InitializeNativeTarget();
	llvm::InitializeNativeTargetAsmPrinter();
	llvm::InitializeNativeTargetAsmParser();

	auto targetTriple = llvm::sys::getDefaultTargetTriple();

	std::string error;
	auto target = llvm::TargetRegistry::lookupTarget(targetTriple, error);

	llvm::TargetOptions options;


	auto rm = llvm::Optional<llvm::Reloc::Model>();

	auto cpu = "generic";
	auto features = "";

	if (!target) {
		reportError("LLVM Error: %s", error.c_str());
		goto error;

	}

	{
		auto targetMachine = target->createTargetMachine(targetTriple, cpu, features, options, rm);

		llvmModule.setDataLayout(targetMachine->createDataLayout());

		auto metadata = llvm::Type::getMetadataTy(context);
		auto metadataThree = new llvm::Type * [3]{ metadata, metadata, metadata };

		llvmDebugDeclare = llvmModule.getOrInsertFunction("llvm.dbg.declare", llvm::FunctionType::get(llvm::Type::getVoidTy(context), llvm::ArrayRef(metadataThree, 3), false));

		while (true) {
			auto job = coffWriterQueue.take();

			if (job.flavor == CoffJobFlavor::FUNCTION) {
				auto function = job.function;

				if (!function) {
					break;
				}

				if (!(function->flags & EXPR_FUNCTION_IS_EXTERNAL)) {
					auto llvmFunction = createLlvmFunction(&state, function);
					auto functionType = static_cast<llvm::FunctionType *>(llvmFunction->getType()->getPointerElementType());

					auto entry = llvm::BasicBlock::Create(context, "entry", llvmFunction);

					auto code = llvm::BasicBlock::Create(context, "code", llvmFunction);

					builder.SetInsertPoint(code);

					state.function = llvmFunction;
					state.entryBlock = entry;

					u32 paramOffset = isStandardSize(static_cast<ExprLiteral *>(function->returns.declarations[0]->type)->typeValue->size) ? 0 : 1;

					for (u32 i = 0; i < function->arguments.declarations.count; i++) {
						auto argument = function->arguments.declarations[i];
						auto argType = static_cast<ExprLiteral *>(function->arguments.declarations[i]->type)->typeValue;


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
							argument->llvmStorage = allocateType(&state, static_cast<ExprLiteral *>(argument->type)->typeValue, argument->name);

							

							builder.CreateStore(llvmArg, argument->llvmStorage);
						}
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

#if BUILD_DEBUG
					if (llvm::verifyFunction(*llvmFunction, &verifyStream)) {
						verifyStream.flush();

						reportError("LLVM Error in %s: %s", function->valueOfDeclaration ? toCString(function->valueOfDeclaration->name) : "(function)", verifyOutput.c_str());
						llvmFunction->print(llvm::errs());

						goto error;
					}
#endif
				}

			}
			else if (job.flavor == CoffJobFlavor::GLOBAL_DECLARATION) {

				auto declaration = job.declaration;

				auto global = createLlvmGlobal(&state, declaration);

				auto llvmType = getLlvmType(state.context, static_cast<ExprLiteral *>(declaration->type)->typeValue);

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
							if (member->flags & (DECLARATION_IS_IMPLICIT_IMPORT | DECLARATION_IMPORTED_BY_USING)) continue;

							++count;
						}

						auto structMember = GET_STRUCT(TYPE_TYPE_INFO_STRUCT_MEMBER);
						auto structMemberPtr = llvm::PointerType::getUnqual(structMember);


						auto membersVariable = createUnnnamedConstant(&state, llvm::ArrayType::get(structMember, count));
						auto members = new llvm::Constant * [count];

						count = 0;

						for (auto member : struct_->members.declarations) {
							if (member->flags & (DECLARATION_IS_IMPLICIT_IMPORT | DECLARATION_IMPORTED_BY_USING)) continue;

							llvm::Constant *initial_value;

							if (member->initialValue && (member->initialValue->flavor != ExprFlavor::TYPE_LITERAL || static_cast<ExprLiteral *>(member->initialValue)->typeValue->flavor != TypeFlavor::NAMESPACE)) {
								auto variable = createUnnnamedConstant(&state, getLlvmType(context, member->initialValue->type));
								variable->setInitializer(createConstant(&state, member->initialValue));
								initial_value = llvm::ConstantExpr::getBitCast(variable, llvm::Type::getInt8PtrTy(context));
							}
							else {
								initial_value = llvm::ConstantPointerNull::get(llvm::Type::getInt8PtrTy(context));
							}

							auto memberName = createLlvmString(&state, member->name);
							auto offset = llvm::ConstantInt::get(int64, member->physicalStorage);
							auto member_type = TO_TYPE_INFO(static_cast<ExprLiteral *>(member->type)->typeValue);


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

						const auto count = enum_->values->declarations.count;

						auto valuesVariable = createUnnnamedConstant(&state, llvm::ArrayType::get(enumValue, count));
						auto values = new llvm::Constant * [count];

						for (u32 i = 0; i < count; i++) {
							auto value = enum_->values->declarations[i];

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


			

			std::cout << verifyOutput << "\n";

			//llvmModule.dump();

			// llvmModule.print(llvm::errs(), nullptr);
			std::error_code err;

			llvm::raw_fd_ostream irOut("out2.ir", err);


			llvm::raw_fd_ostream output("out.obj", err);

			// Based on zig llvm pass manager https://github.com/ziglang/zig/blob/master/src/zig_llvm.cpp
			bool optimize = true;
			
			if (optimize) {
				targetMachine->setOptLevel(llvm::CodeGenOpt::Aggressive);
			}
			else {
				targetMachine->setOptLevel(llvm::CodeGenOpt::None);
				targetMachine->setO0WantsFastISel(true);
			}

			llvm::PassManagerBuilder *pmbuilder = new llvm::PassManagerBuilder;
			pmbuilder->OptLevel = targetMachine->getOptLevel();
			pmbuilder->SizeLevel = 0;
			pmbuilder->DisableTailCalls = !optimize;
			pmbuilder->DisableUnrollLoops = !optimize;
			pmbuilder->SLPVectorize = optimize;
			pmbuilder->LoopVectorize = optimize;
			pmbuilder->LoopsInterleaved = !optimize;
			pmbuilder->RerollLoops = optimize;
			pmbuilder->DisableGVNLoadPRE = !optimize;
			pmbuilder->MergeFunctions = optimize;
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
			

			llvm::TargetLibraryInfoImpl tlii(llvm::Triple(llvmModule.getTargetTriple()));
			tlii.disableAllFunctions();

			llvm::LibFunc chkstk;
			
			//if (!tlii.getLibFunc(llvm::LibFunc, chkstk)) {
			//	reportError("LLVM Error: __chkstk function does not exist");
			//}

			//tlii.setAvailable(chkstk);

			pmbuilder->LibraryInfo = &tlii;

			if (optimize) {
				pmbuilder->Inliner = llvm::createAlwaysInlinerLegacyPass(false);
			}
			else {
				targetMachine->adjustPassManager(*pmbuilder);

				pmbuilder->addExtension(llvm::PassManagerBuilder::EP_EarlyAsPossible, addDiscriminatorsPass);
				pmbuilder->Inliner = llvm::createFunctionInliningPass(llvm::CodeGenOpt::Aggressive, 0, false);
			}

			llvm::legacy::FunctionPassManager fpm(&llvmModule);
			fpm.add(new llvm::TargetLibraryInfoWrapperPass(tlii));
			fpm.add(llvm::createTargetTransformInfoWrapperPass(targetMachine->getTargetIRAnalysis()));
			
#if BUILD_DEBUG
			fpm.add(llvm::createVerifierPass());
#endif
			
			pmbuilder->populateFunctionPassManager(fpm);

			llvm::legacy::PassManager pass;
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

			llvmModule.print(irOut, nullptr);

			output.flush();
		}
	}
error:;
}

#else

void runLlvm() {
	printf("LLVM is not supported in this build.\n");
}

#endif