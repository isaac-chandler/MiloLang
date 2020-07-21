#include "Basic.h"

#include "CoffWriter.h"
#include "Lexer.h"
#include "IrGenerator.h"

static llvm::Type *createLlvmType(llvm::LLVMContext &context, Type *type);

static llvm::Type *getLlvmType(llvm::LLVMContext &context, Type *type) {
	if (!type->llvmType)
		type->llvmType = createLlvmType(context, type);

	return type->llvmType;
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
	else if (type == &TYPE_U64 || type == &TYPE_S64) {
		return llvm::Type::getInt64Ty(context);
	}
	else if (type == &TYPE_F32) {
		return llvm::Type::getFloatTy(context);
	}
	else if (type == &TYPE_F64) {
		return llvm::Type::getDoubleTy(context);
	}
	else if (type == TYPE_VOID_POINTER || type == &TYPE_TYPE || type == &TYPE_STRING) {
		return llvm::Type::getInt8PtrTy(context);
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
				return llvm::StructType::create(toCString(array->name), pointer, int64, int64);
			}
			else {
				return llvm::StructType::create(toCString(array->name), pointer, int64);
			}
		}
	}
	else if (type->flavor == TypeFlavor::STRUCT) {

		if (type->flags & TYPE_STRUCT_IS_UNION) {
			assert(type->size % type->alignment == 0);

			// LLVM doesn't support union types so we just create an array of integers that are the correct size to give the struct the desired alignment
			return llvm::ArrayType::get(llvm::IntegerType::get(context, type->alignment * 8), type->size / type->alignment);
		}
		else {
			auto struct_ = static_cast<TypeStruct *>(type);

			auto llvmType = llvm::StructType::create(context, toCString(struct_->name));

			type->llvmType = llvmType; // Put the empty struct on the type so that if we have a struct that points to itself we don't infinitely recurse

			Array<llvm::Type *> body;

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
		u64 count = function->argumentCount + function->returnCount - 1;
		llvm::Type **arguments = new llvm::Type * [count];

		for (u64 i = 0; i < function->argumentCount; i++) {
			arguments[i] = getLlvmType(context, function->argumentTypes[i]);
		}

		for (u64 i = 1; i < function->returnCount; i++) {
			arguments[function->argumentCount + i - 1] = llvm::PointerType::getUnqual(getLlvmType(context, function->returnTypes[i]));
		}

		return llvm::PointerType::getUnqual(llvm::FunctionType::get(getLlvmType(context, function->returnTypes[0]), llvm::ArrayRef(arguments, arguments + count), false));
	}
	else {
		assert(false);
		return nullptr;
	}
}

struct State {
	llvm::LLVMContext &context;
	llvm::IRBuilder<> &builder;
	llvm::Function *function;
	llvm::BasicBlock *entryBlock;
};

llvm::Value *generateLlvmIr(State *state, Expr *expr);


struct Loop {
	struct ExprLoop *loop;
	llvm::BasicBlock *start;

	Array<llvm::BasicBlock *> endPatches;
};

static Array<Expr *> deferStack;
static Block *currentBlock;

static Array<Loop> loopStack;
static u64 loopCount;

static llvm::Value *allocateType(State *state, Type *type) {
	auto old = state->builder.GetInsertBlock();

	state->builder.SetInsertPoint(state->entryBlock);
	auto value = state->builder.CreateAlloca(getLlvmType(state->context, type));

	state->builder.SetInsertPoint(old);
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



static void generateIncrement(State *state, ExprLoop *loop) {
	auto it = loop->iteratorBlock.declarations[0];
	auto it_index = loop->iteratorBlock.declarations[1];

	if (loop->forBegin->type->flavor == TypeFlavor::POINTER) {
		
	}
	else if (loop->forBegin->type->flavor == TypeFlavor::ARRAY) {
		
	}
	else {
	
	}
}


static void exitBlock(State *state, Block *block, bool isBreak) {
	for (s64 i = deferStack.count - 1; i >= 0; --i) {
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

llvm::Value *generateLllvmIr(State *state, Expr *expr) {
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
				return generateLllvmIr(state, right);
			}
			else if (binary->flags & EXPR_CAST_IS_BITWISE) {
				return state->builder.CreateBitCast(generateLlvmIr(state, right), getLlvmType(state->context, castTo));
			}

			switch (castTo->flavor) {
			case TypeFlavor::STRUCT: {
				assert(castTo == TYPE_ANY);

				auto storage = allocateType(state, right->type);

				state->builder.CreateStore(generateLllvmIr(state, right), storage);

				auto int8p = llvm::Type::getInt8PtrTy(state->context);

				auto any = llvm::ConstantStruct::get(static_cast<llvm::StructType *>(getLlvmType(state->context, TYPE_ANY)),
					llvm::UndefValue::get(int8p),
					llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(getLlvmType(state->context, TYPE_TYPE_INFO))));

				return state->builder.CreateInsertValue(any, state->builder.CreateBitCast(storage, int8p), 0);
			}
			case TypeFlavor::BOOL: {
				if (right->type->flavor == TypeFlavor::STRING) {
					auto nullCheck = state->builder.GetInsertBlock();

					auto string = generateLllvmIr(state, right);

					auto cmp = state->builder.CreateICmpEQ(string, llvm::ConstantPointerNull::get(llvm::Type::getInt8PtrTy(state->context)));
					
					auto zeroCheck = llvm::BasicBlock::Create(state->context, "", state->function);
					auto result = llvm::BasicBlock::Create(state->context, "", state->function);

					state->builder.CreateCondBr(cmp, result, zeroCheck);

					state->builder.SetInsertPoint(zeroCheck);

					auto int8 = llvm::Type::getInt8Ty(state->context);
					cmp = state->builder.CreateICmpNE(state->builder.CreateLoad(int8, string), llvm::ConstantInt::get(int8, 0));

					state->builder.CreateBr(result);


					state->builder.SetInsertPoint(result);
					
					auto int1 = llvm::Type::getInt1Ty(state->context);
					auto phi = state->builder.CreatePHI(int1, 2);
					
					phi->addIncoming(llvm::ConstantInt::get(int1, 0), nullCheck);
					phi->addIncoming(cmp, zeroCheck);

					return phi;
				}
				else if (right->type->flavor == TypeFlavor::ARRAY) {
					if (right->type->flags & TYPE_ARRAY_IS_FIXED) {
						return llvm::ConstantInt::get(llvm::Type::getInt1Ty(state->context), 1);
					}
					else {
						return state->builder.CreateICmpNE(state->builder.CreateExtractValue(generateLllvmIr(state, right), 1), 
							llvm::ConstantInt::get(llvm::Type::getInt64Ty(state->context), 0));
					}
				}
				else if (right->type->flavor == TypeFlavor::FLOAT) {
					auto value = generateLllvmIr(state, right);

					return state->builder.CreateFCmpONE(value, llvm::ConstantFP::get(value->getType(), 0));
				}
				else if (right->type->flavor == TypeFlavor::INTEGER) {
					auto value = generateLllvmIr(state, right);

					return state->builder.CreateICmpNE(value, llvm::ConstantInt::get(value->getType(), 0));
				}
				else {
					auto value = generateLllvmIr(state, right);
					assert(value->getType()->isPointerTy());

					return state->builder.CreateICmpNE(value, llvm::ConstantPointerNull::get(static_cast<llvm::PointerType *>(value->getType())));
				}
			}
			case TypeFlavor::FLOAT: {
				auto value = generateLllvmIr(state, right);

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
				auto value = generateLllvmIr(state, right);

				if (right->type->flavor == TypeFlavor::POINTER || right->type->flavor == TypeFlavor::FUNCTION) {
					return state->builder.CreatePtrToInt(value, getLlvmType(state->context, castTo));
				}
				else if (right->type->flavor == TypeFlavor::FLOAT) {
					if (castTo->flags & TYPE_INTEGER_IS_SIGNED) {
						return state->builder.CreateSIToFP(value, getLlvmType(state->context, castTo));
					}
					else {
						return state->builder.CreateUIToFP(value, getLlvmType(state->context, castTo));
					}
				}
				else {
					return state->builder.CreateIntCast(value, getLlvmType(state->context, castTo), castTo->flags &TYPE_INTEGER_IS_SIGNED ? true : false);
				}
			}
			case TypeFlavor::ARRAY: {
				auto value = generateLllvmIr(state, right);

				if (right->type->flags & TYPE_ARRAY_IS_DYNAMIC) {
					assert(!(left->type->flags & TYPE_ARRAY_IS_FIXED));


					return nullptr;
				}
				else {
					assert(right->type->flags & TYPE_ARRAY_IS_FIXED);
					assert(!(left->type->flags & TYPE_ARRAY_IS_DYNAMIC));

					return nullptr;
				}
			}
			case TypeFlavor::POINTER: {
				if (right->type->flavor == TypeFlavor::ARRAY && (right->type->flags & TYPE_ARRAY_IS_FIXED)) {
					return nullptr;
				}
				else {
					return state->builder.CreatePointerCast(;
				}
			}
			case TypeFlavor::FUNCTION:
			case TypeFlavor::STRING:
				return nullptr; // These casts should be a nop
			case TypeFlavor::TYPE:
			case TypeFlavor::VOID:
				assert(false);
				return nullptr;
			}

			assert(false);
			return nullptr;
		}
		case TOKEN('['): {
			return nullptr;
		}
		case TokenT::EQUAL:
		case TokenT::NOT_EQUAL: {
			assert(left->type == right->type);

			return nullptr;
		}
		case TokenT::GREATER_EQUAL:
		case TokenT::LESS_EQUAL:
		case TOKEN('>'):
		case TOKEN('<'): {
			return nullptr;
		}
		case TOKEN('+'):
		case TOKEN('-'): {
			if (left->type->flavor == TypeFlavor::POINTER) {
				if (right->type->flavor == TypeFlavor::POINTER) {
					assert(binary->op == TOKEN('-'));
					return nullptr;
				}
				else {
					return nullptr;
				}
			}
			else {
				return nullptr;
			}
		}
		case TOKEN('&'):
		case TOKEN('|'):
		case TOKEN('^'):
		case TokenT::SHIFT_LEFT:
		case TokenT::SHIFT_RIGHT: {
			return nullptr;
		}
		case TOKEN('*'):
		case TOKEN('/'):
		case TOKEN('%'): {
			return nullptr;
		}
		case TOKEN('='): {
			if (left->flavor == ExprFlavor::IDENTIFIER && !static_cast<ExprIdentifier *>(left)->structAccess) {
				auto identifier = static_cast<ExprIdentifier *>(left);

				if (identifier->declaration->enclosingScope == &globalBlock) {
					return nullptr;
				}
				else {
					return nullptr;
				}
			}
			else {
				return nullptr;
			}

			return nullptr;
		}
		case TokenT::PLUS_EQUALS:
		case TokenT::MINUS_EQUALS: {
			if (left->type->flavor == TypeFlavor::POINTER) {
				return nullptr;
			}
			else {
				return nullptr;
			}
		case TokenT::LOGIC_AND:
		case TokenT::LOGIC_OR: {
			return nullptr;
		}
		case TokenT::AND_EQUALS:
		case TokenT::OR_EQUALS:
		case TokenT::XOR_EQUALS:
		case TokenT::SHIFT_LEFT_EQUALS:
		case TokenT::SHIFT_RIGHT_EQUALS: {
			return nullptr;
		}
		case TokenT::TIMES_EQUALS:
		case TokenT::DIVIDE_EQUALS:
		case TokenT::MOD_EQUALS: {
			return nullptr;
		}
		default:
			assert(false);
			return nullptr;
		}
		break;
	}
	case ExprFlavor::BLOCK: {
		auto block = static_cast<ExprBlock *>(expr);

		for (auto declaration : block->declarations.declarations) {
			if (!(declaration->flags & (DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING | DECLARATION_IS_IMPLICIT_IMPORT))) {
		
			}
		}

		for (auto subExpr : block->exprs) {
			generateLllvmIr(state, subExpr);
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


		for (u64 i = loopCount; i-- != 0;) {

			if (loopStack[i].loop == break_->refersTo) {
				exitBlock(state, &loopStack[i].loop->iteratorBlock, true);
				loopStack[i].endPatches.add(state->builder.GetInsertBlock());

				state->builder.SetInsertPoint(llvm::BasicBlock::Create(state->context, "", state->function));
				break;
			}
		}

		return nullptr;
	}
	case ExprFlavor::CONTINUE: {
		auto continue_ = static_cast<ExprBreakOrContinue *>(expr);

		u64 begin;


		for (u64 i = loopCount; i-- != 0;) {
			if (loopStack[i].loop == continue_->refersTo) {
				exitBlock(state, &loopStack[i].loop->iteratorBlock, false);
				
				state->builder.CreateBr(loopStack[i].start);

				state->builder.SetInsertPoint(llvm::BasicBlock::Create(state->context, "", state->function));
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

		return nullptr;
	}
	case ExprFlavor::INT_LITERAL: {
		return nullptr;
	}
	case ExprFlavor::FLOAT_LITERAL: {
		return nullptr;
	}
	case ExprFlavor::FOR: {
		auto loop = static_cast<ExprLoop *>(expr);

		auto it = loop->iteratorBlock.declarations[0];
		auto it_index = loop->iteratorBlock.declarations[1];
		
		if ((loop->forBegin->type->flavor != TypeFlavor::INTEGER) && !(loop->flags & EXPR_FOR_BY_POINTER)) {
		
		}
		else {
		
		}

		if (loop->forBegin->type->flavor == TypeFlavor::ARRAY) {
			auto begin = loop->forBegin;

		
			if (loop->forBegin->type->flags & TYPE_ARRAY_IS_FIXED) {
			
			}
			else {
			
			}
		}
		else {
			
		}

		
		pushLoop(state, loop);

		u64 irEnd;

		if (loop->forEnd) {
		
		}

		if (loop->forBegin->type->flavor == TypeFlavor::STRING && !(loop->flags & EXPR_FOR_BY_POINTER)) {

		}
		else {

		}

		if (loop->forBegin->type->flavor == TypeFlavor::STRING) {
			// @StringFormat

		}
		else if (loop->forBegin->type->flavor == TypeFlavor::ARRAY) {
			if (loop->forBegin->type->flags & TYPE_ARRAY_IS_FIXED) {

			}
			else {

			}

		}
		else {

		}

		if (!(loop->flags & EXPR_FOR_BY_POINTER)) {
			if (loop->forBegin->type->flavor == TypeFlavor::ARRAY || loop->forBegin->type->flavor == TypeFlavor::POINTER) {

			}
		}

		deferStack.add(loop);

		if (loop->body) {
			generateLlvmIr(state, loop->body);
		}

		exitBlock(state, &loop->iteratorBlock, false);

		Expr *inc = deferStack.pop();
		assert(inc == loop);

		if (loop->completedBody) {
			generateLlvmIr(state, loop->completedBody);
		}

		popLoop(state);

		return nullptr;
	}
	case ExprFlavor::FUNCTION: {

		return nullptr;
	}
	case ExprFlavor::STRING_LITERAL: {
		return nullptr;
	}
	case ExprFlavor::FUNCTION_CALL: {
		//generateCall(state, call, dest, nullptr);

		return nullptr;
	}
	case ExprFlavor::IDENTIFIER: {
		auto identifier = static_cast<ExprIdentifier *>(expr);

		if (identifier->structAccess) {
			return nullptr;
		}
		else {
			if (identifier->declaration->enclosingScope == &globalBlock) {
				return nullptr;
			}
			else {
				return nullptr;
			}
		}
	}
	case ExprFlavor::SWITCH: {
		auto switch_ = static_cast<ExprSwitch *>(expr);


		ExprSwitch::Case *else_ = nullptr;

		for (auto &case_ : switch_->cases) {
			if (case_.condition) {
				auto condition = static_cast<ExprBinaryOperator *>(case_.condition);

				assert(condition->flavor == ExprFlavor::BINARY_OPERATOR);

				assert(condition->right->type == switch_->condition->type);
			}
			else {
				else_ = &case_;
			}
		}


		if (else_) {
		
		}

		for (auto &case_ : switch_->cases) {
			generateLlvmIr(state, case_.block);

			if (!case_.fallsThrough && &case_ + 1 != switch_->cases.end()) {
				
			}
		}

		if (!else_) {
			
		}

		for (u64 i = 0; i + 1 < switch_->cases.count; i++) {
			auto &case_ = switch_->cases[i];

			if (!case_.fallsThrough) {

			}
		}

		return nullptr;
	}
	case ExprFlavor::IF: {
		auto ifElse = static_cast<ExprIf *>(expr);

		llvm::Value *condition = generateLlvmIr(state, ifElse->condition);

		if (ifElse->ifBody && ifElse->elseBody) {
			/*u64 patchIfZ = state->ir.count;
			Ir &ifZ = state->ir.add();
			ifZ.op = IrOp::IF_Z_GOTO;
			ifZ.a = conditionReg;
			ifZ.opSize = 1;

			addLineMarker(state, ifElse->ifBody);*/
			generateLlvmIr(state, ifElse->ifBody);

			/*u64 patchJump = state->ir.count;
			Ir &jump = state->ir.add();
			jump.op = IrOp::GOTO;

			state->ir[patchIfZ].b = state->ir.count;

			addLineMarker(state, ifElse->elseBody);*/

			generateLlvmIr(state, ifElse->elseBody);

			//state->ir[patchJump].b = state->ir.count;
		}
		else if (ifElse->ifBody) {
			/*u64 patchIfZ = state->ir.count;
			Ir &ifZ = state->ir.add();
			ifZ.op = IrOp::IF_Z_GOTO;
			ifZ.a = conditionReg;
			ifZ.opSize = 1;*/

			//addLineMarker(state, ifElse->ifBody);
			generateLlvmIr(state, ifElse->ifBody);

			//state->ir[patchIfZ].b = state->ir.count;
		}
		else if (ifElse->elseBody) {
			/*u64 patchIfNZ = state->ir.count;
			Ir &ifNZ = state->ir.add();
			ifNZ.op = IrOp::IF_NZ_GOTO;
			ifNZ.a = conditionReg;
			ifNZ.opSize = 1;

			addLineMarker(state, ifElse->elseBody);*/
			generateLlvmIr(state, ifElse->elseBody);

			//state->ir[patchIfNZ].b = state->ir.count;
		}

		return nullptr;
	}
	case ExprFlavor::COMMA_ASSIGNMENT: {
		auto comma = static_cast<ExprCommaAssignment *>(expr);

		/*u64 address = loadAddressOf(state, comma->left[0], state->nextRegister++);

		dest = allocateSpaceForType(state, comma->left[0]->type);

		generateCall(state, static_cast<ExprFunctionCall *>(comma->call), dest, comma);*/

		/*Ir &write = state->ir.add();
		write.op = IrOp::WRITE;
		write.opSize = comma->left[0]->type->size;
		write.a = address;
		write.b = dest;*/


		return nullptr;
	}
	case ExprFlavor::RETURN: {
		auto return_ = static_cast<ExprReturn *>(expr);

		exitBlock(state, nullptr, true);

		u64 result = 0;

		if (return_->returns.count) {
			//u64 result = generateIr(state, return_->returns.values[0], state->nextRegister++);

			for (u64 i = 1; i < return_->returns.count; i++) {
				auto store = generateLlvmIr(state, return_->returns.values[i]);

				/*Ir &write = state->ir.add();
				write.op = IrOp::WRITE;
				write.a = return_->returnsFrom->returns.declarations[i]->physicalStorage;
				write.b = store;
				write.opSize = return_->returns.values[i]->type->size;*/
			}

			/*Ir &ir = state->ir.add();
			ir.op = IrOp::RETURN;
			ir.a = result;
			ir.opSize = return_->returns.values[0]->type->size;*/

		}
		else {
			/*Ir &ir = state->ir.add();
			ir.op = IrOp::RETURN;
			ir.a = 0;
			ir.opSize = 0;*/
		}


		return nullptr;
	}
	case ExprFlavor::STRUCT_DEFAULT: {
		u64 memberSize = 0;

		auto struct_ = static_cast<TypeStruct *>(expr->type);

		for (auto decl : struct_->members.declarations) {
			if (decl->flags & (DECLARATION_IS_UNINITIALIZED | DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING)) continue;

			if (decl->physicalStorage & 7) {
				memberSize = my_max(static_cast<ExprLiteral *>(decl->type)->typeValue->size, memberSize);
			}
		}

		/*u64 addressReg = state->nextRegister++;
		u64 memberTemp = state->nextRegister;
		state->nextRegister += (memberSize + 7) / 8;*/

		for (auto decl : struct_->members.declarations) {
			if (decl->flags & (DECLARATION_IS_UNINITIALIZED | DECLARATION_IS_CONSTANT | DECLARATION_IMPORTED_BY_USING)) continue;

			//generateIrForceDest(state, decl->initialValue, dest + decl->physicalStorage / 8);
		}

		return nullptr;
	}
	case ExprFlavor::TYPE_LITERAL: {
		auto type = static_cast<ExprLiteral *>(expr)->typeValue;

		if (type->flavor == TypeFlavor::NAMESPACE) {
			reportError(expr, "Error: Cannot operate on a namespace");
			return nullptr;
		}


		return nullptr;
	}
	case ExprFlavor::UNARY_OPERATOR: {
		ExprUnaryOperator *unary = static_cast<ExprUnaryOperator *>(expr);

		switch (unary->op) {
		case TOKEN('*'): {
			return nullptr; //loadAddressOf(state, unary->value, dest);
		}
		case TOKEN('-'): {
			auto toNegate = generateLlvmIr(state, unary->value);

			/*Ir &negate = state->ir.add();
			negate.op = IrOp::NEG;
			negate.a = toNegate;
			negate.opSize = unary->value->type->size;
			negate.destSize = unary->type->size;
			negate.dest = dest;*/

			return nullptr;
		}
		case TOKEN('~'): {
			auto toInvert = generateLlvmIr(state, unary->value);

			/*Ir &invert = state->ir.add();
			invert.op = IrOp::NOT;
			invert.a = toInvert;
			invert.opSize = unary->value->type->size;
			invert.destSize = unary->type->size;
			invert.dest = dest;*/

			return nullptr;
		}
		case TOKEN('!'): {
			auto toInvert = generateLlvmIr(state, unary->value);

			/*Ir &invert = state->ir.add();
			invert.op = IrOp::EQUAL;
			invert.a = toInvert;
			invert.b = 0;
			invert.opSize = unary->value->type->size;
			invert.dest = dest;*/

			return nullptr;
		}
		case TokenT::SHIFT_LEFT: {
			auto addressReg = generateLlvmIr(state, unary->value);

			/*Ir &read = state->ir.add();
			read.op = IrOp::READ;
			read.opSize = 8;
			read.destSize = unary->type->size;
			read.a = addressReg;
			read.dest = dest;*/

			return nullptr;
		}
		case TokenT::TYPE_INFO: {
			auto type = generateLllvmIr(state, unary->value);


			return nullptr;
		}
		default:
			assert(false);
			return nullptr;
		}
	}
	case ExprFlavor::WHILE: {
		ExprLoop *loop = static_cast<ExprLoop *>(expr);

		pushLoop(state, loop);

		auto conditionReg = generateLlvmIr(state, loop->whileCondition);

		//u64 patch = state->ir.count; // Patch this manually so it doesn't skip the completed block if it exists

		//Ir &ifZ = state->ir.add();
		//ifZ.op = IrOp::IF_Z_GOTO;
		//ifZ.a = conditionReg;
		//ifZ.opSize = 1;

		if (loop->body) {
			//addLineMarker(state, loop->body);
			generateLlvmIr(state, loop->body);
		}

		/*Ir &jump = state->ir.add();
		jump.op = IrOp::GOTO;
		jump.b = loopStack[loopCount - 1].start;

		state->ir[patch].b = state->ir.count;*/

		if (loop->completedBody) {
			//addLineMarker(state, loop->completedBody);
			generateLlvmIr(state, loop->completedBody);
		}

		popLoop(state);

		return nullptr;
	}
	case ExprFlavor::ARRAY: {
		auto array = static_cast<ExprArray *>(expr);

		if (array->type->flags & TYPE_ARRAY_IS_FIXED) {
			auto elementType = static_cast<TypeArray *>(array->type)->arrayOf;

		/*	u64 addressReg = state->nextRegister++;
			u64 valueReg = allocateSpaceForType(state, elementType);
			for (u64 i = 0; i < array->count; i++) {
				if (i + 1 < array->count && array->storage[i + 1] == nullptr) {
					u64 countReg = state->nextRegister++;
					Ir &count = state->ir.add();
					count.op = IrOp::IMMEDIATE;
					count.dest = countReg;
					count.a = array->count - i;
					count.opSize = 8;

					Ir &address = state->ir.add();
					address.op = IrOp::ADDRESS_OF_LOCAL;
					address.dest = addressReg;
					address.a = dest;
					address.b = dest + i * elementType->size;
					address.opSize = 8;

					u64 patch = state->ir.count;

					generateIrForceDest(state, array->storage[i], valueReg);

					Ir &write = state->ir.add();
					write.op = IrOp::WRITE;
					write.a = addressReg;
					write.b = valueReg;
					write.opSize = elementType->size;

					Ir &add = state->ir.add();
					add.op = IrOp::ADD_CONSTANT;
					add.dest = addressReg;
					add.a = addressReg;
					add.b = elementType->size;
					add.opSize = 8;

					Ir &dec = state->ir.add();
					dec.op = IrOp::ADD_CONSTANT;
					dec.dest = countReg;
					dec.a = countReg;
					dec.b = static_cast<u64>(-1LL);
					dec.opSize = 8;

					Ir &branch = state->ir.add();
					branch.op = IrOp::IF_NZ_GOTO;
					branch.a = countReg;
					branch.b = patch;
					branch.opSize = 8;

					break;
				}
				else {
					Ir &address = state->ir.add();
					address.op = IrOp::ADDRESS_OF_LOCAL;
					address.dest = addressReg;
					address.a = dest;
					address.b = dest + i * elementType->size;
					address.opSize = 8;

					generateIrForceDest(state, array->storage[i], valueReg);

					Ir &write = state->ir.add();
					address.op = IrOp::WRITE;
					address.a = addressReg;
					address.b = valueReg;
					address.opSize = elementType->size;
				}
			}*/
		}
		else {
			// The only time an array literal should have a type other than fixed is the compiler generated default empty array value
			assert(array->count == 0);

			/*Ir &set = state->ir.add();
			set.op = IrOp::SET;
			set.dest = dest;
			set.a = 0;
			set.opSize = array->type->size;
			set.destSize = set.opSize;*/
		}

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

void runLlvm() {
	llvm::LLVMContext context;

	llvm::Module llvmModule("llvm_out", context);
	llvm::IRBuilder<> builder(context);

	State state{ context, builder };

	std::error_code errorCode;
	std::string verifyOutput;

	llvm::raw_string_ostream verifyStream(verifyOutput);

	llvm::InitializeAllTargetInfos();
	llvm::InitializeAllTargets();
	llvm::InitializeAllTargetMCs();
	llvm::InitializeAllAsmParsers();
	llvm::InitializeAllAsmPrinters();

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

		while (true) {
			auto job = coffWriterQueue.take();

			if (job.flavor == CoffJobFlavor::FUNCTION) {
				auto function = job.function;

				if (!function) {
					break;
				}

				auto type = getLlvmType(context, function->type);

				assert(type->isPointerTy());
				assert(type->getPointerElementType()->isFunctionTy());

				auto functionType = static_cast<llvm::FunctionType *>(type->getPointerElementType());

				if (function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
					auto external = llvmModule.getOrInsertFunction(toCString(function->valueOfDeclaration->name), functionType);
				}
				else {
					llvm::Function *llvmFunction;

					if (function->valueOfDeclaration) {
						llvmFunction = llvm::Function::Create(functionType, function->valueOfDeclaration->enclosingScope == &globalBlock ? llvm::Function::ExternalLinkage : llvm::Function::PrivateLinkage, toCString(function->valueOfDeclaration->name), llvmModule);


					}
					else {
						llvmFunction = llvm::Function::Create(functionType, llvm::Function::PrivateLinkage, "", llvmModule);
					}

					auto entry = llvm::BasicBlock::Create(context, "entry", llvmFunction);

					auto code = llvm::BasicBlock::Create(context, "code", llvmFunction);

					builder.SetInsertPoint(code);


					state.function = llvmFunction;
					state.entryBlock = entry;

					generateLllvmIr(&state, function->body);

					if (functionType->getReturnType()->isVoidTy()) {
						builder.CreateRetVoid();
					}
					else {
						// @Incomplete: Actually detect the case where not all control paths return and issue an error
						builder.CreateRet(llvm::Constant::getNullValue(functionType->getReturnType()));
					}

					builder.SetInsertPoint(entry);

					builder.CreateBr(code);

					if (llvm::verifyFunction(*llvmFunction, &verifyStream)) {
						verifyStream.flush();

						reportError("LLVM Error in %s: %s", function->valueOfDeclaration ? toCString(function->valueOfDeclaration->name) : "(function)", verifyOutput.c_str());
						goto error;
					}
				}

			}
		}

		auto int32 = llvm::Type::getInt32Ty(context);
		auto fltused = new llvm::GlobalVariable(llvmModule, int32, true, llvm::GlobalValue::ExternalLinkage, llvm::ConstantInt::get(int32, 0), "_fltused");

		std::cout << verifyOutput << "\n";

		//llvmModule.dump();

		//llvmModule.print(llvm::errs(), nullptr);
		std::error_code err;

		llvm::raw_fd_ostream output("out.obj", err);

		llvm::legacy::PassManager pass;

		auto fileType = llvm::CGFT_ObjectFile;

		if (targetMachine->addPassesToEmitFile(pass, output, nullptr, fileType)) {
			reportError("LLVM Error: can't emit a file of this type");
		}

		pass.run(llvmModule);

		output.flush();
	}
error:;
}