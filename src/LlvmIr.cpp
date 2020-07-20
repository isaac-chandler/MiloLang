#include "Basic.h"

#include "CoffWriter.h"

static llvm::Type *getLlvmType(llvm::LLVMContext &context, Type *type) {
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
	else if (type == TYPE_VOID_POINTER || type == &TYPE_TYPE) {
		return llvm::Type::getInt8PtrTy(context);
	}
	else {
		return nullptr;
	}
}

struct State {
	llvm::LLVMContext &context;
	llvm::IRBuilder<> &builder;
	llvm::Function *function;
	llvm::BasicBlock *entryBlock;
};

llvm::Value *generateLllvmIr(State *state, Expr *expr) {
	return nullptr;
}

void runLlvm() {
	llvm::LLVMContext context;

	llvm::Module llvmModule("llvm_out", context);
	llvm::IRBuilder<> builder(context);


	State state{ context, builder };

	while (true) {
		auto job = coffWriterQueue.take();

		if (job.flavor == CoffJobFlavor::FUNCTION) {
			auto function = job.function;

			if (!function) {
				break;
			}

			auto functionType = llvm::FunctionType::get(llvm::Type::getVoidTy(context), false);

			
			if (function->flags & EXPR_FUNCTION_IS_EXTERNAL) {
				auto external = llvmModule.getOrInsertFunction(toCString(function->valueOfDeclaration->name), functionType);
			}
			else {
				llvm::Function *llvmFunction;

				if (function->valueOfDeclaration) {
					llvmFunction = llvm::Function::Create(functionType, function->valueOfDeclaration->enclosingScope == &globalBlock ? llvm::Function::ExternalLinkage : llvm::Function::PrivateLinkage, toCString(function->valueOfDeclaration->name), llvmModule);


				}
				else {
					llvmFunction = llvm::Function::Create(functionType, llvm::Function::PrivateLinkage, "");
				}

				auto entry = llvm::BasicBlock::Create(context, "entry", llvmFunction);

				auto code = llvm::BasicBlock::Create(context, "code", llvmFunction);

				builder.SetInsertPoint(code);


				state.function = llvmFunction;
				state.entryBlock = entry;

				generateLllvmIr(&state, function->body);

				builder.CreateRetVoid();

				builder.SetInsertPoint(entry);
				builder.CreateBr(code);
			}

		}
	}

	
	//llvmModule.dump();
	
	//llvmModule.print(llvm::errs(), nullptr);

	llvm::InitializeAllTargetInfos();
	llvm::InitializeAllTargets();
	llvm::InitializeAllTargetMCs();
	llvm::InitializeAllAsmParsers();
	llvm::InitializeAllAsmPrinters();

	auto targetTriple = llvm::sys::getDefaultTargetTriple();

	std::string error;
	auto target = llvm::TargetRegistry::lookupTarget(targetTriple, error);

	if (!target) {
		reportError("LLVM Errror: %s", error.c_str());
		goto error;

	}
	{
		auto cpu = "generic";
		auto features = "";

		llvm::TargetOptions options;

		auto rm = llvm::Optional<llvm::Reloc::Model>();

		auto targetMachine = target->createTargetMachine(targetTriple, cpu, features, options, rm);

		llvmModule.setDataLayout(targetMachine->createDataLayout());

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