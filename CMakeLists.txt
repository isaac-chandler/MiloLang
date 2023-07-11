cmake_minimum_required(VERSION 3.10)
project(Milo)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

set(FILES 
src/Basic.cpp
src/Block.cpp
src/BucketedArenaAllocator.cpp
src/CodeView.cpp
src/CoffWriter.cpp
src/CompilerMain.cpp
src/Error.cpp
src/Find.cpp
src/Identifier.cpp
src/Infer.cpp
src/IrGenerator.cpp
src/IrRunner.cpp
src/Lexer.cpp
src/LlvmIr.cpp
src/Parser.cpp
src/Polymorph.cpp
src/String.cpp
src/TypeTable.cpp
src/UTF.cpp
)


include_directories(deps/include)
link_directories(deps/lib)
add_compile_definitions(BUILD_WINDOWS=0 BUILD_LINUX=1 "BUILD_DEBUG=$<IF:$<CONFIG:Debug>,1,0>" BUILD_NO_LLVM=1)

add_executable(milo ${FILES})
target_link_libraries(milo 
dyncall_s
dyncallback_s
)
if(MSVC)
  target_compile_options(milo PRIVATE /W4 /WX)
else()
  target_compile_options(milo PRIVATE -Wall -Wextra)
endif()
