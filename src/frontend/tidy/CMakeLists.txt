set(LLVM_LINK_COMPONENTS support)
set(LLVM_USED_LIBS clangTooling clangBasic clangAST)

add_clang_executable(tidy
  Tidy.cpp
  )

target_link_libraries(tidy
  clangTooling
  clangBasic
  clangASTMatchers
  )
