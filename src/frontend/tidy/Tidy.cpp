#include <iostream>
#include <sstream>

#include "../AngelixCommon.h"


StatementMatcher TidyableExtraIf =
  ifStmt(hasCondition(integerLiteral(equals(1))),
         unless(hasElse(stmt())),
         unless(hasDescendant(compoundStmt()))
        ).bind("tidyable");


class ExtraIfHandler : public MatchFinder::MatchCallback {
public:
  ExtraIfHandler(Rewriter &Rewrite) : Rewrite(Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &Result) {
    if (const IfStmt *ifS = Result.Nodes.getNodeAs<clang::IfStmt>("tidyable")) {
      SourceManager &srcMgr = Rewrite.getSourceMgr();
      const LangOptions &langOpts = Rewrite.getLangOpts();

      if (insideMacro(ifS, srcMgr, langOpts))
        return;

      SourceRange expandedLoc = getExpandedLoc(ifS, srcMgr);

      std::pair<FileID, unsigned> decLoc = srcMgr.getDecomposedExpansionLoc(expandedLoc.getBegin());
      if (srcMgr.getMainFileID() != decLoc.first)
        return;

      unsigned beginLine = srcMgr.getExpansionLineNumber(expandedLoc.getBegin());
      unsigned beginColumn = srcMgr.getExpansionColumnNumber(expandedLoc.getBegin());
      unsigned endLine = srcMgr.getExpansionLineNumber(expandedLoc.getEnd());
      unsigned endColumn = srcMgr.getExpansionColumnNumber(expandedLoc.getEnd());

      std::cout << beginLine << " " << beginColumn << " " << endLine << " " << endColumn << "\n"
                << toString(ifS) << "\n";

      std::string replacement = toString(ifS->getThen());
      std::size_t found = replacement.find(";\n");
      if (found != std::string::npos)
        replacement.erase(found);
      Rewrite.ReplaceText(expandedLoc, replacement);
    }
  }

private:
  Rewriter &Rewrite;
};


class MyASTConsumer : public ASTConsumer {
public:
  MyASTConsumer(Rewriter &R) : HandlerForExtraIfs(R) {
    Matcher.addMatcher(TidyableExtraIf, &HandlerForExtraIfs);
  }

  void HandleTranslationUnit(ASTContext &Context) override {
    Matcher.matchAST(Context);
  }

private:
  ExtraIfHandler HandlerForExtraIfs;
  MatchFinder Matcher;
};


class TidyAction : public ASTFrontendAction {
public:
  TidyAction() {}

  void EndSourceFileAction() override {
    FileID ID = TheRewriter.getSourceMgr().getMainFileID();
    if (INPLACE_MODIFICATION) {
      //overwriteMainChangedFile(TheRewriter);
      TheRewriter.overwriteChangedFiles();
    } else {
      TheRewriter.getEditBuffer(ID).write(llvm::outs());
    }
  }

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, StringRef file) override {
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return llvm::make_unique<MyASTConsumer>(TheRewriter);
  }

private:
  Rewriter TheRewriter;
};


// Apply a custom category to all command-line options so that they are the only ones displayed.
static llvm::cl::OptionCategory MyToolCategory("angelix options");


int main(int argc, const char **argv) {
  // CommonOptionsParser constructor will parse arguments and create a
  // CompilationDatabase.  In case of error it will terminate the program.
  CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);

  // We hand the CompilationDatabase we created and the sources to run over into the tool constructor.
  ClangTool Tool(OptionsParser.getCompilations(), OptionsParser.getSourcePathList());

  return Tool.run(newFrontendActionFactory<TidyAction>().get());
}
