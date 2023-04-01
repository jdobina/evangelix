#include <iostream>
#include <sstream>

#include "../AngelixCommon.h"


bool insideSuspiciousScope(const clang::Stmt* expr, ASTContext* context, SourceManager &srcMgr) {
  static char *suspicious_line_env = getenv("ANGELIX_SUSPICIOUS_LINE");
  if (!suspicious_line_env) {
    std::cerr << "ANGELIX_SUSPICIOUS_LINE env variable not found\n";
    return false;
  }
  unsigned suspicious_line = atoi(suspicious_line_env);

  ArrayRef<ast_type_traits::DynTypedNode> parents = context->getParents(*expr);
  if (parents.size() > 0) {
    const ast_type_traits::DynTypedNode parentNode = *(parents.begin());
    const CompoundStmt *parent = parentNode.get<CompoundStmt>();
    if (!parent)
      return false;

    SourceRange expandedLoc = getExpandedLoc(parent, srcMgr);
    std::pair<FileID, unsigned> decLoc = srcMgr.getDecomposedExpansionLoc(expandedLoc.getBegin());
    if (srcMgr.getMainFileID() != decLoc.first)
      return false;

    unsigned beginLine = srcMgr.getExpansionLineNumber(expandedLoc.getBegin());
    unsigned beginColumn = srcMgr.getExpansionColumnNumber(expandedLoc.getBegin());
    unsigned endLine = srcMgr.getExpansionLineNumber(expandedLoc.getEnd());
    unsigned endColumn = srcMgr.getExpansionColumnNumber(expandedLoc.getEnd());

    std::cout << beginLine << " " << beginColumn << " " << endLine << " " << endColumn << "\n"
              << toString(parent) << "\n";

    if ((suspicious_line >= beginLine) && (suspicious_line <= endLine))
      return true;
  }

  return false;
}


class MissingReturnHandler : public MatchFinder::MatchCallback {
public:
  MissingReturnHandler(Rewriter &Rewrite) : Rewrite(Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &Result) {
    if (const IfStmt *ifS = Result.Nodes.getNodeAs<clang::IfStmt>("repairable")) {
      SourceManager &srcMgr = Rewrite.getSourceMgr();
      const LangOptions &langOpts = Rewrite.getLangOpts();

      if (insideMacro(ifS, srcMgr, langOpts))
        return;

      const Stmt *then = ifS->getThen();
      SourceRange expandedLoc = getExpandedLoc(then, srcMgr);

      std::pair<FileID, unsigned> decLoc = srcMgr.getDecomposedExpansionLoc(expandedLoc.getBegin());
      if (srcMgr.getMainFileID() != decLoc.first)
        return;

      if (!insideSuspiciousScope(then, Result.Context, srcMgr))
        return;

      unsigned beginLine = srcMgr.getExpansionLineNumber(expandedLoc.getBegin());
      unsigned beginColumn = srcMgr.getExpansionColumnNumber(expandedLoc.getBegin());
      unsigned endLine = srcMgr.getExpansionLineNumber(expandedLoc.getEnd());
      unsigned endColumn = srcMgr.getExpansionColumnNumber(expandedLoc.getEnd());

      std::cout << beginLine << " " << beginColumn << " " << endLine << " " << endColumn << "\n"
                << toString(then) << "\n";

      std::ostringstream stringStream;
      stringStream << "    "
                   << "if (1) return 0;"
                   << "\n";
      std::string insertion = stringStream.str();

      Rewrite.InsertText(then->getLocEnd(), insertion, true, true);
    }
  }

private:
  Rewriter &Rewrite;
};


class IfToElseIfHandler : public MatchFinder::MatchCallback {
public:
  IfToElseIfHandler(Rewriter &Rewrite) : Rewrite(Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &Result) {
    if (const IfStmt *ifS = Result.Nodes.getNodeAs<clang::IfStmt>("repairable")) {
      SourceManager &srcMgr = Rewrite.getSourceMgr();
      const LangOptions &langOpts = Rewrite.getLangOpts();

      if (insideMacro(ifS, srcMgr, langOpts))
        return;

      SourceRange expandedLoc = getExpandedLoc(ifS, srcMgr);

      std::pair<FileID, unsigned> decLoc = srcMgr.getDecomposedExpansionLoc(expandedLoc.getBegin());
      if (srcMgr.getMainFileID() != decLoc.first)
        return;

      if (!insideSuspiciousScope(ifS, Result.Context, srcMgr))
        return;

      unsigned beginLine = srcMgr.getExpansionLineNumber(expandedLoc.getBegin());
      unsigned beginColumn = srcMgr.getExpansionColumnNumber(expandedLoc.getBegin());
      unsigned endLine = srcMgr.getExpansionLineNumber(expandedLoc.getEnd());
      unsigned endColumn = srcMgr.getExpansionColumnNumber(expandedLoc.getEnd());

      std::cout << beginLine << " " << beginColumn << " " << endLine << " " << endColumn << "\n"
                << toString(ifS) << "\n";

      std::ostringstream stringStream;
      stringStream << "else ";
      std::string insertion = stringStream.str();

      Rewrite.InsertTextBefore(ifS->getLocStart(), insertion);
    }
  }

private:
  Rewriter &Rewrite;
};


class UninitVarHandler : public MatchFinder::MatchCallback {
public:
  UninitVarHandler(Rewriter &Rewrite) : Rewrite(Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &Result) {
    if (const DeclStmt *declS = Result.Nodes.getNodeAs<clang::DeclStmt>("repairable")) {
      SourceManager &srcMgr = Rewrite.getSourceMgr();
      const LangOptions &langOpts = Rewrite.getLangOpts();

      if (insideMacro(declS, srcMgr, langOpts))
        return;

      SourceRange expandedLoc = getExpandedLoc(declS, srcMgr);

      std::pair<FileID, unsigned> decLoc = srcMgr.getDecomposedExpansionLoc(expandedLoc.getBegin());
      if (srcMgr.getMainFileID() != decLoc.first)
        return;

      if (!insideSuspiciousScope(declS, Result.Context, srcMgr))
        return;

      if (!declS->isSingleDecl())
        return;

      const Decl *d = declS->getSingleDecl();
      if (!isa<VarDecl>(d))
        return;

      const VarDecl *vd = cast<VarDecl>(d);
      if (!vd->getType().getTypePtr()->isIntegerType())
        return;

      unsigned beginLine = srcMgr.getExpansionLineNumber(expandedLoc.getBegin());
      unsigned beginColumn = srcMgr.getExpansionColumnNumber(expandedLoc.getBegin());
      unsigned endLine = srcMgr.getExpansionLineNumber(expandedLoc.getEnd());
      unsigned endColumn = srcMgr.getExpansionColumnNumber(expandedLoc.getEnd());

      std::cout << beginLine << " " << beginColumn << " " << endLine << " " << endColumn << "\n"
                << toString(declS) << "\n";

      std::ostringstream stringStream;
      stringStream << vd->getType().getAsString() << " "
                   << vd->getNameAsString()
                   << " = 0;";
      std::string replacement = stringStream.str();

      Rewrite.ReplaceText(expandedLoc, replacement);
    }
  }

private:
  Rewriter &Rewrite;
};


class MyASTConsumer : public ASTConsumer {
public:
  MyASTConsumer(Rewriter &R) : HandlerForMissingReturns(R),
                               HandlerForIfToElseIfs(R),
                               HandlerForUninitVars(R) {
    if (getenv("ANGELIX_MISSING_RETURNS_DEFECT_CLASS"))
      Matcher.addMatcher(RepairableMissingReturn, &HandlerForMissingReturns);
    if (getenv("ANGELIX_IF_TO_ELSEIFS_DEFECT_CLASS"))
      Matcher.addMatcher(RepairableIfToElseIf, &HandlerForIfToElseIfs);
    if (getenv("ANGELIX_UNINIT_VARS_DEFECT_CLASS"))
      Matcher.addMatcher(RepairableUninitVar, &HandlerForUninitVars);
  }

  void HandleTranslationUnit(ASTContext &Context) override {
    Matcher.matchAST(Context);
  }

private:
  MissingReturnHandler HandlerForMissingReturns;
  IfToElseIfHandler HandlerForIfToElseIfs;
  UninitVarHandler HandlerForUninitVars;
  MatchFinder Matcher;
};


class MakeRepairableAction : public ASTFrontendAction {
public:
  MakeRepairableAction() {}

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

  return Tool.run(newFrontendActionFactory<MakeRepairableAction>().get());
}
