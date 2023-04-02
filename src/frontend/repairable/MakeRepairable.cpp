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

  const CompoundStmt *parent = nullptr;
  ArrayRef<ast_type_traits::DynTypedNode> parents = context->getParents(*expr);
  while (parents.size() > 0) {
    const ast_type_traits::DynTypedNode parentNode = *(parents.begin());
    if ((parent = parentNode.get<CompoundStmt>()))
      break;
    parents = context->getParents(parentNode);
  }

  if (!parent) {
    std::cerr << "can't get parent compound statement\n";
    return false;
  }

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


class MissingLoopBreakHandler : public MatchFinder::MatchCallback {
public:
  MissingLoopBreakHandler(Rewriter &Rewrite) : Rewrite(Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &Result) {
    if (const CompoundStmt *stmt = Result.Nodes.getNodeAs<clang::CompoundStmt>("repairable")) {
      SourceManager &srcMgr = Rewrite.getSourceMgr();
      const LangOptions &langOpts = Rewrite.getLangOpts();

      if (insideMacro(stmt, srcMgr, langOpts))
        return;

      SourceRange expandedLoc = getExpandedLoc(stmt, srcMgr);

      std::pair<FileID, unsigned> decLoc = srcMgr.getDecomposedExpansionLoc(expandedLoc.getBegin());
      if (srcMgr.getMainFileID() != decLoc.first)
        return;

      if (!insideSuspiciousScope(stmt, Result.Context, srcMgr))
        return;

      unsigned beginLine = srcMgr.getExpansionLineNumber(expandedLoc.getBegin());
      unsigned beginColumn = srcMgr.getExpansionColumnNumber(expandedLoc.getBegin());
      unsigned endLine = srcMgr.getExpansionLineNumber(expandedLoc.getEnd());
      unsigned endColumn = srcMgr.getExpansionColumnNumber(expandedLoc.getEnd());

      std::cout << beginLine << " " << beginColumn << " " << endLine << " " << endColumn << "\n"
                << toString(stmt) << "\n";

      std::ostringstream stringStream;
      stringStream << "    "
                   << "if (0) break;"
                   << "\n";
      std::string insertion = stringStream.str();

      Rewrite.InsertText(stmt->getLocEnd(), insertion, true, true);
    }
  }

private:
  Rewriter &Rewrite;
};


class MissingLoopContinueHandler : public MatchFinder::MatchCallback {
public:
  MissingLoopContinueHandler(Rewriter &Rewrite) : Rewrite(Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &Result) {
    if (const CompoundStmt *stmt = Result.Nodes.getNodeAs<clang::CompoundStmt>("repairable")) {
      SourceManager &srcMgr = Rewrite.getSourceMgr();
      const LangOptions &langOpts = Rewrite.getLangOpts();

      if (insideMacro(stmt, srcMgr, langOpts))
        return;

      SourceRange expandedLoc = getExpandedLoc(stmt, srcMgr);

      std::pair<FileID, unsigned> decLoc = srcMgr.getDecomposedExpansionLoc(expandedLoc.getBegin());
      if (srcMgr.getMainFileID() != decLoc.first)
        return;

      if (!insideSuspiciousScope(stmt, Result.Context, srcMgr))
        return;

      unsigned beginLine = srcMgr.getExpansionLineNumber(expandedLoc.getBegin());
      unsigned beginColumn = srcMgr.getExpansionColumnNumber(expandedLoc.getBegin());
      unsigned endLine = srcMgr.getExpansionLineNumber(expandedLoc.getEnd());
      unsigned endColumn = srcMgr.getExpansionColumnNumber(expandedLoc.getEnd());

      std::cout << beginLine << " " << beginColumn << " " << endLine << " " << endColumn << "\n"
                << toString(stmt) << "\n";

      std::ostringstream stringStream;
      stringStream << "\n"
                   << "    "
                   << "if (0) continue;"
                   << "\n";
      std::string insertion = stringStream.str();

      Rewrite.InsertText(stmt->getLocStart().getLocWithOffset(1),
                         insertion, true, true);
    }
  }

private:
  Rewriter &Rewrite;
};


class MissingElseIfHandler : public MatchFinder::MatchCallback {
public:
  MissingElseIfHandler(Rewriter &Rewrite) : Rewrite(Rewrite) {}

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

      const Stmt *then = ifS->getThen();
      unsigned offset = 1;
      std::string colon = "";
      if (isa<BinaryOperator>(then)) {
        offset = 2;
        colon = ";";
      }
      std::ostringstream stringStream;
      stringStream << "\n"
                   << "else if (1) "
                   << toString(then)
                   << colon;
      std::string insertion = stringStream.str();

      Rewrite.InsertText(then->getLocEnd().getLocWithOffset(offset),
                         insertion, true, true);
    }
  }

private:
  Rewriter &Rewrite;
};


class MyASTConsumer : public ASTConsumer {
public:
  MyASTConsumer(Rewriter &R) : HandlerForMissingReturns(R),
                               HandlerForIfToElseIfs(R),
                               HandlerForUninitVars(R),
                               HandlerForMissingLoopBreaks(R),
                               HandlerForMissingLoopContinues(R),
                               HandlerForMissingElseIfs(R) {
    if (getenv("ANGELIX_MISSING_RETURNS_DEFECT_CLASS"))
      Matcher.addMatcher(RepairableMissingReturn, &HandlerForMissingReturns);
    if (getenv("ANGELIX_IF_TO_ELSEIFS_DEFECT_CLASS"))
      Matcher.addMatcher(RepairableIfToElseIf, &HandlerForIfToElseIfs);
    if (getenv("ANGELIX_UNINIT_VARS_DEFECT_CLASS"))
      Matcher.addMatcher(RepairableUninitVar, &HandlerForUninitVars);
    if (getenv("ANGELIX_MISSING_LOOP_BREAKS_DEFECT_CLASS"))
      Matcher.addMatcher(RepairableMissingLoopBreak, &HandlerForMissingLoopBreaks);
    if (getenv("ANGELIX_MISSING_LOOP_CONTINUES_DEFECT_CLASS"))
      Matcher.addMatcher(RepairableMissingLoopContinue, &HandlerForMissingLoopContinues);
    if (getenv("ANGELIX_MISSING_ELSEIFS_DEFECT_CLASS"))
      Matcher.addMatcher(RepairableMissingElseIf, &HandlerForMissingElseIfs);
  }

  void HandleTranslationUnit(ASTContext &Context) override {
    Matcher.matchAST(Context);
  }

private:
  MissingReturnHandler HandlerForMissingReturns;
  IfToElseIfHandler HandlerForIfToElseIfs;
  UninitVarHandler HandlerForUninitVars;
  MissingLoopBreakHandler HandlerForMissingLoopBreaks;
  MissingLoopContinueHandler HandlerForMissingLoopContinues;
  MissingElseIfHandler HandlerForMissingElseIfs;
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
