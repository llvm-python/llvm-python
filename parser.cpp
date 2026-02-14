#include "ast.h"
#include "lexer.h"
#include "parser.h"
#include "codegen.h"
#include <cstdio>
#include <map>
#include <vector>
#include <string>
#include <cctype>
#include <llvm/IR/Function.h>
#include <llvm/Support/raw_ostream.h>

#include "codegen.h"

using namespace ASTNode;

static std::unique_ptr<ExpressionASTNode> ParseExpression();

static std::unique_ptr<ExpressionASTNode> ParsePrimaryExpression();

static std::unique_ptr<ExpressionASTNode> ParseBinaryOperatorRHS(int ExpressionPrecedence,
                                                                 std::unique_ptr<ExpressionASTNode> LHS);

static int CurrentToken;

int getNextToken() {
    return CurrentToken = gettok();
};

std::unique_ptr<ExpressionASTNode> LogError(const char *str) {
    fprintf(stderr, "Error: %s\n", str);
    return nullptr;
}

std::unique_ptr<SignatureASTNode> LogErrorS(const char *str) {
    LogError(str);
    return nullptr;
}

static std::unique_ptr<ExpressionASTNode> ParseExpression() {
    auto LHS = ParsePrimaryExpression();
    if (!LHS) {
        return nullptr;
    } else {
        LHS = ParseBinaryOperatorRHS(0, std::move(LHS));
        return std::move(LHS);
    }
};

static std::unique_ptr<ExpressionASTNode> ParseNumberExpression() {
    auto Result = std::make_unique<NumberExpressionASTNode>(NumVal);
    getNextToken();
    return std::move(Result);
}

static std::unique_ptr<ExpressionASTNode> ParseParenthesisExpression() {
    getNextToken();
    auto ASTNodePtr = ParseExpression();
    if (ASTNodePtr == nullptr) {
        return nullptr;
    }
    if (CurrentToken != ')') {
        return LogError("expected ')'");
    }
    getNextToken();
    return ASTNodePtr;
}

static std::unique_ptr<ExpressionASTNode> ParseIdentifierExpression() {
    std::string IdName = IdentifierStr;
    getNextToken();
    if (CurrentToken != '(') {
        // if it is a variable
        return std::make_unique<VariableExpressionASTNode>(IdName);
    }
    getNextToken();
    std::vector<std::unique_ptr<ExpressionASTNode> > Arguments;
    if (CurrentToken != ')') {
        while (true) {
            auto Argument = ParseExpression();
            if (Argument) {
                Arguments.push_back(std::move(Argument));
            } else {
                return nullptr;
            }
            if (CurrentToken == ')')
                break;
            if (CurrentToken != ',')
                return LogError("Expected ')' or ',' in argument list");
            getNextToken();
        }
    }
    getNextToken();
    return std::make_unique<FunctionCallExpressionASTNode>(IdName, std::move(Arguments));
}

static std::unique_ptr<ExpressionASTNode> ParseIfExpression() {
    getNextToken(); // Process if token

    auto ConditionNode = ParseExpression(); // Create Condition AST Node
    if (ConditionNode == nullptr) {
        return nullptr;
    }

    if (CurrentToken != tok_then) {
        return LogError("Expected Then");
    }
    getNextToken(); // Process Then token

    auto ThenNode = ParseExpression(); // Create Then AST Node
    if (ThenNode == nullptr) {
        return nullptr;
    }

    if (CurrentToken != tok_else) {
        return LogError("Expected Else");
    }
    getNextToken(); // Process Else token

    auto ElseNode = ParseExpression(); // Create Else AST Node
    if (ElseNode == nullptr) {
        return nullptr;
    }

    // Return IfExpressionASTNode
    return std::make_unique<IfExpressionASTNode>(std::move(ConditionNode), std::move(ThenNode), std::move(ElseNode));
}


static std::unique_ptr<ExpressionASTNode> ParseForExpression() {
    getNextToken(); // Process for

    if (CurrentToken != Token::tok_identifier) {
        return LogError("Expected variable after for");
    }

    std::string VarName = IdentifierStr;
    getNextToken(); // Process variable

    if (CurrentToken != '=') {
        return LogError("Expected '=' in the for loop");
    }
    getNextToken(); // Process '='(Equal symbol)

    auto Start = ParseExpression(); // Process Start
    if (Start == nullptr) {
        return nullptr;
    }

    if (CurrentToken != ',') {
        return LogError("Expected ',' after Start in For loop");
    }
    getNextToken(); // Process ','(comma)

    auto End = ParseExpression(); // Process End
    if (End == nullptr) {
        return nullptr;
    }

    // Step is optional value
    std::unique_ptr<ExpressionASTNode> Step;
    if (CurrentToken == ',') {
        // Check Step exists
        getNextToken(); // Process ','(comma)
        Step = ParseExpression();
        if (Step == nullptr) {
            return nullptr;
        }
    }

    if (CurrentToken != Token::tok_in) {
        return LogError("Expected 'in' keyword in the for loop");
    }
    getNextToken(); // Process 'in'

    auto Body = ParseExpression();
    if (Body == nullptr) {
        return nullptr;
    }

    return std::make_unique<ForExpressionASTNode>(VarName, std::move(Start), std::move(End), std::move(Step), std::move(Body));
}

static std::unique_ptr<ExpressionASTNode> ParsePrimaryExpression() {
    switch (CurrentToken) {
        case tok_identifier:
            return ParseIdentifierExpression();
        case tok_number:
            return ParseNumberExpression();
        case '(':
            return ParseParenthesisExpression();
        case tok_if:
            return ParseIfExpression();
        case tok_for:
            return ParseForExpression();
        default:
            return LogError("unexpected token");
    }
}

// Binary Operator
// 높을수록 우선순위 높음
static std::map<char, int> BinaryOperatorPrecedence = {
    {'<', 10},
    {'+', 20},
    {'-', 20},
    {'*', 40},
};

static int GetTokenPrecedence() {
    if (!isascii(CurrentToken)) {
        return -1;
    } else {
        int TokenPrecedence = BinaryOperatorPrecedence[CurrentToken];
        if (TokenPrecedence <= 0) {
            return -1;
        }
        return TokenPrecedence;
    }
}

static std::unique_ptr<ExpressionASTNode> ParseBinaryOperatorRHS
(int ExpressionPrecedence,
 std::unique_ptr<ExpressionASTNode> LHS) {
    while (true) {
        int TokenPrecedence = GetTokenPrecedence();

        if (TokenPrecedence < ExpressionPrecedence) {
            return LHS;
        }

        // 현재 연산자 기억
        int BinaryOperator = CurrentToken;
        getNextToken();

        auto RHS = ParsePrimaryExpression();
        if (!RHS) {
            return nullptr;
        }

        // 재귀
        int NextPrecedence = GetTokenPrecedence();
        if (TokenPrecedence < NextPrecedence) {
            RHS = ParseBinaryOperatorRHS(TokenPrecedence + 1, std::move(RHS));
            if (!RHS) {
                return nullptr;
            }
        }
        LHS = std::make_unique<BinaryExpressionASTNode>(BinaryOperator, std::move(LHS), std::move(RHS));
    }
}

static std::unique_ptr<SignatureASTNode> ParseSignature() {
    if (CurrentToken != tok_identifier) {
        return LogErrorS("Expected function name in signature");
    }
    std::string FunctionName = IdentifierStr;
    getNextToken();

    if (CurrentToken != '(') {
        return LogErrorS("Expected '(' in signature");
    }

    std::vector<std::string> ArgumentNames;
    while (getNextToken() == tok_identifier) {
        ArgumentNames.push_back(IdentifierStr);
    }

    if (CurrentToken != ')') {
        return LogErrorS("Expected ')' in signature");
    }
    getNextToken();
    return std::make_unique<SignatureASTNode>(FunctionName, std::move(ArgumentNames));
}

static std::unique_ptr<FunctionASTNode> ParseDefinition() {
    getNextToken();
    auto Signature = ParseSignature();
    if (!Signature) {
        return nullptr;
    }
    if (auto E = ParseExpression()) {
        return std::make_unique<FunctionASTNode>(std::move(Signature), std::move(E));
    }
    return nullptr;
}

static std::unique_ptr<SignatureASTNode> ParseExtern() {
    getNextToken();
    return ParseSignature();
}

static std::unique_ptr<FunctionASTNode> ParseTopLevelExpression() {
    if (auto E = ParseExpression()) {
        auto Signature = std::make_unique<SignatureASTNode>("__anon_expr", std::vector<std::string>());
        return std::make_unique<FunctionASTNode>(std::move(Signature), std::move(E));
    }
    return nullptr;
}

// Top Level parsing
static void HandleDefinition() {
    auto FunctionAST = ParseDefinition();
    if (FunctionAST != nullptr) {
        auto *FunctionIR = FunctionAST->codegen();
        fprintf(stderr, "Parsed a function definition.\n");
        FunctionIR->print(llvm::errs());
        ExitOnErr(TheJit->addModule(
            llvm::orc::ThreadSafeModule(std::move(TheModule), std::move(TheContext))
        ));
        InitializeModuleAndManagers();
    } else {
        getNextToken();
    }
}

static void HandleExtern() {
    auto SignatureAST = ParseExtern();
    if (SignatureAST != nullptr) {
        auto *FunctionIR = SignatureAST->codegen();
        if (FunctionIR != nullptr) {
            fprintf(stderr, "Parsed an extern\n");
            FunctionIR->print(llvm::errs());
            Signatures[SignatureAST->getName()] = std::move(SignatureAST);
        }
    } else {
        getNextToken();
    }
}

static void HandleTopLevelExpression() {
    auto FunctionAST = ParseTopLevelExpression();
    if (FunctionAST != nullptr) {
        auto *FunctionIR = FunctionAST->codegen();
        if (FunctionIR != nullptr) {
            // print LLVM IR
            fprintf(stderr, "Read top-level expression:\n");
            FunctionIR->print(llvm::errs());

            auto resource_tracker = TheJit->getMainJITDylib().createResourceTracker();
            auto thread_safe_module = llvm::orc::ThreadSafeModule(std::move(TheModule), std::move(TheContext));

            ExitOnErr(TheJit->addModule(std::move(thread_safe_module), resource_tracker));

            InitializeModuleAndManagers();

            auto ExprSymbol = ExitOnErr(TheJit->lookup("__anon_expr"));
            double (*FunctionPointer)() = ExprSymbol.toPtr<double (*)()>();

            fprintf(stderr, "Evaluated to %f\n", FunctionPointer());

            ExitOnErr(resource_tracker->remove());
        }
    } else {
        getNextToken();
    }
}

void MainLoop() {
    while (true) {
        switch (CurrentToken) {
            case tok_eof:
                return;
            case ';':
                getNextToken();
                break;
            case tok_def:
                HandleDefinition();
                break;
            case tok_extern:
                HandleExtern();
                break;
            default:
                HandleTopLevelExpression();
                break;
        }
        fprintf(stderr, ">>> ");
    }
}
