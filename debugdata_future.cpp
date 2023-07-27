static const char* typetable[] = {"Nil", "IntConstant", "Keyword", "Identifier",
                                  "SymbolOperator",
                                  "Symbol", "Operator"};
static const char* operatortable[] = {"", "LeftParen", "RightParen", "LeftSquareBracket", "RightSquareBracket", "Not", "Positive", "Negative", "Multiply", "Divide", "Modulo", "Plus", "Minus", "LessEqual", "GreaterEqual", "LessThan", "GreaterThan", "Equal", "NotEqual", "Xor", "LogicalAnd", "LogicalOr", "Assign", "Comma", "CoutFrom", "CinTo", "LeftBracket", "RightBracket", "Semicolon", "PlusOrPositive", "MinusOrNegative", "ArrayMemoryDereference"};
static const char* symboltable[] = {"", "FunctionCallDefBegin", "FunctionCallDefEnd", "ControlBlockStatementBegin", "ControlBlockStatementEnd", "ArrayAccessDefBegin", "ArrayAccessDefEnd", "BlockBegin", "BlockEnd", "FunctionBodyBegin", "FunctionBodyEnd", "CommaSeparator", "StatementSeparator", "ConditionSeparator"};
static const char* keywordtable[] = {"", "Int", "If", "Else", "For", "While", "Return"};
static const char* identifiertypetable[] = {"Nil", "Integer", "Function", "IntegerArray", "BuiltinObject"};
static const char* astnodetypetable[] = {"BaseASTNode", "IdentifierASTNode", "IntConstantASTNode", "BinaryExpressionASTNode", "FunctionCallASTNode", "BlockASTNode", "IfStatementASTNode", "WhileStatementASTNode", "ForStatementASTNode", "ReturnStatementASTNode", "AsmInlineASTNode"};
/*
auto debugASTNode = [](std::shared_ptr<SyntaxAnalyzer::BaseASTNode> ast, int space = 0) {
    std::function<void(std::shared_ptr<SyntaxAnalyzer::BaseASTNode>, int)> func;
    func = [&](std::shared_ptr<SyntaxAnalyzer::BaseASTNode> ast, int space) {
        std::string prefix = "";
        for (size_t i = 0; i < space; i++)
            prefix += " ";
        if (ast == nullptr) {
            std::cerr << prefix << "null";
            return;
        }
        std::cerr << prefix << "{ type: " << astnodetypetable[ast->getNodeType()] << ", \n";
        switch (ast->getNodeType()) {
        case CPlus::BinaryExpressionASTNode: {
            SyntaxAnalyzer::BinaryExpressionASTNode& bn = *std::dynamic_pointer_cast<SyntaxAnalyzer::BinaryExpressionASTNode>(ast);
            std::cerr << prefix << "  op: " << operatortable[bn.getOperatorRef()] << "\n";
            std::cerr << prefix << "  left: \n";
            func(bn.getLeftRef(), space + 2);
            std::cerr << ", \n"
                      << prefix << "  right: \n";
            func(bn.getRightRef(), space + 2);
            std::cerr << ", \n"
                      << prefix << "}";
        } break;
        case CPlus::IdentifierASTNode: {
            std::shared_ptr<SyntaxAnalyzer::IdentifierASTNode> inp = std::dynamic_pointer_cast<SyntaxAnalyzer::IdentifierASTNode>(ast);
            std::cerr << prefix << "  identifier: " << inp->getIdentifierId() << prefix << "}";
        } break;
        case CPlus::IntConstantASTNode: {
            std::shared_ptr<SyntaxAnalyzer::IntConstantASTNode> icp = std::dynamic_pointer_cast<SyntaxAnalyzer::IntConstantASTNode>(ast);
            std::cerr << prefix << "  value: " << icp->getValue() << "\n"
                      << prefix << "}";
        } break;
        }
    };
    func(std::move(ast), space);
};*/