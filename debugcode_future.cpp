#include <fstream>
std::shared_ptr<LexicalAnalyzer> l = nullptr;
SyntaxAnalyzer* s = nullptr;
std::unordered_map<int,std::string> revMap;
#include "debugdata_future.cpp"
std::string debugLexemes(std::vector<LexicalAnalyzer::Lexeme> const& lexemes, bool print = true) {
    std::stringstream sstream;
    size_t ptr = 0;
    for (auto i :lexemes) {
        sstream << ptr++ << ":\t{ type:" << typetable[i.type] << ","
                  << "value:";
        switch (i.type) {
        case CPlus::Keyword:
            sstream << keywordtable[i.value];
            break;
        case CPlus::SymbolOperator:
            sstream << operatortable[i.value] << "  [WARNING!!!!] ";
            break;
        case CPlus::Operator:
            sstream << operatortable[i.value];
            break;
        case CPlus::Symbol:
            sstream << symboltable[i.value];
            break;
        case CPlus::Identifier:{
            if (revMap[i.value] != "")
                sstream << revMap[i.value];
            else
                sstream << i.value;
        } break;
        default:
            sstream << i.value;
            break;
        }
        sstream << " }" << std::endl;
    }
    if (print)
        std::cerr << sstream.str();
    return sstream.str();
};
std::string debugIdentifier(std::shared_ptr<SyntaxAnalyzer::IdentifierASTNode>& idkp, bool print = true) {
    std::stringstream sstream;
    auto& idk = *idkp;
    sstream << "{\"type\":\"" << identifiertypetable[idk.getIdentifierType()] << "\",\"id\":" << idk.getIdentifierId() << ",\"name\":\"" << revMap[idk.getIdentifierId()] << "\"";
    if (!idk.getDimensions().empty()) {
        sstream << ",\"dimensions\":[";
        bool first = true;
        for (uint32_t dim :idk.getDimensions()) {
            sstream << (first ? "" : ",");
            sstream << dim;
            first = false;
        }
        sstream << "]";
    }
    sstream << ",\"baseOffset\":" << idk.getBaseOffsetRef() << ",\"size\":" << idk.getSize() << ",\"static\":" << idk.isStatic();
    sstream << "}";
    if (print)
        std::cerr << sstream.str();
    return sstream.str();
};

std::string debugAST(std::shared_ptr<SyntaxAnalyzer::BaseASTNode>& ast, bool print = true) {
    std::stringstream sstream;
    std::string prefix = "";
    if (ast == nullptr) {
        sstream << "null";
        return sstream.str();
    }
    sstream << "{\"type\":\"" << astnodetypetable[ast->getNodeType()] << "\",";
    switch (ast->getNodeType()) {
    case CPlus::BinaryExpressionASTNode:{
        SyntaxAnalyzer::BinaryExpressionASTNode& bn = *std::dynamic_pointer_cast<SyntaxAnalyzer::BinaryExpressionASTNode>(ast);
        sstream << "\"op\":\"" << operatortable[bn.getOperatorRef()] << "\",";
        sstream << "\"left\":";
        sstream << debugAST(bn.getLeftRef(),false);
        sstream << ",\"right\":";
        sstream << debugAST(bn.getRightRef(),false);
        sstream << "}";
    } break;
    case CPlus::IdentifierASTNode:{
        std::shared_ptr<SyntaxAnalyzer::IdentifierASTNode> inp = std::dynamic_pointer_cast<SyntaxAnalyzer::IdentifierASTNode>(ast);
        sstream << "\"identifier\":";
        sstream << debugIdentifier(inp,false);
        sstream << "}";
    } break;
    case CPlus::IntConstantASTNode:{
        std::shared_ptr<SyntaxAnalyzer::IntConstantASTNode> icp = std::dynamic_pointer_cast<SyntaxAnalyzer::IntConstantASTNode>(ast);
        sstream << "\"value\":" << icp->getValue() << "}";
    } break;
    case CPlus::FunctionCallASTNode:{
        std::shared_ptr<SyntaxAnalyzer::FunctionCallASTNode> fcp = std::dynamic_pointer_cast<SyntaxAnalyzer::FunctionCallASTNode>(ast);
        sstream << "\"function\":\"" << revMap[fcp->getFuncId()] << "\",\"funcId\":" << fcp->getFuncId() << ",";
        sstream << "\"arguments\":[";
        bool first = true;
        for (auto arg :fcp->getArgumentsRef()) {
            sstream << (first ? "" : ",");
            sstream << debugAST(arg,false);
            first = false;
        }
        sstream << "]}";
    } break;
    case CPlus::IfStatementASTNode:{
        SyntaxAnalyzer::IfStatementASTNode& stmt = *std::dynamic_pointer_cast<SyntaxAnalyzer::IfStatementASTNode>(ast);
        sstream << "\"condition\":"
                << debugAST(stmt.getConditionRef(),false);
        sstream << ",\"truthy\":";
        sstream << debugAST(stmt.getTruthyStmtRef(),false);
        sstream << ",\"falsy\":";
        sstream << debugAST(stmt.getFalsyStmtRef(),false);
        sstream << "}";
    } break;
    case CPlus::WhileStatementASTNode:{
        SyntaxAnalyzer::WhileStatementASTNode& stmt = *std::dynamic_pointer_cast<SyntaxAnalyzer::WhileStatementASTNode>(ast);
        sstream << "\"condition\":"
                << debugAST(stmt.getConditionRef(),false);
        sstream << ",\"loop\":";
        sstream << debugAST(stmt.getLoopStmtRef(),false);
        sstream << "}";
    } break;
    case CPlus::ForStatementASTNode:{
        SyntaxAnalyzer::ForStatementASTNode& stmt = *std::dynamic_pointer_cast<SyntaxAnalyzer::ForStatementASTNode>(ast);
        sstream << "\"init\":";
        sstream << debugAST(stmt.getInitRef(),false);
        sstream << ",\"condition\":"
                << debugAST(stmt.getConditionRef(),false);
        sstream << ",\"iter\":";
        sstream << debugAST(stmt.getIterationRef(),false);
        sstream << ",\"loop\":";
        sstream << debugAST(stmt.getLoopStmtRef(),false);
        sstream << "}";
    } break;
    case CPlus::BlockASTNode:{
        SyntaxAnalyzer::BlockASTNode& stmt = *std::dynamic_pointer_cast<SyntaxAnalyzer::BlockASTNode>(ast);
        sstream << "\"stmts\":[";
        bool first = true;
        for (auto arg :stmt.getStatementsRef()) {
            sstream << (first ? "" : ",");
            sstream << debugAST(arg, false);
            first = false;
        }
        sstream << "]}";
    } break;
    case CPlus::ReturnStatementASTNode:{
        SyntaxAnalyzer::ReturnStatementASTNode& stmt = *std::dynamic_pointer_cast<SyntaxAnalyzer::ReturnStatementASTNode>(ast);
        sstream << "\"expr\":";
        sstream << debugAST(stmt.getExprRef(),false);
        sstream << "}";
    } break;
    default:
        sstream << "\"unknown\": true}";
        break;
    }
    if (print)
        std::cerr << sstream.str();
    return sstream.str();
};

std::string debugASTs(std::vector<std::shared_ptr<SyntaxAnalyzer::BaseASTNode> >& asts, bool print = true) {
    std::stringstream sstream;
    int i = 0;
    for (auto ast :asts) {
        sstream << i++ << ":\t" << debugAST(ast, false) << std::endl;
    }
    if (print)
        std::cerr << sstream.str();
    return sstream.str();
}

int main() {
    int n = 0;
    std::cin >> n;
    std::queue<int> inputData;
    for (size_t i = 0, k; i < n; i++) {
        std::cin >> k;
        inputData.push(k);
    }
    std::stringstream testCpp;
    testCpp << std::cin.rdbuf();
    std::cerr << "Source: \n" << testCpp.str() << std::endl;
    l = std::make_shared<LexicalAnalyzer>(testCpp.str());
    l->analyze();
    for (auto kv : l->getIdentifierMap()) {
        revMap[kv.second] = kv.first;
    }
    debugLexemes(l->getLexemesRef());
    s = new SyntaxAnalyzer(l);
    /*std::cerr << "BeginDeclarationAnalyze" << std::endl;
    size_t ptr = 0;
    auto x = SyntaxAnalyzer::declarationAnalyze(l->getLexemesRef(),ptr,0);
    for (auto idkp :x.second) {
        std::cerr << debugIdentifier(idkp, false) << std::endl;
    }
    std::cerr << "EndDeclarationAnalyze,ptr=" << ptr << std::endl;
    std::cerr << "BeginExpressionAnalyze" << std::endl;
    std::shared_ptr<SyntaxAnalyzer::Scope> scope = std::make_shared<SyntaxAnalyzer::Scope>(nullptr);
    {
        scope->setIdentifier(CPlus::Cin,std::make_shared<SyntaxAnalyzer::IdentifierASTNode>(CPlus::BuiltInObjects,CPlus::Cin,0,true));
        scope->setIdentifier(CPlus::Cout,std::make_shared<SyntaxAnalyzer::IdentifierASTNode>(CPlus::BuiltInObjects,CPlus::Cout,0,true));
        scope->setIdentifier(CPlus::Endl,std::make_shared<SyntaxAnalyzer::IdentifierASTNode>(CPlus::BuiltInObjects,CPlus::Endl,0,true));
        scope->setIdentifier(CPlus::Putchar,std::make_shared<SyntaxAnalyzer::IdentifierASTNode>(CPlus::Function,CPlus::Putchar,0,true));
        std::vector<std::shared_ptr<SyntaxAnalyzer::IdentifierASTNode>> putcharArgs;
        std::vector<std::shared_ptr<SyntaxAnalyzer::BaseASTNode>> putcharStmt;
        putcharArgs.push_back(std::make_shared<SyntaxAnalyzer::IdentifierASTNode>(0,0,false));
        scope->setFunction(CPlus::Putchar,std::make_shared<SyntaxAnalyzer::Function>(CPlus::Putchar,std::move(putcharArgs),nullptr));
    }
    for (auto idk :x.second) {
        int id = idk->getIdentifierId();
        if (id >= CPlus::_PredefinedIdentifierUpperBound)
            scope->setIdentifier(id,std::move(idk));
    }
    ptr = 40;
    auto y = SyntaxAnalyzer::expressionAnalyze(l->getLexemesRef(),ptr,scope);
    debugAST(y);
    std::cerr << "EndExpressionAnalyze,ptr=" << ptr << std::endl;
    */
    s->analyze();
    std::cerr << "Build succeeded!" << std::endl;
    bool first = true;
    std::cout << "{";
    for (auto it = s->getGlobalScope()->funcBegin(); it != s->getGlobalScope()->funcEnd(); it++) {
        auto f = (*it).second;
        if (!first) std::cout << ",";
        first = false;
        std::cout << "\"" << revMap[f->getFuncId()] << "\":{\"id\":" << f->getFuncId() << ",\"name\":\"" << revMap[f->getFuncId()]
                    << "\",\"stackSize\":" << f->getMaxStackSizeRef() << ",\"ast\":";
        auto t = std::dynamic_pointer_cast<SyntaxAnalyzer::BaseASTNode>
            (f->getStatementsRef());
        std::cout << debugAST(t,false) << "}";
    }
    std::cout << "}";
}