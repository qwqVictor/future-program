#include <iostream>
#include <unordered_map>
#include <string>
#include <sstream>
#include <queue>
#include <deque>
#include <stack>
#include <cstdint>
#include <vector>
#include <regex>

namespace CPlus {
    constexpr int strippedLineLength = 3;
    const std::string strippedLines[] = {"#include<iostream>", "#include<cstdio>", "using namespace std;"};

    enum SymbolOperatorEnum {
        LeftParen = 1,
        RightParen,
        LeftSquareBracket,
        RightSquareBracket,
        Not,
        Positive,
        Negative,
        Multiply,
        Divide,
        Modulo,
        Plus,
        Minus,
        LessEqual,
        GreaterEqual,
        LessThan,
        GreaterThan,
        Equal,
        NotEqual,
        Xor,
        LogicalAnd,
        LogicalOr,
        Assign,
        Comma,
        CoutFrom,
        CinTo,
        LeftBracket,
        RightBracket,
        Semicolon,
        PlusOrPositive,  // 加法和正号的二义性
        MinusOrNegative, // 减法和负号的二义性
        ArrayMemoryDereference,
    };

    std::unordered_map<std::string, SymbolOperatorEnum> SymbolOperators = {
        {"(", LeftParen}, {")", RightParen}, {"[", LeftSquareBracket}, {"]", RightSquareBracket},
        {"!", Not}, /* {"+", Positive}, {"-", Negative}, */
        {"*", Multiply}, {"/", Divide}, {"%", Modulo},
        /* {"+", Plus}, {"-", Minus}, */
        {"<=", LessEqual}, {">=", GreaterEqual}, {"<", LessThan}, {">", GreaterThan},
        {"==", Equal}, {"!=", NotEqual},
        {"^", Xor},
        {"&&", LogicalAnd},
        {"||", LogicalOr},
        {"=", Assign},
        {",", Comma},
        {"<<", CoutFrom}, {">>", CinTo},
        {"{", LeftBracket}, {"}", RightBracket}, {";", Semicolon},
        {"+", PlusOrPositive}, {"-", MinusOrNegative}
    };
    std::unordered_map<SymbolOperatorEnum, uint8_t> OperatorPrecedence = {
        {Not, 1}, {Positive, 1}, {Negative, 1},
        {Multiply, 2}, {Divide, 2}, {Modulo, 2},
        {Plus, 3}, {Minus, 3},
        {LessEqual, 4}, {GreaterEqual, 4}, {LessThan, 4}, {GreaterThan, 4},
        {Equal, 5}, {NotEqual, 5},
        {Xor, 6},
        {LogicalAnd, 7},
        {LogicalOr, 8},
        {CoutFrom, 9}, {CinTo, 9},
        {Assign, 10}
    };

    enum SymbolEnum {
        FunctionCallDefBegin = 1,
        FunctionCallDefEnd,
        ControlBlockBegin,
        ControlBlockEnd,
        ArrayAccessDefBegin,
        ArrayAccessDefEnd,
        BlockBegin,
        BlockEnd,
        FunctionBodyBegin,
        FunctionBodyEnd,
        CommaSeparator,
        StatementSeparator,
        ConditionSeparator
    };

    enum KeywordEnum {
        _KeywordNil = 0,
        Int = 1,
        If,
        Else,
        For,
        While,
        Return,
    };
    std::unordered_map<std::string, KeywordEnum> Keywords = {
        {"int", Int}, {"if", If}, {"else", Else}, {"for", For}, {"while", While}, {"return", Return}};

    enum LexemeType {
        _LexemeTypeNil = 0,
        IntConstant = 1,
        Keyword,
        Identifier,
        SymbolOperator,
        Symbol,
        Operator
    };

    enum PredefinedIdentifierEnum {
        Main = 1,
        Cin,
        Cout,
        Endl,
        Putchar,
        _PredefinedIdentifierUpperBound
    };
    const std::string predefinedIdentifiers[_PredefinedIdentifierUpperBound] = {"",
        "main", "cin", "cout", "endl", "putchar"
    };

    enum IdentifierType {
        _IdentifierTypeNil,
        Integer = 1,
        Function,
        IntegerArray,
        BuiltInObjects
    };

    enum AbstractSyntaxTreeNodeType {
        BaseASTNode,
        IdentifierASTNode,
        IntConstantASTNode,
        BinaryExpressionASTNode,
        FunctionCallASTNode,
        BlockASTNode,
        IfStatementASTNode,
        WhileStatementASTNode,
        ForStatementASTNode,
        ReturnStatementASTNode
    };

    constexpr int GlobalObjectsNilBaseOffset = 0;
    constexpr int GlobalVariablesInitialBaseOffset = 1;
    constexpr int NullPointer = 0;

    class CompileError : public std::runtime_error {
    public:
        CompileError(std::string&& message) : std::runtime_error(message) {}
    };
    class RuntimeError : public std::runtime_error {
    public:
        RuntimeError(std::string&& message) : std::runtime_error(message) {}
    };
}

namespace Utilities {
    bool isdigits(std::string const &str) {
        for (char i : str) {
            if (!isdigit(i)) return false;
        }
        return true;
    }

    int strToUInt(std::string const& str) {
        int ret = 0;
        for (char i : str) {
            if (!isdigit(i)) return -1;
            ret = ret * 10 + i - '0';
        }
        return ret;
    }
}

class LexicalAnalyzer {
private:
    class Tokenizer {
    private:
        int ptr;
        std::string source;
        // 对于本题所简化的 C++，所有多于一个字符的 symbol/operator，除了 '||' 和 '&&' 的首字符均为一个合法的 symbol/operator
        // 因此此方法十分实用
        bool isPartOfSymbolOperator(std::string ch) {
            return CPlus::SymbolOperators.count(ch) || ch == "|" || ch == "&";
        }
    public:
        Tokenizer(std::string source) : ptr(0), source(source) {}

        bool hasNextToken() {
            return this->ptr < this->source.length();
        }

        std::string nextToken() {
            // 过滤空白字符
            while (this->ptr < source.length()) {
                if (!isspace(this->source[this->ptr]))
                    break;
                ptr++;
            }
            int s = this->ptr;
            // 判断是否为 symbol 或 operator 开始
            std::string ch = this->source.substr(this->ptr, 1);
            bool startWithSymbolOperator = isPartOfSymbolOperator(this->source.substr(this->ptr, 1));
            for (; this->ptr < source.length(); ptr++) {
                if (s == this->ptr)
                    continue;
                // 检测到空白字符，结束
                if (isspace(this->source[this->ptr])) {
                    break;
                }
                // 如果以 symbol/operator 开始，且字符串加入下一个字符后无法构成 symbol/operator，结束
                if (startWithSymbolOperator) {
                    if (this->ptr + 1 < source.length()) {
                        if (!(CPlus::SymbolOperators.count(this->source.substr(s, this->ptr + 1 - s))))
                            break;
                    }
                    else {
                        break;
                    }
                }
                // 如果不以 symbol/operator 开始，且当前字符为 symbol/operator 的部分，则结束
                if (!startWithSymbolOperator && isPartOfSymbolOperator(this->source.substr(this->ptr, 1))) {
                    break;
                }
            }
            return this->source.substr(s, this->ptr - s);
        }
    };
public:
    struct Lexeme {
        CPlus::LexemeType type;
        int value;
    };

private:
    std::shared_ptr<Tokenizer> tokenizer;
    bool analyzed;
    std::vector<Lexeme> lexemes;
    std::unordered_map<std::string, int> identifierMap;
    int identifierCounter;
    int ptr;

    // 初步解析 token，进行简单的 token 分类，形成最初的词素
    void parseTokens() {
        for (std::string token; tokenizer->hasNextToken();) {
            token = tokenizer->nextToken();
            if (token.empty()) break;
            Lexeme lexeme = {CPlus::LexemeType::_LexemeTypeNil, 0};
            if (CPlus::SymbolOperators.count(token)) {
                lexeme.type = CPlus::LexemeType::SymbolOperator;
                lexeme.value = CPlus::SymbolOperators[token];
            } else if (CPlus::Keywords.count(token)) {
                lexeme.type = CPlus::LexemeType::Keyword;
                lexeme.value = CPlus::Keywords[token];
            } else if (Utilities::isdigits(token)) {
                lexeme.type = CPlus::LexemeType::IntConstant;
                lexeme.value = Utilities::strToUInt(token);
            } else {
                lexeme.type = CPlus::LexemeType::Identifier;
                if (!this->identifierMap.count(token)) {
                    lexeme.value = this->identifierCounter++;
                    this->identifierMap[token] = lexeme.value;
                } else
                    lexeme.value = this->identifierMap[token];
            }
            lexemes.push_back(lexeme);
        }
    }

    // 二次解析词素，消除词素的二义性
    void resolveLexemeTypes() {
        std::stringstream err;
        // 使用栈记录小括号和花括号，从而判断其含义
        std::stack<Lexeme> parenStack, bracketStack;
        // 记录上一次的控制字符，这会影响"()"的含义
        auto lastControlKeyword = CPlus::_KeywordNil;
        // 判断是否在 int 定义内，这会影响"()"和"[]"的含义
        bool insideIntDefinition = false;
        for (size_t i = 0; i < lexemes.size(); i++) {
            Lexeme& lexeme = this->lexemes[i];
            switch (lexeme.type) {
            case CPlus::LexemeType::Keyword: {
                switch (lexeme.value) {
                // 若为 if、for、while 关键字，记录控制字符
                case CPlus::If:
                case CPlus::For:
                case CPlus::While:
                    lastControlKeyword = (CPlus::KeywordEnum)lexeme.value;
                    break;
                // 若为 int 关键字，设定 int 定义
                case CPlus::Int:
                    insideIntDefinition = true;
                    break;
                default:
                    break;
                }
            } break;
            case CPlus::LexemeType::SymbolOperator: {
                switch (lexeme.value) {
                // 消除"+"、"-"二义性
                case CPlus::PlusOrPositive:
                case CPlus::MinusOrNegative:
                    // "+"、"-" 不可能在开头出现
                    if (!i) {
                        err << "Found + or - at position 0";
                        throw CPlus::CompileError(err.str());
                    }
                    lexeme.type = CPlus::Operator;
                    // 如果左侧的词素是 symbol/operator，且不是")"、"]"，则必为正负含义，否则为加减
                    if ((lexemes[i - 1].type == CPlus::Operator && lexemes[i - 1].value != CPlus::RightParen) ||
                        (lexemes[i - 1].type == CPlus::Symbol && lexemes[i - 1].value != CPlus::FunctionCallDefEnd && lexemes[i - 1].value != CPlus::ArrayAccessDefEnd)) {
                        lexeme.value = lexeme.value == CPlus::PlusOrPositive ? CPlus::Positive : CPlus::Negative;
                    } else {
                        lexeme.value = lexeme.value == CPlus::PlusOrPositive ? CPlus::Plus : CPlus::Minus;
                    }
                    break;
                case CPlus::LeftParen:
                    if (!i) {
                        err << "Found ( at position 0";
                        throw CPlus::CompileError(err.str());
                    }
                    // 如果左侧相邻词素是标识符，则必为函数调用或声明
                    if (lexemes[i - 1].type == CPlus::Identifier)
                        lexeme.type = CPlus::Symbol, lexeme.value = CPlus::FunctionCallDefBegin;
                    // 如果左侧相邻词素是关键字，且不是 return，则必为控制块起始
                    else if (lexemes[i - 1].type == CPlus::Keyword && lexemes[i - 1].value != CPlus::Return)
                        lexeme.type = CPlus::Symbol, lexeme.value = CPlus::ControlBlockBegin;
                    // 否则作为普通括号运算符处理
                    else
                        lexeme.type = CPlus::Operator;
                    // 左括号入栈，用于判断对应右括号类型
                    parenStack.push(lexeme);
                    break;
                case CPlus::RightParen: {
                    // 若栈为空则括号不匹配，不合法
                    if (parenStack.empty()) {
                        err << "Unmatching paren at lexeme " << i << ".";
                        throw CPlus::CompileError(err.str());
                    }
                    Lexeme& correspondent = parenStack.top();
                    // 取栈顶对应的左括号判断
                    if (correspondent.type == CPlus::Symbol) {
                        lexeme.type = CPlus::Symbol;
                        if (correspondent.value == CPlus::FunctionCallDefBegin)
                            lexeme.value = CPlus::FunctionCallDefEnd;
                        else if (correspondent.value == CPlus::ControlBlockBegin) {
                            lexeme.value = CPlus::ControlBlockEnd;
                            // 若为控制块结束
                            lastControlKeyword = CPlus::_KeywordNil;
                        }
                    } else
                        lexeme.type = CPlus::Operator, lexeme.value = CPlus::RightParen;
                    parenStack.pop();
                } break;
                case CPlus::LeftBracket: {
                    lexeme.type = CPlus::Symbol,
                    // 同理如果在 int 定义内部，{ 表示函数体开始，否则是语句块开始
                    lexeme.value = insideIntDefinition ? CPlus::FunctionBodyBegin : CPlus::BlockBegin;
                    // 若进入函数体，则不属于 int 定义内部，将其置为 false。
                    insideIntDefinition = false;
                    // 左花括号入栈
                    bracketStack.push(lexeme);
                } break;
                case CPlus::RightBracket: {
                    if (bracketStack.empty()) {
                        err << "Unmatching bracket at lexeme " << i << ".";
                        throw CPlus::CompileError(err.str());
                    }
                    Lexeme& correspondent = bracketStack.top();
                    lexeme.type = CPlus::Symbol;
                    // 同理，根据左花括号判断
                    lexeme.value = correspondent.value == CPlus::FunctionBodyBegin ? CPlus::FunctionBodyEnd : CPlus::BlockEnd;
                    bracketStack.pop();
                } break;
                case CPlus::LeftSquareBracket:
                    lexeme.type = CPlus::Symbol, lexeme.value = CPlus::ArrayAccessDefBegin;
                    break;
                case CPlus::RightSquareBracket:
                    lexeme.type = CPlus::Symbol, lexeme.value = CPlus::ArrayAccessDefEnd;
                    break;
                case CPlus::Comma:
                    lexeme.type = CPlus::Symbol, lexeme.value = CPlus::CommaSeparator;
                    break;
                case CPlus::Semicolon:
                    lexeme.type = CPlus::Symbol;
                    // 若当前不存在控制关键字，则 ; 表示语句分隔符，否则为控制块中的条件分隔符
                    lexeme.value = lastControlKeyword == CPlus::_KeywordNil ? CPlus::StatementSeparator : CPlus::ConditionSeparator;
                    // 同时结束 int 定义
                    insideIntDefinition = false;
                    break;
                default:
                    lexeme.type = CPlus::Operator;
                    break;
                }
            } break;
            default:
                break;
            }
        }
    }

public:
    LexicalAnalyzer(std::string source) {
        // 去除头文件等内容
        for (size_t i = 0; i < CPlus::strippedLineLength; i++) {
            const std::string& line = CPlus::strippedLines[i];
            size_t pos = source.find(line);
            if (pos != std::string::npos)
                source = source.substr(0, pos) + source.substr(pos + line.length());
        }
        this->tokenizer = std::make_shared<Tokenizer>(source);
        this->analyzed = false;
        this->identifierCounter = CPlus::_PredefinedIdentifierUpperBound;
        this->ptr = 0;
        /* 初始化预定义的标识符 */
        for (size_t i = 1; i < CPlus::_PredefinedIdentifierUpperBound; i++) {
            identifierMap[CPlus::predefinedIdentifiers[i]] = i;
        }
    }

    bool isAnalyzed() {
        return this->analyzed;
    }

    void analyze() {
        if (this->analyzed) return;
        this->parseTokens();
        this->resolveLexemeTypes();
        this->tokenizer = nullptr; // cleanup
        this->analyzed = true;
    }

    std::vector<Lexeme>& getLexemesRef() { return this->lexemes; }
    std::unordered_map<std::string, int> getIdentifierMap() { return this->identifierMap; }
};

class SyntaxAnalyzer {
public:
    class BaseASTNode {
    public:
        virtual ~BaseASTNode() = default;
        virtual CPlus::AbstractSyntaxTreeNodeType getNodeType() { return CPlus::BaseASTNode; }
    };

    class IdentifierASTNode : public BaseASTNode {
    private:
        CPlus::IdentifierType type;
        int id;
        std::vector<uint32_t> dimensions;
        std::vector<uint32_t> dimensionOffsets;
        uint32_t size;
        bool _static;
        uint32_t baseOffset;

        /**
         * 计算多维数组偏移量
        */
        static std::vector<uint32_t> calcDimensionOffsets(std::vector<uint32_t> dim) {
            std::vector<uint32_t> offsets;
            std::stack<uint32_t> s;
            s.push(1);
            for (std::vector<uint32_t>::reverse_iterator it = dim.rbegin(); !dim.empty() && it != dim.rend() - 1; it++)
                s.push((*it) * s.top());
            while (!s.empty()) {
                offsets.push_back(s.top());
                s.pop();
            }
            return offsets;
        }

        static uint32_t calcSize(std::vector<uint32_t> dimensions) {
            uint32_t size = 1;
            for (uint32_t dim : dimensions)
                size *= dim;
            return size;
        }
    public:
        IdentifierASTNode() : type(CPlus::_IdentifierTypeNil), id(0), size(0), baseOffset(0), _static(true) {}
        IdentifierASTNode(int id, uint32_t baseOffset, bool _static) : type(CPlus::Integer), id(id), baseOffset(baseOffset), size(1), _static(_static) {}
        IdentifierASTNode(int id, std::vector<uint32_t> dimensions, uint32_t baseOffset, bool _static) : 
            type(CPlus::IntegerArray), id(id), dimensions(dimensions), dimensionOffsets(calcDimensionOffsets(dimensions)), size(calcSize(dimensions)), _static(_static) {}
        IdentifierASTNode(CPlus::IdentifierType type, int id, uint32_t baseOffset, bool _static) : type(type), id(id), size(type == CPlus::Integer ? 1 : 0), baseOffset(baseOffset), _static(_static) {}
        IdentifierASTNode(CPlus::IdentifierType type, int id, std::vector<uint32_t> dimensions, uint32_t baseOffset, bool _static) : 
            type(type), id(id), dimensions(dimensions), dimensionOffsets(calcDimensionOffsets(dimensions)), size(calcSize(dimensions)), baseOffset(baseOffset), _static(_static) {}
        virtual CPlus::AbstractSyntaxTreeNodeType getNodeType() override {
            return CPlus::IdentifierASTNode;
        }

        static bool isThisBuiltInObject(std::shared_ptr<BaseASTNode> astNode, CPlus::PredefinedIdentifierEnum objectId) {
            if (astNode == nullptr) return false;
            if (astNode->getNodeType() != CPlus::IdentifierASTNode) return false;
            std::shared_ptr<IdentifierASTNode> identifier = std::dynamic_pointer_cast<IdentifierASTNode>(std::move(astNode));
            if (identifier->getIdentifierType() != CPlus::BuiltInObjects) return false;
            return identifier->getIdentifierId() == objectId;
        }

        bool isNilIdentifier() {
            return this->type == CPlus::_IdentifierTypeNil;
        }

        CPlus::IdentifierType getIdentifierType() { return this->type; }
        int getIdentifierId() { return this->id; }
        std::vector<uint32_t>& getDimensions() { return this->dimensions; }
        std::vector<uint32_t>& getDimensionOffsets() { return this->dimensionOffsets; }
        uint32_t getSize() { return this->size; }
        uint32_t& getBaseOffsetRef() { return this->baseOffset; }
        bool isStatic() { return this->_static; }
    };

    class IntConstantASTNode : public BaseASTNode {
        int value;
    public:
        IntConstantASTNode(int value) : value(value) {}
        virtual CPlus::AbstractSyntaxTreeNodeType getNodeType() override {
            return CPlus::IntConstantASTNode;
        }
        int getValue() { return value; }
    };

    // 由于我们的实现中不包含三元运算符，Not 等一元运算符也通过 BinaryExpressionASTNode 描述
    // 对于这类一元运算符，将只有 RHS
    class BinaryExpressionASTNode : public BaseASTNode {
        CPlus::SymbolOperatorEnum op;
        std::shared_ptr<BaseASTNode> left;
        std::shared_ptr<BaseASTNode> right;

    public:
        BinaryExpressionASTNode(CPlus::SymbolOperatorEnum op, 
                                std::shared_ptr<BaseASTNode> left, 
                                std::shared_ptr<BaseASTNode> right) : 
            op(op), left(std::move(left)), right(std::move(right)) {}

        virtual CPlus::AbstractSyntaxTreeNodeType getNodeType() override {
            return CPlus::BinaryExpressionASTNode;
        }

        CPlus::SymbolOperatorEnum& getOperatorRef() { return this->op; }
        std::shared_ptr<BaseASTNode>& getLeftRef()  { return this->left; }
        std::shared_ptr<BaseASTNode>& getRightRef() { return this->right; }
        bool initialized() { return this->left != nullptr || this->right != nullptr; }
    };

    class FunctionCallASTNode : public BaseASTNode {
        int funcId;
        std::vector<std::shared_ptr<BaseASTNode> > arguments;
    public:
        FunctionCallASTNode(int funcId,
                            std::vector<std::shared_ptr<BaseASTNode>> arguments) : 
            funcId(funcId), arguments(std::move(arguments)) {}
        virtual CPlus::AbstractSyntaxTreeNodeType getNodeType() override {
            return CPlus::FunctionCallASTNode;
        }

        int getFuncId() { return this->funcId; }
        std::vector<std::shared_ptr<BaseASTNode>>& getArgumentsRef() { return this->arguments; }
    };

    class BlockASTNode : public BaseASTNode {
        std::vector<std::shared_ptr<BaseASTNode> > statements;
        int localVarSize;
    public:
        BlockASTNode(std::vector<std::shared_ptr<BaseASTNode> > statements, int localVarSize) : 
            statements(std::move(statements)), localVarSize(localVarSize) {}
        virtual CPlus::AbstractSyntaxTreeNodeType getNodeType() override {
            return CPlus::BlockASTNode;
        }

        std::vector<std::shared_ptr<BaseASTNode> >& getStatementsRef() {
            return this->statements;
        }
        int& getLocalVarSizeRef() { return this->localVarSize; }
    };

    class IfStatementASTNode : public BaseASTNode {
        std::shared_ptr<BaseASTNode> condition;
        std::shared_ptr<BaseASTNode> truthy_stmt;
        std::shared_ptr<BaseASTNode> falsy_stmt;
    public:
        IfStatementASTNode(std::shared_ptr<BaseASTNode> condition,
                           std::shared_ptr<BaseASTNode> truthy_stmt) :
            condition(std::move(condition)), truthy_stmt(std::move(truthy_stmt)) {} 
        IfStatementASTNode(std::shared_ptr<BaseASTNode> condition,
                           std::shared_ptr<BaseASTNode> truthy_stmt,
                           std::shared_ptr<BaseASTNode> falsy_stmt) :
            condition(std::move(condition)), truthy_stmt(std::move(truthy_stmt)), falsy_stmt(std::move(falsy_stmt)) {} 
        virtual CPlus::AbstractSyntaxTreeNodeType getNodeType() override {
            return CPlus::IfStatementASTNode;
        }

        std::shared_ptr<BaseASTNode>& getConditionRef() { return this->condition; }
        std::shared_ptr<BaseASTNode>& getTruthyStmtRef() { return this->truthy_stmt; }
        std::shared_ptr<BaseASTNode>& getFalsyStmtRef() { return this->falsy_stmt; }
    };

    class WhileStatementASTNode : public BaseASTNode {
        std::shared_ptr<BaseASTNode> condition;
        std::shared_ptr<BaseASTNode> loop_stmt;

    public:
        WhileStatementASTNode(std::shared_ptr<BaseASTNode> condition,
                              std::shared_ptr<BaseASTNode> loop_stmt) : 
            condition(std::move(condition)), loop_stmt(std::move(loop_stmt)) {}
        virtual CPlus::AbstractSyntaxTreeNodeType getNodeType() override {
            return CPlus::WhileStatementASTNode;
        }

        std::shared_ptr<BaseASTNode>& getConditionRef() { return this->condition; }
        std::shared_ptr<BaseASTNode>& getLoopStmtRef() { return this->loop_stmt; }
    };

    class ForStatementASTNode : public BaseASTNode {
        std::shared_ptr<BaseASTNode> init;
        std::shared_ptr<BaseASTNode> condition;
        std::shared_ptr<BaseASTNode> iteration;
        std::shared_ptr<BaseASTNode> loop_stmt;
    public:
        ForStatementASTNode(std::shared_ptr<BaseASTNode> init,
                            std::shared_ptr<BaseASTNode> condition,
                            std::shared_ptr<BaseASTNode> iteration,
                            std::shared_ptr<BaseASTNode> loop_stmt) : 
            init(std::move(init)), 
            // 这里注意如果 for 循环的条件为空则令其为 1
            condition((condition == nullptr || (condition != nullptr && condition->getNodeType() == CPlus::BaseASTNode)) ? 
                std::make_shared<IntConstantASTNode>(1) : std::move(condition)),
            iteration(std::move(iteration)), loop_stmt(std::move(loop_stmt)) {}
        virtual CPlus::AbstractSyntaxTreeNodeType getNodeType() override {
            return CPlus::ForStatementASTNode;
        }

        std::shared_ptr<BaseASTNode>& getInitRef() { return this->init; }
        std::shared_ptr<BaseASTNode>& getConditionRef() { return this->condition; }
        std::shared_ptr<BaseASTNode>& getIterationRef() { return this->iteration; }
        std::shared_ptr<BaseASTNode>& getLoopStmtRef() { return this->loop_stmt; }
    };

    class ReturnStatementASTNode : public BaseASTNode {
        std::shared_ptr<BaseASTNode> expr;
    public:
        ReturnStatementASTNode(std::shared_ptr<BaseASTNode> expr) : expr(std::move(expr)) {}
        virtual CPlus::AbstractSyntaxTreeNodeType getNodeType() override {
            return CPlus::ReturnStatementASTNode;
        }

        std::shared_ptr<BaseASTNode>& getExprRef() { return this->expr; }
    };

    class Function {
        int funcId;
        std::vector<std::shared_ptr<IdentifierASTNode> > argumentIds;
        std::shared_ptr<BlockASTNode> statements;
        uint32_t maxStackSize;

    public:
        Function(int funcId, std::vector<std::shared_ptr<IdentifierASTNode> > argumentIds) :
            funcId(funcId), argumentIds(std::move(argumentIds)), statements(nullptr), maxStackSize(0) {}
        Function(int funcId, std::vector<std::shared_ptr<IdentifierASTNode> > argumentIds, 
                 std::shared_ptr<BlockASTNode> statements, uint32_t maxStackSize) :
            funcId(funcId), argumentIds(std::move(argumentIds)), statements(std::move(statements)), maxStackSize(maxStackSize) {}
        
        int getFuncId() { return this->funcId; }
        std::vector<std::shared_ptr<IdentifierASTNode> >& getArgumentIdsRef() { return this->argumentIds; }
        std::shared_ptr<BlockASTNode>& getStatementsRef() { return this->statements; }
        uint32_t& getMaxStackSizeRef() { return this->maxStackSize; }
    };

    class Scope {
        std::unordered_map<int, std::shared_ptr<class IdentifierASTNode> > identifiers;
        std::unordered_map<int, std::shared_ptr<class Function> > functions;
        std::shared_ptr<Scope> parentScope;

    public:
        Scope() : parentScope(nullptr) {}
        Scope(std::shared_ptr<Scope> parentScope) : parentScope(parentScope) {}

        void setIdentifier(int id, std::shared_ptr<class IdentifierASTNode> identifier) {
            this->identifiers[id] = std::move(identifier);
        }

        void setFunction(int id, std::shared_ptr<class Function> function) {
            this->functions[id] = std::move(function);
        }

        std::shared_ptr<class IdentifierASTNode> lookup(int identifierId) {
            if (this->identifiers.count(identifierId))
                return (*(this->identifiers.find(identifierId))).second;
            else if (this->parentScope != nullptr)
                return this->parentScope->lookup(identifierId);
            else
                return nullptr;
        }

        std::shared_ptr<class Function> lookupFunc(int funcId) {
            if (this->identifiers.count(funcId))
                return (*(this->functions.find(funcId))).second;
            else if (this->parentScope != nullptr)
                return this->parentScope->lookupFunc(funcId);
            else
                return nullptr;
        }

        std::unordered_map<int, std::shared_ptr<class Function> >::const_iterator funcBegin() {
            return this->functions.cbegin();
        }

        std::unordered_map<int, std::shared_ptr<class Function> >::const_iterator funcEnd() {
            return this->functions.cend();
        }

        std::unordered_map<int, std::shared_ptr<class IdentifierASTNode> >::const_iterator idBegin() {
            return this->identifiers.cbegin();
        }

        std::unordered_map<int, std::shared_ptr<class IdentifierASTNode> >::const_iterator idEnd() {
            return this->identifiers.cend();
        }
    };

private:
    bool analyzed = false;
    std::shared_ptr<LexicalAnalyzer> lex;
    std::shared_ptr<Scope> globalScope;

public:
    SyntaxAnalyzer(std::shared_ptr<LexicalAnalyzer> lex) : lex(std::move(lex)) {
        this->globalScope = std::make_shared<Scope>();
        globalScope->setIdentifier(CPlus::Cin, std::make_shared<IdentifierASTNode>(CPlus::BuiltInObjects, CPlus::Cin, CPlus::GlobalObjectsNilBaseOffset, true));
        globalScope->setIdentifier(CPlus::Cout, std::make_shared<IdentifierASTNode>(CPlus::BuiltInObjects, CPlus::Cout, CPlus::GlobalObjectsNilBaseOffset, true));
        globalScope->setIdentifier(CPlus::Endl, std::make_shared<IdentifierASTNode>(CPlus::BuiltInObjects, CPlus::Endl, CPlus::GlobalObjectsNilBaseOffset, true));
        globalScope->setIdentifier(CPlus::Putchar, std::make_shared<IdentifierASTNode>(CPlus::Function, CPlus::Putchar, CPlus::GlobalObjectsNilBaseOffset, true));
        std::vector<std::shared_ptr<IdentifierASTNode> > putcharArgs;
        std::vector<std::shared_ptr<BaseASTNode> > putcharStmt;
        putcharArgs.push_back(std::make_shared<IdentifierASTNode>(CPlus::Integer, 0, 0, false));
        globalScope->setFunction(CPlus::Putchar, std::make_shared<Function>(CPlus::Putchar, std::move(putcharArgs), nullptr /* putchar 特别处理*/, 0 ));
    }
    ~SyntaxAnalyzer() {}

    static std::pair<uint32_t, std::vector<std::shared_ptr<IdentifierASTNode> > > declarationAnalyze(std::vector<LexicalAnalyzer::Lexeme>& lexemes, size_t& ptr, uint32_t baseOffset, bool isStatic = false) {
        std::vector<std::shared_ptr<IdentifierASTNode> > ret;
        std::vector<uint32_t> dimensions;
        int id = 0, currentDimension = 0;
        CPlus::IdentifierType type = CPlus::_IdentifierTypeNil;
        bool insideArrayDefinition = false;

        for (bool iterate = true; iterate; ptr++) {
            LexicalAnalyzer::Lexeme& current = lexemes[ptr];
            switch (current.type) {
            case CPlus::Symbol: {
                switch (current.value) {
                case CPlus::StatementSeparator:
                case CPlus::FunctionCallDefEnd:
                    iterate = false;
                    continue;
                    break;
                case CPlus::CommaSeparator: {
                    // 完成当前的一个定义解析
                    std::shared_ptr<IdentifierASTNode> identifier = std::make_shared<IdentifierASTNode>(type, id, dimensions, baseOffset, isStatic);
                    baseOffset += identifier->getSize();
                    ret.push_back(std::move(identifier));
                    dimensions.clear();
                    id = 0;
                    // 重置类型
                    type = CPlus::_IdentifierTypeNil;
                    continue;
                } break;
                // 鉴定为数组
                case CPlus::ArrayAccessDefBegin:
                    type = CPlus::IntegerArray;
                    insideArrayDefinition = true;
                    break;
                case CPlus::ArrayAccessDefEnd:
                    dimensions.push_back(currentDimension);
                    currentDimension = 0;
                    insideArrayDefinition = false;
                    break;
                }
            } break;
            case CPlus::Keyword:
                continue;
                break;
            case CPlus::IntConstant:
                // 如果是数组内部则是当前数组维度
                if (insideArrayDefinition)
                    currentDimension = current.value;
                else
                    continue;
                break;
            case CPlus::Identifier:
                id = current.value;
                type = CPlus::Integer;
                break;
            default:
                continue;
                break;
            }
        }
        // 若最终类型未重置，说明有一个未添加的
        if (type != CPlus::_IdentifierTypeNil) {
            std::shared_ptr<IdentifierASTNode> identifier = std::make_shared<IdentifierASTNode>(type, id, dimensions, baseOffset, isStatic);
            baseOffset += identifier->getSize();
            ret.push_back(std::move(identifier));
        }
        // 返回添加当前所有变量后的偏移量和变量
        return std::make_pair(baseOffset, std::move(ret));
    }

    static std::pair<std::shared_ptr<IdentifierASTNode>, std::shared_ptr<Function> > functionAnalyze(
                                                    std::vector<LexicalAnalyzer::Lexeme>& lexemes,
                                                    size_t& ptr, std::shared_ptr<Scope> const& scope) {
        std::stringstream err;
        std::shared_ptr<IdentifierASTNode> funcIdentifier = nullptr;
        std::shared_ptr<Function> func = nullptr;
        int id = 0;
        uint32_t maxStackSize = 0;
        std::vector<std::vector<IdentifierASTNode> > args;
        std::shared_ptr<Scope> newScope = std::make_shared<Scope>(scope);
        uint32_t baseOffset = 0;
        for (bool iterate = true; iterate;) {
            LexicalAnalyzer::Lexeme& lexeme = lexemes[ptr];
            switch (lexeme.type) {
                case CPlus::Keyword:
                    ptr++;
                    continue;
                    break;
                case CPlus::Identifier:
                    id = lexeme.value;
                    ptr++;
                    continue;
                    break;
                case CPlus::Symbol:
                    switch (lexeme.value) {
                        case CPlus::FunctionCallDefBegin: {
                            ptr++;
                            std::pair<int, std::vector<std::shared_ptr<IdentifierASTNode> > > offsetAndIds = declarationAnalyze(lexemes, ptr, 0);
                            std::vector<std::shared_ptr<IdentifierASTNode> > &args = offsetAndIds.second;
                            baseOffset = offsetAndIds.first;
                            maxStackSize = baseOffset;
                            for (std::shared_ptr<IdentifierASTNode> astNode : args) {
                                int argId = astNode->getIdentifierId();
                                newScope->setIdentifier(argId, astNode);
                            }
                            func = std::make_shared<Function>(id, std::move(args));
                            funcIdentifier = std::make_shared<IdentifierASTNode>(CPlus::Function, id, CPlus::GlobalObjectsNilBaseOffset, true);
                            scope->setIdentifier(id, funcIdentifier);
                            scope->setFunction(id, func);
                        } break;
                        case CPlus::FunctionBodyBegin: {
                            ptr++;
                            func->getStatementsRef() = std::move(std::dynamic_pointer_cast<BlockASTNode>(statementsAnalyze(lexemes, ptr, newScope, baseOffset, maxStackSize, false, true)));
                            func->getMaxStackSizeRef() = maxStackSize;
                            iterate = false;
                        } break;
                    }
                    break;
                default:
                    err << "Unexpected lexeme in function analyze at " << ptr;
                    throw CPlus::CompileError(err.str());
                    break;
            }
        }
        if (func == nullptr) {
            err << "Unknown problem causes func to be null!";
            throw CPlus::CompileError(err.str());
        }
        return std::make_pair(std::move(funcIdentifier), std::move(func));
    }

    static std::shared_ptr<BaseASTNode> statementsAnalyze(std::vector<LexicalAnalyzer::Lexeme>& lexemes,
                                                          size_t& ptr, 
                                                          std::shared_ptr<Scope> const& scope,
                                                          uint32_t baseOffset, // 基础偏移量
                                                          uint32_t& maxStackSize, // 记录整个函数的语句块内分配内存数上限
                                                          bool onlyOneStatement = false, // 是否只解析一条语句（用于 if、for、while 等语句块）
                                                          bool forceBlock = false   // 强制返回语句块
                                                          ) {
        std::stringstream err;
        std::vector<std::shared_ptr<BaseASTNode> > statements;
        // 为了变量回收，需要记录语句块中直接声明的变量内存
        uint32_t maxSize = 0;
        // 处理 If-Else-If 结构时，需要记录上一个 If 语句信息
        std::shared_ptr<IfStatementASTNode> lastIf = nullptr;
        // 初始基础偏移量
        uint32_t initialBaseOffset = baseOffset;
        for (bool iterate = true; iterate;) {
            if (onlyOneStatement && !statements.empty()) {
                iterate = false;
                continue;
            }
            LexicalAnalyzer::Lexeme& lexeme = lexemes[ptr];
            switch (lexeme.type) {
                case CPlus::Symbol: {
                    switch (lexeme.value) {
                        case CPlus::BlockEnd:
                            iterate = false;
                            ptr++;
                            break;
                        case CPlus::FunctionBodyEnd:
                            iterate = false;
                            ptr++;
                            // 补充最后的 ret 指令
                            if ((!statements.empty() && statements.back()->getNodeType() != CPlus::ReturnStatementASTNode) || statements.empty())
                                statements.push_back(std::make_shared<ReturnStatementASTNode>(nullptr));
                            break;
                        case CPlus::BlockBegin: {
                            std::shared_ptr<Scope> newScope = std::make_shared<Scope>(scope);
                            ptr++;
                            std::shared_ptr<BlockASTNode> block = std::move(std::dynamic_pointer_cast<BlockASTNode>(
                                statementsAnalyze(lexemes, ptr, newScope, baseOffset, maxStackSize, false, true)
                            ));
                            statements.push_back(std::move(block));
                        } break;
                    }
                } break;
                case CPlus::Operator:
                case CPlus::Identifier: {
                    std::shared_ptr<BaseASTNode> expr = std::move(expressionAnalyze(lexemes, ptr, scope));
                    statements.push_back(std::move(expr));
                } break;
                case CPlus::Keyword: {
                    switch (lexeme.value) {
                        case CPlus::Int: {
                            std::pair<uint32_t, std::vector<std::shared_ptr<IdentifierASTNode> > > offsetAndIds = declarationAnalyze(lexemes, ptr, baseOffset);
                            baseOffset = offsetAndIds.first;
                            std::vector<std::shared_ptr<IdentifierASTNode> > &ids = offsetAndIds.second;
                            for (std::vector<std::shared_ptr<IdentifierASTNode> >::iterator it = ids.begin(); it != ids.end(); it++) {
                                int id = (*it)->getIdentifierId();
                                scope->setIdentifier(id, std::move(*it));
                            }
                            maxSize = std::max(baseOffset - initialBaseOffset, maxSize);
                            maxStackSize = std::max(maxStackSize, baseOffset);
                        } break;

                        case CPlus::If: {
                            LexicalAnalyzer::Lexeme& cbBegin = lexemes[++ptr];
                            if (cbBegin.type != CPlus::Symbol || cbBegin.value != CPlus::ControlBlockBegin) {
                                err << "Expecting ( at lexeme " << ptr << " for if stmt";
                                throw CPlus::CompileError(err.str());
                            }
                            ptr++;
                            std::shared_ptr<BaseASTNode> condition = std::move(expressionAnalyze(lexemes, ptr, scope));
                            std::shared_ptr<BaseASTNode> body = std::move(statementsAnalyze(lexemes, ptr, scope, baseOffset, maxStackSize, true));
                            lastIf = std::make_shared<IfStatementASTNode>(std::move(condition), std::move(body));
                            statements.push_back(lastIf);
                        } break;

                        case CPlus::Else: {
                            ptr++;
                            if (lastIf != nullptr) {
                                lastIf->getFalsyStmtRef() = std::move(statementsAnalyze(lexemes, ptr, scope, baseOffset, maxStackSize, true));
                                if (lastIf->getFalsyStmtRef()->getNodeType() == CPlus::IfStatementASTNode)
                                    lastIf = std::dynamic_pointer_cast<IfStatementASTNode>(lastIf->getFalsyStmtRef());
                                else
                                    lastIf = nullptr;
                            }
                            else {
                                err << "Standalone else found at lexeme " << ptr - 1;
                                throw CPlus::CompileError(err.str());
                            }
                        } break;

                        case CPlus::While: {
                            LexicalAnalyzer::Lexeme& cbBegin = lexemes[++ptr];
                            if (cbBegin.type != CPlus::Symbol || cbBegin.value != CPlus::ControlBlockBegin) {
                                err << "Expecting ( at lexeme " << ptr << " for if stmt";
                                throw CPlus::CompileError(err.str());
                            }
                            ptr++;
                            std::shared_ptr<BaseASTNode> condition = std::move(expressionAnalyze(lexemes, ptr, scope));
                            std::shared_ptr<BaseASTNode> body = std::move(statementsAnalyze(lexemes, ptr, scope, baseOffset, maxStackSize, true));
                            statements.push_back(std::make_shared<WhileStatementASTNode>(std::move(condition), std::move(body)));
                        } break;

                        case CPlus::For: {
                            LexicalAnalyzer::Lexeme& cbBegin = lexemes[++ptr];
                            if (cbBegin.type != CPlus::Symbol || cbBegin.value != CPlus::ControlBlockBegin) {
                                err << "Expecting ( at lexeme " << ptr << " for if stmt";
                                throw CPlus::CompileError(err.str());
                            }
                            ptr++;
                            std::shared_ptr<BaseASTNode> init = std::move(expressionAnalyze(lexemes, ptr, scope));
                            std::shared_ptr<BaseASTNode> condition = std::move(expressionAnalyze(lexemes, ptr, scope));
                            std::shared_ptr<BaseASTNode> iteration = std::move(expressionAnalyze(lexemes, ptr, scope));
                            std::shared_ptr<BaseASTNode> body = std::move(statementsAnalyze(lexemes, ptr, scope, baseOffset, maxStackSize, true));
                            statements.push_back(std::make_shared<ForStatementASTNode>(std::move(init), 
                                                    std::move(condition), std::move(iteration), std::move(body)));
                        } break;

                        case CPlus::Return: {
                            ptr++;
                            std::shared_ptr<BaseASTNode> expr = std::move(expressionAnalyze(lexemes, ptr, scope));
                            statements.push_back(std::make_shared<ReturnStatementASTNode>(std::move(expr)));
                        } break;
                    }
                } break;
                default:
                    break;
            }
        }
        if (statements.empty()) return std::make_shared<BaseASTNode>();
        if (statements.size() == 1 && !forceBlock)
            return std::move(statements[0]);
        return std::move(std::make_shared<BlockASTNode>(std::move(statements), maxSize));
    }

    static std::shared_ptr<BaseASTNode> expressionAnalyze(std::vector<LexicalAnalyzer::Lexeme>& lexemes, 
                                            size_t& ptr, std::shared_ptr<Scope> const &scope) {
        std::stringstream err;
        std::stack<std::shared_ptr<BaseASTNode> > operatorStack; // 符号栈
        std::vector<std::shared_ptr<BaseASTNode> > postfix;      // 后缀表达式
        for (bool iterate = true; iterate; ptr++) {
            LexicalAnalyzer::Lexeme& lexeme = lexemes[ptr];      // 当前词素
            switch (lexeme.type) {
                case CPlus::LexemeType::Identifier: {            // 若为标识符，先推入后缀表达式，在 Symbol 中处理函数调用
                    std::shared_ptr<IdentifierASTNode> identifier = scope->lookup(lexeme.value);
                    if (identifier != nullptr)
                        postfix.push_back(std::move(identifier));
                    else {
                        err << "Unknown identifier id " << lexeme.value << " at lexeme " << ptr;
                        throw CPlus::CompileError(err.str());
                    }
                } break;
                case CPlus::LexemeType::IntConstant: {           // 将 int 常量推入后缀表达式
                    postfix.push_back(std::move(std::make_shared<IntConstantASTNode>(lexeme.value)));
                } break;
                case CPlus::LexemeType::Operator: {
                    switch (lexeme.value) {
                        case CPlus::RightParen: {
                            while (!operatorStack.empty()) {
                                CPlus::SymbolOperatorEnum lastOp = std::dynamic_pointer_cast<BinaryExpressionASTNode>(operatorStack.top())->getOperatorRef();
                                // 不断弹出栈中的运算符，并加入后缀表达式，直到遇到对应的左括号
                                if (lastOp == CPlus::LeftParen) {
                                    operatorStack.pop();
                                    break;
                                }
                                postfix.push_back(std::move(operatorStack.top()));
                                operatorStack.pop();
                            }
                        } break;
                        case CPlus::LeftParen:
                            // 左括号仅压栈，不能进入后缀表达式
                            break;
                        default: {
                            while (!operatorStack.empty()) {
                                CPlus::SymbolOperatorEnum lastOp = std::dynamic_pointer_cast<BinaryExpressionASTNode>(operatorStack.top())->getOperatorRef();
                                // 按左结合原则，从符号栈中弹出优先级相同或更高的运算符
                                // 注意处理赋值运算符，它是右结合的，所以相同优先级不弹出
                                if ((lexeme.value == CPlus::Assign || lexeme.value == CPlus::Not || lexeme.value == CPlus::Positive || lexeme.value == CPlus::Negative)
                                        ? (CPlus::OperatorPrecedence[(CPlus::SymbolOperatorEnum)lexeme.value] > CPlus::OperatorPrecedence[lastOp])
                                        : (CPlus::OperatorPrecedence[(CPlus::SymbolOperatorEnum)lexeme.value] >= CPlus::OperatorPrecedence[lastOp])) {
                                    if (lastOp != CPlus::LeftParen) {
                                        postfix.push_back(std::move(operatorStack.top()));
                                        operatorStack.pop();
                                    }
                                    else break;
                                } else
                                    break;
                            }
                        } break;
                    }
                    // 只要不是右括号，最终都应当压栈
                    if (lexeme.value != CPlus::RightParen)
                        operatorStack.push(std::move(std::make_shared<BinaryExpressionASTNode>((CPlus::SymbolOperatorEnum)lexeme.value, nullptr, nullptr)));
                } break;
                case CPlus::LexemeType::Symbol: {
                    // 这个分支需要处理许多情况，如递归生成函数调用 AST、数组下标解引用等，并负责解析分隔符终止循环
                    switch (lexeme.value) {
                        // 分隔符，标志语句结束
                        // 这个简化的 C++ 版本没有逗号表达式，因此不需要考虑逗号表达式的问题
                        case CPlus::CommaSeparator:
                        case CPlus::ConditionSeparator:
                        case CPlus::StatementSeparator:
                        // 这两个结束符标志着递归处理的过程结束
                        case CPlus::ArrayAccessDefEnd:
                        case CPlus::FunctionCallDefEnd:
                        case CPlus::ControlBlockEnd:
                            iterate = false;
                            break;
                        case CPlus::ArrayAccessDefBegin: {
                            // 取出标识符
                            std::shared_ptr<IdentifierASTNode> identifier = std::move(std::dynamic_pointer_cast<IdentifierASTNode>(postfix.back()));
                            postfix.pop_back();
                            std::deque<std::shared_ptr<BaseASTNode> > arrayPostfix;
                            for (uint32_t offset : identifier->getDimensionOffsets()) {
                                ptr++;
                                // 递归解析每个下标内容
                                std::shared_ptr<BaseASTNode> index = std::move(expressionAnalyze(lexemes, ptr, scope));
                                // 和每维度基础偏移大小相乘
                                std::shared_ptr<BaseASTNode> indexWithDim = offset != 1 ? std::make_shared<BinaryExpressionASTNode>(CPlus::Multiply, 
                                        std::make_shared<IntConstantASTNode>(offset), std::move(index)) : std::move(index);
                                arrayPostfix.push_back(std::move(indexWithDim));
                            }
                            // 提前处理偏移相加
                            while (arrayPostfix.size() > 1) {
                                std::shared_ptr<BaseASTNode> first = std::move(arrayPostfix.front());
                                arrayPostfix.pop_front();
                                std::shared_ptr<BaseASTNode> second = std::move(arrayPostfix.front());
                                arrayPostfix.pop_front();
                                arrayPostfix.push_front(std::make_shared<BinaryExpressionASTNode>(CPlus::Plus, std::move(first), std::move(second)));
                            }
                            // 数组访问 AST
                            std::shared_ptr<BinaryExpressionASTNode> arrayAccess = std::make_shared<BinaryExpressionASTNode>(CPlus::ArrayMemoryDereference, 
                                                                                            std::move(identifier), std::move(arrayPostfix.front()));
                            arrayPostfix.pop_front();
                            postfix.push_back(std::move(arrayAccess));
                            ptr--;
                        } break;
                        case CPlus::FunctionCallDefBegin: {
                            int funcId = std::dynamic_pointer_cast<IdentifierASTNode>(postfix.back())->getIdentifierId();
                            postfix.pop_back();
                            std::vector<std::shared_ptr<BaseASTNode> > args;
                            ptr++;
                            std::shared_ptr<Function> func = std::move(scope->lookupFunc(funcId));
                            // 如果能找到函数，则不进行
                            if (func != nullptr) {
                                for (size_t i = 0; i < func->getArgumentIdsRef().size(); i++)
                                    args.push_back(std::move(expressionAnalyze(lexemes, ptr, scope)));
                                if (func->getArgumentIdsRef().size() == 0)
                                    ptr++;
                            }
                            else {
                                do {
                                    args.push_back(std::move(expressionAnalyze(lexemes, ptr, scope)));
                                } while (lexemes[ptr - 1].type == CPlus::Symbol && lexemes[ptr - 1].value == CPlus::CommaSeparator);
                            }
                            std::shared_ptr<FunctionCallASTNode> functionCall = std::make_shared<FunctionCallASTNode>(funcId, std::move(args));
                            postfix.push_back(std::move(functionCall));
                            ptr--;
                        } break;
                    }
                } break;
                default:
                    break;
            }
        }
        // 将符号栈剩余的表达式推入后缀表达式
        while (!operatorStack.empty()) {
            postfix.push_back(std::move(operatorStack.top()));
            operatorStack.pop();
        }
        // 复用符号栈，生成 AST
        for (std::vector<std::shared_ptr<BaseASTNode> >::iterator it = postfix.begin(); it != postfix.end(); it++) {
            std::shared_ptr<BaseASTNode> node = std::move(*it);
            if (node->getNodeType() == CPlus::BinaryExpressionASTNode) {
                std::shared_ptr<BinaryExpressionASTNode> bNode = std::move(std::dynamic_pointer_cast<BinaryExpressionASTNode>(node));
                if (!(bNode->initialized())) {
                    switch (bNode->getOperatorRef()) {
                        case CPlus::Not:
                        case CPlus::Positive:
                        case CPlus::Negative: {
                            // 这几个一元运算符只处理右侧
                            if (operatorStack.size() < 1) {
                                err << "Not enough operand for single op. ptr=" << ptr;
                                throw CPlus::CompileError(err.str());
                            }
                            std::shared_ptr<BaseASTNode> right = std::move(operatorStack.top());
                            operatorStack.pop();
                            bNode->getRightRef() = std::move(right);
                        } break;
                        default: {
                            if (operatorStack.size() < 2) {
                                err << "Not enough operand for binary op. ptr=" << ptr;
                                throw CPlus::CompileError(err.str());
                            }
                            std::shared_ptr<BaseASTNode> right = std::move(operatorStack.top());
                            operatorStack.pop();
                            std::shared_ptr<BaseASTNode> left = std::move(operatorStack.top());
                            operatorStack.pop();
                            bNode->getLeftRef()  = std::move(left);
                            bNode->getRightRef() = std::move(right);
                        } break;
                    }
                }
                operatorStack.push(std::move(bNode));
            }
            else
                operatorStack.push(std::move(node));
        }
        if (operatorStack.size() > 1) {
            err << "Unexpected size for final expr stack, size: " << operatorStack.size();
            throw CPlus::CompileError(err.str());
        }
        return operatorStack.empty() ? std::make_shared<BaseASTNode>() : std::move(operatorStack.top());
    }

    void analyze() {
        if (!this->lex->isAnalyzed())
            throw CPlus::CompileError("Syntax analyzer received an not well analyzed lexical analysis result.");
        if (this->analyzed) return;
        std::stringstream err;
        uint32_t baseOffset = CPlus::GlobalVariablesInitialBaseOffset;
        std::vector<LexicalAnalyzer::Lexeme> lexemes = this->lex->getLexemesRef();
        for (size_t ptr = 0; ptr < lexemes.size();) {
            // 需要在最外层处理的只有 int 开头的声明了
            if (lexemes[ptr].type == CPlus::Keyword && lexemes[ptr].value == CPlus::Int) {
                if (ptr + 2 >= lexemes.size()) {
                    err << "Unexpected position of int at lexeme " << ptr;
                    throw CPlus::CompileError(err.str());
                }
                // 函数解析
                if (lexemes[ptr + 2].type == CPlus::Symbol && lexemes[ptr + 2].value == CPlus::FunctionCallDefBegin) {
                    std::pair<std::shared_ptr<IdentifierASTNode>, std::shared_ptr<Function> > identifierAndFunc = functionAnalyze(lexemes, ptr, globalScope);
                    std::shared_ptr<IdentifierASTNode> funcIdentifier = std::move(identifierAndFunc.first);
                    std::shared_ptr<Function> func = std::move(identifierAndFunc.second);
                    if (func != nullptr) {
                        int id = func->getFuncId();
                        globalScope->setIdentifier(id, funcIdentifier);
                        globalScope->setFunction(id, std::move(func));
                    }
                    else {
                        err << "Malformed function at " << ptr << ", whose parse result is null.";
                        throw CPlus::CompileError(err.str());
                    }
                }
                // 变量解析
                else {
                    std::pair<int, std::vector<std::shared_ptr<IdentifierASTNode> > > offsetAndIds = declarationAnalyze(lexemes, ptr, baseOffset, true);
                    baseOffset = offsetAndIds.first;
                    std::vector<std::shared_ptr<IdentifierASTNode> > ids = std::move(offsetAndIds.second);
                    for (std::vector<std::shared_ptr<IdentifierASTNode> >::iterator it = ids.begin(); it != ids.end(); it++) {
                        int id = (*it)->getIdentifierId();
                        globalScope->setIdentifier(id, std::move(*it));
                    }
                }
            }
            else {
                err << "Unexpected lexeme {type:" << lexemes[ptr].type << ",value:" << lexemes[ptr].value << "} at lexeme " << ptr;
                throw CPlus::CompileError(err.str());
            }
        }
        this->lex = nullptr;
    }

    std::shared_ptr<Scope> getGlobalScope() {
        return this->globalScope;
    }
};

class Runtime {
    // 全局作用域，其中包含静态变量和函数
    std::shared_ptr<SyntaxAnalyzer::Scope> globalScope;
    // 内存
    std::vector<int> globalMemory;
    std::stack<std::vector<int> > stackFrames;
    std::stack<int> espStack;
    // 输入流
    std::queue<int>& in;
    // 输出流
    std::ostream& out;

    // 判断一个 AST 节点是否可获取内存地址
    static bool referenceable(std::shared_ptr<SyntaxAnalyzer::BaseASTNode> rawAstNode) {
        switch (rawAstNode->getNodeType()) {
            case CPlus::IdentifierASTNode: {
                std::shared_ptr<SyntaxAnalyzer::IdentifierASTNode> astNode = std::dynamic_pointer_cast<SyntaxAnalyzer::IdentifierASTNode>(std::move(rawAstNode));
                switch (astNode->getIdentifierType()) {
                    // 只有整数和整数数组可以
                    case CPlus::Integer:
                    case CPlus::IntegerArray:
                        return true;
                    default: return false;
                }
            } break;
            case CPlus::BinaryExpressionASTNode: {
                std::shared_ptr<SyntaxAnalyzer::BinaryExpressionASTNode> astNode = std::dynamic_pointer_cast<SyntaxAnalyzer::BinaryExpressionASTNode>(std::move(rawAstNode));
                switch (astNode->getOperatorRef()) {
                case CPlus::ArrayMemoryDereference:
                    return referenceable(astNode->getLeftRef());
                default:
                    return false;
                }
            } break;
            default: return false;
        }
        return false;
    }

    // 获取指针
    std::pair<bool, uint32_t> pointerEval(std::shared_ptr<SyntaxAnalyzer::BaseASTNode> rawAstNode) {
        switch (rawAstNode->getNodeType()) {
            case CPlus::IdentifierASTNode: {
                std::shared_ptr<SyntaxAnalyzer::IdentifierASTNode> astNode = std::dynamic_pointer_cast<SyntaxAnalyzer::IdentifierASTNode>(std::move(rawAstNode));
                return std::make_pair(astNode->isStatic(), astNode->getBaseOffsetRef());
            } break;
            case CPlus::BinaryExpressionASTNode: {
                std::shared_ptr<SyntaxAnalyzer::BinaryExpressionASTNode> astNode = std::dynamic_pointer_cast<SyntaxAnalyzer::BinaryExpressionASTNode>(std::move(rawAstNode));
                if (astNode->getOperatorRef() != CPlus::ArrayMemoryDereference) {
                    throw CPlus::RuntimeError("Invalid pointer evaluation: astNode is not array memory dereference.");
                }
                std::pair<bool, uint32_t> leftHand = pointerEval(astNode->getLeftRef());
                uint32_t address = leftHand.second + eval(astNode->getRightRef());
                return std::make_pair(leftHand.first, address);
            } break;
        }
        throw CPlus::RuntimeError("Unexpectedly reached undefined pointer evaluation!");
    }

    int eval(std::shared_ptr<SyntaxAnalyzer::BaseASTNode> rawAstNode, bool* shouldStop = nullptr) {
        if (rawAstNode == nullptr)
            return 0;
        std::stringstream err;
#define deref(pointer) (pointer.first ? this->globalMemory[pointer.second] : this->stackFrames.top()[pointer.second])
        switch (rawAstNode->getNodeType()) {
            case CPlus::IdentifierASTNode: {
                std::shared_ptr<SyntaxAnalyzer::IdentifierASTNode> astNode = std::dynamic_pointer_cast<SyntaxAnalyzer::IdentifierASTNode>(std::move(rawAstNode));
                if (!referenceable(astNode)) {
                    err << "Warning: invalid evaluation: not referenceable identifier " << astNode->getIdentifierId();
                    throw CPlus::RuntimeError(err.str());
                }
                std::pair<bool, uint32_t> pointer = pointerEval(astNode);
                return deref(pointer);
            } break;
            case CPlus::IntConstantASTNode: {
                std::shared_ptr<SyntaxAnalyzer::IntConstantASTNode> astNode = std::dynamic_pointer_cast<SyntaxAnalyzer::IntConstantASTNode>(std::move(rawAstNode));
                return astNode->getValue();
            } break;
            case CPlus::BinaryExpressionASTNode: {
                std::shared_ptr<SyntaxAnalyzer::BinaryExpressionASTNode> astNode = std::dynamic_pointer_cast<SyntaxAnalyzer::BinaryExpressionASTNode>(std::move(rawAstNode));
                switch (astNode->getOperatorRef()) {
// 本题有一个隐性条件，也是大部分 C/C++ 编译器的行为
// 二元运算符求值时，先求左值再求右值
#define evalTwoOps(Op)                        \
{                                             \
    int left = eval(astNode->getLeftRef()); \
    int right = eval(astNode->getRightRef()); \
    return left Op right; \
}
            case CPlus::Not:
                return !eval(astNode->getRightRef());
                break;
            case CPlus::Positive:
                return eval(astNode->getRightRef());
                break;
            case CPlus::Negative:
                return -eval(astNode->getRightRef());
                break;
            case CPlus::Multiply:
                evalTwoOps(*);
                break;
            case CPlus::Divide:
                evalTwoOps(/);
                break;
            case CPlus::Modulo:
                evalTwoOps(%);
                break;
            case CPlus::Plus:
                evalTwoOps(+);
                break;
            case CPlus::Minus:
                evalTwoOps(-);
                break;
            case CPlus::LessEqual:
                evalTwoOps(<=);
                break;
            case CPlus::GreaterEqual:
                evalTwoOps(>=);
                break;
            case CPlus::LessThan:
                evalTwoOps(<);
                break;
            case CPlus::GreaterThan:
                evalTwoOps(>);
                break;
            case CPlus::Equal:
                evalTwoOps(==);
                break;
            case CPlus::NotEqual:
                evalTwoOps(!=);
                break;
            case CPlus::Xor:
                evalTwoOps(^);
                break;
            case CPlus::LogicalAnd:
                evalTwoOps(&&);
                break;
            case CPlus::LogicalOr:
                evalTwoOps(||);
                break;
            case CPlus::CoutFrom: {
                bool isEndl = SyntaxAnalyzer::IdentifierASTNode::isThisBuiltInObject(astNode->getRightRef(), CPlus::Endl);
                int outdata;
                if (!SyntaxAnalyzer::IdentifierASTNode::isThisBuiltInObject(astNode->getLeftRef(), CPlus::Cout))
                    eval(astNode->getLeftRef());
                if (isEndl)
                    this->out << std::endl;
                else
                    this->out << eval(astNode->getRightRef());
                return 0;
            } break;
            case CPlus::CinTo: {
                if (!SyntaxAnalyzer::IdentifierASTNode::isThisBuiltInObject(astNode->getLeftRef(), CPlus::Cin))
                    eval(astNode->getLeftRef());
                if (referenceable(astNode->getRightRef())) {
                    if (this->in.empty()) {
                        err << "Not sufficient input.";
                        throw CPlus::RuntimeError(err.str());
                    }
                    else {
                        std::pair<bool, uint32_t> pointer = pointerEval(astNode->getRightRef());
                        deref(pointer) = this->in.front();
                        this->in.pop();
                    }
                } else {
                    err << "Invalid cin to: not referenceable RHS.";
                    throw CPlus::RuntimeError(err.str());
                }
                return 0;
            } break;
            case CPlus::Assign: {
                if (referenceable(astNode->getLeftRef())) {
                    std::pair<bool, uint32_t> pointer = pointerEval(astNode->getLeftRef());
                    int value = eval(astNode->getRightRef());
                    deref(pointer) = value;
                    return value;
                } else {
                    err << "Invalid assignment: LHS is not referenceable.";
                    throw CPlus::RuntimeError(err.str());
                    return 0;
                }
            } break;
            case CPlus::ArrayMemoryDereference: {
                if (!referenceable(astNode)) {
                    err << "Invalid dereference: not referenceable LHS";
                    throw CPlus::RuntimeError(err.str());
                }
                std::pair<bool, uint32_t> pointer = pointerEval(astNode);
                return deref(pointer);
            } break;
#undef deref
#undef evalTwoOps
                }
            } break;
            case CPlus::FunctionCallASTNode: {
                std::shared_ptr<SyntaxAnalyzer::FunctionCallASTNode> astNode = std::dynamic_pointer_cast<SyntaxAnalyzer::FunctionCallASTNode>(std::move(rawAstNode));
                if (astNode->getFuncId() == CPlus::Putchar) {
                    char ch = eval(astNode->getArgumentsRef()[0]);
                    out << ch;
                    return ch;
                }
                else {
                    int i = 0;
                    std::vector<int> args;
                    for (std::vector<std::shared_ptr<SyntaxAnalyzer::BaseASTNode> >::iterator it = astNode->getArgumentsRef().begin(); it != astNode->getArgumentsRef().end(); it++, i++) {
                        args.push_back(eval(*it));
                    }
                    return callFunc(astNode->getFuncId(), args, i);
                }
            } break;
            case CPlus::BlockASTNode: {
                std::shared_ptr<SyntaxAnalyzer::BlockASTNode> astNode = std::dynamic_pointer_cast<SyntaxAnalyzer::BlockASTNode>(std::move(rawAstNode));
                uint32_t previousEsp = this->espStack.top();
                this->espStack.push(previousEsp + astNode->getLocalVarSizeRef());
                int retVal;
                for (std::vector<std::shared_ptr<SyntaxAnalyzer::BaseASTNode> >::iterator it = astNode->getStatementsRef().begin(); it != astNode->getStatementsRef().end(); it++) {
                    retVal = eval(*it, shouldStop);
                    if (shouldStop && *shouldStop)
                        break;
                }
                for (uint32_t i = previousEsp; i < this->espStack.top(); i++)
                    this->stackFrames.top()[i] = 0;
                this->espStack.pop();
                return retVal;
            } break;
            case CPlus::IfStatementASTNode: {
                std::shared_ptr<SyntaxAnalyzer::IfStatementASTNode> astNode = std::dynamic_pointer_cast<SyntaxAnalyzer::IfStatementASTNode>(std::move(rawAstNode));
                if (eval(astNode->getConditionRef()))
                    return eval(astNode->getTruthyStmtRef(), shouldStop);
                else
                    return eval(astNode->getFalsyStmtRef(), shouldStop);
            } break;
            case CPlus::WhileStatementASTNode: {
                std::shared_ptr<SyntaxAnalyzer::WhileStatementASTNode> astNode = std::dynamic_pointer_cast<SyntaxAnalyzer::WhileStatementASTNode>(std::move(rawAstNode));
                int retVal;
                while (eval(astNode->getConditionRef())) {
                    retVal = eval(astNode->getLoopStmtRef(), shouldStop);
                    if (shouldStop && *shouldStop)
                        break;
                }
                return retVal;
            } break;
            case CPlus::ForStatementASTNode: {
                std::shared_ptr<SyntaxAnalyzer::ForStatementASTNode> astNode = std::dynamic_pointer_cast<SyntaxAnalyzer::ForStatementASTNode>(std::move(rawAstNode));
                int retVal;
                for (eval(astNode->getInitRef()); eval(astNode->getConditionRef()); eval(astNode->getIterationRef())) {
                    retVal = eval(astNode->getLoopStmtRef(), shouldStop);
                    if (shouldStop && *shouldStop)
                        break;
                }
                return retVal;
            } break;
            case CPlus::ReturnStatementASTNode: {
                std::shared_ptr<SyntaxAnalyzer::ReturnStatementASTNode> astNode = std::dynamic_pointer_cast<SyntaxAnalyzer::ReturnStatementASTNode>(std::move(rawAstNode));
                if (shouldStop)
                    *shouldStop = true;
                return eval(astNode->getExprRef());
            } break;
            case CPlus::BaseASTNode: {
                return 0;
            } break;
            default:
                break;
        }
        err << "Warning: Unexpectedly reached undefined evaluation.";
        throw CPlus::RuntimeError(err.str());
    }
public:
    Runtime(std::shared_ptr<SyntaxAnalyzer::Scope> globalScope,
            std::queue<int>& in,
            std::ostream& out) : globalScope(std::move(globalScope)), in(in), out(out) {
        // 分配全局数据区内存
        uint32_t globalSize = 0;
        for (std::unordered_map<int, std::shared_ptr<SyntaxAnalyzer::IdentifierASTNode> >::const_iterator it = this->globalScope->idBegin(); it != this->globalScope->idEnd(); it++)
            globalSize = std::max(globalSize, (*it).second->getBaseOffsetRef() + (*it).second->getSize());
        this->globalMemory.reserve(globalSize);
        std::fill(globalMemory.data(), globalMemory.data() + globalSize, 0);
    }
    ~Runtime() {}

    int callFunc(int funcId, std::vector<int> const &args, int const esp) {
        std::stringstream err;
        std::shared_ptr<SyntaxAnalyzer::Function> func = this->globalScope->lookupFunc(funcId);
        if (func != nullptr) {
            bool shouldStop = false;
            this->stackFrames.push(std::vector<int>{});
            std::vector<int>& stackFrame = this->stackFrames.top();
            // 分配栈帧内存
            stackFrame.reserve(func->getMaxStackSizeRef());
            std::fill(stackFrame.data(), stackFrame.data() + func->getMaxStackSizeRef(), 0);
            // 填入参数
            for (std::vector<int>::const_iterator it = args.cbegin(); it != args.cend(); it++)
                stackFrame.push_back(*it);
            this->espStack.push(esp);
            int retVal = eval(func->getStatementsRef(), &shouldStop);
            stackFrames.pop();
            espStack.pop();
            return retVal;
        }
        else {
            err << "Fatal: function " << funcId << " not found!";
            throw CPlus::RuntimeError(err.str());
        }
    }

    int callFunc(int funcId) {
        std::vector<int> args;
        return callFunc(funcId, args, 0);
    }
};
#ifndef FUTURE_DEBUG
int main() {
    int n;
    std::cin >> n;
    std::queue<int> inputData;
    for (size_t i = 0, k; i < n; i++) {
        std::cin >> k;
        inputData.push(k);
    }
    std::stringstream testCpp;
    testCpp << std::cin.rdbuf();
    try {
    std::shared_ptr<LexicalAnalyzer> lex = std::make_shared<LexicalAnalyzer>(testCpp.str());
    lex->analyze();
    std::shared_ptr<SyntaxAnalyzer> syntx = std::make_shared<SyntaxAnalyzer>(lex);
    lex = nullptr;
    syntx->analyze();
        try {
            std::shared_ptr<Runtime> rt = std::make_shared<Runtime>(syntx->getGlobalScope(), inputData, std::cout);
            return rt->callFunc(CPlus::Main);
        } catch (CPlus::RuntimeError& err) {
            std::cerr << "Runtime error: \n  " << err.what() << std::endl;
            return -1;
        }
    } catch (CPlus::CompileError& err) {
        std::cerr << "Compile error: \n  " << err.what() << std::endl;
        return -2;
    }
}
#else
#include "debugcode_future.cpp"
#endif