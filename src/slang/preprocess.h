#ifndef SLANG_PREPROCESS_H
#define SLANG_PREPROCESS_H

#include <string>
#include <vector>

#include "slang/token.h"
#include "slang/lexer.h"

namespace slang {

/// A #define macro (without the name).
class Macro {
public:
    Macro() {}

    Macro(const std::vector<std::string>& args,
          const std::vector<Token>& rule)
        : args_(args), rule_(rule)
    {}

    std::vector<Token> apply(std::vector<Token>& args) const;

    const std::vector<std::string>& args() const { return args_; }
    const std::vector<Token>& rule() const { return rule_; }
private:
    std::vector<std::string> args_;
    std::vector<Token> rule_;
};

/// The preprocessor : expands macros and handles preprocessor directives.
class Preprocessor {
public:
    Preprocessor(Lexer& lexer, Logger& logger);

    Token preprocess();

private:
    void lex();
    void eat(Token::Type);
    void expect(Token::Type);

    void parse_directive();
    void parse_pragma();
    void parse_if();
    void parse_endif();
    void parse_else();
    void parse_ifndef();
    void parse_ifdef();
    void parse_define();

    std::ostream& error();
    std::ostream& warn();

    Lexer& lexer_;
    Logger& logger_;
    Token lookup_;
    std::vector<Token> buffer_;
    size_t buf_index_;
    std::unordered_map<std::string, Macro> macros_;
};

} // namespace slang

#endif // SLANG_PREPROCESS_H
