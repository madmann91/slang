#ifndef SLANG_PREPROCESS_H
#define SLANG_PREPROCESS_H

#include <string>
#include <vector>
#include <list>

#include "slang/token.h"
#include "slang/lexer.h"

namespace slang {

/// A #define macro (without the name).
class Macro {
public:
    typedef std::vector<Token> Arg;

    Macro() {}

    Macro(const std::unordered_map<std::string, int>& args,
          const std::vector<Token>& rule)
        : args_(args), rule_(rule)
    {}

    std::vector<Token> apply(const std::vector<Arg>& args) const;

    const std::unordered_map<std::string, int>& args() const { return args_; }
    const std::vector<Token>& rule() const { return rule_; }

    bool has_args() const { return args_.size() != 0; }
    int num_args() const { return args_.size(); }
private:
    std::unordered_map<std::string, int> args_;
    std::vector<Token> rule_;
};

/// The preprocessor : expands macros and handles preprocessor directives.
class Preprocessor {
public:
    Preprocessor(Lexer& lexer, Logger& logger, int max_depth = 1024);

    /// Extracts the next preprocessed token from the stream.
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

    void expand_macro(const Macro&, std::list<Token>&, int);
    std::vector<Macro::Arg> parse_macro_args(const std::string&, const Macro&, const std::list<Token>&);
    std::list<Token>::iterator expansion_site(const std::list<Token>&, std::list<Token>::iterator);

    std::ostream& error();
    std::ostream& warn();

    Lexer& lexer_;
    Logger& logger_;
    Token lookup_;
    int max_depth_;
    std::list<Token> buffer_;
    std::unordered_map<std::string, bool> expanded_;
    std::unordered_map<std::string, Macro> macros_;
};

} // namespace slang

#endif // SLANG_PREPROCESS_H
