#ifndef SLANG_PREPROCESS_H
#define SLANG_PREPROCESS_H

#include <string>
#include <vector>
#include <list>

#include "slang/token.h"
#include "slang/lexer.h"

namespace slang {

/// A preprocessor macro (without the name).
class Macro {
public:
    typedef std::vector<Token> Arg;

    Macro() {}

    Macro(const std::unordered_map<std::string, size_t>& args,
          const std::vector<Token>& rule)
        : args_(args), rule_(rule)
    {}

    /// Applies the macro rule with the given arguments, pushes the result into the vector
    void apply(const std::vector<Arg>&, std::vector<Token>&) const;

    const std::unordered_map<std::string, size_t>& args() const { return args_; }
    const std::vector<Token>& rule() const { return rule_; }

    bool has_args() const { return args_.size() != 0; }
    int num_args() const { return args_.size(); }

private:
    std::unordered_map<std::string, size_t> args_;
    std::vector<Token> rule_;
};

/// The preprocessor : expands macros and handles preprocessor directives.
class Preprocessor {
public:
    Preprocessor(Lexer& lexer, Logger& logger, size_t max_depth = 1024);

    /// Extracts the next preprocessed token from the stream.
    Token preprocess();

private:
    struct Context {
        Context() {}
        Context(int a, int b, const std::string& name)
            : first(a), last(b), cur(a), macro_name(name)
        {}

        int first, last, cur;
        std::string macro_name;
    };

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

    void start_expansion(const Macro& macro);

    std::ostream& error();
    std::ostream& warn();

    Lexer& lexer_;
    Logger& logger_;
    Token lookup_;
    size_t max_depth_;
    std::vector<Context> stack_;
    std::vector<Token> buffer_;
    std::unordered_map<std::string, bool> expanded_;
    std::unordered_map<std::string, Macro> macros_;
};

} // namespace slang

#endif // SLANG_PREPROCESS_H
