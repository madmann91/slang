#ifndef SLANG_PREPROCESS_H
#define SLANG_PREPROCESS_H

#include <string>
#include <vector>
#include <list>
#include <functional>

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
    Preprocessor(std::function<Token()> input, Logger& logger, size_t max_depth = 1024);

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

    struct State {
        enum Branch {
            BRANCH_IF,
            BRANCH_ELSE,
            BRANCH_ELIF
        };

        State() {}
        State(bool e, bool d, Branch b)
            : enabled(e), done(d), branch(b)
        {}

        bool enabled;
        bool done;
        Branch branch;
    };

    void next();
    void eat(Token::Type);
    void expect(Token::Type);

    void eat_line(bool);

    void parse_directive();
    void parse_pragma();
    void parse_if();
    void parse_endif();
    void parse_else();
    void parse_elif();
    void parse_ifdef_ifndef(bool);
    void parse_define();

    void start_expansion(const Macro& macro);

    bool evaluate_condition();

    std::ostream& error();
    std::ostream& warn();

    std::function<Token()> input_;

    Logger& logger_;
    Token prev_, lookup_;
    size_t max_depth_;
    bool expand_;
    std::vector<State> state_stack_;
    std::vector<Context> ctx_stack_;
    std::vector<Token> ctx_buffer_;
    std::unordered_map<std::string, bool> expanded_;
    std::unordered_map<std::string, Macro> macros_;
};

} // namespace slang

#endif // SLANG_PREPROCESS_H
