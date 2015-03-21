#ifndef SLANG_PREPROCESS_H
#define SLANG_PREPROCESS_H

#include <string>
#include <vector>
#include <functional>
#include <unordered_map>
#include <unordered_set>

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

/// GLSL profiles as specified in the version preprocessor directive
enum class Profile {
    PROFILE_CORE,
    PROFILE_COMPAT,
    PROFILE_ES
};

/// The preprocessor : expands macros and handles preprocessor directives.
class Preprocessor {
public:
    Preprocessor(Lexer& lexer, Logger& logger,
                 std::function<bool(int, Profile)> version_handler = default_version_handler,
                 std::function<bool(const std::vector<Token>&)> pragma_handler = default_pragma_handler,
                 size_t max_depth = 1024);

    /// Extracts the next preprocessed token from the stream.
    Token preprocess();

    /// Default version directive handler (does nothing)
    static bool default_version_handler(int, Profile) { return true; }

    /// Default pragma directive handler (does nothing)
    static bool default_pragma_handler(const std::vector<Token>&) { return true; }

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

    struct ExprValue {
        ExprValue() : error(true) {}
        ExprValue(int i) : error(false), value(i) {}

        template <typename F>
        ExprValue apply(F f) const {
            ExprValue other = *this;
            other.value = f(value);
            return other;
        }

        bool error;
        int value;
    };

    struct BinOp {
        enum Type {
            BINOP_MUL,
            BINOP_DIV,
            BINOP_MOD,
            BINOP_ADD,
            BINOP_SUB,
            BINOP_LSHIFT,
            BINOP_RSHIFT,
            BINOP_LT,
            BINOP_LE,
            BINOP_GT,
            BINOP_GE,
            BINOP_EQ,
            BINOP_NEQ,
            BINOP_AND,
            BINOP_XOR,
            BINOP_OR,
            BINOP_ANDAND,
            BINOP_OROR,
            BINOP_UNKNOWN
        };

        BinOp();
        BinOp(Token);

        ExprValue apply(ExprValue, ExprValue) const;

        static const int max_pred;

        Type type;
        int pred;
        bool rassoc;
    };

    void next();
    void eat(Token::Type);
    void expect(Token::Type);

    void eat_line(bool);
    bool check_newline();

    void parse_directive();
    void parse_pragma();
    void parse_if();
    void parse_endif();
    void parse_else();
    void parse_elif();
    void parse_ifdef_ifndef(bool);
    void parse_define();
    void parse_undef();
    void parse_version();

    bool expand(bool);

    bool evaluate_condition();
    ExprValue evaluate_primary();
    ExprValue evaluate_binary(ExprValue, int);

    std::ostream& error();
    std::ostream& warn();

    Lexer& lexer_;
    Logger& logger_;

    std::function<bool(int, Profile)>              version_handler_;
    std::function<bool(const std::vector<Token>&)> pragma_handler_;

    Token prev_, lookup_;
    size_t max_depth_;
    bool first_;
    std::vector<State> state_stack_;
    std::vector<Context> ctx_stack_;
    std::vector<Token> ctx_buffer_;
    std::unordered_set<std::string> expanded_;
    std::unordered_map<std::string, Macro> macros_;
};

inline std::ostream& operator << (std::ostream& out, Profile p) {
    switch (p) {
        case Profile::PROFILE_CORE:   out << "core";          break;
        case Profile::PROFILE_COMPAT: out << "compatibility"; break;
        case Profile::PROFILE_ES:     out << "es";            break;
        default:
            assert(0 && "Invalid profile");
    }
    return out;
}

} // namespace slang

#endif // SLANG_PREPROCESS_H
