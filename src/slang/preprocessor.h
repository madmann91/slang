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

/// A preprocessor macro (without the name). Can be a builtin macro or a user defined macro.
class Macro {
public:
    typedef std::vector<Token> Arg;

    Macro() {}

    /// Builds a macro that has no argument and the given rule.
    Macro(const std::vector<Token>& rule)
        : rule_(rule)
    {}

    /// Builds a macro that has no argument and the given builtin function.
    Macro(std::function<std::vector<Token>(const std::vector<Arg>&)> builtin)
        : builtin_(builtin)
    {}

    /// Builds a macro with the given map from argument name to argument index and the given rule.
    Macro(const std::unordered_map<std::string, size_t>& args,
          const std::vector<Token>& rule)
        : args_(args), rule_(rule)
    {}

    /// Builds a macro with the given map from argument name to argument index and the builtin function.
    Macro(const std::unordered_map<std::string, size_t>& args,
          std::function<std::vector<Token>(const std::vector<Arg>&)> builtin)
        : args_(args), builtin_(builtin)
    {}

    /// Returns the arguments of the macro.
    const std::unordered_map<std::string, size_t>& args() const { return args_; }
    /// Returns the expansion rule of the macro.
    const std::vector<Token>& rule() const { assert(!is_builtin()); return rule_; }
    /// Calls the builtin function associated with this macro (e.g. __FILE__ returns the source index).
    std::vector<Token> builtin(const std::vector<Arg>& args) const { assert(is_builtin()); return builtin_(args); }

    /// Determines if the macro has any arguments.
    bool has_args() const { return args_.size() != 0; }
    /// Returns the number of arguments of the macro.
    size_t num_args() const { return args_.size(); }

    /// Returns true if the macro is a builtin macro (like __FILE__, or __LINE__).
    bool is_builtin() const { return static_cast<bool>(builtin_); }

private:
    std::unordered_map<std::string, size_t> args_;
    std::function<std::vector<Token>(const std::vector<Arg>&)> builtin_;
    std::vector<Token> rule_;
};

/// GLSL profiles as specified in the version preprocessor directive.
enum class Profile {
    PROFILE_CORE,
    PROFILE_COMPAT,
    PROFILE_ES
};

/// The possible behaviors of an extension directive.
enum class ExtBehavior {
    BEHAVIOR_REQUIRE,
    BEHAVIOR_ENABLE,
    BEHAVIOR_WARN,
    BEHAVIOR_DISABLE
};

/// The preprocessor : expands macros and handles preprocessor directives.
class Preprocessor {
public:
    typedef std::function<void(int, Profile)>                    VersionHandler;
    typedef std::function<void(const std::vector<Token>&)>       PragmaHandler;
    typedef std::function<void(const std::string&, ExtBehavior)> ExtensionHandler;

    /// Creates a preprocessor that reads tokens from the given lexer.
    Preprocessor(Lexer& lexer, Logger& logger,
                 VersionHandler version_handler = default_version_handler,
                 PragmaHandler pragma_handler = default_pragma_handler,
                 ExtensionHandler ext_handler = default_extension_handler,
                 size_t max_depth = 1024);

    /// Extracts the next preprocessed token from the stream.
    Token preprocess();

    /// Registers the given macro in the preprocessor (replaces it if it already exists).
    void register_macro(const std::string& name, const Macro& macro) { macros_[name] = macro; }
    /// Determines if the given macro is registered.
    bool is_registered(const std::string& name) const { return macros_.find(name) != macros_.end(); }

    /// Adds the __FILE__ macro to the preprocessor.
    void register_file_macro();
    /// Adds the __LINE__ macro to the preprocessor.
    void register_line_macro();
    /// Adds the __VERSION__ macro to the preprocessor.
    void register_version_macro(int ver = 440);

    /// Adds the GL_core_profile macro to the preprocessor.
    void register_core_macro();
    /// Adds the GL_compatibility_profile macro to the preprocessor.
    void register_compatibility_macro();
    /// Adds the GL_es_profile macro to the preprocessor.
    void register_es_macro();

    /// Add all builtin macros to the preprocessor.
    void register_builtin_macros() {
        register_file_macro();
        register_line_macro();
        register_version_macro(440);
        register_core_macro();
    }

    /// Returns the number of errors generated during preprocessing.
    size_t error_count() const { return err_count_; }
    /// Returns the number of warnings generated during preprocessing.
    size_t warn_count() const { return warn_count_; }

    /// Default version directive handler (does nothing).
    static void default_version_handler(int, Profile) {}
    /// Default pragma directive handler (does nothing).
    static void default_pragma_handler(const std::vector<Token>&) {}
    /// Default extension directive handler (does nothing).
    static void default_extension_handler(const std::string&, ExtBehavior) {}

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
    void parse_extension();
    void parse_line();
    void parse_error();

    void apply(const Macro&, const std::vector<Macro::Arg>&, std::vector<Token>&);
    bool concat(const Token&, const Token&, Token&);
    bool expand(bool);

    bool evaluate_condition();
    ExprValue evaluate_primary();
    ExprValue evaluate_binary(ExprValue, int);

    std::ostream& error();
    std::ostream& warn();

    size_t err_count_, warn_count_;

    Lexer& lexer_;
    Logger& logger_;

    VersionHandler   version_handler_;
    PragmaHandler    pragma_handler_;
    ExtensionHandler ext_handler_;

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
