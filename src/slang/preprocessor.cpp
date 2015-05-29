#include <cassert>
#include <sstream>
#include <cmath>

#include "slang/preprocessor.h"

namespace slang {

Preprocessor::Preprocessor(Lexer& lexer, Logger& logger,
                           VersionHandler version_handler,
                           PragmaHandler pragma_handler,
                           ExtensionHandler ext_handler,
                           size_t max_depth)
    : err_count_(0), warn_count_(0), lexer_(lexer), logger_(logger)
    , version_handler_(version_handler), pragma_handler_(pragma_handler)
    , ext_handler_(ext_handler), max_depth_(max_depth), first_(true)
{
    next();
}

Token Preprocessor::preprocess() {
    while (!lookup_.isa(Token::TOK_EOF)) {
        // Parse preprocessor directives
        if (ctx_stack_.empty()) {
            while (lookup_.isa(Token::TOK_SHARP)) {
                if (lookup_.new_line()) {
                    // This is a preprocessor directive
                    parse_directive();
                } else {
                    error() << "\'#\' symbol in the middle of a line\n";
                    break;
                }
            }

            if (lookup_.isa(Token::TOK_EOF)) break;
        }

        // Skip tokens in a disabled #if, #else or #elif branch
        if (state_stack_.size() > 0 && !state_stack_.back().enabled) {
            next();
            continue;
        }

        // Expand macros if needed
        if (lookup_.isa(Token::TOK_IDENT) && expand(true))
            continue;

        break;
    }

    // For a missing #endif directive, emit an error (just once)
    if (!prev_.isa(Token::TOK_EOF) && lookup_.isa(Token::TOK_EOF) && state_stack_.size() > 0) {
        error() << "Missing " << state_stack_.size() << " #endif directive(s)\n";
    }

    first_ = false;

    Token tok = lookup_;
    next();
    return tok;
}

void Preprocessor::register_file_macro() {
    macros_["__FILE__"] = Macro([this] (const std::vector<Macro::Arg>&) {
        Literal lit(lexer_.source_index(), false);
        std::ostringstream os;
        os << lit;
        return std::vector<Token>(1, Token(Location::zero(), os.str(), lit, false));
    });
}

void Preprocessor::register_line_macro() {
    macros_["__LINE__"] = Macro([this] (const std::vector<Macro::Arg>&) {
        Literal lit(lexer_.line_index(), false);
        std::ostringstream os;
        os << lit;
        return std::vector<Token>(1, Token(Location::zero(), os.str(), lit, false));
    });
}

void Preprocessor::register_version_macro(int ver) {
    macros_["__VERSION__"] = Macro([this, ver] (const std::vector<Macro::Arg>&) {
        Literal lit(ver, false);
        std::ostringstream os;
        os << lit;
        return std::vector<Token>(1, Token(Location::zero(), os.str(), lit, false));
    });
}

void Preprocessor::register_core_macro() {
    macros_["GL_core_profile"] = Macro(std::vector<Token>(1,
        Token(Location::zero(), "1", Literal(1, false), false)));
}

void Preprocessor::register_compatibility_macro() {
    macros_["GL_compatibility_profile"] = Macro(std::vector<Token>(1,
        Token(Location::zero(), "1", Literal(1, false), false)));
}

void Preprocessor::register_es_macro() {
    macros_["GL_es_profile"] = Macro(std::vector<Token>(1,
        Token(Location::zero(), "1", Literal(1, false), false)));
}

void Preprocessor::next() {
    if (ctx_stack_.empty()) prev_ = lookup_;

    while (!ctx_stack_.empty() && ctx_stack_.back().cur >= ctx_stack_.back().last) {
        ctx_buffer_.resize(ctx_stack_.back().first);
        expanded_.erase(ctx_stack_.back().macro_name);
        ctx_stack_.pop_back();
    }

    if (ctx_stack_.empty()) {
        lookup_ = lexer_.lex();
    } else {
        // Read token from stacked context
        lookup_ = ctx_buffer_[ctx_stack_.back().cur++];
    }
}

void Preprocessor::eat(Token::Type type) {
    assert(lookup_.isa(type));
    next();
}

void Preprocessor::expect(Token::Type type) {
    if (!lookup_.isa(type))
        error() << "\'" << Token::type_string(type) << "\' expected\n";
    next();
}

void Preprocessor::eat_line(bool report) {
    // Eat every token until we reach a newline
    while (!lookup_.isa(Token::TOK_EOF) && !lookup_.new_line()) {
        if (report) {
            error() << "Additional characters after preprocessor directive\n";
            report = false;
        }
        next();
    }
}

bool Preprocessor::check_newline() {
    if (lookup_.new_line()) {
        error() << "Unexpected end of line in preprocessor directive\n";
        return false;
    }
    return true;
}

void Preprocessor::parse_directive() {
    eat(Token::TOK_SHARP);

    if (lookup_.isa(Token::TOK_IDENT)) {
        if (lookup_.ident() == "pragma") {
            parse_pragma();
        } else if (lookup_.ident() == "if") {
            parse_if();
        } else if (lookup_.ident() == "endif") {
            parse_endif();
        } else if (lookup_.ident() == "else") {
            parse_else();
        } else if (lookup_.ident() == "elif") {
            parse_elif();
        } else if (lookup_.ident() == "ifndef") {
            parse_ifdef_ifndef(true);
        } else if (lookup_.ident() == "ifdef") {
            parse_ifdef_ifndef(false);
        } else if (lookup_.ident() == "define") {
            parse_define();
        } else if (lookup_.ident() == "undef") {
            parse_undef();
        } else if (lookup_.ident() == "version") {
            if (first_) {
                parse_version();
            } else {
                error() << "Version directive must occur before anything else\n";
                eat_line(false);
            }
        } else if (lookup_.ident() == "extension") {
            parse_extension();
        } else if (lookup_.ident() == "line") {
            parse_line();
        } else if (lookup_.ident() == "error") {
            parse_error();
        } else {
            error() << "Unknown preprocessor directive \'" << lookup_.ident() << "\'\n";
        }
    } else {
        error() << "Preprocessor directive name expected\n";
    }

    first_ = false;
}

void Preprocessor::parse_pragma() {
    eat(Token::TOK_IDENT);

    std::vector<Token> line;
    while (!lookup_.isa(Token::TOK_EOF) && !lookup_.new_line()) {
        line.push_back(lookup_);
        next();
    }

    pragma_handler_(line);
}

void Preprocessor::parse_if() {
    eat(Token::TOK_IDENT);
    bool cond = evaluate_condition();

    // Check if current branch was taken
    if (state_stack_.empty() || state_stack_.back().enabled) {
        state_stack_.emplace_back(cond, cond, State::BRANCH_IF);
    } else {
        state_stack_.emplace_back(false, true, State::BRANCH_IF);
    }

    eat_line(true);
}

void Preprocessor::parse_endif() {
    eat(Token::TOK_IDENT);
    if (state_stack_.empty()) {
        error() << "#endif outside of an #if\n";
    } else {
        state_stack_.pop_back();
    }
    eat_line(true);
}

void Preprocessor::parse_else() {
    eat(Token::TOK_IDENT);
    if (state_stack_.empty()) {
        error() << "#else outside of an #if\n";
    } else if (state_stack_.back().branch == State::BRANCH_ELSE) {
        error() << "Only one #else directive allowed inside a condition\n";
        state_stack_.back().enabled = false;
    } else {
        if (state_stack_.back().done) {
            state_stack_.back().enabled = false;
        } else {
            state_stack_.back().enabled = !state_stack_.back().enabled;
            state_stack_.back().done |= state_stack_.back().enabled;
        }
        state_stack_.back().branch = State::BRANCH_ELSE;
    }
    eat_line(true);
}

void Preprocessor::parse_elif() {
    eat(Token::TOK_IDENT);
    bool cond = evaluate_condition();

    if (state_stack_.empty()) {
        error() << "#elif outside of an #if\n";
    } else if (state_stack_.back().branch == State::BRANCH_ELSE) {
        error() << "#elif cannot follow #else\n";
        state_stack_.back().enabled = false;
    } else {
        if (state_stack_.back().enabled || state_stack_.back().done) {
            state_stack_.back().enabled = false;
        } else {
            state_stack_.back().enabled = cond;
            state_stack_.back().done |= cond;
        }
        state_stack_.back().branch = State::BRANCH_ELIF;
    }
    eat_line(true);
}

void Preprocessor::parse_ifdef_ifndef(bool flag) {
    eat(Token::TOK_IDENT);

    if (lookup_.new_line()) {
        error() << "Incomplete #ifdef or #ifndef directive\n";
        state_stack_.emplace_back(false, true, State::BRANCH_IF);
    } else {
        if (!lookup_.isa(Token::TOK_IDENT) || lookup_.new_line()) {
            error() << "Expected identifier after #ifdef or #ifndef\n";
            state_stack_.emplace_back(false, true, State::BRANCH_IF);
        } else {
            // Check if current branch was taken
            if (state_stack_.empty() || state_stack_.back().enabled) {
                bool cond = flag ^ (macros_.find(lookup_.ident()) != macros_.end());
                state_stack_.emplace_back(cond, cond, State::BRANCH_IF);
            } else {
                state_stack_.emplace_back(false, true, State::BRANCH_IF);
            }
        }

        next();
        eat_line(true);
    }
}

void Preprocessor::parse_define() {
    eat(Token::TOK_IDENT);
    if (!check_newline()) return;

    // Read macro name
    std::string macro;
    if (lookup_.isa(Token::TOK_IDENT)) {
        macro = lookup_.ident();
        eat(Token::TOK_IDENT);
    } else {
        error() << "Macro identifier expected\n";
        eat_line(false);
        return;
    }

    // Parse macro arguments
    std::unordered_map<std::string, int> args;
    if (lookup_.isa(Token::TOK_LPAREN) && !lookup_.new_line()) {
        eat(Token::TOK_LPAREN);

        while (lookup_.isa(Token::TOK_IDENT) && !lookup_.new_line()) {
            args.emplace(std::make_pair(lookup_.ident(), args.size()));
            eat(Token::TOK_IDENT);

            if (!check_newline()) return;

            if (!lookup_.isa(Token::TOK_COMMA))
                break;

            eat(Token::TOK_COMMA);
        }

        if (!check_newline()) return;

        expect(Token::TOK_RPAREN);
    }

    // Read macro body
    std::vector<Token> body;
    while (!lookup_.new_line() &&
           !lookup_.isa(Token::TOK_EOF)) {
        body.push_back(lookup_);
        next();
    }

    if (!body.empty()) {
        if (body.front().isa(Token::TOK_SHARPSHARP) || body.back().isa(Token::TOK_SHARPSHARP)) {
            error() << "Concatenation operator \'##\' cannot appear at the beginnning or the end of a macro body\n";
            eat_line(false);
            return;
        }

        Token prev;
        for (size_t i = 0; i < body.size(); i++) {
            if (prev.isa(Token::TOK_SHARPSHARP) && body[i].isa(Token::TOK_SHARPSHARP)) {
                error() << "Cannot have two concatenation operators \'##\' next to each other\n";
                eat_line(false);
                return;
            }
            prev = body[i];
        }
    }

    if (macros_.find(macro) != macros_.end())
        warn() << "Redefinition of macro \'" << macro << "\'\n";

    macros_[macro] = Macro(args, body);
}

void Preprocessor::parse_undef() {
    eat(Token::TOK_IDENT);
    if (!check_newline()) return;

    if (lookup_.isa(Token::TOK_IDENT)) {
        if (macros_.find(lookup_.ident()) == macros_.end())
            warn() << "Unknown macro \'" << lookup_.ident() << "\'\n";
        else
            macros_.erase(lookup_.ident());

        eat(Token::TOK_IDENT);
    } else {
        error() << "Macro identifier expected\n";
        next();
    }
    eat_line(true);
}

void Preprocessor::parse_version() {
    eat(Token::TOK_IDENT);
    if (!check_newline()) return;

    if (lookup_.isa(Token::TOK_LIT) && lookup_.lit().isa(Literal::LIT_INT)) {
        int version = lookup_.lit().as_int();
        if (version > 440) {
            warn() << "GLSL version not supported, defaulting to 440\n";
            version = 440;
        }
        eat(Token::TOK_LIT);

        // Optional profile argument (default is core)
        Profile profile = Profile::PROFILE_CORE;
        if (lookup_.isa(Token::TOK_IDENT) && !lookup_.new_line()) {
            // Profile parameter only allowed for GLSL > 1.50
            if (version >= 150) {
                if (lookup_.ident() == "core") {
                    profile = Profile::PROFILE_CORE;
                } else if (lookup_.ident() == "compatibility") {
                    profile = Profile::PROFILE_COMPAT;
                } else if (lookup_.ident() == "es") {
                    profile = Profile::PROFILE_ES;
                } else {
                    error() << "Invalid profile string\n";
                }

                if (version == 300 && profile != Profile::PROFILE_ES) {
                    error() << "Profile must be 'es' for GLSL version 300\n";
                    profile = Profile::PROFILE_ES;
                }
            } else {
                error() << "Profile argument provided for GLSL version less than 150\n";
            }
            eat(Token::TOK_IDENT);
        } else if (version == 300) {
            error() << "Profile string is mandatory for GLSL version 300\n";
            profile = Profile::PROFILE_ES;
        }

        version_handler_(version, profile);
    } else {
        error() << "Version number expected\n";
        next();
    }

    eat_line(true);
}

void Preprocessor::parse_extension() {
    eat(Token::TOK_IDENT);
    if (!check_newline()) return;

    std::string name;
    if (lookup_.isa(Token::TOK_IDENT)) {
        name = lookup_.ident();
        eat(Token::TOK_IDENT);
    } else {
        error() << "Extension name expected\n";
    }

    if (!check_newline()) return;
    expect(Token::TOK_COLON);
    if (!check_newline()) return;

    // Check that the behavior is one of : require, enable, warn, or disable
    ExtBehavior behavior = ExtBehavior::BEHAVIOR_ENABLE;
    if (lookup_.isa(Token::TOK_IDENT)) {
        if (lookup_.ident() == "require") {
            behavior = ExtBehavior::BEHAVIOR_REQUIRE;
        } else if (lookup_.ident() == "enable") {
            behavior = ExtBehavior::BEHAVIOR_ENABLE;
        } else if (lookup_.ident() == "warn") {
            behavior = ExtBehavior::BEHAVIOR_WARN;
        } else if (lookup_.ident() == "disable") {
            behavior = ExtBehavior::BEHAVIOR_DISABLE;
        } else {
            error() << "Extension behavior must be one of "
                       "\'require\', \'enable\', \'warn\', \'disable\'\n";
        }
        eat(Token::TOK_IDENT);
    } else {
        error() << "Extension behavior expected\n";
        next();
    }

    ext_handler_(name, behavior);

    eat_line(true);
}

void Preprocessor::parse_line() {
    eat(Token::TOK_IDENT);
    if (!check_newline()) return;

    // Expand macros
    while (lookup_.isa(Token::TOK_IDENT) && expand(false)) ;

    int line = lexer_.line_index();
    if (lookup_.isa(Token::TOK_LIT) && lookup_.lit().isa(Literal::LIT_INT)) {
        assert(lookup_.lit().as_int() >= 0);
        line = lookup_.lit().as_int();
        eat(Token::TOK_LIT);
    } else {
        error() << "Line index expected\n";
        eat_line(false);
        return;
    }

    lexer_.set_line_index(lookup_.new_line() ? line : line - 1);

    // Optional source index
    if (!lookup_.new_line()) {
        // Expand macros (again)
        while (lookup_.isa(Token::TOK_IDENT) && expand(false)) ;

        int source = lexer_.source_index();
        if (lookup_.isa(Token::TOK_LIT) && lookup_.lit().isa(Literal::LIT_INT)) {
            assert(lookup_.lit().as_int() >= 0);
            source = lookup_.lit().as_int();
            eat(Token::TOK_LIT);
        } else {
            error() << "Source file index expected\n";
        }
        lexer_.set_source_index(source);
    }

    eat_line(true);
}

void Preprocessor::parse_error() {
    eat(Token::TOK_IDENT);

    std::ostringstream os;
    bool prev = false;
    while (!lookup_.new_line() && !lookup_.isa(Token::TOK_EOF)) {
        if (prev) os << ' ';
        os << lookup_;
        next();
        prev = true;
    }

    error() << "#error directive with message : \'" << os.str() << "\'\n";
}

void Preprocessor::apply(const Macro& macro, const std::vector<Macro::Arg>& args, std::vector<Token>& buffer) {
    bool concat_next = false;
    for (auto& tok : macro.rule()) {

        // May have to replace an argument here
        if (tok.isa(Token::TOK_IDENT)) {
            auto arg_index = macro.args().find(tok.ident());
            if (arg_index != macro.args().end() && arg_index->second < macro.num_args()) {
                // This is a macro argument, has to be replaced
                int first = 0;
                if (concat_next) {
                    concat_next = false;
                    Token next;
                    if (!args[arg_index->second].empty() &&
                        concat(buffer.back(), args[arg_index->second].front(), next)) {
                        buffer.back() = next;
                        first = 1;
                    }
                }
                buffer.insert(buffer.end(), args[arg_index->second].begin() + first, args[arg_index->second].end());
                continue;
            }
        }

        if (tok.isa(Token::TOK_SHARPSHARP)) {
            assert(!concat_next);
            concat_next = !buffer.empty();
            continue;
        }

        if (concat_next) {
            concat_next = false;
            Token next;
            if (concat(buffer.back(), tok, next)) {
                buffer.back() = next;
                continue;
            }
        }

        buffer.push_back(tok);
    }
}

inline Location concat_loc(const Token& a, const Token& b) {
    return Location(a.loc().start(), b.loc().end());
}

class NullBuffer : public std::streambuf
{
public:
    int overflow(int c) { return c; }
};

bool Preprocessor::concat(const Token& a, const Token& b, Token& c) {
    std::stringstream str;
    str << a << b;

    // Redirect errors to null output stream
    NullBuffer null_buffer;
    std::ostream null_stream(&null_buffer);
    Logger logger("", null_stream, null_stream);
    Lexer lex(str, lexer_.keywords(), logger);

    // Get the concatenated token
    Token tok = lex.lex();
    if (tok.isa(Token::TOK_EOF) || !lex.lex().isa(Token::TOK_EOF) || lex.error_count() > 0) {
        error() << "Invalid tokens for concatenation operator\n";
        return false;
    }

    switch (tok.type()) {
        case Token::TOK_IDENT:
            c = Token(concat_loc(a, b), tok.ident(), lexer_.keywords(), a.new_line() | b.new_line());
            break;
        case Token::TOK_LIT:
            c = Token(concat_loc(a, b), tok.str(), tok.lit(), a.new_line() | b.new_line());
            break;
        default:
            c = Token(concat_loc(a, b), tok.type(), a.new_line() | b.new_line());
            break;
    }

    return true;
}

bool Preprocessor::expand(bool lines_allowed) {
    assert(lookup_.isa(Token::TOK_IDENT));

    // Avoid infinite macro expansion
    if (expanded_.find(lookup_.ident()) != expanded_.end())
        return false;

    // Tries to find a macro
    auto macro = macros_.find(lookup_.ident());
    if (macro == macros_.end())
        return false;

    // Mark this macro as expanded
    expanded_.insert(macro->first);

    // Read arguments
    std::vector<Macro::Arg> args;
    if (macro->second.has_args()) {
        eat(Token::TOK_IDENT);

        if (lookup_.isa(Token::TOK_LPAREN) && (lines_allowed || !lookup_.new_line())) {
            eat(Token::TOK_LPAREN);
            args.emplace_back();

            int par_count = 0;
            while (!lookup_.isa(Token::TOK_EOF)) {
                if (lookup_.new_line() && !lines_allowed)
                    break;

                // Allow calling a macro inside a macro argument
                if (lookup_.isa(Token::TOK_RPAREN)) {
                    if (par_count == 0) {
                        break;
                    } else {
                        args.back().emplace_back(lookup_);
                        par_count--;
                    }
                } else if (lookup_.isa(Token::TOK_LPAREN)) {
                    args.back().emplace_back(lookup_);
                    par_count++;
                } else if (lookup_.isa(Token::TOK_COMMA) && par_count == 0) {
                    args.emplace_back();
                } else {
                    args.back().emplace_back(lookup_);
                }
                next();
            }

            if (!lookup_.isa(Token::TOK_RPAREN)) {
                error() << "Missing closing parenthesis in invocation of macro \'" << macro->first << "\'\n";
                return false;
            }

            if (args.size() != macro->second.args().size()) {
                eat(Token::TOK_RPAREN);
                error() << "Invalid number of arguments for macro \'" << macro->first << "\' "
                        << "(got " << args.size() << ", expected " << macro->second.args().size() << ")\n";
                return false;
            }
        } else {
            error() << "Missing arguments in invocation of macro \'" << macro->first << "\'\n";
            return false;
        }
    }

    // Save the lookup token (it is about to be dropped)
    if (ctx_stack_.empty()) prev_ = lookup_;

    // Expand the macro
    if (ctx_stack_.size() < max_depth_) {
        int first = ctx_buffer_.size();
        if (macro->second.is_builtin()) {
            std::vector<Token> tokens = macro->second.builtin(args);
            ctx_buffer_.insert(ctx_buffer_.end(), tokens.begin(), tokens.end());
        } else {
            apply(macro->second, args, ctx_buffer_);
        }
        int last = ctx_buffer_.size();

        ctx_stack_.emplace_back(first, last, macro->first);
        next();
        return true;
    } else {
        error() << "Maximum macro expansion depth reached\n";
        return false;
    }
}

Preprocessor::BinOp::BinOp()
    : type(BINOP_UNKNOWN), pred(-1), rassoc(false)
{}

Preprocessor::BinOp::BinOp(Token tok) {
    switch (tok.type()) {
        case Token::TOK_MUL:    type = BINOP_MUL;    pred = 3;  rassoc = false; break;
        case Token::TOK_DIV:    type = BINOP_DIV;    pred = 3;  rassoc = false; break;
        case Token::TOK_MOD:    type = BINOP_MOD;    pred = 3;  rassoc = false; break;
        case Token::TOK_ADD:    type = BINOP_ADD;    pred = 4;  rassoc = false; break;
        case Token::TOK_SUB:    type = BINOP_SUB;    pred = 4;  rassoc = false; break;
        case Token::TOK_LSHIFT: type = BINOP_LSHIFT; pred = 5;  rassoc = false; break;
        case Token::TOK_RSHIFT: type = BINOP_RSHIFT; pred = 5;  rassoc = false; break;
        case Token::TOK_LT:     type = BINOP_LT;     pred = 6;  rassoc = false; break;
        case Token::TOK_LEQ:    type = BINOP_LE;     pred = 6;  rassoc = false; break;
        case Token::TOK_GT:     type = BINOP_GT;     pred = 6;  rassoc = false; break;
        case Token::TOK_GEQ:    type = BINOP_GE;     pred = 6;  rassoc = false; break;
        case Token::TOK_EQ:     type = BINOP_EQ;     pred = 7;  rassoc = false; break;
        case Token::TOK_NEQ:    type = BINOP_NEQ;    pred = 7;  rassoc = false; break;
        case Token::TOK_AND:    type = BINOP_AND;    pred = 8;  rassoc = false; break;
        case Token::TOK_XOR:    type = BINOP_XOR;    pred = 9;  rassoc = false; break;
        case Token::TOK_OR:     type = BINOP_OR;     pred = 10; rassoc = false; break;
        case Token::TOK_ANDAND: type = BINOP_ANDAND; pred = 11; rassoc = false; break;
        case Token::TOK_OROR:   type = BINOP_OROR;   pred = 12; rassoc = false; break;
        default:
            type = BINOP_UNKNOWN;
            pred = -1;
            rassoc = false;
    }
}

Preprocessor::ExprValue Preprocessor::BinOp::apply(Preprocessor::ExprValue left,
                                                   Preprocessor::ExprValue right) const {
    if (right.error || left.error) return ExprValue();

    switch (type) {
        case BINOP_MUL:    return ExprValue(left.value * right.value);
        case BINOP_DIV:    return ExprValue(left.value / right.value);
        case BINOP_MOD:    return ExprValue(left.value % right.value);
        case BINOP_ADD:    return ExprValue(left.value + right.value);
        case BINOP_SUB:    return ExprValue(left.value - right.value);
        case BINOP_LSHIFT: return ExprValue(left.value << right.value);
        case BINOP_RSHIFT: return ExprValue(left.value >> right.value);
        case BINOP_LT:     return ExprValue(left.value < right.value);
        case BINOP_LE:     return ExprValue(left.value <= right.value);
        case BINOP_GT:     return ExprValue(left.value > right.value);
        case BINOP_GE:     return ExprValue(left.value >= right.value);
        case BINOP_EQ:     return ExprValue(left.value == right.value);
        case BINOP_NEQ:    return ExprValue(left.value != right.value);
        case BINOP_AND:    return ExprValue(left.value & right.value);
        case BINOP_XOR:    return ExprValue(left.value ^ right.value);
        case BINOP_OR:     return ExprValue(left.value | right.value);
        case BINOP_ANDAND: return ExprValue(left.value && right.value);
        case BINOP_OROR:   return ExprValue(left.value || right.value);
        default:           return ExprValue();
    }
}

const int Preprocessor::BinOp::max_pred = 12;

bool Preprocessor::evaluate_condition() {
    ExprValue left = evaluate_primary();
    ExprValue value = evaluate_binary(left, BinOp::max_pred);
    return (value.error) ? false : value.value != 0;
}

Preprocessor::ExprValue Preprocessor::evaluate_primary() {
    if (lookup_.new_line()) {
        error() << "Incomplete preprocessor condition\n";
        return ExprValue();
    }

    // Expand macros
    while (lookup_.isa(Token::TOK_IDENT) && expand(false)) ;

    // Expansion may lead to an empty line
    if (lookup_.new_line()) {
        error() << "Incomplete preprocessor condition\n";
        return ExprValue();
    }

    if (lookup_.isa(Token::TOK_IDENT)) {
        // Non expanded macros or 'defined' operator
        if (lookup_.ident() == "defined") {
            eat(Token::TOK_IDENT);
            expect(Token::TOK_LPAREN);
            ExprValue value;
            if (lookup_.isa(Token::TOK_IDENT)) {
                value = (macros_.find(lookup_.ident()) != macros_.end()) ? ExprValue(1) : ExprValue(0);
                eat(Token::TOK_IDENT);
            } else {
                error() << "Macro identifier expected in operator \'defined\'\n";
                value = ExprValue();
            }
            expect(Token::TOK_RPAREN);
            return value;
        } else {
            std::string ident = lookup_.ident();
            eat(Token::TOK_IDENT);
            warn() << "Undefined macro identifier \'" << ident << "\'\n";
            return ExprValue(0);
        }
    } else if (lookup_.isa(Token::TOK_LIT)) {
        // Literals
        ExprValue value;
        Literal lit = lookup_.lit();

        eat(Token::TOK_LIT);
        switch (lit.type()) {
            case Literal::LIT_INT:  value = ExprValue(lit.as_int());  break;
            case Literal::LIT_UINT: value = ExprValue(lit.as_uint()); break;
            default:
                value = ExprValue();
                error() << "Integer literal expected in preprocessor condition\n";
        }
        return value;
    } else if (lookup_.isa(Token::TOK_ADD)) {
        // Unary plus
        while (lookup_.isa(Token::TOK_ADD)) {
            eat(Token::TOK_ADD);
        }
        return evaluate_primary();
    } else if (lookup_.isa(Token::TOK_SUB)) {
        // Unary minus
        int sign = 1;
        while (lookup_.isa(Token::TOK_SUB)) {
            eat(Token::TOK_SUB);
            sign = -sign;
        }
        return evaluate_primary().apply([sign] (int i) { return i * sign; });
    } else if (lookup_.isa(Token::TOK_NEG)) {
        // Unary bitwise not
        int mask = 0;
        while (lookup_.isa(Token::TOK_NEG)) {
            eat(Token::TOK_NEG);
            mask = ~mask;
        }
        return evaluate_primary().apply([mask] (int i) { return i ^ mask; });
    } else if (lookup_.isa(Token::TOK_NOT)) {
        // Unary logical not
        int times = 0;
        while (lookup_.isa(Token::TOK_NOT)) {
            eat(Token::TOK_NOT);
            times = (times + 1) % 2;
        }
        return evaluate_primary().apply([times] (int i) { int j[2] = {!!i, !i }; return j[times]; });
    } else if (lookup_.isa(Token::TOK_LPAREN)) {
        // Parenthetical grouping
        eat(Token::TOK_LPAREN);
        ExprValue left = evaluate_primary();
        ExprValue value = evaluate_binary(left, BinOp::max_pred);
        expect(Token::TOK_RPAREN);
        return value;
    }

    next();
    error() << "Invalid token in preprocessor condition\n";
    return ExprValue();
}

Preprocessor::ExprValue Preprocessor::evaluate_binary(ExprValue left, int precedence) {
    while (true) {
        if (lookup_.new_line())
            return left;

        BinOp binop(lookup_);
        if (binop.type == BinOp::BINOP_UNKNOWN || binop.pred > precedence)
            return left;

        next();
        ExprValue right = evaluate_primary();

        BinOp next_binop(lookup_);
        while (!lookup_.new_line() && next_binop.type != BinOp::BINOP_UNKNOWN &&
               (next_binop.pred < binop.pred || (next_binop.pred == binop.pred && binop.rassoc))) {
            right = evaluate_binary(right, next_binop.pred);
            next_binop = BinOp(lookup_);
        }

        // Catch division by zero here
        if (binop.type == BinOp::BINOP_DIV && !right.error && right.value == 0) {
            error() << "Division by zero in preprocessor condition\n";
            left = ExprValue();
        } else {
            left = binop.apply(left, right);
        }
    }
}

std::ostream& Preprocessor::error() {
    err_count_++;
    if (ctx_stack_.size()) {
        return logger_.error(prev_.loc().end())
            << "[in expansion of \'" << ctx_stack_.back().macro_name << "\' "
            << lookup_.loc().start() << "] ";
    }
    return logger_.error(prev_.loc().end());
}

std::ostream& Preprocessor::warn() {
    warn_count_++;
    if (ctx_stack_.size()) {
        return logger_.warn(prev_.loc().end())
            << "[in expansion of \'" << ctx_stack_.back().macro_name << "\' "
            << lookup_.loc().start() << "] ";
    }
    return logger_.warn(prev_.loc().end());
}

} // namespace slang