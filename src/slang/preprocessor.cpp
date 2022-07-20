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

Token Preprocessor::next_token() {
    while (!lookup_.isa(Token::END)) {
        // Parse preprocessor directives
        if (ctx_stack_.empty()) {
            while (lookup_.isa(Token::SHARP)) {
                if (lookup_.new_line()) {
                    // This is a preprocessor directive
                    parse_directive();
                    first_ = false;
                } else {
                    error() << "\'#\' symbol in the middle of a line\n";
                    break;
                }
            }

            if (lookup_.isa(Token::END)) break;
        }

        // Skip tokens in a disabled #if, #else or #elif branch
        if (state_stack_.size() > 0 && !state_stack_.back().enabled) {
            next();
            continue;
        }

        // Expand macros if needed
        if (lookup_.isa(Token::IDENT) && expand(true))
            continue;

        break;
    }

    // For a missing #endif directive, emit an error (just once)
    if (!prev_.isa(Token::END) && lookup_.isa(Token::END) && state_stack_.size() > 0) {
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
        return std::vector<Token>(1, Token(Location::zero(), os.str(), lit, false, false));
    });
}

void Preprocessor::register_line_macro() {
    macros_["__LINE__"] = Macro([this] (const std::vector<Macro::Arg>&) {
        Literal lit(lexer_.line_index(), false);
        std::ostringstream os;
        os << lit;
        return std::vector<Token>(1, Token(Location::zero(), os.str(), lit, false, false));
    });
}

void Preprocessor::register_version_macro(int ver) {
    macros_["__VERSION__"] = Macro([this, ver] (const std::vector<Macro::Arg>&) {
        Literal lit(ver, false);
        std::ostringstream os;
        os << lit;
        return std::vector<Token>(1, Token(Location::zero(), os.str(), lit, false, false));
    });
}

void Preprocessor::register_core_macro() {
    macros_["GL_core_profile"] = Macro(std::vector<Token>(1,
        Token(Location::zero(), "1", Literal(1, false), false, false)));
}

void Preprocessor::register_compatibility_macro() {
    macros_["GL_compatibility_profile"] = Macro(std::vector<Token>(1,
        Token(Location::zero(), "1", Literal(1, false), false, false)));
}

void Preprocessor::register_es_macro() {
    macros_["GL_es_profile"] = Macro(std::vector<Token>(1,
        Token(Location::zero(), "1", Literal(1, false), false, false)));
}

void Preprocessor::next() {
    if (ctx_stack_.empty()) prev_ = lookup_;

    while (!ctx_stack_.empty() && ctx_stack_.back().cur >= ctx_stack_.back().last) {
        ctx_buffer_.resize(ctx_stack_.back().first);
        expanded_.erase(ctx_stack_.back().macro_name);
        ctx_stack_.pop_back();
    }

    if (ctx_stack_.empty()) {
        lookup_ = lexer_.next_token();
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
    while (!lookup_.isa(Token::END) && !lookup_.new_line()) {
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
    eat(Token::SHARP);

    if (lookup_.isa(Token::IDENT)) {
        if (lookup_.ident() == "if") {
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
        } else {
            // The following directives should be ignored if they are in a disabled branch
            if (state_stack_.size() > 0 && !state_stack_.back().enabled)
                return;

            if (lookup_.ident() == "pragma") {
                parse_pragma();
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
        }
    } else {
        error() << "Preprocessor directive name expected\n";
    }
}

void Preprocessor::parse_pragma() {
    eat(Token::IDENT);

    std::vector<Token> line;
    while (!lookup_.isa(Token::END) && !lookup_.new_line()) {
        line.push_back(lookup_);
        next();
    }

    pragma_handler_(line);
}

void Preprocessor::parse_if() {
    eat(Token::IDENT);
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
    eat(Token::IDENT);
    if (state_stack_.empty()) {
        error() << "#endif outside of an #if\n";
    } else {
        state_stack_.pop_back();
    }
    eat_line(true);
}

void Preprocessor::parse_else() {
    eat(Token::IDENT);
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
    eat(Token::IDENT);
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
    eat(Token::IDENT);

    if (lookup_.new_line()) {
        error() << "Incomplete #ifdef or #ifndef directive\n";
        state_stack_.emplace_back(false, true, State::BRANCH_IF);
    } else {
        if (!lookup_.isa(Token::IDENT) || lookup_.new_line()) {
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
    eat(Token::IDENT);
    if (!check_newline()) return;

    // Read macro name
    std::string macro;
    if (lookup_.isa(Token::IDENT)) {
        macro = lookup_.ident();
        eat(Token::IDENT);
    } else {
        error() << "Macro identifier expected\n";
        eat_line(false);
        return;
    }

    // Parse macro arguments
    std::unordered_map<std::string, size_t> args;
    if (lookup_.isa(Token::LPAREN) && !lookup_.new_line() && !lookup_.spaces()) {
        eat(Token::LPAREN);

        while (lookup_.isa(Token::IDENT) && !lookup_.new_line()) {
            args.emplace(std::make_pair(lookup_.ident(), args.size()));
            eat(Token::IDENT);

            if (!check_newline()) return;

            if (!lookup_.isa(Token::COMMA))
                break;

            eat(Token::COMMA);
        }

        if (!check_newline()) return;
        expect(Token::RPAREN);
    }

    // Read macro body
    std::vector<Token> body;
    while (!lookup_.new_line() && !lookup_.isa(Token::END)) {
        body.push_back(lookup_);
        next();
    }

    if (!body.empty()) {
        if (body.front().isa(Token::SHARPSHARP) || body.back().isa(Token::SHARPSHARP)) {
            error() << "Concatenation operator \'##\' cannot appear at the beginnning or the end of a macro body\n";
            eat_line(false);
            return;
        }

        Token prev;
        for (size_t i = 0; i < body.size(); i++) {
            if (prev.isa(Token::SHARPSHARP) && body[i].isa(Token::SHARPSHARP)) {
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
    eat(Token::IDENT);
    if (!check_newline()) return;

    if (lookup_.isa(Token::IDENT)) {
        if (macros_.find(lookup_.ident()) == macros_.end())
            warn() << "Unknown macro \'" << lookup_.ident() << "\'\n";
        else
            macros_.erase(lookup_.ident());

        eat(Token::IDENT);
    } else {
        error() << "Macro identifier expected\n";
        next();
    }
    eat_line(true);
}

void Preprocessor::parse_version() {
    eat(Token::IDENT);
    if (!check_newline()) return;

    if (lookup_.isa(Token::LIT) && lookup_.lit().isa(Literal::INT)) {
        int version = lookup_.lit().as_int();
        if (version > 440) {
            warn() << "GLSL version not supported, defaulting to 440\n";
            version = 440;
        }
        eat(Token::LIT);

        // Optional profile argument (default is core)
        Profile profile = Profile::PROFILE_CORE;
        if (lookup_.isa(Token::IDENT) && !lookup_.new_line()) {
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
            eat(Token::IDENT);
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
    eat(Token::IDENT);
    if (!check_newline()) return;

    std::string name;
    if (lookup_.isa(Token::IDENT)) {
        name = lookup_.ident();
        eat(Token::IDENT);
    } else {
        error() << "Extension name expected\n";
    }

    if (!check_newline()) return;
    expect(Token::COLON);
    if (!check_newline()) return;

    // Check that the behavior is one of : require, enable, warn, or disable
    ExtBehavior behavior = ExtBehavior::BEHAVIOR_ENABLE;
    if (lookup_.isa(Token::IDENT)) {
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
        eat(Token::IDENT);
    } else {
        error() << "Extension behavior expected\n";
        next();
    }

    ext_handler_(name, behavior);

    eat_line(true);
}

void Preprocessor::parse_line() {
    eat(Token::IDENT);
    if (!check_newline()) return;

    // Expand macros
    while (lookup_.isa(Token::IDENT) && expand(false)) ;

    int line = lexer_.line_index();
    if (lookup_.isa(Token::LIT) && lookup_.lit().isa(Literal::INT)) {
        assert(lookup_.lit().as_int() >= 0);
        line = lookup_.lit().as_int();
        eat(Token::LIT);
    } else {
        error() << "Line index expected\n";
        eat_line(false);
        return;
    }

    lexer_.set_line_index(lookup_.new_line() ? line : line - 1);

    // Optional source index
    if (!lookup_.new_line()) {
        // Expand macros (again)
        while (lookup_.isa(Token::IDENT) && expand(false)) ;

        int source = lexer_.source_index();
        if (lookup_.isa(Token::LIT) && lookup_.lit().isa(Literal::INT)) {
            assert(lookup_.lit().as_int() >= 0);
            source = lookup_.lit().as_int();
            eat(Token::LIT);
        } else {
            error() << "Source file index expected\n";
        }
        lexer_.set_source_index(source);
    }

    eat_line(true);
}

void Preprocessor::parse_error() {
    eat(Token::IDENT);

    std::ostringstream os;
    bool prev = false;
    while (!lookup_.new_line() && !lookup_.isa(Token::END)) {
        if (prev) os << ' ';
        os << lookup_;
        next();
        prev = true;
    }

    error() << "#error directive with message : \'" << os.str() << "\'\n";
}

void Preprocessor::apply_arguments(const Token& tok,
                                   const Macro& macro,
                                   const std::vector<Macro::Arg>& args,
                                   std::vector<Token>& buffer,
                                   bool do_concat = false) {
    Token c;

    // Concatenation of invalid tokens will just
    // append the tokens in the buffer without modification

    if (tok.isa(Token::IDENT)) {
        auto arg_index = macro.args().find(tok.ident());
        if (arg_index != macro.args().end() && arg_index->second < macro.num_args()) {
            const std::vector<Token>& arg = args[arg_index->second];
            int first = 0;
            if (do_concat && !arg.empty() &&
                concat(buffer.back(), arg.front(), c)) {
                first = 1;
                buffer.back() = c;
            }
            buffer.insert(buffer.end(), arg.begin() + first, arg.end());
            return;
        }
    }

    if (do_concat && concat(buffer.back(), tok, c))
        buffer.back() = c;
    else
        buffer.push_back(tok);
}

void Preprocessor::apply(const Macro& macro, const std::vector<Macro::Arg>& args, std::vector<Token>& buffer) {
    const std::vector<Token>& rule = macro.rule();

    for (size_t i = 0; i < rule.size(); i++) {
        // Handle concatenation operator
        if (i + 2 < rule.size() &&
            rule[i + 1].isa(Token::SHARPSHARP)) {
            size_t pos = buffer.size();
            apply_arguments(rule[i], macro, args, buffer);
            apply_arguments(rule[i + 2], macro, args, buffer, pos != buffer.size());
            i += 2;
            continue;
        }

        assert(!rule[i].isa(Token::SHARPSHARP));
        apply_arguments(rule[i], macro, args, buffer);
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
    Logger logger("", false, null_stream, null_stream);
    Lexer lex(str, lexer_.keywords(), logger);

    // Get the concatenated token
    Token tok = lex.next_token();
    if (tok.isa(Token::END) || !lex.next_token().isa(Token::END) || lex.error_count() > 0) {
        error() << "Invalid tokens for concatenation operator\n";
        return false;
    }

    auto new_line = a.new_line() | b.new_line();
    auto spaces   = a.spaces()   | b.spaces();

    switch (tok.type()) {
        case Token::IDENT:
            c = Token(concat_loc(a, b), tok.ident(), lexer_.keywords(), new_line, spaces);
            break;
        case Token::LIT:
            c = Token(concat_loc(a, b), tok.str(), tok.lit(), new_line, spaces);
            break;
        default:
            c = Token(concat_loc(a, b), tok.type(), new_line, spaces);
            break;
    }

    return true;
}

bool Preprocessor::expand(bool lines_allowed) {
    assert(lookup_.isa(Token::IDENT));

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
        eat(Token::IDENT);

        if (lookup_.isa(Token::LPAREN) && (lines_allowed || !lookup_.new_line())) {
            eat(Token::LPAREN);
            args.emplace_back();

            int par_count = 0;
            while (!lookup_.isa(Token::END)) {
                if (lookup_.new_line() && !lines_allowed)
                    break;

                // Allow calling a macro inside a macro argument
                if (lookup_.isa(Token::RPAREN)) {
                    if (par_count == 0) {
                        break;
                    } else {
                        args.back().emplace_back(lookup_);
                        par_count--;
                    }
                } else if (lookup_.isa(Token::LPAREN)) {
                    args.back().emplace_back(lookup_);
                    par_count++;
                } else if (lookup_.isa(Token::COMMA) && par_count == 0) {
                    args.emplace_back();
                } else {
                    args.back().emplace_back(lookup_);
                }
                next();
            }

            if (!lookup_.isa(Token::RPAREN)) {
                error() << "Missing closing parenthesis in invocation of macro \'" << macro->first << "\'\n";
                return false;
            }

            if (args.size() != macro->second.args().size()) {
                eat(Token::RPAREN);
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
        case Token::MUL:    type = BINOP_MUL;    pred = 3;  rassoc = false; break;
        case Token::DIV:    type = BINOP_DIV;    pred = 3;  rassoc = false; break;
        case Token::MOD:    type = BINOP_MOD;    pred = 3;  rassoc = false; break;
        case Token::ADD:    type = BINOP_ADD;    pred = 4;  rassoc = false; break;
        case Token::SUB:    type = BINOP_SUB;    pred = 4;  rassoc = false; break;
        case Token::LSHIFT: type = BINOP_LSHIFT; pred = 5;  rassoc = false; break;
        case Token::RSHIFT: type = BINOP_RSHIFT; pred = 5;  rassoc = false; break;
        case Token::LT:     type = BINOP_LT;     pred = 6;  rassoc = false; break;
        case Token::LEQ:    type = BINOP_LE;     pred = 6;  rassoc = false; break;
        case Token::GT:     type = BINOP_GT;     pred = 6;  rassoc = false; break;
        case Token::GEQ:    type = BINOP_GE;     pred = 6;  rassoc = false; break;
        case Token::EQ:     type = BINOP_EQ;     pred = 7;  rassoc = false; break;
        case Token::NEQ:    type = BINOP_NEQ;    pred = 7;  rassoc = false; break;
        case Token::AND:    type = BINOP_AND;    pred = 8;  rassoc = false; break;
        case Token::XOR:    type = BINOP_XOR;    pred = 9;  rassoc = false; break;
        case Token::OR:     type = BINOP_OR;     pred = 10; rassoc = false; break;
        case Token::ANDAND: type = BINOP_ANDAND; pred = 11; rassoc = false; break;
        case Token::OROR:   type = BINOP_OROR;   pred = 12; rassoc = false; break;
        default:
            type = BINOP_UNKNOWN;
            pred = -1;
            rassoc = false;
    }
}

Preprocessor::ExprValue Preprocessor::BinOp::apply(Preprocessor::ExprValue left,
                                                   Preprocessor::ExprValue right) const {
    if (type == BINOP_ANDAND) {
        if (!left.value) return ExprValue(0);
        return right.error ? ExprValue() : (right.value ? 1 : 0);
    }

    if (type == BINOP_OROR) {
        if (left.value) return ExprValue(1);
        return right.error ? ExprValue() : (right.value ? 1 : 0);
    }

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
        default:           return ExprValue();
    }
}

bool Preprocessor::BinOp::is_lazy(Preprocessor::ExprValue left) const {
    return (type == BINOP_ANDAND && left.value == 0) ||
           (type == BINOP_OROR   && left.value != 0);
}

const int Preprocessor::BinOp::max_pred = 12;

bool Preprocessor::evaluate_condition() {
    ExprValue left = evaluate_primary(false);
    ExprValue value = evaluate_binary(left, BinOp::max_pred, false);
    return (value.error) ? false : value.value != 0;
}

Preprocessor::ExprValue Preprocessor::evaluate_primary(bool lazy) {
    if (lookup_.new_line()) {
        error() << "Incomplete preprocessor condition\n";
        return ExprValue();
    }

    // Expand macros
    while (lookup_.isa(Token::IDENT) && expand(false)) ;

    // Expansion may lead to an empty line
    if (lookup_.new_line()) {
        error() << "Incomplete preprocessor condition\n";
        return ExprValue();
    }

    if (lookup_.isa(Token::IDENT)) {
        // Non expanded macros or 'defined' operator
        if (lookup_.ident() == "defined") {
            eat(Token::IDENT);
            expect(Token::LPAREN);
            ExprValue value;
            if (lookup_.isa(Token::IDENT)) {
                value = (macros_.find(lookup_.ident()) != macros_.end()) ? ExprValue(1) : ExprValue(0);
                eat(Token::IDENT);
            } else {
                error() << "Macro identifier expected in operator \'defined\'\n";
            }
            expect(Token::RPAREN);
            return value;
        } else {
            std::string ident = lookup_.ident();
            eat(Token::IDENT);
            warn() << "Undefined macro identifier \'" << ident << "\'\n";
            return ExprValue(0);
        }
    } else if (lookup_.isa(Token::LIT)) {   
        // Literals
        ExprValue value;
        Literal lit = lookup_.lit();

        eat(Token::LIT);
        switch (lit.type()) {
            case Literal::INT:  value = ExprValue(lit.as_int());  break;
            case Literal::UINT: value = ExprValue(lit.as_uint()); break;
            default:
                error() << "Integer literal expected in preprocessor condition\n";
        }
        return value;
    } else if (lookup_.isa(Token::ADD)) {
        // Unary plus
        while (lookup_.isa(Token::ADD)) {
            eat(Token::ADD);
        }
        return evaluate_primary(lazy);
    } else if (lookup_.isa(Token::SUB)) {
        // Unary minus
        int sign = 1;
        while (lookup_.isa(Token::SUB)) {
            eat(Token::SUB);
            sign = -sign;
        }
        return evaluate_primary(lazy).apply([sign] (int i) { return i * sign; });
    } else if (lookup_.isa(Token::NEG)) {
        // Unary bitwise not
        int mask = 0;
        while (lookup_.isa(Token::NEG)) {
            eat(Token::NEG);
            mask = ~mask;
        }
        return evaluate_primary(lazy).apply([mask] (int i) { return i ^ mask; });
    } else if (lookup_.isa(Token::NOT)) {
        // Unary logical not
        int times = 0;
        while (lookup_.isa(Token::NOT)) {
            eat(Token::NOT);
            times = (times + 1) % 2;
        }
        return evaluate_primary(lazy).apply([times] (int i) { int j[2] = {!!i, !i }; return j[times]; });
    } else if (lookup_.isa(Token::LPAREN)) {
        // Parenthetical grouping
        eat(Token::LPAREN);
        ExprValue left = evaluate_primary(lazy);
        ExprValue value = evaluate_binary(left, BinOp::max_pred, lazy);
        expect(Token::RPAREN);
        return value;
    }

    next();
    error() << "Invalid token in preprocessor condition\n";
    return ExprValue();
}

Preprocessor::ExprValue Preprocessor::evaluate_binary(ExprValue left, int precedence, bool lazy) {
    while (true) {
        if (lookup_.new_line())
            return left;

        BinOp binop(lookup_);
        if (binop.type == BinOp::BINOP_UNKNOWN || binop.pred > precedence)
            return left;

        const bool next_lazy = lazy || binop.is_lazy(left);

        next();
        ExprValue right = evaluate_primary(next_lazy);

        BinOp next_binop(lookup_);
        while (!lookup_.new_line() && next_binop.type != BinOp::BINOP_UNKNOWN &&
               (next_binop.pred < binop.pred || (next_binop.pred == binop.pred && binop.rassoc))) {
            right = evaluate_binary(right, next_binop.pred, next_lazy);
            next_binop = BinOp(lookup_);
        }

        // If the result is not needed, do not evaluate and just do the parsing
        if (lazy) continue;

        // Catch division by zero here
        if ((binop.type == BinOp::BINOP_DIV ||
             binop.type == BinOp::BINOP_MOD) &&
            !right.error && right.value == 0) {
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
