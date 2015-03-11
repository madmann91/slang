#include <cassert>

#include "slang/preprocess.h"

namespace slang {

void Macro::apply(const std::vector<Arg>& args, std::vector<Token>& buffer) const {
    for (auto tok: rule_) {
        if (tok.isa(Token::TOK_IDENT)) {
            auto arg_index = args_.find(tok.ident());
            if (arg_index != args_.end() && arg_index->second < args.size()) {
                // This is a macro argument, has to be replaced
                buffer.insert(buffer.end(), args[arg_index->second].begin(), args[arg_index->second].end());
                continue;
            }
        }

        buffer.push_back(tok);
    }
}

Preprocessor::Preprocessor(Lexer& lexer, Logger& logger, size_t max_depth)
    : lexer_(lexer), logger_(logger), max_depth_(max_depth), version_(110), profile_(PROFILE_CORE)
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
        if (lookup_.isa(Token::TOK_IDENT) && !expanded_[lookup_.ident()]) {
            auto macro = macros_.find(lookup_.ident());
            if (macro != macros_.end()) {
                expanded_[macro->first] = true;
                start_expansion(macro->second);
                continue;
            }
        }

        break;
    }

    // For a missing #endif directive, emit an error (just once)
    if (!prev_.isa(Token::TOK_EOF) && lookup_.isa(Token::TOK_EOF) && state_stack_.size() > 0) {
        error() << "Missing " << state_stack_.size() << " #endif directive(s)\n";
    }

    Token tok = lookup_;
    next();
    return tok;
}

void Preprocessor::next() {
    prev_ = lookup_;

    while (!ctx_stack_.empty() && ctx_stack_.back().cur >= ctx_stack_.back().last) {
        ctx_buffer_.resize(ctx_stack_.back().first);
        expanded_[ctx_stack_.back().macro_name] = false;
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
        error() << "\'" << type << "\' expected\n";
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
            parse_version();
        } else {
            error() << "Unknown preprocessor directive \'" << lookup_.ident() << "\'\n";
        }
    } else {
        error() << "Preprocessor directive name expected\n";
    }
}

void Preprocessor::parse_pragma() {
    assert(0 && "Not implemented");
}

void Preprocessor::parse_if() {
    eat(Token::TOK_IDENT);
    // Check if current branch was taken
    if (state_stack_.empty() || state_stack_.back().enabled) {
        bool cond = evaluate_condition();
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
    if (state_stack_.empty()) {
        error() << "#elif outside of an #if\n";
    } else if (state_stack_.back().branch == State::BRANCH_ELSE) {
        error() << "#elif cannot follow #else\n";
        state_stack_.back().enabled = false;
    } else {
        if (state_stack_.back().enabled || state_stack_.back().done) {
            state_stack_.back().enabled = false;
        } else {
            bool enabled = evaluate_condition();
            state_stack_.back().enabled = enabled;
            state_stack_.back().done |= enabled;
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
    }

    // Parse macro arguments
    std::unordered_map<std::string, size_t> args;
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

    if (macros_.find(macro) != macros_.end())
        warn() << "Redefinition of macro \'" << macro << "\'\n";

    macros_[macro] = Macro(args, body);
}

void Preprocessor::parse_undef() {
    eat(Token::TOK_IDENT);
    if (lookup_.isa(Token::TOK_IDENT) && !lookup_.new_line()) {
        if (macros_.find(lookup_.ident()) == macros_.end())
            warn() << "Unknown macro \'" << lookup_.ident() << "\'\n";
        else
            macros_.erase(lookup_.ident());

        eat(Token::TOK_IDENT);
    } else {
        error() << "Macro identifier expected\n";
    }
}

void Preprocessor::parse_version() {
    eat(Token::TOK_IDENT);
    if (lookup_.isa(Token::TOK_LIT) && lookup_.lit().isa(Literal::LIT_INT) && !lookup_.new_line()) {
        version_ = lookup_.lit().as_int();
        if (version_ > 440) {
            warn() << "GLSL version not supported, defaulting to 440\n";
            version_ = 440;
        }

        eat(Token::TOK_LIT);
        if (lookup_.isa(Token::TOK_IDENT) && !lookup_.new_line()) {
            // Profile parameter only allowed for GLSL > 1.50
            if (version_ >= 150) {
                if (lookup_.ident() == "core") {
                    profile_ = PROFILE_CORE;
                } else if (lookup_.ident() == "compatibility") {
                    profile_ = PROFILE_COMPAT;
                } else if (lookup_.ident() == "es") {
                    profile_ = PROFILE_ES;
                } else {
                    error() << "Invalid profile string\n";
                }

                if (version_ == 300 && profile_ != PROFILE_ES) {
                    error() << "Profile must be 'es' for GLSL version 300\n";
                }
            } else {
                error() << "Profile argument provided for GLSL version less than 150\n";
            }
            eat(Token::TOK_IDENT);
        } else if (version_ == 300) {
            error() << "Profile string is mandatory for GLSL version 300\n";
            profile_ = PROFILE_ES;
        } else {
            profile_ = PROFILE_CORE;
        }
    } else {
        error() << "Version number expected\n";
    }
}

void Preprocessor::start_expansion(const Macro& macro) {
    std::string macro_name = lookup_.ident();

    // Read arguments
    std::vector<Macro::Arg> args;
    if (macro.has_args()) {
        eat(Token::TOK_IDENT);
        if (lookup_.isa(Token::TOK_LPAREN)) {
            eat(Token::TOK_LPAREN);
            args.emplace_back();
            while (!lookup_.isa(Token::TOK_EOF) && !lookup_.isa(Token::TOK_RPAREN)) {
                if (lookup_.isa(Token::TOK_COMMA)) {
                    args.emplace_back();
                } else {
                    args.back().emplace_back(lookup_);
                }
                next();
            }

            if (!lookup_.isa(Token::TOK_RPAREN)) {
                error() << "Missing closing parenthesis in invocation of macro \'" << macro_name << "\'\n";
                return;
            }

            if (args.size() != macro.args().size()) {
                eat(Token::TOK_RPAREN);
                error() << "Invalid number of arguments for macro \'" << macro_name << "\' "
                        << "(got " << args.size() << ", expected " << macro.args().size() << ")\n";
                return;
            }
        } else {
            error() << "Missing arguments in invocation of macro \'" << macro_name << "\'\n";
            return;
        }
    }

    // Expand the macro
    if (ctx_stack_.size() < max_depth_) {
        int first = ctx_buffer_.size();
        macro.apply(args, ctx_buffer_);
        int last = ctx_buffer_.size();

        ctx_stack_.emplace_back(first, last, macro_name);
        next();
    } else {
        error() << "Maximum macro expansion depth reached\n";
    }
}

bool Preprocessor::evaluate_condition() {
    return true;
}

std::ostream& Preprocessor::error() {
    return logger_.error(prev_.loc().end());
}

std::ostream& Preprocessor::warn() {
    return logger_.warn(prev_.loc().end());
}

} // namespace slang
