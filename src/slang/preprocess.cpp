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

Preprocessor::Preprocessor(std::function<Token()> input, Logger& logger, size_t max_depth)
    : input_(input), logger_(logger), max_depth_(max_depth)
{
    next();
}

Token Preprocessor::preprocess() {
    while (!lookup_.isa(Token::TOK_EOF)) {
        // Parse preprocessor directives
        if (stack_.empty()) {
            while (lookup_.isa(Token::TOK_SHARP)) {
                if (lookup_.new_line()) {
                    // This is a preprocessor directive
                    parse_directive();
                } else {
                    error() << "\'#\' symbol in the middle of a line\n";
                    break;
                }
            }
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

    Token tok = lookup_;
    next();
    return tok;
}

void Preprocessor::next() {
    while (!stack_.empty() && stack_.back().cur >= stack_.back().last) {
        buffer_.resize(stack_.back().first);
        expanded_[stack_.back().macro_name] = false;
        stack_.pop_back();
    }

    if (stack_.empty()) {
        lookup_ = input_();
    } else {
        // Read token from stacked context
        lookup_ = buffer_[stack_.back().cur++];
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
        } else if (lookup_.ident() == "ifndef") {
            parse_ifndef();
        } else if (lookup_.ident() == "ifdef") {
            parse_ifdef();
        } else if (lookup_.ident() == "define") {
            parse_define();
        }
    } else {
        error() << "Preprocessor directive name expected\n";
    }
}

void Preprocessor::parse_pragma() {
    assert(0 && "Not implemented");
}

void Preprocessor::parse_if() {
    assert(0 && "Not implemented");
}

void Preprocessor::parse_endif() {
    assert(0 && "Not implemented");
}

void Preprocessor::parse_else() {
    assert(0 && "Not implemented");
}

void Preprocessor::parse_ifndef() {
    assert(0 && "Not implemented");
}

void Preprocessor::parse_ifdef() {
    assert(0 && "Not implemented");
}

void Preprocessor::parse_define() {
    // DefineMacro ::= define ident ( ( ident (,ident)* ) )? (ident)*
    eat(Token::TOK_IDENT);

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
    if (lookup_.isa(Token::TOK_LPAREN)) {
        eat(Token::TOK_LPAREN);

        while (lookup_.isa(Token::TOK_IDENT)) {
            args.emplace(std::make_pair(lookup_.ident(), args.size()));
            eat(Token::TOK_IDENT);

            if (!lookup_.isa(Token::TOK_COMMA))
                break;

            eat(Token::TOK_COMMA);
        }

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
    if (stack_.size() < max_depth_) {
        int first = buffer_.size();
        macro.apply(args, buffer_);
        int last = buffer_.size();

        stack_.emplace_back(first, last, macro_name);
        next();
    } else {
        error() << "Maximum macro expansion depth reached\n";
    }
}

std::ostream& Preprocessor::error() {
    return logger_.error(lookup_.loc().start());
}

std::ostream& Preprocessor::warn() {
    return logger_.warn(lookup_.loc().start());
}

} // namespace slang
