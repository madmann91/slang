#include <cassert>

#include "slang/preprocess.h"

namespace slang {

std::list<Token> Macro::apply(const std::vector<Arg>& args) const {
    std::list<Token> buffer;
    for (auto tok: rule_) {
        if (tok.isa(Token::TOK_IDENT)) {
            auto arg_index = args_.find(tok.ident());
            if (arg_index != args_.end()) {
                // This is a macro argument, has to be replaced
                buffer.insert(buffer.end(), args[arg_index->second].begin(), args[arg_index->second].end());
                continue;
            }
        }

        buffer.push_back(tok);
    }
    return buffer;
}

Preprocessor::Preprocessor(Lexer& lexer, Logger& logger, int max_depth)
    : lexer_(lexer), logger_(logger), max_depth_(max_depth)
{
    lex();
}

Token Preprocessor::preprocess() {
    while (!lookup_.isa(Token::TOK_EOF) && buffer_.empty()) {
        // Parse preprocessor directives
        while (lookup_.isa(Token::TOK_SHARP)) {
            if (lookup_.new_line()) {
                // This is a preprocessor directive
                parse_directive();
            } else {
                error() << "\'#\' symbol in the middle of a line\n";
                break;
            }
        }

        // Expand macros if needed
        if (lookup_.isa(Token::TOK_IDENT)) {
            auto macro = macros_.find(lookup_.ident());
            if (macro != macros_.end()) {
                start_expansion(macro->second);
            } else {
                break;
            }
        } else {
            break;
        }
    }

    // Return tokens resulting from a macro expansion if present
    if (buffer_.size()) {
        Token tok = buffer_.front();
        buffer_.pop_front();
        return tok;
    }

    Token tok = lookup_;
    lex();
    return tok;
}

void Preprocessor::lex() {
    lookup_ = lexer_.lex();
}

void Preprocessor::eat(Token::Type type) {
    assert(lookup_.isa(type));
    lex();
}

void Preprocessor::expect(Token::Type type) {
    if (!lookup_.isa(type))
        error() << "\'" << type << "\' expected\n";
    lex();
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
    std::unordered_map<std::string, int> args;
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
        lex();
    }

    if (macros_.find(macro) != macros_.end())
        warn() << "Redefinition of macro \'" << macro << "\'\n";

    macros_[macro] = Macro(args, body);
}

void Preprocessor::start_expansion(const Macro& macro) {
    std::string macro_name = lookup_.ident();
    eat(Token::TOK_IDENT);

    // Read arguments
    std::vector<Macro::Arg> args;
    if (macro.has_args()) {
        if (lookup_.isa(Token::TOK_LPAREN)) {
            eat(Token::TOK_LPAREN);
            if (!lookup_.isa(Token::TOK_RPAREN)) {
                args.emplace_back();
                while (!lookup_.isa(Token::TOK_EOF) && !lookup_.isa(Token::TOK_RPAREN)) {
                    if (lookup_.isa(Token::TOK_COMMA)) {
                        args.emplace_back();
                    } else {
                        args.back().emplace_back(lookup_);
                    }
                    lex();
                }
            }
            expect(Token::TOK_RPAREN);
        } else {
            error() << "Missing arguments in invocation of macro \'" << macro_name << "\'\n";
        }
    }

    // Expand the macro
    expanded_.clear();
    buffer_ = expand_macro(macro, macro_name, args, 0);
}

std::list<Token> Preprocessor::expand_macro(const Macro& macro,
                                            const std::string& name,
                                            const std::vector<Macro::Arg>& args,
                                            int depth) {
    assert(!expanded_[name]);
    if (depth > max_depth_) {
        error() << "Maximum macro expansion depth reached\n";
        return std::list<Token>();
    }

    if (args.size() != macro.args().size()) {
        error() << "Invalid number of arguments given to macro \'" << name
                << "\' (found " << args.size() << ", expected " << macro.num_args()
                << ")\n";
        return std::list<Token>();
    }

    std::list<Token> buffer = macro.apply(args);
    expanded_[name] = true;

    // Expand macros present in the expanded string
    auto first = buffer.begin();
    while (first != buffer.end()) {
        if (first->isa(Token::TOK_IDENT)) {
            auto next_macro = macros_.find(first->ident());
            if (next_macro != macros_.end() && !expanded_[first->ident()]) {
                std::string next_name = first->ident();
                std::vector<Macro::Arg> next_args;
                auto last = first;

                next_args = macro_args(next_name, next_macro->second, last, buffer);
                auto next_buffer = expand_macro(next_macro->second, next_name, next_args, depth + 1);
                if (next_buffer.empty()) {
                    first = last;
                } else {
                    first = next_buffer.begin();
                    buffer.splice(last, next_buffer, first, next_buffer.end());
                }
                continue;
            }
        }

        first++;
    }

    expanded_[name] = false;
    return buffer;
}

std::vector<Macro::Arg> Preprocessor::macro_args(const std::string& name,
                                                 const Macro& macro,
                                                 std::list<Token>::iterator& first,
                                                 std::list<Token>& buffer) {
    auto it = first;
    assert(it->isa(Token::TOK_IDENT));
    it++;

    if (!macro.has_args()) {
        buffer.erase(first, it);
        first = it;
        return std::vector<Macro::Arg>();
    }

    if (it == buffer.end() || !it->isa(Token::TOK_LPAREN)) {
        error() << "Missing arguments in invocation of macro \'" << name << "\'\n";
        buffer.erase(first, it);
        first = it;
        return std::vector<Macro::Arg>();
    }

    it++;

    // Read argument values
    std::vector<Macro::Arg> args;
    if (it != buffer.end() && !it->isa(Token::TOK_RPAREN)) {
        args.emplace_back();
        while (it != buffer.end() && !it->isa(Token::TOK_RPAREN)) {
            if (it->isa(Token::TOK_COMMA)) {
                args.emplace_back();
            } else {
                args.back().push_back(*it);
            }
            it++;
        }
    }

    // Assert that there is a closing parenthesis
    if (it != buffer.end()) {
        it++;
        assert(it == buffer.end());
    } else {
        error() << "Missing ending parenthesis in invocation of macro \'" << name << "\'\n";
    }

    buffer.erase(first, it);
    first = it;

    return args;
}

std::ostream& Preprocessor::error() {
    return logger_.error(lookup_.loc().start());
}

std::ostream& Preprocessor::warn() {
    return logger_.warn(lookup_.loc().start());
}

} // namespace slang
