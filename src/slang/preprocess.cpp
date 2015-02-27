#include <cassert>

#include "slang/preprocess.h"

namespace slang {

Preprocessor::Preprocessor(Lexer& lexer, Logger& logger, int max_depth)
    : lexer_(lexer), logger_(logger), max_depth_(max_depth)
{
    lex();
}

Token Preprocessor::preprocess() {
    // Return tokens resulting from a macro expansion if present
    if (buffer_.size()) {
        Token tok = buffer_.front();
        buffer_.pop_front();
        return tok;
    }

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
            expanded_.clear();
        }
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

void Preprocessor::expand_macro(const Macro& macro, std::list<Token>& buffer, int depth) {
    if (depth > max_depth_) {
        error() << "Maximum macro expansion depth reached\n";
        return;
    }

    std::string name = buffer.front().ident();

    // Do not recursively expand macros that have already been expanded
    if (expanded_[name]) return;

    expanded_[name] = true;
    buffer.pop_front();

    // Read macro arguments (may not be present)
    std::vector<Macro::Arg> args;
    if (buffer.size() > 0) {
        if (buffer.front().isa(Token::TOK_LPAREN)) {
            args = parse_macro_args(name, macro, buffer);
        } else {
            assert(0 && "Buffer should only contain macro name and arguments");
        }

        buffer.clear();
    }

    if (args.size() != macro.args().size()) {
        error() << "Invalid number of arguments given to macro \'" << name
                << "\' (found " << args.size() << ", expected " << macro.num_args()
                << ")\n";
        args.resize(macro.num_args());
    }

    // Expand the macro into the buffer
    for (auto tok: macro.rule()) {
        if (tok.isa(Token::TOK_IDENT)) {
            auto arg_index = macro.args().find(tok.ident());
            if (arg_index == macro.args().end()) {
                // This is not a macro argument
                buffer.push_back(tok);
            } else {
                // This is a macro argument, has to be replaced
                buffer.insert(buffer.end(), args[arg_index->second].begin(), args[arg_index->second].end());
            }
        }
    }

    // Expand macros present in the expanded string
    auto first = buffer.begin();
    while (first != buffer.end()) {
        if (first->isa(Token::TOK_IDENT)) {
            auto next_macro = macros_.find(lookup_.ident());
            if (next_macro != macros_.end()) {
                auto last = (next_macro->second.has_args()) ? expansion_site(buffer, first) : std::next(first);

                std::list<Token> exp_buf;
                exp_buf.splice(exp_buf.begin(), buffer, first, last);
                expand_macro(next_macro->second, exp_buf, depth + 1);

                first = exp_buf.begin();
                buffer.splice(last, exp_buf, first, exp_buf.end());
            }
        } else {
            first++;
        }
    }
}

std::vector<Macro::Arg> Preprocessor::parse_macro_args(const std::string& name, const Macro& macro, const std::list<Token>& buffer) {
    auto it = buffer.begin();
    assert(it->isa(Token::TOK_LPAREN));

    it++;

    // Read argument values
    std::vector<Macro::Arg> args;
    if (!it->isa(Token::TOK_RPAREN)) {
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

    return args;
}

std::list<Token>::iterator Preprocessor::expansion_site(const std::list<Token>& buffer, std::list<Token>::iterator first) {
    assert(first->isa(Token::TOK_IDENT));
    first++;
    return first;
}

std::ostream& Preprocessor::error() {
    return logger_.error(lookup_.loc().start());
}

std::ostream& Preprocessor::warn() {
    return logger_.warn(lookup_.loc().start());
}

} // namespace slang
