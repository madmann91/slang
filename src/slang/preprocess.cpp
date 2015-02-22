#include <cassert>

#include "slang/preprocess.h"

namespace slang {

Preprocessor::Preprocessor(Lexer& lexer, Logger& logger)
    : lexer_(lexer), logger_(logger), buf_index_(0)
{
    lex();
}

Token Preprocessor::preprocess() {
    // Return tokens resulting from a macro expansion if present
    if (buf_index_ < buffer_.size()) {
        return buffer_[buf_index_++];
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
        if (macros_.find(lookup_.ident()) != macros_.end()) {

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
    std::vector<std::string> args;
    if (lookup_.isa(Token::TOK_LPAREN)) {
        eat(Token::TOK_LPAREN);

        while (lookup_.isa(Token::TOK_IDENT)) {
            args.push_back(lookup_.ident());
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

std::ostream& Preprocessor::error() {
    return logger_.error(lookup_.loc().start());
}

std::ostream& Preprocessor::warn() {
    return logger_.warn(lookup_.loc().start());
}

} // namespace slang
