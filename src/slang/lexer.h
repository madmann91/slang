#ifndef SLANG_LEXER_H
#define SLANG_LEXER_H

#include <iostream>
#include <string>

#include "slang/keyword.h"
#include "slang/token.h"
#include "slang/logger.h"

namespace slang {

/// The lexer : produces a stream of tokens from a stream of characters.
class Lexer {
public:
    /// Builds a lexer with the given stream and set of keywords, errors will be redirected to the logger object.
    Lexer(std::istream& stream, const Keywords& keywords, Logger& logger, int line = 1, int source = 0);

    /// Returns the next token in the stream and updates the current position.
    Token lex();

    /// Returns the number of errors generated during lexing.
    int error_count() const { return err_count_; }

    /// Sets the current line index.
    void set_line_index(int line);
    /// Returns the current line index.
    int line_index() const;

    /// Sets the current line index.
    void set_source_index(int source);
    /// Returns the current line index.
    int source_index() const;

private:
    void next();

    Literal parse_int(bool);
    Literal parse_float();
    Literal parse_literal();
    std::string parse_ident();

    bool eat_suffix();
    
    Token make_literal(const Literal&);
    Token make_ident(const std::string&);
    Token make_token(Token::Type);

    std::ostream& error();

    int err_count_;

    int c_;
    bool new_line_;
    int source_;

    Position cur_, prev_;
    std::istream& stream_;
    const Keywords& keys_;
    Logger& logger_;
};

} // namespace slang

#endif // SLANG_LEXER_H
