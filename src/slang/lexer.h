#ifndef SLANG_LEXER_H
#define SLANG_LEXER_H

#include <iostream>
#include <string>

#include "slang/keyword.h"
#include "slang/token.h"
#include "slang/logger.h"

namespace slang {

/// The lexer : produces a stream of tokens from a stream of characters.
/// Errors are reported through a logger object.
class Lexer {
public:
    Lexer(std::istream& stream, const Keywords& keywords, Logger& logger);

    /// Returns the next token in the stream and goes to the next one.
    Token lex();

    /// Returns the set of recognized keywords
    const Keywords& keywords() const { return keys_; }
    /// Returns the logger object used to report errors
    Logger& logger() { return logger_; }

private:
    void next();

    Literal parse_int(bool);
    Literal parse_float();
    Literal parse_literal();
    std::string parse_ident();

    bool eat_suffix();
    
    Token make_literal(const Literal&) const;
    Token make_ident(const std::string&) const;
    Token make_token(Token::Type) const;

    std::ostream& error();

    int c_;

    Position cur_, prev_;
    std::istream& stream_;
    const Keywords& keys_;
    Logger& logger_;
};

} // namespace slang

#endif // SLANG_LEXER_H
