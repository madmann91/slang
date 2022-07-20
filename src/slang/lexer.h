#ifndef SLANG_LEXER_H
#define SLANG_LEXER_H

#include <iostream>
#include <string>

#include "slang/keyword.h"
#include "slang/token.h"
#include "slang/logger.h"

namespace slang {

/// Base class for all classes that can produce tokens.
class TokenStream {
public:
    virtual ~TokenStream() = default;
    virtual Token next_token() = 0;
};

/// The lexer : produces a stream of tokens from a stream of characters.
class Lexer : public TokenStream {
public:
    /// Builds a lexer with the given stream and set of keywords, errors will be redirected to the logger object.
    Lexer(std::istream& stream, const Keywords& keywords, Logger& logger, int line = 1, int source = 0);

    /// Returns the next token in the stream and updates the current position.
    Token next_token() override;

    /// Returns the number of errors generated during lexing.
    size_t error_count() const { return err_count_; }

    /// Sets the current line index.
    void set_line_index(int line);
    /// Returns the current line index.
    int line_index() const;

    /// Sets the current line index.
    void set_source_index(int source);
    /// Returns the current line index.
    int source_index() const;

    /// Returns the set of keywords associated with this lexer
    const Keywords& keywords() const { return keys_; }

private:
    void eat_spaces();
    void eat_single_line_comment();
    void eat_multi_line_comment();

    bool accept(int);
    bool accept(const std::string&);
    void next();

    Literal parse_hex_or_octal_int(bool);
    Literal parse_int_or_float(bool);
    Literal parse_literal();
    Literal::Type parse_suffix();
    std::string parse_ident();
    bool parse_bom();

    Token make_literal(const Literal&);
    Token make_ident(const std::string&);
    Token make_token(Token::Type);

    void reset_pos(int);
    void reset_flags();
    std::ostream& error();

    size_t err_count_;

    int c_;
    bool new_line_;
    bool spaces_;
    int source_;
    std::string str_;

    Position cur_, prev_;
    std::istream& stream_;
    const Keywords& keys_;
    Logger& logger_;
};

} // namespace slang

#endif // SLANG_LEXER_H
