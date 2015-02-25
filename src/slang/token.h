#ifndef SLANG_TOKEN_H
#define SLANG_TOKEN_H

#include <string>
#include <cassert>

#include "slang/location.h"
#include "slang/keyword.h"

namespace slang {

/// A literal value as specified in the language.
class Literal {
public:
    enum Type {
        LIT_DOUBLE,
        LIT_FLOAT,
        LIT_INT,
        LIT_UINT,
        LIT_BOOL,
        LIT_UNKNOWN
    };

    Literal() : type_(LIT_UNKNOWN) {}

    Literal(double d)   : type_(LIT_DOUBLE) { value_.dbl_val = d; }
    Literal(float f)    : type_(LIT_FLOAT)  { value_.flt_val = f; }
    Literal(int i)      : type_(LIT_INT)    { value_.int_val = i; }
    Literal(unsigned u) : type_(LIT_UINT)   { value_.uint_val = u; }
    Literal(bool b)     : type_(LIT_BOOL)   { value_.bool_val = b; }

    bool isa(Type type) const { return type_ == type; }
    bool valid() const { return type_ != LIT_UNKNOWN; }
    Type type() const { return type_; }

    double   as_double() const { assert(isa(LIT_DOUBLE)); return value_.dbl_val;  }
    float    as_float()  const { assert(isa(LIT_FLOAT));  return value_.flt_val;  }
    int      as_int()    const { assert(isa(LIT_INT));    return value_.int_val;  }
    unsigned as_uint()   const { assert(isa(LIT_UINT));   return value_.uint_val; }
    bool     as_bool()   const { assert(isa(LIT_BOOL));   return value_.bool_val; }

private:
    Type type_;
    union {
        double   dbl_val;
        float    flt_val;
        int      int_val;
        unsigned uint_val;
        bool     bool_val;
    } value_;
};

/// A lexer token. Can represent a special character, a literal, an identifier or a keyword.
class Token {
public:
    enum Type {
#define SLANG_TOK(tok, str) TOK_##tok,
#include "slang/tokenlist.h"
    };

    Token() : type_(TOK_UNKNOWN) {}
    /// Creates a token which is not an identifier nor a literal.
    Token(const Location& loc, Type type, bool new_line)
        : loc_(loc), type_(type), new_line_(new_line)
    {}
    /// Creates an identifier or keyword.
    Token(const Location& loc, const std::string& str, const Keywords& keys, bool new_line)
        : loc_(loc), type_(TOK_IDENT), ident_(str), key_(keys.keyword(str)), new_line_(new_line)
    {}
    /// Creates a literal.
    Token(const Location& loc, Literal lit, bool new_line)
        : loc_(loc), type_(TOK_LIT), lit_(lit), new_line_(new_line)
    {}

    bool isa(Type type) const { return type_ == type; }
    bool is_eof() const { return type_ == TOK_EOF; }
    bool is_ident() const { return type_ == TOK_IDENT && key_.is_unknown(); }
    bool is_keyword() const { return !key_.is_unknown(); }

    Type type() const { return type_; }
    Location loc() const { return loc_; }
    Key key() const { return key_; }

    Literal lit() const { return lit_; }
    const std::string& ident() const { return ident_; }

    bool new_line() const { return new_line_; }
    
private:
    Location loc_;
    Type type_;
    Literal lit_;
    std::string ident_;
    Key key_;
    bool new_line_;
};

inline std::ostream& operator << (std::ostream& out, const Literal& lit) {
    switch (lit.type()) {
        case Literal::LIT_FLOAT:
            out << std::to_string(lit.as_float());
            break;
        case Literal::LIT_DOUBLE:
            out << std::to_string(lit.as_double()) << "lf";
            break;
        case Literal::LIT_INT:
            out << lit.as_int();
            break;
        case Literal::LIT_UINT:
            out << lit.as_uint();
            break;
        default:
        case Literal::LIT_UNKNOWN:
            assert(0 && "Invalid literal");
            break;
    }

    return out;
}

inline std::ostream& operator << (std::ostream& out, Token::Type type) {
    switch (type) {
#define SLANG_TOK(tok, str) case Token::TOK_##tok: out << str; break;
#include "slang/tokenlist.h"
    }

    return out;
}

inline std::ostream& operator << (std::ostream& out, const Token& token) {
    if (token.isa(Token::TOK_IDENT)) {
        out << token.ident();
    } else if (token.isa(Token::TOK_LIT)) {
        out << token.lit();
    } else {
        out << token.type();
    }
    
    return out;
}

} // namespace slang

#endif // SLANG_TOKEN_H
