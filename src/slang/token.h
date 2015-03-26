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
        LIT_UNKNOWN
    };

    Literal() : type_(LIT_UNKNOWN) {}

    Literal(double d, bool s)   : type_(LIT_DOUBLE), suffix_(s) { value_.dbl_val = d; }
    Literal(float f, bool s)    : type_(LIT_FLOAT),  suffix_(s) { value_.flt_val = f; }
    Literal(int i, bool s)      : type_(LIT_INT),    suffix_(s) { value_.int_val = i; }
    Literal(unsigned u, bool s) : type_(LIT_UINT),   suffix_(s) { value_.uint_val = u; }

    bool isa(Type type) const { return type_ == type; }
    bool valid() const { return type_ != LIT_UNKNOWN; }
    Type type() const { return type_; }

    bool has_suffix() const { return suffix_; }

    double   as_double() const { assert(isa(LIT_DOUBLE)); return value_.dbl_val;  }
    float    as_float()  const { assert(isa(LIT_FLOAT));  return value_.flt_val;  }
    int      as_int()    const { assert(isa(LIT_INT));    return value_.int_val;  }
    unsigned as_uint()   const { assert(isa(LIT_UINT));   return value_.uint_val; }

    double value() const {
        switch (type_) {
            case LIT_DOUBLE:  return value_.dbl_val;
            case LIT_FLOAT:   return (double)value_.flt_val;
            case LIT_INT:     return (double)value_.int_val;
            case LIT_UINT:    return (double)value_.uint_val;
            default:
            case LIT_UNKNOWN:
                assert(0 && "Invalid literal");
                return 0.0;
        }
    }

private:
    Type type_;
    bool suffix_;
    union {
        double   dbl_val;
        float    flt_val;
        int      int_val;
        unsigned uint_val;
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
        : loc_(loc), type_(type), str_(type_string(type)), new_line_(new_line)
    {}
    /// Creates an identifier or keyword.
    Token(const Location& loc, const std::string& str, const Keywords& keys, bool new_line)
        : loc_(loc), type_(TOK_IDENT), str_(str), key_(keys.keyword(str)), new_line_(new_line)
    {}
    /// Creates a literal.
    Token(const Location& loc, const std::string& str, Literal lit, bool new_line)
        : loc_(loc), type_(TOK_LIT), str_(str), lit_(lit), new_line_(new_line)
    {}

    bool isa(Type type) const { return type_ == type; }
    bool is_eof() const { return type_ == TOK_EOF; }
    bool is_ident() const { return type_ == TOK_IDENT && key_.is_unknown(); }
    bool is_keyword() const { return !key_.is_unknown(); }

    Type type() const { return type_; }
    Location loc() const { return loc_; }
    Key key() const { return key_; }

    Literal lit() const { return lit_; }
    const std::string& str() const { return str_; }
    const std::string& ident() const { assert(type_ == TOK_IDENT); return str_; }

    bool new_line() const { return new_line_; }

    static std::string type_string(Type type) {
        static auto hash_type = [] (Type type) { return (size_t)type; };
        static const std::unordered_map<Type, std::string, decltype(hash_type)> type_to_str(
            {
        #define SLANG_TOK(tok, str) {TOK_##tok, str},
        #include "slang/tokenlist.h"
            }, 64, hash_type);
        auto it = type_to_str.find(type);
        assert(it != type_to_str.end());
        return it->second;
    }

private:
    Location loc_;
    Type type_;
    std::string str_;
    Literal lit_;
    Key key_;
    bool new_line_;
};

inline std::ostream& operator << (std::ostream& out, const Literal& lit) {
    switch (lit.type()) {
        case Literal::LIT_FLOAT:
            out << std::to_string(lit.as_float());
            if (lit.has_suffix()) out << "f";
            break;
        case Literal::LIT_DOUBLE:
            assert(lit.has_suffix());
            out << std::to_string(lit.as_double()) << "lf";
            break;
        case Literal::LIT_INT:
            out << lit.as_int();
            if (lit.has_suffix()) out << "i";
            break;
        case Literal::LIT_UINT:
            out << lit.as_uint();
            if (lit.has_suffix()) out << "u";
            break;
        default:
        case Literal::LIT_UNKNOWN:
            assert(0 && "Invalid literal");
            break;
    }

    return out;
}

inline std::ostream& operator << (std::ostream& out, const Token& token) {
    out << token.str();
    return out;
}

} // namespace slang

#endif // SLANG_TOKEN_H
