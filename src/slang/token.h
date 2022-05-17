#ifndef SLANG_TOKEN_H
#define SLANG_TOKEN_H

#include <string>
#include <cassert>
#include <iomanip>

#include "slang/location.h"
#include "slang/keyword.h"

namespace slang {

/// A literal value as specified in the language.
class Literal {
public:
    enum Type {
        DOUBLE,
        FLOAT,
        INT,
        UINT,
        BOOL,
        UNKNOWN
    };

    Literal() : type_(UNKNOWN) {}

    Literal(double d, bool s)   : type_(DOUBLE), suffix_(s)     { value_.dbl_val = d; }
    Literal(float f, bool s)    : type_(FLOAT),  suffix_(s)     { value_.flt_val = f; }
    Literal(int i, bool s)      : type_(INT),    suffix_(s)     { value_.int_val = i; }
    Literal(unsigned u, bool s) : type_(UINT),   suffix_(s)     { value_.uint_val = u; }
    Literal(bool b)             : type_(BOOL),   suffix_(false) { value_.bool_val = b; }

    bool isa(Type type) const { return type_ == type; }
    bool valid() const { return type_ != UNKNOWN; }
    Type type() const { return type_; }

    bool has_suffix() const { return suffix_; }

    double   as_double() const { assert(isa(DOUBLE)); return value_.dbl_val;  }
    float    as_float()  const { assert(isa(FLOAT));  return value_.flt_val;  }
    int      as_int()    const { assert(isa(INT));    return value_.int_val;  }
    unsigned as_uint()   const { assert(isa(UINT));   return value_.uint_val; }
    bool     as_bool()   const { assert(isa(BOOL));   return value_.bool_val; }

    double value() const {
        switch (type_) {
            case DOUBLE:  return value_.dbl_val;
            case FLOAT:   return (double)value_.flt_val;
            case INT:     return (double)value_.int_val;
            case UINT:    return (double)value_.uint_val;
            default:
            case UNKNOWN:
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
        bool     bool_val;
    } value_;
};

/// A lexer token. Can represent a special character, a literal, an identifier or a keyword.
class Token {
public:
    enum Type {
#define SLANG_TOK(tok, str) tok,
#include "slang/tokenlist.h"
    };

    Token() : type_(UNKNOWN) {}
    /// Creates a token which is not an identifier nor a literal.
    Token(const Location& loc, Type type, bool new_line, bool spaces)
        : loc_(loc), type_(type), str_(type_string(type)), new_line_(new_line), spaces_(spaces)
    {}
    /// Creates an identifier or keyword.
    Token(const Location& loc, const std::string& str, const Keywords& keys, bool new_line, bool spaces)
        : loc_(loc), type_(IDENT), str_(str), key_(keys.keyword(str)), new_line_(new_line), spaces_(spaces)
    {}
    /// Creates a literal.
    Token(const Location& loc, const std::string& str, Literal lit, bool new_line, bool spaces)
        : loc_(loc), type_(LIT), str_(str), lit_(lit), new_line_(new_line), spaces_(spaces)
    {}

    bool isa(Type type) const { return type_ == type; }
    bool is_eof() const { return type_ == END; }
    bool is_ident() const { return type_ == IDENT && key_.is_unknown(); }
    bool is_keyword() const { return !key_.is_unknown(); }

    Type type() const { return type_; }
    Location loc() const { return loc_; }
    Key key() const { return key_; }

    Literal lit() const { assert(type_ == LIT); return lit_; }
    const std::string& ident() const { assert(type_ == IDENT); return str_; }

    const std::string& str() const { return str_; }

    /// Returns true if a new line was encountered before parsing the token.
    bool new_line() const { return new_line_; }
    /// Returns true if one or more spaces were encountered before parsing the token.
    bool spaces() const { return spaces_; }

    static std::string type_string(Type type) {
        static auto hash_type = [] (Type type) { return (size_t)type; };
        static const std::unordered_map<Type, std::string, decltype(hash_type)> type_to_str(
            {
        #define SLANG_TOK(tok, str) {tok, str},
        #include "slang/tokenlist.h"
            }, 256, hash_type);
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
    bool new_line_ : 1;
    bool spaces_ : 1;
};

template <typename T>
std::string to_string_nz(T t) {
    auto str = std::to_string(t);
    auto pos = str.find('.');
    if (pos != std::string::npos) {
        int i = str.length() - 1, j = pos + 1;
        while (i > j && str[i] == '0') i--;
        str.resize(i + 1);
    }
    return str;
}

inline std::ostream& operator << (std::ostream& out, const Literal& lit) {
    switch (lit.type()) {
        case Literal::FLOAT:
            out << to_string_nz(lit.as_float());
            if (lit.has_suffix()) out << "f";
            break;
        case Literal::DOUBLE:
            assert(lit.has_suffix());
            out << to_string_nz(lit.as_double()) << "lf";
            break;
        case Literal::INT:
            out << lit.as_int();
            if (lit.has_suffix()) out << "i";
            break;
        case Literal::UINT:
            out << lit.as_uint();
            if (lit.has_suffix()) out << "u";
            break;
        case Literal::BOOL:
            if (lit.as_bool()) out << "true";
            else out << "false";
            break;
        default:
        case Literal::UNKNOWN:
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
