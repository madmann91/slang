#include <cctype>
#include <cstdint>
#include <cassert>
#include <cmath>

#include "slang/lexer.h"
#include "slang/logger.h"

namespace slang {

inline bool is_hexa(int c) {
    return (c >= '0' && c <='9') ||
           (c >= 'A' && c <= 'F') ||
           (c >= 'a' && c <= 'f');
}

inline bool is_octal(int c) {
    return c >= '0' && c <= '7';
}

template <typename T>
T read_octal(int c) {
    assert(std::isdigit(c) && (c - '0') < 8);
    return T(c - '0');
}

template <typename T>
T read_decimal(int c) {
    assert(std::isdigit(c));
    return T(c - '0');
}

template <typename T>
T read_hexadecimal(int c) {
    assert(std::isalnum(c));
    if (c >= 'A') {
        return T(c - 'A' + 10);
    }
    return T(c - '0');
}

Lexer::Lexer(std::istream& stream, const Keywords& keys, Logger& logger, int line, int source)
    : err_count_(0), new_line_(true), source_(source), stream_(stream), keys_(keys), logger_(logger)
{
    prev_ = Position(line, 1);
    cur_  = Position(line, 0);
    c_ = 0;
    next();

    // Parse UTF-8 byte order mark, if present
    if (c_ == 0xEF) {
        next();
        if (c_ == 0xBB) {
            next();
            if (c_ == 0xBF) {
                prev_ = Position(line, 1);
                cur_  = Position(line, 0);
                next();
                return;
            }
        }
        error() << "Unknown Byte Order Mark, only UTF-8 is supported\n";
    }
}

Token Lexer::lex() {
    while (true) {
        // Strip spaces
        while (std::isspace(c_)) {
            next();
        }

        prev_ = cur_;

        if (stream_.eof()) {
            // End-of-file case, early exit
            return make_token(Token::END);
        }

        str_.clear();

        if (std::isalpha(c_) || c_ == '_') {
            if (try_parse("true"))  return make_literal(Literal(true));
            if (try_parse("false")) return make_literal(Literal(false));

            // Begins with a letter, must be an identifier
            return make_ident(parse_ident());
        }

        if (std::isdigit(c_)) {
            // Integer or floating point number
            return make_literal(parse_literal());
        }

        if (c_ == '.') {
            next();
            if (std::isdigit(c_))
                return make_literal(parse_float(true));
            else
                return make_token(Token::DOT);
        }

        // Now for operators and special characters
        int d = c_;
        next();

        switch (d) {

    #define MAKE_BIN_OP(tok, tok_assign) \
        if (c_ == '=') { \
            next(); \
            return make_token(tok_assign); \
        } \
        return make_token(tok);

            case '/':
                if (c_ == '/') {
                    // One line comment
                    next();
                    while (!stream_.eof() && c_ != '\n') {
                        next();
                    }
                    continue;
                } else if (c_ == '*') {
                    // Multiline comment
                    while (true) {
                        next();

                        if (stream_.eof()) {
                            error() << "End-of-file reached before end of multiline comment\n";
                            break;
                        }

                        if (c_ == '*') {
                            next();
                            if (c_ == '/') {
                                next();
                                break;
                            }
                        }
                    }

                    continue;
                } else {
                    MAKE_BIN_OP(Token::DIV, Token::ASSIGN_DIV)
                }

            case '*': MAKE_BIN_OP(Token::MUL,    Token::ASSIGN_MUL)
            case '%': MAKE_BIN_OP(Token::MOD,    Token::ASSIGN_MOD)
            case '!': MAKE_BIN_OP(Token::NOT,    Token::NEQ)
            case '=': MAKE_BIN_OP(Token::ASSIGN, Token::EQ)

    #undef MAKE_BIN_OP

    #define MAKE_INC_OP(tok, tok_assign, tok_inc) \
        if (c_ == '=') { \
            next(); \
            return make_token(tok_assign); \
        } else if (c_ == d) { \
            next(); \
            return make_token(tok_inc); \
        } \
        return make_token(tok);

            case '+': MAKE_INC_OP(Token::ADD, Token::ASSIGN_ADD, Token::INC)
            case '-': MAKE_INC_OP(Token::SUB, Token::ASSIGN_SUB, Token::DEC)

    #undef MAKE_INC_OP

    #define MAKE_CMP_OP(tok, tok_eq, tok_shift, tok_assign_shift) \
        if (c_ == '=') { \
            next(); \
            return make_token(tok_eq); \
        } else if (c_ == d) { \
            next(); \
            if (c_ == '=') { \
                next(); \
                return make_token(tok_assign_shift); \
            } \
            return make_token(tok_shift); \
        } \
        return make_token(tok);

            case '>': MAKE_CMP_OP(Token::GT, Token::GEQ, Token::RSHIFT, Token::ASSIGN_RSHIFT)
            case '<': MAKE_CMP_OP(Token::LT, Token::LEQ, Token::LSHIFT, Token::ASSIGN_LSHIFT)

    #undef MAKE_CMP_OP

    #define MAKE_BIT_OP(tok, tok_logical, tok_assign) \
        if (c_ == d) { \
            next(); \
            return make_token(tok_logical); \
        } else if (c_ == '=') { \
            next(); \
            return make_token(tok_assign); \
        } \
        return make_token(tok);

            case '&': MAKE_BIT_OP(Token::AND, Token::ANDAND, Token::ASSIGN_AND)
            case '|': MAKE_BIT_OP(Token::OR,  Token::OROR,   Token::ASSIGN_OR)
            case '^': MAKE_BIT_OP(Token::XOR, Token::XORXOR, Token::ASSIGN_XOR)

    #undef MAKE_BIT_OP

            case '{': return make_token(Token::LBRACE);
            case '}': return make_token(Token::RBRACE);
            case '(': return make_token(Token::LPAREN);
            case ')': return make_token(Token::RPAREN);
            case '[': return make_token(Token::LBRACKET);
            case ']': return make_token(Token::RBRACKET);

            case '?': return make_token(Token::QMARK);
            case ':': return make_token(Token::COLON);
            case '~': return make_token(Token::NEG);

            case ';': return make_token(Token::SEMICOLON);
            case ',': return make_token(Token::COMMA);

            case '#':
                if (c_ == '#') {
                    next();
                    return make_token(Token::SHARPSHARP);
                }
                return make_token(Token::SHARP);

            default:
                error() << "Unknown token\n";
                return make_token(Token::UNKNOWN);
        }
    }
}

void Lexer::set_line_index(int line) {
    prev_.set_line(line);
    cur_.set_line(line);
}

int Lexer::line_index() const {
    return cur_.line();
}

void Lexer::set_source_index(int source) {
    source_ = source;
}

int Lexer::source_index() const {
    return source_;
}

void Lexer::next() {
    if (c_ == '\n') {
        cur_.inc_line();
        cur_.reset_col();
        new_line_ = true;
    } else {
        cur_.inc_col();
    }

    str_ += c_;
    c_ = stream_.get();

    // Handle line escaping
    while (c_ == '\\' && stream_.peek() == '\n') {
        cur_.inc_line();
        cur_.reset_col();
        // Eat newline
        stream_.get();
        c_ = stream_.get();
    }
}

Literal Lexer::parse_int(bool octal) {
    std::int64_t sum = 0;

    if (octal) {
        while (std::isdigit(c_)) {
            if (!is_octal(c_)) {
                error() << "Invalid digit in octal number\n";
            } else {
                sum = sum * 8 + read_octal<std::int64_t>(c_);
            }
            next();
        }
    } else {
        if (!is_hexa(c_))
            error() << "Hexadecimal numbers need at least one digit\n";

        while (is_hexa(c_)) {
            sum = sum * 16 + read_hexadecimal<std::int64_t>(c_);
            next();
        }
    }
    
    // Parse suffix (if any)
    if (isalpha(c_)) {
        int d = c_;
        next();

        if (eat_suffix()) {

            switch (d) {
                case 'U':
                case 'u':
                    return Literal((unsigned)sum, true);
                case 'I':
                case 'i':
                    return Literal((int)sum, true);
                default:
                    error() << "Invalid suffix on integer constant\n";
                    break;
            }
        }
    }

    // Default case (no suffix)
    return Literal((int)sum, false);
}

Literal Lexer::parse_float(bool dot) {
    double num = 0;

    if (!dot) {
        while (std::isdigit(c_)) {
            num = num * 10.0 + read_decimal<double>(c_);
            next();
        }
    }

    // Fractional part
    if (dot || c_ == '.') {
        dot = true;

        next();
        double pow = 0.1;
        while (std::isdigit(c_)) {
            num += read_decimal<double>(c_) * pow;
            pow  *= 0.1;
            next();
        }
    }

    // Parse exponent (if any)
    if (c_ == 'e' || c_ == 'E') {
        next();

        int exp_sign = 1;
        if (c_ == '+') {
            next();
        } else if (c_ == '-') {
            next();
            exp_sign = -1;
        }

        if (!std::isdigit(c_)) {
            // In this case, ignore the exponent
            error() << "Expected a number after exponent\n";
        } else {
            double exp = 0;
            while (std::isdigit(c_)) {
                exp = exp * 10.0 + read_decimal<double>(c_);
                next();
            }
            num = num * std::pow(10.0, exp_sign * exp);
        }
    }

    // Parse suffix (if any)
    if (isalpha(c_)) {
        int d = c_;
        next();

        switch (d) {
            case 'F':
            case 'f':
                if (eat_suffix()) return Literal((float)num, true);
                break;

            case 'U':
            case 'u':
                if (eat_suffix()) {
                    if (dot) error() << "Invalid suffix on floating point constant\n";
                    return Literal((unsigned)num, true);
                }
                break;

            case 'I':
            case 'i':
                if (eat_suffix()) {
                    if (dot) error() << "Invalid suffix on floating point constant\n";
                    return Literal((int)num, true);
                }
                break;

            case 'L':
                if (c_ == 'F') {
                    next();
                    if (eat_suffix()) return Literal(num, true);
                }
                break;

            case 'l':
                if (c_ == 'f') {
                    next();
                    if (eat_suffix()) return Literal(num, true);
                }
                break;

            default:
                error() << "Invalid literal suffix\n";
                break;
        }
    }

    // Default case (without suffix)
    return (dot) ? Literal((float)num, false) : Literal((int)num, false);
}

Literal Lexer::parse_literal() {
    if (c_ == '0') {
        // Can be octal or hexadecimal
        next();
        if (c_ == 'x' || c_ == 'X') {
            // Hexadecimal number
            next();
            return parse_int(false);
        } else if (c_ == '.') {
            return parse_float(false);
        }

        // Zero and octal numbers handled here
        return parse_int(true);
    }

    // Parse floating point and decimal integers
    return parse_float(false);
}

std::string Lexer::parse_ident() {
    while (std::isalnum(c_) || c_ == '_') {
        next();
    }

    return str_;
}

bool Lexer::try_parse(const std::string& str) {
    for (auto c : str) {
        if (c_ != c)
            return false;
        next();
    }

    return !std::isalnum(c_) && c_ != '_';
}

bool Lexer::eat_suffix() {
    bool ok = true;
    while (std::isalpha(c_)) {
        ok = false;
        next();
    }
    if (!ok) error() << "Invalid literal suffix\n";
    return ok;
}

inline bool reset_bool(bool& value) {
    bool old = value;
    value = false;
    return old;
}

Token Lexer::make_literal(const Literal& l) {
    return Token(Location(prev_, cur_), str_, l, reset_bool(new_line_));
}

Token Lexer::make_ident(const std::string& ident) {
    return Token(Location(prev_, cur_), ident, keys_, reset_bool(new_line_));
}

Token Lexer::make_token(Token::Type type) {
    return Token(Location(prev_, cur_), type, reset_bool(new_line_));
}

std::ostream& Lexer::error() {
    err_count_++;
    return logger_.error(cur_);
}

} // namespace slang
