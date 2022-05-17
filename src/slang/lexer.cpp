#include <cctype>
#include <cstdint>
#include <cassert>
#include <cmath>

#include "slang/lexer.h"
#include "slang/logger.h"

namespace slang {

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
    : err_count_(0), new_line_(true), spaces_(false), source_(source), stream_(stream), keys_(keys), logger_(logger)
{
    reset_pos(line);
    c_ = 0;
    next();

    // Parse UTF-8 byte order mark, if present
    if (parse_bom())
        reset_pos(line);
}

Token Lexer::lex() {
    struct ResetFlags {
        Lexer& lexer;

        ResetFlags(Lexer& lexer) : lexer(lexer) {}
        ~ResetFlags() { lexer.reset_flags(); }
    } reset_on_exit(*this);
    while (true) {
        eat_spaces();

        prev_ = cur_;
        if (stream_.eof()) {
            // End-of-file case, early exit
            return make_token(Token::END);
        }

        str_.clear();
        if (std::isalpha(c_) || c_ == '_') {
            if (accept("true"))  return make_literal(Literal(true));
            if (accept("false")) return make_literal(Literal(false));

            // Begins with a letter, must be an identifier
            return make_ident(parse_ident());
        }

        if (std::isdigit(c_)) {
            // Integer or floating point number
            return make_literal(parse_literal());
        }

        if (accept('.')) {
            if (std::isdigit(c_))
                return make_literal(parse_int_or_float(true));
            else
                return make_token(Token::DOT);
        }

        // Now parse operators and special characters
        if (accept('{')) return make_token(Token::LBRACE);
        if (accept('}')) return make_token(Token::RBRACE);
        if (accept('(')) return make_token(Token::LPAREN);
        if (accept(')')) return make_token(Token::RPAREN);
        if (accept('[')) return make_token(Token::LBRACKET);
        if (accept(']')) return make_token(Token::RBRACKET);
        if (accept('?')) return make_token(Token::QMARK);
        if (accept(':')) return make_token(Token::COLON);
        if (accept('~')) return make_token(Token::NEG);
        if (accept(';')) return make_token(Token::SEMICOLON);
        if (accept(',')) return make_token(Token::COMMA);

        if (accept('#')) {
            if (accept('#'))
                return make_token(Token::SHARPSHARP);
            return make_token(Token::SHARP);
        }

    #define MAKE_BIN_OP(tok, tok_assign) \
        if (accept('=')) \
            return make_token(tok_assign); \
        return make_token(tok);

        if (accept('/')) {
            if (accept('/')) {
                eat_single_line_comment();
                continue;
            } else if (accept('*')) {
                eat_multi_line_comment();
                continue;
            } else {
                MAKE_BIN_OP(Token::DIV, Token::ASSIGN_DIV)
            }
        }

        if (accept('*')) { MAKE_BIN_OP(Token::MUL,    Token::ASSIGN_MUL) }
        if (accept('%')) { MAKE_BIN_OP(Token::MOD,    Token::ASSIGN_MOD) }
        if (accept('!')) { MAKE_BIN_OP(Token::NOT,    Token::NEQ)        }
        if (accept('=')) { MAKE_BIN_OP(Token::ASSIGN, Token::EQ)         }

    #undef MAKE_BIN_OP

    #define MAKE_INC_OP(d, tok, tok_assign, tok_inc) \
        if (accept('=')) { \
            return make_token(tok_assign); \
        } else if (accept(d)) { \
            return make_token(tok_inc); \
        } \
        return make_token(tok);

        if (accept('+')) { MAKE_INC_OP('+', Token::ADD, Token::ASSIGN_ADD, Token::INC) }
        if (accept('-')) { MAKE_INC_OP('-', Token::SUB, Token::ASSIGN_SUB, Token::DEC) }

    #undef MAKE_INC_OP

    #define MAKE_CMP_OP(d, tok, tok_eq, tok_shift, tok_assign_shift) \
        if (accept('=')) { \
            return make_token(tok_eq); \
        } else if (accept(d)) { \
            if (accept('=')) \
                return make_token(tok_assign_shift); \
            return make_token(tok_shift); \
        } \
        return make_token(tok);

        if (accept('>')) { MAKE_CMP_OP('>', Token::GT, Token::GEQ, Token::RSHIFT, Token::ASSIGN_RSHIFT) }
        if (accept('<')) { MAKE_CMP_OP('<', Token::LT, Token::LEQ, Token::LSHIFT, Token::ASSIGN_LSHIFT) }

    #undef MAKE_CMP_OP

    #define MAKE_BIT_OP(d, tok, tok_logical, tok_assign) \
        if (accept(d)) \
            return make_token(tok_logical); \
        else if (accept('=')) \
            return make_token(tok_assign); \
        return make_token(tok);

        if (accept('&')) { MAKE_BIT_OP('&', Token::AND, Token::ANDAND, Token::ASSIGN_AND) }
        if (accept('|')) { MAKE_BIT_OP('|', Token::OR,  Token::OROR,   Token::ASSIGN_OR)  }
        if (accept('^')) { MAKE_BIT_OP('^', Token::XOR, Token::XORXOR, Token::ASSIGN_XOR) }

    #undef MAKE_BIT_OP

        error() << "Unknown token\n";
        next();
        return make_token(Token::UNKNOWN);
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

void Lexer::eat_spaces() {
    while (std::isspace(c_)) {
        spaces_ = true;
        next();
    }
}

void Lexer::eat_single_line_comment() {
    while (!stream_.eof() && c_ != '\n') next();
}

void Lexer::eat_multi_line_comment() {
    while (true) {
        while (accept('*')) {
            if (accept('/'))
                return;
        }
        next();
        if (stream_.eof()) {
            error() << "End-of-file reached before end of multiline comment\n";
            break;
        }
    }
}

bool Lexer::accept(int c) {
    if (c_ == c) {
        next();
        return true;
    }
    return false;
}

bool Lexer::accept(const std::string& str) {
    for (auto c : str) {
        if (!accept(c))
            return false;
    }
    return true;
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

Literal Lexer::parse_hex_or_octal_int(bool is_octal) {
    const int base = is_octal ? 8 : 16;

    if (is_octal) {
        while (c_ >= '0' && c_ <= '9') next();
    } else {
        while (std::isxdigit(c_)) next();
    }

    auto type = parse_suffix();
    if (type != Literal::UINT && type != Literal::INT && type != Literal::UNKNOWN) {
        error() << "Invalid suffix on integer constant\n";
        type = Literal::UNKNOWN;
    }

    if (type == Literal::UINT)
        return Literal(static_cast<unsigned>(std::strtoul(str_.c_str(), nullptr, base)), true);
    return Literal(static_cast<int>(std::strtol(str_.c_str(), nullptr, base)), type != Literal::UNKNOWN);
}

Literal Lexer::parse_int_or_float(bool has_dot) {
    if (!has_dot) {
        while (std::isdigit(c_)) next();
        has_dot = accept('.');
    }

    // Fractional part
    if (has_dot) {
        while (std::isdigit(c_)) next();
    }

    // Parse exponent (if any)
    bool has_exp = accept('e') || accept('E');
    if (has_exp) {
        accept('+') || accept('-');
        if (!std::isdigit(c_)) {
            // In this case, ignore the exponent
            error() << "Expected a number after exponent\n";
        } else {
            while (std::isdigit(c_)) next();
        }
    }

    const bool is_float = has_dot || has_exp;

    auto type = parse_suffix();
    if (is_float && (type == Literal::INT || type == Literal::UINT)) {
        error() << "Invalid suffix on floating-point constant\n";
        type = Literal::UNKNOWN;
    }

    switch (type) {
        case Literal::FLOAT:
            return Literal(std::strtof(str_.c_str(), nullptr), true);
        case Literal::UINT:
            return Literal(static_cast<unsigned>(std::strtoul(str_.c_str(), nullptr, 10)), true);
        case Literal::INT:
            return Literal(static_cast<int>(std::strtol(str_.c_str(), nullptr, 10)), true);
        case Literal::DOUBLE:
            return Literal(std::strtod(str_.c_str(), nullptr), true);
        default:
            return is_float
                ? Literal(std::strtof(str_.c_str(), nullptr), false)
                : Literal(static_cast<int>(std::strtol(str_.c_str(), nullptr, 10)), false);
    }
}

Literal Lexer::parse_literal() {
    if (accept('0')) {
        if (accept('x') || accept('X'))
            return parse_hex_or_octal_int(false);
        else if (accept('.'))
            return parse_int_or_float(true);
        return parse_hex_or_octal_int(true);
    }

    // Parse floating point and decimal integers
    return parse_int_or_float(false);
}

Literal::Type Lexer::parse_suffix() {
    bool ate_char = false;
    auto type = Literal::UNKNOWN;
    if (accept('f') || accept('F')) {
        type = Literal::FLOAT;
    } else if (accept('u') || accept('U')) {
        type = Literal::UINT;
    } else if (accept('i') || accept('I')) {
        type = Literal::INT;
    } else {
        if (accept('l')) {
            if (accept('f'))
                type = Literal::DOUBLE;
            else
                ate_char = true;
        }
        if (accept('L')) {
            if (accept('F'))
                type = Literal::DOUBLE;
            else
                ate_char = true;
        }
    }

    // Check for trailing characters
    for (; std::isalpha(c_); ate_char = true, next()) ;

    if (ate_char) {
        error() << "Invalid literal suffix\n";
        type = Literal::UNKNOWN;
    }
    return type;
}

std::string Lexer::parse_ident() {
    while (std::isalnum(c_) || c_ == '_') next();
    return str_;
}

bool Lexer::parse_bom() {
    if (accept(0xEF)) {
        if (accept(0xBB) && accept(0xBF))
            return true;
        error() << "Unknown Byte Order Mark, only UTF-8 is supported\n";
    }
    return false;
}

Token Lexer::make_literal(const Literal& l) {
    return Token(Location(prev_, cur_), str_, l, new_line_, spaces_);
}

Token Lexer::make_ident(const std::string& ident) {
    return Token(Location(prev_, cur_), ident, keys_, new_line_, spaces_);
}

Token Lexer::make_token(Token::Type type) {
    return Token(Location(prev_, cur_), type, new_line_, spaces_);
}

void Lexer::reset_pos(int line) {
    prev_ = Position(line, 1);
    cur_  = Position(line, 0);
}

void Lexer::reset_flags() {
    new_line_ = false;
    spaces_ = false;
}

std::ostream& Lexer::error() {
    err_count_++;
    return logger_.error(cur_);
}

} // namespace slang
