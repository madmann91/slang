#ifndef SLANG_PRINT_H
#define SLANG_PRINT_H

#include <ostream>
#include "slang/token.h"

namespace slang {

/// Pretty printer class. Used by the print method in each AST node.
class Printer {
public:
    Printer(std::ostream& out,
            const std::string& sep = "    ",
            int indent = 0,
            bool force_pars = false,
            bool terminal = true)
        : out_(out), sep_(sep), indent_(indent), force_pars_(force_pars), term_(terminal)
    {}

    void indent() { indent_++; }
    void unindent() { assert(indent_ > 0); indent_--; }

    bool force_pars() const { return force_pars_; }

    void new_line() {
        out_ << "\n";
        for (int i = 0; i < indent_; i++) {
            out_ << sep_;
        }
    }

    template <typename T>
    std::ostream& operator << (const T& t) {
        out_ << t;
        return out_;
    }

    /// Prints a literal
    std::ostream& print_literal(const Literal& lit) {
        if (term_) out_ << "\033[1;37m";
        out_ << lit;
        if (term_) out_ << "\033[0m";
        return out_;
    }

    /// Prints a keyword
    std::ostream& print_keyword(const std::string& str) {
        if (term_) out_ << "\033[1;36m" << str << "\033[0m";
        else out_ << str;
        return out_;
    }

    /// Prints a function or structure name
    std::ostream& print_name(const std::string& str) {
        if (term_) out_ << "\033[1;32m" << str << "\033[0m";
        else out_ << str;
        return out_;
    }

    /// Prints an error symbol for incorrectly parsed elements
    std::ostream& print_error() {
        if (term_) out_ << "\033[1;31m<error>\033[0m";
        else out_ << "<error>";
        return out_;
    }

private:
    std::ostream& out_;
    std::string sep_;
    int indent_;
    bool force_pars_;
    bool term_;
};

} // namespace slang

#endif // SLANG_PRINT_H
