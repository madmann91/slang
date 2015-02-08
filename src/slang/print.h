#ifndef SLANG_PRINT_H
#define SLANG_PRINT_H

#include <ostream>

namespace slang {

/// Pretty printer class. Used by the print method in each AST node.
/// The indentation level, indentation symbol and parentheses mode
/// can be set at construction time.
class Printer {
public:
    Printer(std::ostream& out,
            const std::string& sep = "    ",
            int indent = 0, bool force_pars = false)
        : out_(out), sep_(sep), indent_(indent), force_pars_(force_pars)
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

private:
    std::ostream& out_;
    std::string sep_;
    int indent_;
    bool force_pars_;
};

} // namespace slang

#endif // SLANG_PRINT_H