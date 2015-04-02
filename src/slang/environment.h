#ifndef SLANG_ENVIRONMENT_H
#define SLANG_ENVIRONMENT_H

#include <vector>
#include <string>
#include <algorithm>
#include <ostream>

#include "slang/cast.h"
#include "slang/sema.h"

namespace slang {

namespace ast {
    class Node;
}

/// Holds a declaration (function, variable or datatype).
class Symbol {
public:
    Symbol(ast::Node* decl, ast::Node* def)
        : decl_(decl), def_(def)
    {}

    const ast::Node* decl() const { return decl_; }
    const ast::Node* def() const { return def_; }

    void set_def(ast::Node* def) { def_ = def; }
    void set_decl(ast::Node* decl) { decl_ = decl; }

private:
    ast::Node* decl_;
    ast::Node* def_;
};

/// An environment : holds variables, structures and functions names.
class Environment {
public:
    Environment()
        : parent_(nullptr)
    {}

    /// Lookup for a declaration in the environment and its parents (returns nullptr if not found).
    Symbol* lookup(const std::string& name) {
        auto it = symbols_.find(name);

        if (it != symbols_.end())
            return &it->second;

        Environment* p = parent_;
        while (p) {
            Symbol* s = p->lookup(name);
            if (s) return s;
            p = p->parent_;
        }

        return nullptr;
    }

    /// Returns the symbol table of this environment.
    const std::unordered_map<std::string, Symbol>& symbols() const { return symbols_; }
    /// Pushes a symbol in this environment.
    void push_symbol(const std::string& name, Symbol&& symbol) { symbols_.emplace(name, symbol); }
    /// Returns the number of symbols in this environment.
    int num_symbols() const { return symbols_.size(); }

    /// Sets the parent of this environment.
    void set_parent(Environment* parent) { parent_ = parent; }
    /// Gets the parent of this environment (if any).
    Environment* parent() { return parent_; }
    /// Gets the parent of this environment (if any).
    const Environment* parent() const { return parent_; }

private:
    std::unordered_map<std::string, Symbol> symbols_;
    Environment* parent_;
};

} // namespace slang

#endif // SLANG_ENVIRONMENT_H
