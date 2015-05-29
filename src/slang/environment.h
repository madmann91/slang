#ifndef SLANG_ENVIRONMENT_H
#define SLANG_ENVIRONMENT_H

#include <vector>
#include <string>
#include <unordered_map>

#include "slang/ptr.h"
#include "slang/cast.h"
#include "slang/types.h"

namespace slang {

namespace ast {
    class Node;
}

/// Holds a declaration (function, variable or datatype).
class Symbol {
public:
    Symbol(const ast::Node* decl, const ast::Node* def, Type* type)
        : decl_(decl), def_(def), type_(type)
    {}

    const ast::Node* decl() const { return decl_; }
    const ast::Node* def() const { return def_; }
    Type* type() const { return type_; }

    void set_def(const ast::Node* def) { def_ = def; }
    void set_decl(const ast::Node* decl) { decl_ = decl; }
    void set_type(Type* type) { type_ = type; }

private:
    const ast::Node* decl_;
    const ast::Node* def_;
    Type* type_;
};

/// An environment : holds variables, structures and functions names.
class Environment {
public:
    Environment(const ast::Node* scope = nullptr)
        : parent_(nullptr), scope_(scope)
    {}

    /// Lookup for a symbol in this environment and its parents (returns nullptr if not found).
    Symbol* lookup_symbol(const std::string& name) {
        if (auto symbol = find_symbol(name))
            return symbol;

        Environment* p = parent_;
        while (p) {
            Symbol* s = p->find_symbol(name);
            if (s) return s;
            p = p->parent_;
        }

        return nullptr;
    }

    /// Find a symbol in this environment only (and not its parents), returns nullptr if not found.
    Symbol* find_symbol(const std::string& name) {
        auto it = symbols_.find(name);
        if (it != symbols_.end())
            return &it->second;
        return nullptr;
    }

    /// Returns the symbol table of this environment.
    const std::unordered_map<std::string, Symbol>& symbols() const { return symbols_; }
    /// Pushes a symbol in this environment.
    void push_symbol(const std::string& name, Symbol&& symbol) {
        symbols_.emplace(name, std::move(symbol));
    }
    /// Returns the number of symbols in this environment.
    int num_symbols() const { return symbols_.size(); }

    /// Sets the parent of this environment.
    void set_parent(Environment* parent) { parent_ = parent; }
    /// Gets the parent of this environment (if any).
    Environment* parent() { return parent_; }
    /// Gets the parent of this environment (if any).
    const Environment* parent() const { return parent_; }

    /// Returns the enclosing scope AST node, if any.
    const ast::Node* scope() const { return scope_; }

private:
    std::unordered_map<std::string, Symbol> symbols_;
    Environment* parent_;
    const ast::Node* scope_;
};

} // namespace slang

#endif // SLANG_ENVIRONMENT_H
