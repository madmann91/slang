#ifndef SLANG_ENVIRONMENT_H
#define SLANG_ENVIRONMENT_H

#include <vector>
#include <string>
#include <unordered_map>

#include "slang/ptr.h"
#include "slang/cast.h"
#include "slang/location.h"

namespace slang {

namespace ast {
    class Node;
}

class Type;

/// Holds a declaration (function, variable or datatype). Structures,
/// interfaces, or variables have only one definition in one environment.
/// Functions can have multiple definitions : prototypes and overloads.
class Symbol {
public:
    typedef std::unordered_multimap<const Type*, const ast::Node*> DefMap;

    Symbol(std::initializer_list<DefMap::value_type> defs);

    bool is_function() const;
    bool is_structure() const;
    bool is_interface() const;
    bool is_variable() const;
    bool is_argument() const;

    /// Returns the type of the first definition associated with this symbol, or
    /// the minimum type (from the subtype relation) of all the declarations (if they agree).
    const Type* type() const { return type_; }
    /// Returns the location of the first definition associated with this symbol.
    const Location& location() const { return loc_; }

    const DefMap& defs() const { return defs_; }
    void push_def(const Type* type, const ast::Node* node);
    size_t num_defs() const { return defs_.size(); }

private:
    const Type* type_;
    Location loc_;
    DefMap defs_;
};

/// An environment : holds variables, structures and functions names.
class Environment {
public:
    Environment(const ast::Node* scope = nullptr)
        : parent_(nullptr), scope_(scope)
    {}

    /// Lookup for a symbol in this environment and its parents (returns nullptr if not found).
    Symbol* lookup_symbol(const std::string& name);
    /// Find a symbol in this environment only (and not its parents), returns nullptr if not found.
    Symbol* find_symbol(const std::string& name);

    /// Returns the symbol table of this environment.
    const std::unordered_map<std::string, Symbol>& symbols() const { return symbols_; }
    /// Pushes a symbol in this environment.
    void push_symbol(const std::string& name, Symbol&& symbol) {
        symbols_.emplace(name, std::move(symbol));
    }
    /// Returns the number of symbols in this environment.
    size_t num_symbols() const { return symbols_.size(); }

    /// Sets the parent of this environment.
    void set_parent(Environment* parent) { parent_ = parent; }
    /// Gets the parent of this environment (if any).
    Environment* parent() { return parent_; }
    /// Gets the parent of this environment (if any).
    const Environment* parent() const { return parent_; }

    /// Returns the enclosing scope AST node, if any.
    const ast::Node* scope() const { return scope_; }
    /// Returns the nearest enclosing function, if any.
    const ast::Node* closest_function() const;
    /// Returns the nearest enclosing loop, if any.
    const ast::Node* closest_loop() const;
    /// Returns the nearest enclosing switch statement, if any.
    const ast::Node* closest_switch() const;

private:
    std::unordered_map<std::string, Symbol> symbols_;
    Environment* parent_;
    const ast::Node* scope_;
};

} // namespace slang

#endif // SLANG_ENVIRONMENT_H
