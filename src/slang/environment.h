#ifndef SLANG_ENVIRONMENT_H
#define SLANG_ENVIRONMENT_H

#include <vector>
#include <string>
#include <algorithm>

namespace slang {

namespace ast {
    class Node;
}

/// Holds a declaration (function, variable or datatype)
class Name {
public:
    Name(const std::string& name)
        : name_(name), decl_(nullptr), def_(nullptr)
    {}

    const std::string& name() const { return name_; }
    ast::Node* declaration() const { return decl_; }
    ast::Node* definition() const { return def_; }

    void set_definition(ast::Node* def) { def_ = def; }
    void set_declaration(ast::Node* decl) { decl_ = decl; }

private:
    std::string name_;
    ast::Node* decl_;
    ast::Node* def_;
};

/// An environment : holds variables, structures and functions names
class Environment {
public:
    Environment()
        : parent_(nullptr)
    {}

    /// Lookup for a declaration in the environment
    Name* lookup(const std::string& name) {
        auto it = std::find_if(names_.begin(), names_.end(), [name] (const Name & n) {
            return n.name() == name;
        });

        if (it != names_.end())
            return &(*it);

        Environment* p = parent_;
        while (p) {
            Name* n = p->lookup(name);
            if (n) return n;
            p = p->parent_;
        }

        return nullptr;
    }

    /// Pushes a name in this environment
    void push_name(const Name& name) { names_.push_back(name); }

    /// Sets the parent of this environment
    void set_parent(Environment* parent) { parent_ = parent; }

    /// Gets the parent of this environment (if any)
    Environment* parent() { return parent_; }

    /// Gets the parent of this environment (if any)
    const Environment* parent() const { return parent_; }

private:
    std::vector<Name> names_;
    Environment* parent_;
};

} // namespace slang

#endif // SLANG_ENVIRONMENT_H
