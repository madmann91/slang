#include "environment.h"
#include "ast.h"

namespace slang {

template <typename T>
inline bool is_node(const Symbol::DefMap& defs) {
    assert(!defs.empty());
    assert(defs.begin()->second);
    return defs.begin()->second->isa<T>() != nullptr;
}

bool Symbol::is_function() const { return is_node<ast::FunctionDecl>(defs_); }
bool Symbol::is_structure() const { return is_node<ast::StructType>(defs_); }
bool Symbol::is_interface() const { return is_node<ast::InterfaceType>(defs_); }
bool Symbol::is_variable() const { return is_node<ast::Variable>(defs_); }
bool Symbol::is_argument() const { return is_node<ast::Arg>(defs_); }

const slang::Type* Symbol::type() const { assert(!defs_.empty()); return defs_.begin()->first; }
const Location& Symbol::location() const { assert(!defs_.empty()); return defs_.begin()->second->loc(); }

Symbol* Environment::lookup_symbol(const std::string& name) {
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

Symbol* Environment::find_symbol(const std::string& name) {
    auto it = symbols_.find(name);
    if (it != symbols_.end())
        return &it->second;
    return nullptr;
}

template <typename T>
inline const ast::Node* closest_node(const Environment* env) {
    while (env && !env->scope()->isa<T>()) {
        env = env->parent();
    }
    return env ? env->scope() : nullptr;
}

const ast::Node* Environment::closest_function() const { return closest_node<ast::FunctionDecl>(this); }
const ast::Node* Environment::closest_loop() const { return closest_node<ast::LoopStmt>(this); }
const ast::Node* Environment::closest_switch() const { return closest_node<ast::SwitchStmt>(this); }

} // namespace slang