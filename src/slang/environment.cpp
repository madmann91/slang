#include "slang/environment.h"
#include "slang/ast.h"
#include "slang/types.h"

namespace slang {

Symbol::Symbol(DefMap&& defs)
    : defs_(defs)
{
    assert(defs.size() > 0);
    type_ = defs.begin()->first;
    loc_ = defs.begin()->second->loc();
}

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

void Symbol::push_def(const Type* type, const ast::Node* node) {
    defs_.insert(std::make_pair(type, node));
}

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

void Environment::push_symbol(const std::string& name, Symbol&& symbol) {
    assert(symbols_.find(name) == symbols_.end());
    symbols_.emplace(name, std::move(symbol));
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
