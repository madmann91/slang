#ifndef SLANG_SEMA_H
#define SLANG_SEMA_H

#include <unordered_set>
#include <unordered_map>
#include <string>

#include "slang/environment.h"
#include "slang/logger.h"
#include "slang/types.h"
#include "slang/ast.h"
#include "slang/ptr.h"

namespace slang {

/// Helper class for semantic analysis. Types are hashed and stored uniquely,
/// which means type equality can be checked with pointer equality.
class Sema {
public:
    Sema(Logger& logger)
        : logger_(logger), env_(nullptr)
    {}

    ~Sema() {
        for (auto type : types_)
            delete type;
    }

    /// Returns the current environment.
    Environment* env() {
        return env_;
    }

    /// Pushes a new, empty environment on the stack.
    void push_env(const ast::Node* scope = nullptr) {
        Environment* env = new Environment(scope);
        env->set_parent(env_);
        env_list_.push_back(env);
        env_ = env;
    }

    /// Pops an environment from the stack.
    void pop_env() {
        assert(env_ != nullptr);
        env_ = env_->parent();
    }

    /// Creates a new identifier, if the name is not already used in the current environment.
    void new_identifier(const ast::Node* node, const std::string& name, Symbol&& symbol) {
        if (auto prev_symbol = env()->find_symbol(name)) {
            error(node) << "Identifier \'" << name << "\' has already been defined (line "
                        << prev_symbol->location().start().line() << ")\n";
        } else {
            env()->push_symbol(name, std::forward<Symbol>(symbol));
        }
    }

    /// Displays an error message with the Logger object.
    std::ostream& error(const ast::Node* node) {
        return logger_.error(node->loc());
    }

    /// Creates an error type. For expressions that fail typechecking.
    const ErrorType* error_type() { return new_type<ErrorType>(); }
    /// Creates a primitive type.
    const PrimType* prim_type(PrimType::Prim prim) { return new_type<PrimType>(prim); }
    /// Creates a function type from a return type and a list of arguments.
    const FunctionType* function_type(const Type* ret, const FunctionType::ArgList& args) {
        return new_type<FunctionType>(ret, args);
    }
    /// Creates a structure type from a list of members and a name.
    const StructType* struct_type(const std::string& name, const StructType::MemberMap& members) {
        return new_type<StructType>(name, members);
    }
    /// Creates an interface type type from a list of members and a name.
    const InterfaceType* interface_type(const std::string& name, const InterfaceType::MemberMap& members) {
        return new_type<InterfaceType>(name, members);
    }
    /// Creates an array whose size is unknown.
    const IndefiniteArrayType* indefinite_array_type(const Type* elem) {
        return new_type<IndefiniteArrayType>(elem);
    }
    /// Creates an array of known size.
    const DefiniteArrayType* definite_array_type(const Type* elem, int size) {
        return new_type<DefiniteArrayType>(elem, size);
    }

    /// Checks the type of an expression, and expects the given type as a result.
    const Type* check(const ast::Expr* expr, TypeExpectation expected) {
        const Type* found = expr->check(*this, expected);
        expr->assign_type(found);
        if (expected.type() && found != expected.type()) {
            error(expr) << "Expected \'" << expected.type()->to_string()
                        << "\', but found \'" << found->to_string()
                        << "\'\n";
        }
        return found;
    }

    /// Checks the type of an expression, without any constraint on the result type.
    const Type* check(const ast::Expr* expr) { return check_assign(expr, TypeExpectation(nullptr)); }
    /// Checks the type of an AST type.
    const Type* check(const ast::Type* type) { return check_assign(type); }
    /// Checks the type of a declaration.
    const Type* check(const ast::Decl* decl) { return check_assign(decl); }
    /// Checks a statement.
    void check(const ast::Stmt* stmt) { stmt->check(*this); }
    /// Checks the type of a function argument.
    const Type* check(const ast::Arg* arg) { return check_assign(arg); }
    /// Checks the type of a variable, given the type of the corresponding declaration.
    const Type* check(const ast::Variable* var, const slang::Type* var_type) { return check_assign(var, var_type); }

private:
    template <typename T>
    const Type* check_assign(const T* t) {
        const Type* found = t->check(*this);
        t->assign_type(found);
        return found;
    }

    template <typename T, typename... Args>
    const Type* check_assign(const T* t, Args... args) {
        const Type* found = t->check(*this, args...);
        t->assign_type(found);
        return found;
    }

    template <typename T, typename... Args>
    const T* new_type(Args... args) {
        T t(std::forward<Args>(args)...);
        auto it = types_.find(&t);
        if (it != types_.end())
            return (*it)->as<T>();

        const T* pt = new T(t);
        types_.emplace(pt);
        return pt;
    }

    struct HashType {
        size_t operator () (const Type* t) const {
            return t->hash();
        }
    };

    struct EqualType {
        bool operator () (const Type* t1, const Type* t2) const {
            return t1->equals(t2);
        }
    };

    Logger& logger_;

    std::unordered_set<const Type*, HashType, EqualType> types_;
    PtrVector<Environment> env_list_;
    Environment* env_;
};

} // namespace slang

#endif // SLANG_SEMA_H
