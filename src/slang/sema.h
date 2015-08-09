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

/// Helper class for semantic analysis.
class Sema : public TypeTable {
public:
    Sema(Logger& logger)
        : logger_(logger), env_(nullptr), err_count_(0), warn_count_(0) {
        push_env();
        builtin_env_ = env_;
        push_env();
    }

    ~Sema() {
        pop_env(2);
        for (auto type : types_)
            delete type;
    }

    /// Returns the current environment.
    Environment* env() { return env_; }
    /// Returns the builtin environment.
    Environment* builtin_env() { return builtin_env_; }

    /// Pushes a new, empty environment on the stack.
    void push_env(const ast::Node* scope = nullptr) {
        Environment* env = new Environment(scope);
        env->set_parent(env_);
        env_list_.push_back(env);
        env_ = env;
    }

    /// Pops one or several environments from the stack.
    void pop_env(int n = 1) {
        while (n > 0) {
            assert(env_ != nullptr);
            env_ = env_->parent();
            n--;
        }
    }

    /// Creates a new identifier, if the name is not already used in the current environment.
    void new_symbol(const std::string&, const Type*, const ast::Node*);
    /// Emits the "symbol already defined" error message.
    void error_redefinition(const std::string&, const Symbol*, const ast::Node*);

    /// Implicit convert the given primitive types so that their fundamental types match.
    void implicit_convert(const PrimType*&, const PrimType*&);
    /// Implicit convert the given types.
    void implicit_convert(const Type*&, const Type*&);

    /// Expects identical types in an AST node.
    bool expect_type(const ast::Node*, const Type*, const Type*);
    /// Expects a non-void type as an argument or variable.
    bool expect_nonvoid(const ast::Node*, const std::string&, const Type*);
    /// Expects equal primitive types inside an operator.
    bool expect_equal(const ast::OpExpr*, const PrimType*, const PrimType*);
    /// Expects compatible primitive types inside an operator (e.g. for scalar vs. vector operators).
    bool expect_compatible(const ast::OpExpr*, const PrimType*, const PrimType*);
    /// Expects a numeric type in an operator (that can be added, subtracted, ...).
    bool expect_numeric(const ast::OpExpr*, const PrimType*);
    /// Expects an integer type in an operator (uint or int).
    bool expect_integer(const ast::OpExpr*, const PrimType*);
    /// Expects a comparable type in an operator (that can be compared with >, <, ...).
    bool expect_ordered(const ast::OpExpr*, const PrimType*);
    /// Expects a boolean type in an operator.
    bool expect_boolean(const ast::OpExpr*, const PrimType*);
    /// Expects a floating point type in an operator (float or double).
    bool expect_floating(const ast::OpExpr*, const PrimType*);
    /// Expects an l-value.
    bool expect_lvalue(const ast::Expr*);

    /// Checks the type of an expression, and expects the given type as a result.
    const Type* check(const ast::Expr* expr, const Type* expected) {
        const Type* found = check_assign(expr, expected);
        if (expected && !found->isa<ErrorType>())
            expect_type(expr, found, expected);
        return found;
    }

    /// Checks the type of an expression, without any constraint on the result type.
    const Type* check(const ast::Expr* expr) { return check_assign(expr, nullptr); }
    /// Checks the type of an AST type.
    const Type* check(const ast::Type* type) { return check_assign(type); }
    /// Checks the type of a declaration.
    const Type* check(const ast::Decl* decl) { return check_assign(decl); }
    /// Checks a statement.
    void check(const ast::Stmt* stmt) { stmt->check(*this); }
    /// Checks a loop condition.
    void check(const ast::LoopCond* cond) { cond->check(*this); }
    /// Checks the type of a function argument.
    const Type* check(const ast::Arg* arg) { return check_assign(arg); }
    /// Checks the type of a variable, given the type of the corresponding declaration.
    const Type* check(const ast::Variable* var,
                      const Type* var_type,
                      const Type::Qualifier qual) {
        return check_assign(var, var_type, qual);
    }

    /// Returns the number of errors generated during type checking.
    size_t error_count() const { return err_count_; }
    /// Returns the number of warnings generated during type checking.
    size_t warn_count() const { return warn_count_; }

    /// Displays an error message with the Logger object.
    std::ostream& error(const ast::Node* node) {
        err_count_++;
        return logger_.error(node->loc());
    }

    /// Displays a warning message with the Logger object.
    std::ostream& warn(const ast::Node* node) {
        warn_count_++;
        return logger_.warn(node->loc());
    }

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

    Logger& logger_;
    PtrVector<Environment> env_list_;
    Environment* env_, *builtin_env_;
    size_t err_count_, warn_count_;
};

} // namespace slang

#endif // SLANG_SEMA_H
