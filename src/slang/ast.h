#ifndef SLANG_AST_H
#define SLANG_AST_H

#include <vector>
#include <string>

#include "slang/location.h"
#include "slang/token.h"
#include "slang/environment.h"
#include "slang/cast.h"
#include "slang/ptr.h"

namespace slang {

class Printer;
class Sema;

namespace ast {

class Type;
class Expr;
class Decl;
class Stmt;

/// Base class for AST nodes.
class Node : public Cast<Node> {
public:
    Node() {}
    virtual ~Node() {}

    const Location& loc() const { return loc_; }
    void set_location(const Location& loc) { loc_ = loc; }
    virtual void print(Printer&) const = 0;

private:
    Location loc_;
};

/// Nodes that have a name.
class HasName {
public:
    HasName(const std::string& name = std::string()) : name_(name) {}
    virtual ~HasName() {}

    const std::string& name() const { return name_; }
    void set_name(const std::string& name) { name_ = name; }
    bool is_unnamed() const { return name_ == ""; }

protected:
    std::string name_;
};

/// Nodes that have an environment bound.
class HasEnv {
public:
    virtual ~HasEnv() {}

    void set_env(Environment* env) { env_ = env; }
    Environment* env() { return env_; }
    const Environment* env() const { return env_; }

protected:
    Environment* env_;
};

/// Nodes that have a semantic type associated with them.
/// Semantic types are assigned when the nodes are type-checked.
class Typeable {
public:
    Typeable() : assigned_type_(nullptr) {}
    virtual ~Typeable() {}

    void assign_type(const slang::Type* type) const { assigned_type_ = type; }
    const slang::Type* assigned_type() const { return assigned_type_; }

private:
    mutable const slang::Type* assigned_type_;
};

/// Base class for expressions.
class Expr : public Node, public Typeable {
public:
    virtual ~Expr() {}
    virtual const slang::Type* check(Sema&, const slang::Type*) const = 0;
};

/// List of expressions separated by a comma.
class ExprList : public Expr {
public:
    const PtrVector<Expr>& exprs() const { return exprs_; }
    void push_expr(Expr* expr) { exprs_.push_back(expr); }
    size_t num_exprs() const { return exprs_.size(); }

    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const override;

private:
    PtrVector<Expr> exprs_;
};

/// An expression that the parser could not parse.
class ErrorExpr : public Expr {
public:
    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const override;
};

/// An expression composed of a literal.
class LiteralExpr : public Expr {
public:
    Literal lit() const { return lit_; }
    void set_literal(const Literal& lit) { lit_ = lit; }

    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const override;

private:
    Literal lit_;
};

/// An expression composed of an identifier.
class IdentExpr : public Expr {
public:
    const std::string& ident() const { return ident_; }
    void set_ident(const std::string& ident) { ident_ = ident; }

    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const override;

private:
    std::string ident_;
};

/// A field selection expression.
class FieldExpr : public Expr {
public:
    Expr* left() { return left_.get(); }
    const Expr* left() const { return left_.get(); }
    void set_left(Expr* left) { left_.reset(left); }

    const std::string& field_name() const { return field_name_; }
    void set_field_name(const std::string& field_name) { field_name_ = field_name; }

    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const override;

private:
    std::string field_name_;
    Ptr<Expr> left_;
};

/// A array index expression.
class IndexExpr : public Expr {
public:
    Expr* left() { return left_.get(); }
    const Expr* left() const { return left_.get(); }
    void set_left(Expr* left) { left_.reset(left); }

    Expr* index() { return index_.get(); }
    const Expr* index() const { return index_.get(); }
    void set_index(Expr* index) { index_.reset(index); }

    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const override;

private:
    Ptr<Expr> left_, index_;
};

/// A function/constructor call expression.
class CallExpr : public Expr {
public:
    bool is_constructor() const { return static_cast<bool>(type_); }
    bool is_function() const { return static_cast<bool>(expr_); }

    const Expr* function() const { return expr_.get(); }
    void set_function(Expr* expr) { expr_.reset(expr); }

    const Type* constructor() const { return type_.get(); }
    void set_constructor(Type* type) { type_.reset(type); }

    const PtrVector<Expr>& args() const { return args_; }
    void push_arg(Expr* arg) { args_.push_back(arg); }
    size_t num_args() const { return args_.size(); }

    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const override;

private:
    PtrVector<Expr> args_;
    Ptr<Type> type_;
    Ptr<Expr> expr_;
};

class OpExpr : public Expr {
public:
    virtual ~OpExpr() {}
    virtual std::string op_string() const = 0;
};

/// An unary operation expression.
class UnOpExpr : public OpExpr {
public:
    enum Type {
        UNOP_INC,
        UNOP_DEC,
        UNOP_NOT,
        UNOP_BIT_NOT,
        UNOP_MINUS,
        UNOP_PLUS,
        UNOP_POST_INC,
        UNOP_POST_DEC,
        UNOP_UNKNOWN
    };

    UnOpExpr() : type_(UNOP_UNKNOWN) {}

    const Expr* operand() const { return op_.get(); }
    Expr* operand() { return op_.get(); }
    void set_operand(Expr* op) { op_.reset(op); }
    
    Type type() const { return type_; }
    void set_type(Type type) { type_ = type; }

    std::string op_string() const override;
    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const override;

private:
    Type type_;
    Ptr<Expr> op_;
};

/// A conditional expression (a ? b : c).
class CondExpr : public Expr {
public:
    Expr* cond() { return cond_.get(); }
    const Expr* cond() const { return cond_.get(); }
    void set_cond(Expr* cond) { cond_.reset(cond); }

    Expr* if_true() { return if_true_.get(); }
    const Expr* if_true() const { return if_true_.get(); }
    void set_if_true(Expr* if_true) { if_true_.reset(if_true); }

    Expr* if_false() { return if_false_.get(); }
    const Expr* if_false() const { return if_false_.get(); }
    void set_if_false(Expr* if_false) { if_false_.reset(if_false); }

    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const override;

private:
    Ptr<Expr> cond_, if_true_, if_false_;
};

/// An assignment expression (=, +=, -=, ...).
class AssignOpExpr : public OpExpr {
public:
    enum Type {
        ASSIGN_EQUAL,
        ASSIGN_ADD,
        ASSIGN_SUB,
        ASSIGN_MUL,
        ASSIGN_DIV,
        ASSIGN_MOD,
        ASSIGN_LSHIFT,
        ASSIGN_RSHIFT,
        ASSIGN_AND,
        ASSIGN_XOR,
        ASSIGN_OR,
        ASSIGN_UNKNOWN
    };

    AssignOpExpr() : type_(ASSIGN_UNKNOWN) {}

    Type type() const { return type_; }
    void set_type(Type type) { type_ = type; }

    Expr* left() { return left_.get(); }
    const Expr* left() const { return left_.get(); }
    void set_left(Expr* left) { left_.reset(left); }

    Expr* right() { return right_.get(); }
    const Expr* right() const { return right_.get(); }
    void set_right(Expr* right) { right_.reset(right); }

    std::string op_string() const override;
    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const override;

private:
    Type type_;
    Ptr<Expr> left_, right_;
};

/// A binary operation expression.
class BinOpExpr : public OpExpr {
public:
    enum Type {
        BINOP_MUL,
        BINOP_DIV,
        BINOP_MOD,
        BINOP_ADD,
        BINOP_SUB,
        BINOP_LSHIFT,
        BINOP_RSHIFT,
        BINOP_LT,
        BINOP_GT,
        BINOP_LEQ,
        BINOP_GEQ,
        BINOP_EQ,
        BINOP_NEQ,
        BINOP_AND,
        BINOP_XOR,
        BINOP_OR,
        BINOP_ANDAND,
        BINOP_XORXOR,
        BINOP_OROR,
        BINOP_UNKNOWN
    };

    BinOpExpr() : type_(BINOP_UNKNOWN) {}

    Expr* left() { return left_.get(); }
    const Expr* left() const { return left_.get(); }
    void set_left(Expr* left) { left_.reset(left); }

    Expr* right() { return right_.get(); }
    const Expr* right() const { return right_.get(); }
    void set_right(Expr* right) { right_.reset(right); }

    Type type() const { return type_; }
    void set_type(Type type) { type_ = type; }

    std::string op_string() const override;
    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const override;

private:
    Type type_;
    Ptr<Expr> left_, right_;
};

/// Structure initializer expression.
class InitExpr : public Expr {
public:
    const PtrVector<Expr>& exprs() const { return exprs_; }
    void push_expr(Expr* expr) { exprs_.push_back(expr); }
    size_t num_exprs() const { return exprs_.size(); }

    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const override;

private:
    PtrVector<Expr> exprs_;
};

/// Type qualifier, base class.
class TypeQualifier : public Node {
public:
    virtual ~TypeQualifier() {}
};

/// Storage qualifiers.
class StorageQualifier : public TypeQualifier {
public:
    enum Storage {
#define SLANG_KEY_QUAL_STORAGE(key, str) STORAGE_##key,
#include "slang/keywordlist.h"
    };

    Storage storage() const { return storage_; }
    void set_storage(Storage storage) { storage_ = storage; }

    void print(Printer&) const override;

protected:
    Storage storage_;
};

/// Precision qualifier.
class PrecisionQualifier : public TypeQualifier {
public:
    enum Precision {
#define SLANG_KEY_QUAL_PREC(key, str) PREC_##key,
#include "slang/keywordlist.h"
    };

    Precision precision() const { return prec_; }
    void set_precision(Precision prec) { prec_ = prec; }

    void print(Printer&) const override;

private:
    Precision prec_;
};

/// Interpolation qualifier.
class InterpQualifier : public TypeQualifier {
public:
    enum Interp {
#define SLANG_KEY_QUAL_INTERP(key, str) INTERP_##key,
#include "slang/keywordlist.h"
    };

    Interp interp() const { return interp_; }
    void set_interp(Interp interp) { interp_ = interp; }

    void print(Printer&) const override;

private:
    Interp interp_;
};

/// Subroutine qualifier (contains a list of type names).
class SubroutineQualifier : public TypeQualifier {
public:
    const std::vector<std::string>& names() const { return names_; }
    void push_name(const std::string& name) { names_.push_back(name); }
    size_t num_names() const { return names_.size(); }

    void print(Printer&) const override;

protected:
    std::vector<std::string> names_;
};

/// Layout qualifier (contains a map from identifiers to values).
class LayoutQualifier : public TypeQualifier {
public:
    const PtrMap<std::string, Expr>& layouts() const { return layouts_; }
    void push_layout(const std::string& name, Expr* expr) {
        assert(layouts_.find(name) == layouts_.end());
        layouts_.emplace(name, expr);
    }
    size_t num_layouts() const { return layouts_.size(); }

    void print(Printer&) const override;

private:
    PtrMap<std::string, Expr> layouts_;
};

/// Invariance qualifier.
class InvariantQualifier : public TypeQualifier {
public:
    void print(Printer&) const override;
};

/// Attribute qualifier (deprecated in GLSL 4.0).
class AttributeQualifier : public TypeQualifier {
public:
    void print(Printer&) const override;
};

/// Varying qualifier (deprecated in GLSL 4.0).
class VaryingQualifier : public TypeQualifier {
public:
    void print(Printer&) const override;
};

/// Array specifier, can have several dimensions.
class ArraySpecifier : public Node {
public:
    const PtrVector<Expr>& dims() const { return dims_; }
    void push_dim(Expr* dim) { dims_.push_back(dim); }
    size_t num_dims() const { return dims_.size(); }

    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const;

private:
    PtrVector<Expr> dims_;
};

/// Nodes that have an array specifier.
class HasArraySpecifier {
public:
    virtual ~HasArraySpecifier() {}

    const ArraySpecifier* array_specifier() const { return array_spec_.get(); }
    ArraySpecifier* array_specifier() { return array_spec_.get(); }
    void set_array_specifier(ArraySpecifier* array_spec) { array_spec_.reset(array_spec); }

protected:
    Ptr<ArraySpecifier> array_spec_;
};

/// Base class for types.
class Type : public Node, public Typeable, public HasArraySpecifier {
public:
    virtual ~Type() {}

    bool has_qualifier() const { return quals_.size() != 0; }

    const PtrVector<TypeQualifier>& qualifiers() const { return quals_; }
    void push_qualifier(TypeQualifier* qual) { quals_.push_back(qual); }
    size_t num_qualifers() const { return quals_.size(); }

    virtual const slang::Type* check(Sema&) const = 0;

protected:
    PtrVector<TypeQualifier> quals_;
};

/// Nodes that have a type.
class HasType {
public:
    virtual ~HasType() {}

    void set_type(Type* type) { type_.reset(type); }
    Type* type() { return type_.get(); }
    const Type* type() const { return type_.get(); }

protected:
    Ptr<Type> type_;
};

/// A type that the parser cannot parse.
class ErrorType : public Type {
public:
    void print(Printer&) const override;
    const slang::Type* check(Sema&) const override;
};

/// Primitive type.
class PrimType : public Type {
public:
    enum Prim {
#define SLANG_KEY_DATA(key, str, type, rows, cols) PRIM_##key,
#include "slang/keywordlist.h"
    };

    Prim prim() const { return prim_; }
    void set_prim(Prim prim) { prim_ = prim; }

    void print(Printer&) const override;
    const slang::Type* check(Sema&) const override;

private:
    Prim prim_;
};

/// A type referenced by a name (can be structure, or typedef).
class NamedType : public Type, public HasName {
public:
    void print(Printer&) const override;
    const slang::Type* check(Sema&) const override;
};

/// Base class for declarations.
class Decl : public Node, public Typeable {
public:
    virtual ~Decl() {}
    virtual const slang::Type* check(Sema&) const = 0;
};

/// A list of declarations.
class DeclList : public Node, public HasEnv {
public:
    const PtrVector<Decl>& decls() const { return decls_; }
    void push_decl(Decl* d) { decls_.push_back(d); }
    size_t num_decls() const { return decls_.size(); }

    void print(Printer&) const override;

private:
    PtrVector<Decl> decls_;
};

/// Base class for statements.
class Stmt : public Node {
public:
    virtual ~Stmt() {}
    virtual void check(Sema&) const = 0;
    virtual bool has_return() const { return false; }
};

/// A list of statements.
class StmtList : public Stmt, public HasEnv {
public:
    const PtrVector<Stmt>& stmts() const { return stmts_; }
    void push_stmt(Stmt* s) { stmts_.push_back(s); }
    size_t num_stmts() const { return stmts_.size(); }

    void print(Printer&) const override;
    void check(Sema&) const override;
    bool has_return() const override {
        for (auto stmt : stmts_) {
            if (stmt->has_return())
                return true;
        }
        return false;
    }

private:
    PtrVector<Stmt> stmts_;
};

/// A default precision declaration.
class PrecisionDecl : public Decl {
public:
    Type* type() { return type_.get(); }
    const Type* type() const { return type_.get(); }
    void set_type(Type* type) { type_.reset(type); }

    PrecisionQualifier* precision() { return prec_.get(); }
    const PrecisionQualifier* precision() const { return prec_.get(); }
    void set_precision(PrecisionQualifier* prec) { prec_.reset(prec); }

    void print(Printer&) const override;
    const slang::Type* check(Sema&) const override;

private:
    Ptr<Type> type_;
    Ptr<PrecisionQualifier> prec_;
};

/// A variable declaration.
class Variable : public Node, public Typeable, public HasName, public HasArraySpecifier {
public:
    Expr* init() { return init_.get(); }
    const Expr* init() const { return init_.get(); }
    void set_init(Expr* init) { init_.reset(init); }

    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const;

private:
    Ptr<Expr> init_;
};

/// A list of variable declarations.
class VariableDecl : public Decl, public HasType {
public:
    const PtrVector<Variable>& vars() const { return vars_; }
    void push_var(Variable* var) { vars_.push_back(var); }
    size_t num_vars() const { return vars_.size(); }

    void print(Printer&) const override;
    const slang::Type* check(Sema&) const override;

private:
    PtrVector<Variable> vars_;
};

/// Compound type : structure or interface block.
class CompoundType : public Type, public HasName {
public:
    const PtrVector<VariableDecl>& fields() const { return fields_; }
    void push_field(VariableDecl* field) { fields_.push_back(field); }
    size_t num_fields() const { return fields_.size(); }

protected:
    PtrVector<VariableDecl> fields_;
};

/// Structure type.
class StructType : public CompoundType {
public:
    void print(Printer&) const override;
    const slang::Type* check(Sema&) const override;
};

/// Interface block type.
class InterfaceType : public CompoundType {
public:
    void print(Printer&) const override;
    const slang::Type* check(Sema&) const override;
};

/// Function argument.
class Arg : public Node, public Typeable, public HasName, public HasType, public HasArraySpecifier {
public:
    void print(Printer&) const override;
    const slang::Type* check(Sema&) const;
};

/// Function prototype of function definition.
class FunctionDecl : public Decl, public HasType, public HasName {
public:
    bool is_prototype() const { return !static_cast<bool>(body_); }

    StmtList* body() { return body_.get(); }
    const StmtList* body() const { return body_.get(); }
    void set_body(StmtList* body) { body_.reset(body); }

    const PtrVector<Arg>& args() const { return args_; }
    void push_arg(Arg* arg) { args_.push_back(arg); }
    size_t num_args() const { return args_.size(); }

    void print(Printer&) const override;
    const slang::Type* check(Sema&) const override;

private:
    PtrVector<Arg> args_;
    Ptr<StmtList> body_;
};

/// Condition in a loop clause.
class LoopCond : public Node {
public:
    bool is_expr() const { return static_cast<bool>(expr_); }
    bool is_var() const { return static_cast<bool>(var_); }

    Expr* expr() { return expr_.get(); }
    const Expr* expr() const { return expr_.get(); }
    void set_expr(Expr* expr) { expr_.reset(expr); }

    Type* var_type() { return var_type_.get(); }
    const Type* var_type() const { return var_type_.get(); }
    void set_var_type(Type* type) { var_type_.reset(type); }

    Variable* var() { return var_.get(); }
    const Variable* var() const { return var_.get(); }
    void set_var(Variable* var) { var_.reset(var); }

    void print(Printer&) const override;
    void check(Sema& sema) const;

private:
    Ptr<Expr> expr_;
    Ptr<Variable> var_;
    Ptr<Type> var_type_;
};

/// A declaration statement.
class DeclStmt : public Stmt {
public:
    Decl* decl() { return decl_.get(); }
    const Decl* decl() const { return decl_.get(); }
    void set_decl(Decl* decl) { decl_.reset(decl); }

    void print(Printer&) const override;
    void check(Sema&) const override;

private:
    Ptr<Decl> decl_;
};

/// A declaration statement.
class ExprStmt : public Stmt {
public:
    Expr* expr() { return expr_.get(); }
    const Expr* expr() const { return expr_.get(); }
    void set_expr(Expr* expr) { expr_.reset(expr); }

    void print(Printer&) const override;
    void check(Sema&) const override;

private:
    Ptr<Expr> expr_;
};

/// If-else statement.
class IfStmt : public Stmt {
public:
    Expr* cond() { return cond_.get(); }
    const Expr* cond() const { return cond_.get(); }
    void set_cond(Expr* cond) { cond_.reset(cond); }

    Stmt* if_true() { return if_true_.get(); }
    const Stmt* if_true() const { return if_true_.get(); }
    void set_if_true(Stmt* if_true) { if_true_.reset(if_true); }

    Stmt* if_false() { return if_false_.get(); }
    const Stmt* if_false() const { return if_false_.get(); }
    void set_if_false(Stmt* if_false) { if_false_.reset(if_false); }

    void print(Printer&) const override;
    void check(Sema&) const override;
    bool has_return() const override {
        return if_true_->has_return() || (if_false_ && if_false_->has_return());
    }

private:
    Ptr<Expr> cond_;
    Ptr<Stmt> if_true_, if_false_;
};

/// Switch statement
class SwitchStmt : public Stmt {
public:
    Expr* expr() { return expr_.get(); }
    const Expr* expr() const { return expr_.get(); }
    void set_expr(Expr* expr) { expr_.reset(expr); }

    StmtList* list() { return list_.get(); }
    const StmtList* list() const { return list_.get(); }
    void set_list(StmtList* list) { list_.reset(list); }

    void print(Printer&) const override;
    void check(Sema&) const override;
    bool has_return() const override { return list_->has_return(); }

private:
    Ptr<StmtList> list_;
    Ptr<Expr> expr_;
};

/// Case label statement.
class CaseLabelStmt : public Stmt {
public:
    Expr* expr() { return expr_.get(); }
    const Expr* expr() const { return expr_.get(); }
    void set_expr(Expr* expr) { expr_.reset(expr); }

    bool is_default() const { return !static_cast<bool>(expr_); }

    void print(Printer&) const override;
    void check(Sema&) const override;

private:
    Ptr<Expr> expr_;
};

/// Base class for loop statements.
class LoopStmt : public Stmt {
public:
    virtual ~LoopStmt() {}

    LoopCond* cond() { return cond_.get(); }
    const LoopCond* cond() const { return cond_.get(); }
    void set_cond(LoopCond* cond) { cond_.reset(cond); }

    Stmt* body() { return body_.get(); }
    const Stmt* body() const { return body_.get(); }
    void set_body(Stmt* body) { body_.reset(body); }

    bool has_return() const override { return body_->has_return(); }

protected:
    Ptr<LoopCond> cond_;
    Ptr<Stmt> body_;
};

/// For loop statement.
class ForLoopStmt : public LoopStmt {
public:
    Stmt* init() { return init_.get(); }
    const Stmt* init() const { return init_.get(); }
    void set_init(Stmt* init) { init_.reset(init); }

    Expr* iter() { return iter_.get(); }
    const Expr* iter() const { return iter_.get(); }
    void set_iter(Expr* iter) { iter_.reset(iter); }

    void print(Printer&) const override;
    void check(Sema&) const override;

private:
    Ptr<Stmt> init_;
    Ptr<Expr> iter_;
};

/// While loop statement.
class WhileLoopStmt : public LoopStmt {
public:
    void print(Printer&) const override;
    void check(Sema&) const override;
};

/// Do-While loop statement.
class DoWhileLoopStmt : public LoopStmt {
public:
    void print(Printer&) const override;
    void check(Sema&) const override;
};

/// Break statement.
class BreakStmt : public Stmt {
public:
    void print(Printer& out) const override;
    void check(Sema&) const override;
};

/// Continue statement.
class ContinueStmt : public Stmt {
public:
    void print(Printer& out) const override;
    void check(Sema&) const override;
};

/// Discard statement
class DiscardStmt : public Stmt {
public:
    void print(Printer& out) const override;
    void check(Sema&) const override;
};

/// Return statement.
class ReturnStmt : public Stmt {
public:
    Expr* value() { return value_.get(); }
    const Expr* value() const { return value_.get(); }
    void set_value(Expr* value) { value_.reset(value); }

    bool has_value() const { return static_cast<bool>(value_);}

    void print(Printer& out) const override;
    void check(Sema&) const override;
    bool has_return() const override { return true; }

private:
    Ptr<Expr> value_;
};

} // namespace ast

} // namespace slang

#endif // SLANG_AST_H
