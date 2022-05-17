#ifndef SLANG_AST_H
#define SLANG_AST_H

#include <vector>
#include <string>
#include <algorithm>

#include "slang/location.h"
#include "slang/token.h"
#include "slang/types.h"
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

/// A module, which contains the list of top-level declarations.
class Module : public Node {
public:
    PtrVector<Decl>& decls() { return decls_; }
    PtrVectorView<Decl> decls() const { return make_view(decls_); }
    void push_decl(Decl* d) { decls_.emplace_back(d); }
    size_t num_decls() const { return decls_.size(); }
    const Decl* decl(int i) const { return decls_[i].get(); }

    void print(Printer&) const override;

private:
    PtrVector<Decl> decls_;
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

/// Nodes that have a semantic type associated with them.
/// Semantic types are assigned when the nodes are type-checked.
template <typename T>
class Typeable {
public:
    Typeable() : assigned_type_(nullptr) {}
    virtual ~Typeable() {}

    void assign_type(const T& type) const { assigned_type_ = type; }
    T assigned_type() const { return assigned_type_; }

private:
    mutable T assigned_type_;
};

/// Base class for expressions.
class Expr : public Node, public Typeable<const slang::Type*> {
public:
    virtual ~Expr() {}
    virtual const slang::Type* check(Sema&, const slang::Type*) const = 0;
    virtual bool is_lvalue(Sema&) const { return false; }
    virtual bool is_constant(Sema&) const { return false; }
};

/// List of expressions separated by a comma.
class ExprList : public Expr {
public:
    PtrVector<Expr>& exprs() { return exprs_; }
    PtrVectorView<Expr> exprs() const { return make_view(exprs_); }
    void push_expr(Expr* expr) { exprs_.emplace_back(expr); }
    size_t num_exprs() const { return exprs_.size(); }
    const Expr* expr(int i) const { return exprs_[i].get(); }

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

    bool is_lvalue(Sema&) const override;

private:
    std::string ident_;
};

/// A field selection expression.
class FieldExpr : public Expr {
public:
    Ptr<Expr>& left() { return left_; }
    const Expr* left() const { return left_.get(); }

    const std::string& field_name() const { return field_name_; }
    void set_field_name(const std::string& field_name) { field_name_ = field_name; }

    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const override;

    bool is_lvalue(Sema&) const override;

private:
    std::string field_name_;
    Ptr<Expr> left_;
};

/// A array index expression.
class IndexExpr : public Expr {
public:
    Ptr<Expr>& left() { return left_; }
    const Expr* left() const { return left_.get(); }

    Ptr<Expr>& index() { return index_; }
    const Expr* index() const { return index_.get(); }

    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const override;

    bool is_lvalue(Sema&) const override;

private:
    Ptr<Expr> left_, index_;
};

/// A function/constructor call expression.
class CallExpr : public Expr {
public:
    bool is_constructor() const { return static_cast<bool>(type_); }
    bool is_function() const { return static_cast<bool>(expr_); }

    Ptr<Expr>& function() { return expr_; }
    const Expr* function() const { return expr_.get(); }

    Ptr<Type>& constructor() { return type_; }
    const Type* constructor() const { return type_.get(); }

    PtrVector<Expr>& args() { return args_; }
    PtrVectorView<Expr> args() const { return make_view(args_); }
    void push_arg(Expr* arg) { args_.emplace_back(arg); }
    size_t num_args() const { return args_.size(); }
    const Expr* arg(int i) const { return args_[i].get(); }

    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const override;

private:
    PtrVector<Expr> args_;
    Ptr<Type> type_;
    Ptr<Expr> expr_;
};

/// An expression which contains an operator.
class OpExpr : public Expr {
public:
    virtual ~OpExpr() {}
    virtual std::string op_string() const = 0;
};

/// An unary operation expression.
class UnOpExpr : public OpExpr {
public:
    enum Type {
        INC,       ///< Pre-increment operator
        DEC,       ///< Pre-decrement operator
        NOT,       ///< Logical negation
        BIT_NOT,   ///< Bitwise negation
        MINUS,     ///< Minus sign
        PLUS,      ///< Plus sign
        POST_INC,  ///< Post-increment
        POST_DEC,  ///< Post-decrement
        UNKNOWN
    };

    UnOpExpr() : type_(UNKNOWN) {}

    Ptr<Expr>& operand() { return op_; }
    const Expr* operand() const { return op_.get(); }
    
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
    Ptr<Expr>& cond() { return cond_; }
    const Expr* cond() const { return cond_.get(); }

    Ptr<Expr>& if_true() { return if_true_; }
    const Expr* if_true() const { return if_true_.get(); }

    Ptr<Expr>& if_false() { return if_false_; }
    const Expr* if_false() const { return if_false_.get(); }

    void print(Printer&) const override;
    const slang::Type* check(Sema&, const slang::Type*) const override;

private:
    Ptr<Expr> cond_, if_true_, if_false_;
};

/// An assignment expression (=, +=, -=, ...).
class AssignOpExpr : public OpExpr {
public:
    enum Type {
        ASSIGN,         ///< Assignment operator (=)
        ASSIGN_ADD,     ///< Addition (+=)
        ASSIGN_SUB,     ///< Subtraction (-=)
        ASSIGN_MUL,     ///< Multiplication (*=)
        ASSIGN_DIV,     ///< Division (/=)
        ASSIGN_MOD,     ///< Modulus (%=)
        ASSIGN_LSHIFT,  ///< Left shift (<<=)
        ASSIGN_RSHIFT,  ///< Right shift (>>=)
        ASSIGN_AND,     ///< Bitwise and (&=)
        ASSIGN_XOR,     ///< Bitwise xor (^=)
        ASSIGN_OR,      ///< Bitwise or (|=)
        UNKNOWN
    };

    AssignOpExpr() : type_(UNKNOWN) {}

    Type type() const { return type_; }
    void set_type(Type type) { type_ = type; }

    Ptr<Expr>& left() { return left_; }
    const Expr* left() const { return left_.get(); }

    Ptr<Expr>& right() { return right_; }
    const Expr* right() const { return right_.get(); }

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
        MUL,        ///< Multiplication (*)
        DIV,        ///< Division (/)
        MOD,        ///< Modulus (%)
        ADD,        ///< Addition (+)
        SUB,        ///< Subtraction (-)
        LSHIFT,     ///< Left shift (<<)
        RSHIFT,     ///< Right shift (>>)
        LT,         ///< Less than (<)
        GT,         ///< Greater than (>)
        LEQ,        ///< Less or equal (<=)
        GEQ,        ///< Greater or equal (>=)
        EQ,         ///< Equal (==)
        NEQ,        ///< Not equal (!=)
        AND,        ///< And (&)
        XOR,        ///< Xor (^)
        OR,         ///< Or (|)
        ANDAND,     ///< Logical and (&&)
        XORXOR,     ///< Logical Xor (^^)
        OROR,       ///< Logical or (||)
        UNKNOWN
    };

    BinOpExpr() : type_(UNKNOWN) {}

    Type type() const { return type_; }
    void set_type(Type type) { type_ = type; }

    Ptr<Expr>& left() { return left_; }
    const Expr* left() const { return left_.get(); }

    Ptr<Expr>& right() { return right_; }
    const Expr* right() const { return right_.get(); }

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
    PtrVector<Expr>& exprs() { return exprs_; }
    PtrVectorView<Expr> exprs() const { return make_view(exprs_); }
    void push_expr(Expr* expr) { exprs_.emplace_back(expr); }
    size_t num_exprs() const { return exprs_.size(); }
    const Expr* expr(int i) const { return exprs_[i].get(); }

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
#define SLANG_KEY_QUAL_STORAGE(key, str) key,
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
#define SLANG_KEY_QUAL_PREC(key, str) key,
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
#define SLANG_KEY_QUAL_INTERP(key, str) key,
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
    using Layout = std::pair<std::string, Ptr<Expr>>;

    std::vector<Layout>& layouts() { return layouts_; }
    void push_layout(const std::string& name, Expr* expr) {
        assert(!find_layout(name));
        layouts_.emplace_back(name, Ptr<Expr>(expr));
    }
    const Expr* find_layout(const std::string& name) const {
        auto it = std::find_if(layouts_.begin(), layouts_.end(), [&] (const Layout& layout) {
            return layout.first == name;
        });
        return it != layouts_.end() ? it->second.get() : NULL;
    }
    size_t num_layouts() const { return layouts_.size(); }

    void print(Printer&) const override;

private:
    std::vector<Layout> layouts_;
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
    PtrVector<Expr>& dims() { return dims_; }
    PtrVectorView<Expr> dims() const { return make_view(dims_); }
    void push_dim(Expr* dim) { dims_.emplace_back(dim); }
    size_t num_dims() const { return dims_.size(); }
    const Expr* dim(int i) const { return dims_[i].get(); }

    void print(Printer&) const override;

private:
    PtrVector<Expr> dims_;
};

/// Nodes that have an array specifier.
class HasArraySpecifier {
public:
    virtual ~HasArraySpecifier() {}

    Ptr<ArraySpecifier>& array_specifier() { return array_spec_; }
    const ArraySpecifier* array_specifier() const { return array_spec_.get(); }

protected:
    Ptr<ArraySpecifier> array_spec_;
};

/// Base class for types.
class Type : public Node,
             public Typeable<slang::QualifiedType>,
             public HasArraySpecifier {
public:
    virtual ~Type() {}

    bool has_qualifier() const { return quals_.size() != 0; }

    PtrVector<TypeQualifier>& qualifiers() { return quals_; }
    PtrVectorView<TypeQualifier> qualifiers() const { return make_view(quals_); }
    void push_qualifier(TypeQualifier* qual) { quals_.emplace_back(qual); }
    size_t num_qualifers() const { return quals_.size(); }
    const TypeQualifier* qualifier(int i) const { return quals_[i].get(); }

    virtual slang::QualifiedType check(Sema&) const = 0;

protected:
    PtrVector<TypeQualifier> quals_;
};

/// Nodes that have a type.
class HasType {
public:
    virtual ~HasType() {}

    Ptr<Type>& type() { return type_; }
    const Type* type() const { return type_.get(); }

protected:
    Ptr<Type> type_;
};

/// A type that the parser cannot parse.
class ErrorType : public Type {
public:
    void print(Printer&) const override;
    slang::QualifiedType check(Sema&) const override;
};

/// Primitive type.
class PrimType : public Type {
public:
    enum Prim {
#define SLANG_KEY_DATA(key, str, type, rows, cols) key,
#include "slang/keywordlist.h"
    };

    Prim prim() const { return prim_; }
    void set_prim(Prim prim) { prim_ = prim; }

    void print(Printer&) const override;
    slang::QualifiedType check(Sema&) const override;

private:
    Prim prim_;
};

/// A type referenced by a name (can be structure, or typedef).
class NamedType : public Type, public HasName {
public:
    void print(Printer&) const override;
    slang::QualifiedType check(Sema&) const override;
};

/// Base class for declarations.
class Decl : public Node, public Typeable<slang::QualifiedType> {
public:
    virtual ~Decl() {}
    virtual slang::QualifiedType check(Sema&) const = 0;
};

/// Base class for statements.
class Stmt : public Node {
public:
    virtual ~Stmt() {}
    virtual void check(Sema&) const = 0;
};

/// A list of statements.
class StmtList : public Stmt {
public:
    PtrVector<Stmt>& stmts() { return stmts_; }
    PtrVectorView<Stmt> stmts() const { return make_view(stmts_); }
    void push_stmt(Stmt* s) { stmts_.emplace_back(s); }
    size_t num_stmts() const { return stmts_.size(); }
    const Stmt* stmt(int i) const { return stmts_[i].get(); }

    void print(Printer&) const override;
    void check(Sema&) const override;

private:
    PtrVector<Stmt> stmts_;
};

/// A default precision declaration.
class PrecisionDecl : public Decl, public HasType {
public:
    Ptr<PrecisionQualifier>& precision() { return prec_; }
    const PrecisionQualifier* precision() const { return prec_.get(); }

    void print(Printer&) const override;
    slang::QualifiedType check(Sema&) const override;

private:
    Ptr<PrecisionQualifier> prec_;
};

/// A variable declaration.
class Variable : public Node,
                 public Typeable<slang::QualifiedType>,
                 public HasName, public HasArraySpecifier {
public:
    Ptr<Expr>& init() { return init_; }
    const Expr* init() const { return init_.get(); }

    void print(Printer&) const override;
    slang::QualifiedType check(Sema&, slang::QualifiedType) const;

private:
    Ptr<Expr> init_;
};

/// A list of variable declarations.
class VariableDecl : public Decl, public HasType {
public:
    PtrVector<Variable>& vars() { return vars_; }
    PtrVectorView<Variable> vars() const { return make_view(vars_); }
    void push_var(Variable* var) { vars_.emplace_back(var); }
    size_t num_vars() const { return vars_.size(); }
    const Variable* var(int i) const { return vars_[i].get(); }

    void print(Printer&) const override;
    slang::QualifiedType check(Sema&) const override;

private:
    PtrVector<Variable> vars_;
};

/// Compound type : structure or interface block.
class CompoundType : public Type, public HasName {
public:
    PtrVector<VariableDecl>& fields() { return fields_; }
    PtrVectorView<VariableDecl> fields() const { return make_view(fields_); }
    void push_field(VariableDecl* field) { fields_.emplace_back(field); }
    size_t num_fields() const { return fields_.size(); }
    const VariableDecl* field(int i) const { return fields_[i].get(); }

protected:
    PtrVector<VariableDecl> fields_;
};

/// Structure type.
class StructType : public CompoundType {
public:
    void print(Printer&) const override;
    slang::QualifiedType check(Sema&) const override;
};

/// Interface block type.
class InterfaceType : public CompoundType {
public:
    void print(Printer&) const override;
    slang::QualifiedType check(Sema&) const override;
};

/// Function argument.
class Arg : public Node,
            public Typeable<slang::QualifiedType>,
            public HasName, public HasType,
            public HasArraySpecifier {
public:
    void print(Printer&) const override;
    slang::QualifiedType check(Sema&) const;
};

/// Function prototype of function definition.
class FunctionDecl : public Decl, public HasType, public HasName {
public:
    bool is_prototype() const { return !static_cast<bool>(body_); }

    Ptr<StmtList>& body() { return body_; }
    const StmtList* body() const { return body_.get(); }

    PtrVector<Arg>& args() { return args_; }
    PtrVectorView<Arg> args() const { return make_view(args_); }
    void push_arg(Arg* arg) { args_.emplace_back(arg); }
    size_t num_args() const { return args_.size(); }
    const Arg* arg(int i) const { return args_[i].get(); }

    void print(Printer&) const override;
    slang::QualifiedType check(Sema&) const override;

private:
    PtrVector<Arg> args_;
    Ptr<StmtList> body_;
};

/// Condition in a loop clause.
class LoopCond : public Node {
public:
    bool is_expr() const { return static_cast<bool>(expr_); }
    bool is_var() const { return static_cast<bool>(var_); }

    Ptr<Expr>& expr() { return expr_; }
    const Expr* expr() const { return expr_.get(); }

    Ptr<Type>& var_type() { return var_type_; }
    const Type* var_type() const { return var_type_.get(); }

    Ptr<Variable>& var() { return var_; }
    const Variable* var() const { return var_.get(); }

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
    Ptr<Decl>& decl() { return decl_; }
    const Decl* decl() const { return decl_.get(); }

    void print(Printer&) const override;
    void check(Sema&) const override;

private:
    Ptr<Decl> decl_;
};

/// A declaration statement.
class ExprStmt : public Stmt {
public:
    Ptr<Expr>& expr() { return expr_; }
    const Expr* expr() const { return expr_.get(); }

    void print(Printer&) const override;
    void check(Sema&) const override;

private:
    Ptr<Expr> expr_;
};

/// If-else statement.
class IfStmt : public Stmt {
public:
    Ptr<Expr>& cond() { return cond_; }
    const Expr* cond() const { return cond_.get(); }

    Ptr<Stmt>& if_true() { return if_true_; }
    const Stmt* if_true() const { return if_true_.get(); }

    Ptr<Stmt>& if_false() { return if_false_; }
    const Stmt* if_false() const { return if_false_.get(); }

    void print(Printer&) const override;
    void check(Sema&) const override;

private:
    Ptr<Expr> cond_;
    Ptr<Stmt> if_true_, if_false_;
};

/// Switch statement
class SwitchStmt : public Stmt {
public:
    Ptr<Expr>& expr() { return expr_; }
    const Expr* expr() const { return expr_.get(); }

    Ptr<StmtList>& list() { return list_; }
    const StmtList* list() const { return list_.get(); }

    void print(Printer&) const override;
    void check(Sema&) const override;

private:
    Ptr<StmtList> list_;
    Ptr<Expr> expr_;
};

/// Case label statement.
class CaseLabelStmt : public Stmt {
public:
    Ptr<Expr>& expr() { return expr_; }
    const Expr* expr() const { return expr_.get(); }

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

    Ptr<LoopCond>& cond() { return cond_; }
    const LoopCond* cond() const { return cond_.get(); }

    Ptr<Stmt>& body() { return body_; }
    const Stmt* body() const { return body_.get(); }

protected:
    Ptr<LoopCond> cond_;
    Ptr<Stmt> body_;
};

/// For loop statement.
class ForLoopStmt : public LoopStmt {
public:
    Ptr<Stmt>& init() { return init_; }
    const Stmt* init() const { return init_.get(); }

    Ptr<Expr>& iter() { return iter_; }
    const Expr* iter() const { return iter_.get(); }

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
    Ptr<Expr>& value() { return value_; }
    const Expr* value() const { return value_.get(); }

    bool has_value() const { return static_cast<bool>(value_);}

    void print(Printer& out) const override;
    void check(Sema&) const override;

private:
    Ptr<Expr> value_;
};

} // namespace ast

} // namespace slang

#endif // SLANG_AST_H
