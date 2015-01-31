#ifndef SLANG_AST_H
#define SLANG_AST_H

#include <vector>
#include <string>
#include <ostream>

#include "slang/location.h"
#include "slang/token.h"
#include "slang/environment.h"

namespace slang {

namespace ast {

class Node {
public:
    Node() {}
    virtual ~Node() {}

    const Location& loc() const { return loc_; }
    void set_location(const Location& loc) { loc_ = loc; }
    virtual void print(std::ostream&) const = 0;

private:
    Location loc_;
};

/// Nodes that have a name
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

/// Nodes that have an environment bound
class HasEnv {
public:
    HasEnv(Environment* env = nullptr) : env_(env) {}
    virtual ~HasEnv() { delete env_; }

    void set_env(Environment* env) { env_ = env; }
    Environment* env() { return env_; }
    const Environment* env() const { return env_; }

protected:
    Environment* env_;
};

/// A list of nodes with an associated environment.
/// Can represent the root node and scopes (function bodies, if/else, ...).
class List : public Node, public HasEnv {
public:
    virtual ~List() {
        for (auto n : nodes_) delete n;
    }

    const std::vector<Node*>& nodes() const { return nodes_; }
    void push_node(Node* n) { nodes_.push_back(n); }
    int num_nodes() const { return nodes_.size(); }

    void print(std::ostream&) const;

private:
    std::vector<Node*> nodes_;
};

/// Base class for expressions
class Expr : public Node {
};

/// An expression that the parser could not parse
class ErrorExpr : public Expr {
public:
    void print(std::ostream&) const;
};

/// An expression composed of a literal
class LiteralExpr : public Expr {
public:
    Literal lit() const { return lit_; }
    void set_literal(const Literal& lit) { lit_ = lit; }

    void print(std::ostream&) const;

private:
    Literal lit_;
};

/// An expression composed of an identifier
class IdentExpr : public Expr, public HasName {
public:
    void print(std::ostream&) const;
};

/// A field selection expression
class FieldExpr : public Expr {
public:
    FieldExpr() : left_(nullptr) {}
    virtual ~FieldExpr() { delete left_; }

    Expr* left() { return left_; }
    const Expr* left() const { return left_; }
    void set_left(Expr* left) { left_ = left; }

    const std::string& field_name() const { return field_name_; }
    void set_field_name(const std::string& field_name) { field_name_ = field_name; }

    void print(std::ostream&) const;

private:
    std::string field_name_;
    Expr* left_;
};

/// A array index expression
class IndexExpr : public Expr {
public:
    IndexExpr() : left_(nullptr), index_(nullptr) {}

    virtual ~IndexExpr() {
        delete left_;
        delete index_;
    }

    Expr* left() { return left_; }
    const Expr* left() const { return left_; }
    void set_left(Expr* left) { left_ = left; }

    Expr* index() { return index_; }
    const Expr* index() const { return index_; }
    void set_index(Expr* index) { index_ = index; }

    void print(std::ostream&) const;

private:
    Expr* left_;
    Expr* index_;
};

/// An unary operation expression
class UnOpExpr : public Expr {
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

    UnOpExpr() : type_(UNOP_UNKNOWN), op_(nullptr) {}
    virtual ~UnOpExpr() { delete op_; }

    const Expr* operand() const { return op_; }
    Expr* operand() { return op_; }
    
    Type type() const { return type_; }

    void set_operand(Expr* op) { op_ = op; }
    void set_type(Type type) { type_ = type; }

    void print(std::ostream&) const;

private:
    Type type_;
    Expr* op_;
};

/// A binary operation expression
class BinOpExpr : public Expr {
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
        BINOP_OROR,
        BINOP_UNKNOWN
    };

    BinOpExpr() : left_(nullptr), right_(nullptr), type_(BINOP_UNKNOWN) {}

    Expr* left() { return left_; }
    const Expr* left() const { return left_; }

    Expr* right() { return right_; }
    const Expr* right() const { return right_; }

    Type type() const { return type_; }

    void set_left(Expr* left) { left_ = left; }
    void set_right(Expr* left) { right_ = left; }
    void set_type(Type type) { type_ = type; }

    void print(std::ostream&) const;

private:
    Expr* left_;
    Expr* right_;
    Type type_;
};

/// Type qualifier, base class
class TypeQualifier : public Node {
public:
    virtual ~TypeQualifier() {}
};

/// Storage qualifiers
class StorageQualifier : public TypeQualifier {
public:
    enum Storage {
#define SLANG_KEY_QUAL_STORAGE(key, str) STORAGE_##key,
#include "slang/keywordlist.h"
    };

    Storage storage() const { return storage_; }
    void set_storage(Storage storage) { storage_ = storage; }

    void print(std::ostream&) const;

protected:
    Storage storage_;
};

/// Precision qualifier
class PrecisionQualifier : public TypeQualifier {
public:
    enum Precision {
#define SLANG_KEY_QUAL_PREC(key, str) PREC_##key,
#include "slang/keywordlist.h"
    };

    Precision precision() const { return prec_; }
    void set_precision(Precision prec) { prec_ = prec; }

    void print(std::ostream&) const;

private:
    Precision prec_;
};

/// Interpolation qualifier
class InterpQualifier : public TypeQualifier {
public:
    enum Interp {
#define SLANG_KEY_QUAL_INTERP(key, str) INTERP_##key,
#include "slang/keywordlist.h"
    };

    Interp interp() const { return interp_; }
    void set_interp(Interp interp) { interp_ = interp; }

    void print(std::ostream&) const;

private:
    Interp interp_;
};

/// Subroutine qualifier (contains a list of type names)
class SubroutineQualifier : public TypeQualifier {
public:
    const std::vector<std::string>& type_names() const { return names_; }
    void push_type_name(const std::string& name) { names_.push_back(name); }

    void print(std::ostream&) const;

protected:
    std::vector<std::string> names_;
};

/// Layout qualifier (contains a map from identifiers to values)
class LayoutQualifier : public TypeQualifier {
public:
    virtual ~LayoutQualifier() {
        for (auto l : layouts_) delete l.second;
    }

    const std::unordered_map<std::string, Expr*>& layouts() const { return layouts_; }
    void push_layout(const std::string& name, Expr* expr) {
        assert(layouts_.find(name) == layouts_.end());
        layouts_.emplace(name, expr);
    }

    void print(std::ostream&) const;

private:
    std::unordered_map<std::string, Expr*> layouts_;
};

/// Array specifier, can have several dimensions
class ArraySpecifier : public Node {
public:
    virtual ~ArraySpecifier() {
        for (auto d : dims_) delete d;
    }

    const std::vector<Expr*>& dims() const { return dims_; }
    void push_dim(Expr* dim) { dims_.push_back(dim); }

    void print(std::ostream& out) const;

private:
    std::vector<Expr*> dims_;
};

/// Nodes that have an array specifier
class HasArraySpecifier {
public:
    HasArraySpecifier() : array_spec_(nullptr) {}
    virtual ~HasArraySpecifier() {
        delete array_spec_;
    }

    const ArraySpecifier* array_specifier() const { return array_spec_; }
    ArraySpecifier* array_specifier() { return array_spec_; }
    void set_array_specifier(ArraySpecifier* array_spec) { array_spec_ = array_spec; }

protected:
    ArraySpecifier* array_spec_;
};

/// Base class for types
class Type : public Node, public HasArraySpecifier {
public:
    virtual ~Type() {
        for (auto q : quals_) delete q;
    }

    bool has_qualifier() const { return quals_.size() != 0; }

    const std::vector<TypeQualifier*>& qualifiers() const { return quals_; }
    void push_qualifier(TypeQualifier* qual) { quals_.push_back(qual); }

protected:
    std::vector<TypeQualifier*> quals_;
};

/// Nodes that have a type
class HasType {
public:
    HasType(Type* type = nullptr) : type_(type) {}
    virtual ~HasType() { delete type_; }

    void set_type(Type* type) { type_ = type; }
    Type* type() { return type_; }
    const Type* type() const { return type_; }

protected:
    Type* type_;
};

/// Primitive type
class PrimType : public Type {
public:
    enum Prim {
#define SLANG_KEY_DATA(key, str) PRIM_##key,
#include "slang/keywordlist.h"
    };

    Prim prim() const { return prim_; }
    void set_prim(Prim prim) { prim_ = prim; }

    void print(std::ostream&) const;

private:
    Prim prim_;
};

class NamedType : public Type, public HasName {
public:
    void print(std::ostream&) const;
};

/// A default precision declaration
class PrecisionDecl : public Node {
public:
    PrecisionDecl() : prim_(nullptr), prec_(nullptr) {}

    virtual ~PrecisionDecl() {
        delete prim_;
        delete prec_;
    }

    void set_precision(PrecisionQualifier* prec) { prec_ = prec; }
    void set_prim(PrimType* prim) { prim_ = prim; }

    const PrimType* prim() const { return prim_; }
    const PrecisionQualifier* precision() const { return prec_; }

    void print(std::ostream&) const;

private:
    PrimType* prim_;
    PrecisionQualifier* prec_;
};

/// A variable definition, a structure field or a function parameter
class Variable : public Node, public HasName, public HasArraySpecifier {
public:
    Variable() : init_(nullptr) {}

    virtual ~Variable() {
        delete init_;
    }

    const Expr* initializer() const { return init_; }
    void set_initializer(Expr* init) { init_ = init; }

    void print(std::ostream&) const;

private:
    Expr* init_;
};

/// A list of variable declarations
class VariableDecl : public Node, public HasType {
public:
    virtual ~VariableDecl() {
        for (auto v : vars_) delete v;
    }

    const std::vector<Variable*>& vars() const { return vars_; }
    int num_vars() const { return vars_.size(); }
    void push_var(Variable* var) { vars_.push_back(var); }

    void print(std::ostream&) const;

private:
    std::vector<Variable*> vars_;
};

/// Structure type
class StructType : public Type, public HasName {
public:
    virtual ~StructType() {
        for (auto f : fields_) delete f;
    }

    const std::vector<VariableDecl*>& fields() const { return fields_; }
    int num_fields() const { return fields_.size(); }
    void push_field(VariableDecl* field) { fields_.push_back(field); }

    void print(std::ostream&) const;

private:
    std::vector<VariableDecl*> fields_;
};

/// Function argument
class Arg : public Node, public HasName, public HasType, public HasArraySpecifier {
public:
    void print(std::ostream&) const;
};

/// Function prototype of function definition
class FunctionDecl : public Node, public HasType, public HasName {
public:
    FunctionDecl(List* body = nullptr) { body_ = body; }
    virtual ~FunctionDecl() { delete body_; }

    bool is_prototype() const { return body_ == nullptr; }
    List* body() { return body_; }
    const List* body() const { return body_; }
    void set_body(List* body) { body_ = body; }

    const std::vector<Arg*>& args() { return args_; }
    int num_args() const { return args_.size(); }
    void push_arg(Arg* arg) { args_.push_back(arg); }

    void print(std::ostream&) const;

private:
    std::vector<Arg*> args_;
    List* body_;
};

} // namespace ast

} // namespace slang

#endif // SLANG_AST_H
