#include "slang/sema.h"
#include "slang/ast.h"

namespace slang {

static bool is_unsized(const Type* type) {
    if (type->isa<IndefiniteArrayType>())
        return true;

    if (auto array = type->isa<ArrayType>())
        return is_unsized(array->elem());

    return false;
}

inline bool is_void(const Type* type) {
    if (auto prim = type->isa<PrimType>())
        return prim->prim() == PrimType::PRIM_VOID;

    return false;
}

void Sema::implicit_convert(const PrimType*& a, const PrimType*& b) {
    PrimType base_a(a->prim());
    PrimType base_b(b->prim());
    if (base_a.subtype(&base_b)) {
        b = prim_type(a->prim(), b->rows(), b->cols());
    } else if(base_b.subtype(&base_a)) {
        a = prim_type(b->prim(), a->rows(), a->cols());
    }
}

void Sema::implicit_convert(const Type*& a, const Type*& b) {
    if (a->subtype(b)) {
        b = a;
    } else if (b->subtype(a)) {
        a = b;
    }
}

void Sema::new_symbol(const std::string& name, QualifiedType type, const ast::Node* node) {
    assert(!name.empty());
    if (auto prev_symbol = env()->find_symbol(name))
        error_redefinition(name, prev_symbol, node);
    else
        env()->push_symbol(name, Symbol({std::make_pair(type, node)}));
}

void Sema::error_redefinition(const std::string& name, const Symbol* symbol, const ast::Node* node) {
    error(node) << "Identifier \'" << name << "\' has already been defined (line "
                << symbol->location().start().line() << ")\n";
}

bool Sema::expect_type(const ast::Node* node, const Type* found, const Type* expected) {
    if (!found->subtype(expected)) {
        error(node) << "Expected \'" << expected->to_string()
                    << "\', but found \'" << found->to_string()
                    << "\'\n";
        return true;
    }
    return false;
}

bool Sema::expect_equal(const ast::OpExpr* expr, const PrimType* a, const PrimType* b) {
    if (a != b) {
        error(expr) << "Expected \'" << a->to_string() << "\' for operator \'" << expr->op_string()
                    << "\', but got \'" << b->to_string() << "\'\n";
        return false;
    }
    return true;
}

bool Sema::expect_compatible(const ast::OpExpr* expr, const PrimType* a, const PrimType* b) {
    if (a->prim() != b->prim()) {
        if (a->is_scalar()) std::swap(a, b);
        const slang::PrimType* c = prim_type(a->prim());
        error(expr) << "Expected \'" << c->to_string() << "\' for operator \'" << expr->op_string()
                    << "\', but got \'" << b->to_string() << "\'\n";
        return false;
    }
    return true;
}

bool Sema::expect_nonvoid(const ast::Node* node, const std::string& name, const Type* type) {
    assert(!name.empty());
    if (is_void(type)) {
        error(node) << "Argument or variable \'" << name << "\' of type \'void\'\n";
        return false;
    }
    return true;
}

bool Sema::expect_lvalue(const ast::Expr* expr) {
    if (!expr->is_lvalue(*this)) {
        error(expr) << "l-value expected\n";
        return false;
    }
    return true;
}

static bool expect_in_op(Sema& sema, const ast::OpExpr* expr, const std::string& msg,
                         const PrimType* type, bool (PrimType::*f) () const) {
    if (!(type->*f)()) {
        sema.error(expr) << "Expected " << msg << " for operator \'" << expr->op_string()
                         << "\', but got \'" << type->to_string() << "\'\n";
        return false;
    }
    return true;
}

bool Sema::expect_numeric(const ast::OpExpr* expr, const PrimType* type) { return expect_in_op(*this, expr, "numeric type", type, &PrimType::is_numeric); }
bool Sema::expect_integer(const ast::OpExpr* expr, const PrimType* type) { return expect_in_op(*this, expr, "integer type", type, &PrimType::is_integer); }
bool Sema::expect_ordered(const ast::OpExpr* expr, const PrimType* type) { return expect_in_op(*this, expr, "comparable type", type, &PrimType::is_ordered); }
bool Sema::expect_boolean(const ast::OpExpr* expr, const PrimType* type) { return expect_in_op(*this, expr, "boolean type", type, &PrimType::is_boolean); }
bool Sema::expect_floating(const ast::OpExpr* expr, const PrimType* type) { return expect_in_op(*this, expr, "floating point type", type, &PrimType::is_floating); }

namespace ast {

const slang::Type* ExprList::check(Sema& sema, const slang::Type*) const {
    if (exprs().empty())
        return sema.error_type();

    for (auto expr : exprs())
        sema.check(expr);

    return exprs().back()->assigned_type();
}

const slang::Type* ErrorExpr::check(Sema& sema, const slang::Type*) const {
    return sema.error_type();
}

const slang::Type* LiteralExpr::check(Sema& sema, const slang::Type* expected) const {
    switch (lit_.type()) {
        case Literal::LIT_DOUBLE: return sema.prim_type(slang::PrimType::PRIM_DOUBLE);
        case Literal::LIT_FLOAT:  return sema.prim_type(slang::PrimType::PRIM_FLOAT);
        case Literal::LIT_INT:    return sema.prim_type(slang::PrimType::PRIM_INT);
        case Literal::LIT_UINT:   return sema.prim_type(slang::PrimType::PRIM_UINT);
        case Literal::LIT_BOOL:   return sema.prim_type(slang::PrimType::PRIM_BOOL);
        default:
            assert(0 && "Unknown literal type");
            return sema.error_type();
    }
}

const slang::Type* IdentExpr::check(Sema& sema, const slang::Type*) const {
    if (auto symbol = sema.env()->lookup_symbol(ident()))
        return symbol->type();

    sema.error(this) << "Unknown identifier \'" << ident() << "\'\n";
    return sema.error_type();
}

bool IdentExpr::is_lvalue(Sema& sema) const {
    if (auto symbol = sema.env()->lookup_symbol(ident())) {
        const QualifiedType& full_type = symbol->full_type();
        return full_type.storage_qualifier() != slang::StorageQualifier::STORAGE_CONST &&
               full_type.storage_qualifier() != slang::StorageQualifier::STORAGE_IN;
    }
    return false;
}

inline bool check_components(const std::string& components, const std::string& set) {
    bool ok = true;
    for (auto c : components)
        ok &= set.find(c) != std::string::npos;

    return ok;
}

const slang::Type* FieldExpr::check(Sema& sema, const slang::Type*) const {
    if (field_name().empty())
        return sema.error_type();

    const slang::Type* left_type = sema.check(left());
    if (auto compound = left_type->isa<slang::CompoundType>()) {
        auto field_type = compound->member_type(field_name());
        if (field_type)
            return field_type->type();
        else
            sema.error(this) << "\'" << field_name() << "\' is not a member of \'" << compound->name() << "\'\n";
    } else if (auto prim_type = left_type->isa<slang::PrimType>()) {
        if (prim_type->cols() == 1) {
            // Check the number of components
            if (field_name().length() > 4) {
                sema.error(this) << "Swizzling operator can only contain 4 elements at most\n";
                return sema.error_type();
            }

            // Check the component names
            std::string rgba("rgba", 0, prim_type->rows());
            std::string xyzw("xyzw", 0, prim_type->rows());
            std::string stpq("stpq", 0, prim_type->rows());
            if (!check_components(field_name(), rgba) &&
                !check_components(field_name(), xyzw) &&
                !check_components(field_name(), stpq)) {
                sema.error(this) << "Valid components are \'" + xyzw + "\', \'" + rgba + "\', or \'" + stpq + "\' here\n";
                return sema.error_type();
            }

            return sema.prim_type(prim_type->prim(), field_name().length());
        }

        sema.error(this) << "Swizzling operator can only be used on vectors or scalars\n";
    } else {
        sema.error(this) << "Expected an aggregate type in left operand of field expression\n";
    }

    return sema.error_type();
}

bool FieldExpr::is_lvalue(Sema& sema) const {
    return left()->is_lvalue(sema);
}

const slang::Type* IndexExpr::check(Sema& sema, const slang::Type*) const {
    const slang::Type* left_type = sema.check(left());
    const slang::Type* index_type = sema.check(index());

    if (auto index_prim = index_type->isa<slang::PrimType>()) {
        if (index_prim->size() == 1 &&
            (index_prim->prim() == slang::PrimType::PRIM_INT ||
             index_prim->prim() == slang::PrimType::PRIM_UINT)) {
            if (auto array = left_type->isa<slang::ArrayType>()) {
                return array->elem();
            } else if (auto prim = left_type->isa<slang::PrimType>()) {
                if (prim->is_vector()) {
                    return sema.prim_type(prim->prim());
                } else if (prim->is_matrix()) {
                    return sema.prim_type(prim->prim(), prim->rows());
                }
            }
            sema.error(this) << "Expected an array or aggregate type in left operand of index expression\n";
            return sema.error_type();
        }
    }

    sema.error(this) << "Expected an integer as array index\n";
    return sema.error_type();
}

bool IndexExpr::is_lvalue(Sema& sema) const {
    return left()->is_lvalue(sema);
}

inline bool match_signature(const CallExpr* call, const slang::FunctionType* fn_type) {
    if (fn_type->num_args() == call->num_args()) {
        for (size_t i = 0; i < call->num_args(); i++) {
            if (call->args()[i]->assigned_type() != fn_type->args()[i].type())
                return false;
        }
        return true;
    }
    return false;
}

static const slang::Type* check_function_call(Sema& sema, const CallExpr* call) {
    const slang::CallableType* call_type = sema.check(call->function())->isa<slang::CallableType>();
    if (!call_type) {
        sema.error(call->function()) << "Expression is not callable\n";
        return sema.error_type();
    }

    // Check arguments
    for (size_t i = 0; i < call->num_args(); i++)
        sema.check(call->args()[i]);

    std::string candidates;
    if (auto overloaded = call_type->isa<slang::OverloadedFunctionType>()) {
        for (auto sign : overloaded->signatures()) {
            if (match_signature(call, sign))
                return sign->ret().type();
        }

        // TODO : Add implicit conversion rules

        for (size_t i = 0; i < overloaded->num_signatures(); i++) {
            candidates += "\'" + overloaded->signatures()[i]->to_string() + "\'";
            if (i < overloaded->num_signatures() - 1) candidates += ", ";
        }
    } else if (auto sign = call_type->isa<slang::FunctionType>()) {
        if (match_signature(call, sign))
            return sign->ret().type();
        candidates = sign->to_string();
    }

    sema.error(call) << "No matching function was found for this call (candidates are : " << candidates << ")\n";
    return sema.error_type();
}

inline bool check_constructor_args(Sema& sema, const CallExpr* call, size_t n) {
    if (n != call->num_args()) {
        sema.error(call) << "Invalid number of arguments for constructor, expected " << n
                         << " and got " << call->num_args() << "\n";
        return false;
    }
    return true;
}

static const slang::Type* check_constructor_call(Sema& sema, const CallExpr* call) {
    slang::QualifiedType full_type = sema.check(call->constructor());
    if (full_type.is_qualified())
        sema.error(call->constructor()) << "Constructors cannot be qualified\n"; 

    const slang::Type* type = full_type.type();

    if (auto prim = type->isa<slang::PrimType>()) {
        if (prim->prim() == slang::PrimType::PRIM_VOID) {
            sema.error(call) << "Cannot construct type \'void\'\n";
            return sema.error_type();
        }

        bool from_matrix = false;
        bool from_scalar = false;
        size_t num_components = 0, prev_components = 0;
        for (size_t i = 0; i < call->num_args(); i++) {
            const slang::Type* arg_type = sema.check(call->args()[i]);
            if (auto arg_prim = arg_type->isa<slang::PrimType>()) {
                from_matrix |= arg_prim->is_matrix();
                from_scalar |= arg_prim->is_scalar();
                prev_components = num_components;
                num_components += arg_prim->size();
            } else {
                sema.error(call->args()[i]) << "Primitive type expected in constructor arguments\n";
                return sema.error_type();
            }
        }

        if (from_matrix && prim->is_matrix()) {
            if (call->num_args() > 1) {
                sema.error(call) << "Matrix constructors can only accept one matrix argument\n";
                return sema.error_type();
            }
        } else if (prev_components >= prim->size()) {
            sema.error(call) << "Too many components for constructor, expected " << prim->size()
                             << " and got " << num_components << "\n";
            return sema.error_type();
        } else if (num_components < prim->size() && (call->num_args() > 1 || prim->is_scalar() || !from_scalar)) {
            sema.error(call) << "Not enough components for constructor, expected " << prim->size()
                             << " and got " << num_components << "\n";
            return sema.error_type();
        }
    } else if (auto struct_type = type->isa<slang::StructType>()) {
        if (!check_constructor_args(sema, call, struct_type->num_members()))
            return sema.error_type();
        for (size_t i = 0; i < struct_type->num_members(); i++)
            sema.check(call->args()[i], struct_type->members()[i].second.type());
    } else if (auto indef_array = type->isa<slang::IndefiniteArrayType>()) {
        for (auto arg : call->args())
            sema.check(arg, indef_array->elem());
        // For indefinite arrays, return a definite array type
        return sema.array_type(indef_array->elem(), call->num_args());
    } else if (auto def_array = type->isa<slang::DefiniteArrayType>()) {
        if (!check_constructor_args(sema, call, def_array->size()))
            return sema.error_type();
        for (auto arg : call->args())
            sema.check(arg, def_array->elem());
    } else {
        assert(0 && "Unknown constructor type");
    }

    return type;
}

const slang::Type* CallExpr::check(Sema& sema, const slang::Type*) const {
    if (is_constructor())
        return check_constructor_call(sema, this);
    else
        return check_function_call(sema, this);
}

const slang::Type* CondExpr::check(Sema& sema, const slang::Type* expected) const {
    sema.check(cond(), sema.prim_type(slang::PrimType::PRIM_BOOL));

    const slang::Type* type_true = sema.check(if_true(), expected);
    const slang::Type* type_false = sema.check(if_false(), expected);
    sema.implicit_convert(type_true, type_false);
    if (type_true == type_false) {
        return type_true;
    } else {
        sema.error(this) << "Operands types must match in ternary operator\n";
    }

    return sema.error_type();
}

static const slang::Type* check_equal(Sema& sema, const OpExpr* expr, const slang::Type* left, const slang::Type* right) {
    sema.implicit_convert(left, right);
    if (left != right) {
        sema.error(expr) << "Operands of \'" << expr->op_string() << "\' must be of the same type\n";
        return sema.error_type();
    }
    return left;
}

static const slang::Type* check_arithmetic(Sema& sema, const OpExpr* expr, const slang::PrimType* a, const slang::PrimType* b) {
    if (!sema.expect_numeric(expr, a) |
        !sema.expect_numeric(expr, b)) {
        return sema.error_type();
    }

    sema.implicit_convert(a, b);

    if ((a->is_scalar() && b->is_scalar()) ||
        (a->is_vector() && b->is_vector()) ||
        (a->is_matrix() && b->is_matrix())) {
        sema.expect_equal(expr, a, b);
        return a;
    }

    if (b->is_scalar() || a->is_scalar()) {
        sema.expect_compatible(expr, a, b);
        return a->is_scalar() ? b : a;
    }

    sema.error(expr) << "Incompatible types for operator \'" << expr->op_string() << "\'\n";
    return sema.error_type();
}

static const slang::Type* check_product(Sema& sema, const OpExpr* expr, const slang::PrimType* a, const slang::PrimType* b) {
    if (!sema.expect_numeric(expr, a) |
        !sema.expect_numeric(expr, b)) {
        return sema.error_type();
    }

    sema.implicit_convert(a, b);

    if ((a->is_scalar() && b->is_scalar()) ||
        (a->is_vector() && b->is_vector())) {
        sema.expect_equal(expr, a, b);
        return a;
    }

    sema.expect_compatible(expr, a, b);
    if (b->is_scalar() || a->is_scalar())
        return a->is_scalar() ? b : a;

    size_t rows = a->rows();
    size_t cols = a->cols();
    if (a->is_vector()) std::swap(rows, cols);

    if (cols != b->rows()) {
        sema.error(expr) << "Incompatible dimensions for operator \'" << expr->op_string() << "\'\n";
        return sema.error_type();
    }

    return sema.prim_type(a->prim(), rows, b->cols());
}

static const slang::Type* check_shift(Sema& sema, const OpExpr* expr, const slang::PrimType* a, const slang::PrimType* b) {
    if (!sema.expect_integer(expr, a) |
        !sema.expect_integer(expr, b)) {
        return sema.error_type();
    }

    if (a->is_scalar() && !b->is_scalar()) {
        sema.error(expr) << "If left operand of \'" << expr->op_string()
                         << "\' is a scalar, right operand has to be a scalar\n";
        return sema.error_type();
    } else if (a->is_vector() && b->is_vector() && a != b) {
        sema.error(expr) << "Vector operands must have the same size\n";
        return sema.error_type();
    }

    return a;
}

template <bool conv = false>
const slang::Type* check_bitwise(Sema& sema, const OpExpr* expr, const slang::PrimType* a, const slang::PrimType* b) {
    if (!sema.expect_integer(expr, a) |
        !sema.expect_integer(expr, b)) {
        return sema.error_type();
    }

    if (conv)
        sema.implicit_convert(a, b);

    if ((a->is_scalar() && b->is_scalar()) ||
        (a->is_vector() && b->is_vector())) {
        sema.expect_equal(expr, a, b);
        return a;
    }

    if ((a->is_scalar() && b->is_vector()) ||
        (a->is_vector() && b->is_scalar())) {
        sema.expect_compatible(expr, a, b);
        return b->is_scalar() ? a : b;
    }

    return a;
}

static const slang::Type* check_modulus(Sema& sema, const OpExpr* expr, const slang::PrimType* a, const slang::PrimType* b) {
    return check_bitwise<true>(sema, expr, a, b);
}

static const slang::Type* check_comparison(Sema& sema, const OpExpr* expr, const slang::PrimType* a, const slang::PrimType* b) {
    if (!sema.expect_ordered(expr, a) |
        !sema.expect_ordered(expr, b)) {
        return sema.error_type();
    }

    sema.implicit_convert(a, b);

    sema.expect_equal(expr, a, b);
    return sema.prim_type(slang::PrimType::PRIM_BOOL);
}

static const slang::Type* check_logical(Sema& sema, const OpExpr* expr, const slang::PrimType* a, const slang::PrimType* b) {
    sema.expect_boolean(expr, a);
    sema.expect_boolean(expr, b);
    return sema.prim_type(slang::PrimType::PRIM_BOOL);
}

const slang::Type* UnOpExpr::check(Sema& sema, const slang::Type*) const {
    const slang::Type* op_type = sema.check(operand());

    const slang::PrimType* prim = op_type->isa<slang::PrimType>();
    if (!prim) {
        sema.error(this) << "Operator \'" << op_string() << "\' expects a primitive type\n";
        return sema.error_type();
    }

    switch (type()) {
        case UNOP_INC:
        case UNOP_POST_INC:
        case UNOP_DEC:
        case UNOP_POST_DEC:
            sema.expect_lvalue(operand());
        case UNOP_MINUS:
        case UNOP_PLUS:
            sema.expect_numeric(this, prim);
            return op_type;

        case UNOP_NOT:
            sema.expect_boolean(this, prim);
            return op_type;

        case UNOP_BIT_NOT:
            sema.expect_integer(this, prim);
            return op_type;

        default:
            assert(0 && "Unknown operator");
            return sema.error_type();
    }
}

const slang::Type* AssignOpExpr::check(Sema& sema, const slang::Type*) const {
    const slang::Type* left_type = sema.check(left());
    const slang::Type* right_type = sema.check(right());

    sema.expect_lvalue(left());

    if (type() == ASSIGN_EQUAL)
        return check_equal(sema, this, left_type, right_type);

    const slang::PrimType* left_prim = left_type->isa<slang::PrimType>();
    const slang::PrimType* right_prim = right_type->isa<slang::PrimType>();
    if (!left_prim || !right_prim) {
        sema.error(this) << "Operator \'" << op_string() << "\' expects primitive types as operands\n";
        return sema.error_type();
    }

    switch (type()) {
        case ASSIGN_ADD:
        case ASSIGN_SUB:
        case ASSIGN_DIV:
            return check_arithmetic(sema, this, left_prim, right_prim);

        case ASSIGN_MUL:
            return check_product(sema, this, left_prim, right_prim);

        case ASSIGN_MOD:
            return check_modulus(sema, this, left_prim, right_prim);

        case ASSIGN_LSHIFT:
        case ASSIGN_RSHIFT:
            return check_shift(sema, this, left_prim, right_prim);

        case ASSIGN_AND:
        case ASSIGN_XOR:
        case ASSIGN_OR:
            return check_bitwise(sema, this, left_prim, right_prim);

        default:
            assert(0 && "Unknown operator");
            return sema.error_type();
    }
}

const slang::Type* BinOpExpr::check(Sema& sema, const slang::Type*) const {
    const slang::Type* left_type = sema.check(left());
    const slang::Type* right_type = sema.check(right());

    if (type() == BINOP_NEQ ||
        type() == BINOP_EQ) {
        check_equal(sema, this, left_type, right_type);
        return sema.prim_type(slang::PrimType::PRIM_BOOL);
    }

    const slang::PrimType* left_prim = left_type->isa<slang::PrimType>();
    const slang::PrimType* right_prim = right_type->isa<slang::PrimType>();
    if (!left_prim || !right_prim) {
        sema.error(this) << "Operator \'" << op_string() << "\' expects primitive types as operands\n";
        return sema.error_type();
    }

    switch (type()) {
        case BINOP_ADD:
        case BINOP_SUB:
        case BINOP_DIV:
            return check_arithmetic(sema, this, left_prim, right_prim);

        case BINOP_MUL:
            return check_product(sema, this, left_prim, right_prim);

        case BINOP_MOD:
            return check_modulus(sema, this, left_prim, right_prim);

        case BINOP_LSHIFT:
        case BINOP_RSHIFT:
            return check_shift(sema, this, left_prim, right_prim);

        case BINOP_LT:
        case BINOP_GT:
        case BINOP_LEQ:
        case BINOP_GEQ:
            return check_comparison(sema, this, left_prim, right_prim);

        case BINOP_AND:
        case BINOP_XOR:
        case BINOP_OR:
            return check_bitwise(sema, this, left_prim, right_prim);

        case BINOP_ANDAND:
        case BINOP_XORXOR:
        case BINOP_OROR:
            return check_logical(sema, this, left_prim, right_prim);

        default:
            assert(0 && "Unknown operator");
            return sema.error_type();
    }
}

const slang::Type* InitExpr::check(Sema& sema, const slang::Type* expected) const {
    assert(expected && "Init expressions need a type expectation for type deduction");

    if (auto array_type = expected->isa<slang::ArrayType>()) {
        if (num_exprs() == 0)
            return sema.array_type(array_type->elem(), 0);

        // Find the minimum element type (according to the subtype relation)
        auto elem = sema.check(exprs()[0], array_type->elem());
        for (size_t i = 1; i < num_exprs(); i++) {
            auto cur_elem = sema.check(exprs()[i], array_type->elem());
            if (cur_elem->subtype(elem)){
                elem = cur_elem;
            } else if (!elem->subtype(cur_elem)) {
                // Prevent other errors
                sema.error(this) << "Element types do not match in array initializer\n";
                break;
            }
        }

        if (elem->subtype(array_type->elem()))
            elem = array_type->elem();

        return sema.array_type(elem, num_exprs());
    } else if (auto struct_type = expected->isa<slang::StructType>()) {
        if (exprs().size() != struct_type->num_members()) {
            sema.error(this) << "Invalid number of members in structure initializer\n";
            return sema.error_type();
        }

        for (size_t i = 0; i < exprs().size(); i++)
            sema.check(exprs()[i], struct_type->members()[i].second.type());
        return expected;
    } else if (auto prim_type = expected->isa<slang::PrimType>()) {
        if (prim_type->size() > 1) {
            if (prim_type->is_vector() && exprs().size() != prim_type->rows()) {
                sema.error(this) << "Invalid number of components in vector initializer\n";
                return sema.error_type();
            } else if (prim_type->is_matrix() && exprs().size() != prim_type->cols()) {
                sema.error(this) << "Invalid number of components in matrix initializer\n";
                return sema.error_type();
            }

            const size_t n = prim_type->is_matrix() ? prim_type->rows() : 1;
            const slang::Type* component = sema.prim_type(prim_type->prim(), n);
            for (auto expr : exprs())
                sema.check(expr, component);
            return expected;
        }
    }

    sema.error(this) << "Initializer lists can only be used with aggregate types\n";
    return sema.error_type();
}

inline bool integer_value(Sema& sema, const Expr* expr, int& result) {
    const slang::Type* type = sema.check(expr);
    if (auto prim = type->isa<slang::PrimType>()) {
        if (prim->prim() == slang::PrimType::PRIM_INT ||
            prim->prim() == slang::PrimType::PRIM_UINT) {
            // TODO : Reduce expressions using codegen
            auto lit = expr->as<LiteralExpr>();
            result = lit->lit().as_int();
            return true;
        }
    }
    return false;
}

static const slang::Type* check_array_specifier(Sema& sema, const ArraySpecifier* spec, const slang::Type* type) {
    if (!spec) return type;

    const slang::Type* cur_type = type;
    for (size_t i = 0; i < spec->num_dims(); i++) {
        const Expr* dim = spec->dims()[spec->num_dims() - 1 - i];
        if (dim) {
            int size;
            if (integer_value(sema, dim, size) && size >= 0) {
                cur_type = sema.array_type(cur_type, size);
                continue;
            } else {
                sema.error(spec) << "Invalid array dimension\n";
            }
        }

        cur_type = sema.array_type(cur_type);
    }

    return cur_type;
}

static slang::StorageQualifier check_qualifiers(Sema& sema, const Type* type) {
    slang::StorageQualifier q = slang::StorageQualifier::STORAGE_NONE;
    int storage_count = 0;

    for (auto qual : type->qualifiers()) {
        if (auto storage = qual->isa<StorageQualifier>()) {
            switch (storage->storage()) {
                case StorageQualifier::STORAGE_CONST:     q = slang::StorageQualifier::STORAGE_CONST;     break;
                case StorageQualifier::STORAGE_IN:        q = slang::StorageQualifier::STORAGE_IN;        break;
                case StorageQualifier::STORAGE_OUT:       q = slang::StorageQualifier::STORAGE_OUT;       break;
                case StorageQualifier::STORAGE_INOUT:     q = slang::StorageQualifier::STORAGE_INOUT;     break;
                case StorageQualifier::STORAGE_UNIFORM:   q = slang::StorageQualifier::STORAGE_UNIFORM;   break;
                case StorageQualifier::STORAGE_BUFFER:    q = slang::StorageQualifier::STORAGE_BUFFER;    break;
                case StorageQualifier::STORAGE_SHARED:    q = slang::StorageQualifier::STORAGE_SHARED;    break;
                default:
                    // TODO: implement missing storage qualifiers
                    break;
            }
            storage_count++;
        } else if (qual->isa<VaryingQualifier>()) {
            q = slang::StorageQualifier::STORAGE_VARYING;
            storage_count++;
        } else if (qual->isa<AttributeQualifier>()) {
            q = slang::StorageQualifier::STORAGE_ATTRIBUTE;
            storage_count++;
        } else {
            // TODO: implement missing qualifiers
        }
    }

    if (storage_count > 1)
        sema.error(type) << "Type mentions more than one storage qualifier\n";

    return q;
}

inline slang::QualifiedType check_full_type(Sema& sema, const Type* node, const slang::Type* type) {
    return slang::QualifiedType(check_array_specifier(sema, node->array_specifier(), type),
                                check_qualifiers(sema, node));
}

slang::QualifiedType ErrorType::check(Sema& sema) const {
    return slang::QualifiedType(sema.error_type());
}

slang::QualifiedType PrimType::check(Sema& sema) const {
    const slang::Type* prim_type = nullptr;
    switch (prim()) {
#define SLANG_KEY_DATA(key, str, type, rows, cols) \
        case PRIM_##key: prim_type = sema.prim_type(slang::PrimType::PRIM_##type, rows, cols); break;
#include "slang/keywordlist.h"
        default:
            assert(0 && "Unknown type");
            return sema.error_type();
    }
    return check_full_type(sema, this, prim_type);
}

slang::QualifiedType NamedType::check(Sema& sema) const {
    if (auto symbol = sema.env()->lookup_symbol(name())) {
        if (symbol->is_structure()) {
            return check_full_type(sema, this, symbol->type());
        }
    }

    sema.error(this) << "\'" << name() << "\' is not a valid type name\n";
    return sema.error_type();
}

static bool compound_members(Sema& sema, const CompoundType* compound, slang::CompoundType::MemberList& members) {
    // Creates the member list of a compound type from the AST node
    sema.push_env();
    for (auto field : compound->fields()) {
        sema.check(field);
        for (auto var : field->vars()) {
            auto it = std::find_if(members.begin(), members.end(),
                [var] (const slang::CompoundType::Member& member) {
                    return member.first == var->name();
                });
            if (it != members.end()) {
                sema.error(var) << "\'" << var->name() << "\' is already a member of \'" << compound->name() << "\'\n";
                return false;
            }
            members.push_back({var->name(), var->assigned_type()});
        }
    }
    sema.pop_env();

    return true;
}

slang::QualifiedType StructType::check(Sema& sema) const {
    slang::CompoundType::MemberList members;

    if (!num_fields())
        sema.error(this) << "Structures must have at least one field\n";

    const slang::Type* type;
    if (compound_members(sema, this, members)) {
        type = sema.struct_type(name(), members);
    } else {
        type = sema.error_type();
    }

    // Register the structure in the environment if it has a name
    if (name().length() > 0) {
        sema.new_symbol(name(), type, this);
    }

    return check_full_type(sema, this, type);
}

slang::QualifiedType InterfaceType::check(Sema& sema) const {
    slang::CompoundType::MemberList members;
    if (compound_members(sema, this, members)) {
        return check_full_type(sema, this, sema.interface_type(name(), members));
    }
    return sema.error_type();
}

slang::QualifiedType PrecisionDecl::check(Sema& sema) const {
    const slang::Type* prim = sema.check(type()).type();
    if (auto type = prim->isa<slang::PrimType>()) {
        if (type->is_floating())
            return prim;
    }
    sema.error(this) << "Floating point type expected in precision declaration\n";
    return sema.error_type();
}

slang::QualifiedType VariableDecl::check(Sema& sema) const {
    slang::QualifiedType var_type = sema.check(type());
    for (auto var : vars())
        sema.check(var, var_type);
    return var_type;
}

static bool arguments_differ(const slang::FunctionType* fn_a,
                             const slang::FunctionType* fn_b) {

    if (fn_a->args().size() != fn_b->args().size())
        return true;

    for (size_t i = 0; i < fn_a->args().size(); i++) {
        if (fn_a->args()[i] != fn_b->args()[i])
            return true;
    }

    return false;
}

static void expect_overload(Sema& sema, const ast::FunctionDecl* fn_decl,
                            const slang::FunctionType* fn_type, Symbol* symbol) {
    // If this is not a prototype, make sure the function is not redefined
    if (!fn_decl->is_prototype()) {
        auto range = symbol->defs().equal_range(fn_type);
        for (auto it = range.first; it != range.second; it++) {
            const ast::FunctionDecl* other_fn_decl = it->second->as<ast::FunctionDecl>();
            if (!other_fn_decl->is_prototype()) {
                sema.error(fn_decl) << "Redefinition of function \'" << fn_decl->name() << "\'\n";
                return;
            }
        }
    }

    // Check function overloading rules
    for (auto def : symbol->defs()) {
        const slang::FunctionType* other_fn_type = def.first.type()->as<FunctionType>();
        const ast::FunctionDecl* other_fn_decl = def.second->as<ast::FunctionDecl>();

        // If the function has already a prototype, we are done checking
        if (other_fn_decl->is_prototype() && other_fn_type == fn_type)
            return;

        // We have to check that arguments differ
        if (!arguments_differ(fn_type, other_fn_type)) {
            sema.error(fn_decl) << "Cannot overload function \'" << fn_decl->name() << "\', argument types must differ\n";
            return;
        }
    }

    // Register the function in the environment
    symbol->push_def(fn_type, fn_decl);

    // Build the overloaded function type.
    if (auto symbol_fn = symbol->type()->isa<slang::FunctionType>()) {
        symbol->set_type(sema.overloaded_function_type({fn_type, symbol_fn}));
    } else {
        const slang::OverloadedFunctionType* overload = symbol->type()->isa<slang::OverloadedFunctionType>();
        assert(overload && "Function symbol type must be either FunctionType or OverloadedFunctionType");
        slang::OverloadedFunctionType::SignatureList sign_list = overload->signatures();
        sign_list.push_back(fn_type);
        symbol->set_type(sema.overloaded_function_type(sign_list));
    }
}

static slang::FunctionType::ArgList normalize_args(Sema& sema, const ast::FunctionDecl* fn_decl,
                                                   const slang::FunctionType::ArgList& args) {
    assert(fn_decl->args().size() == args.size());

    // Ensure there is only one void parameter
    int void_count = 0;
    for (size_t i = 0; i < args.size(); i++) {
        if (auto prim = args[i].type()->isa<slang::PrimType>()) {
            if (prim->prim() == slang::PrimType::PRIM_VOID) {
                void_count++;
            }
        }
        if (void_count == 1 && i != 0)
            sema.error(fn_decl->args()[i]) << "\'void\' must be the only parameter\n";
    }

    // Remove the void parameter
    if (args.size() == 1 && void_count == 1)
        return slang::FunctionType::ArgList();

    return args;
}

slang::QualifiedType FunctionDecl::check(Sema& sema) const {
    slang::FunctionType::ArgList arg_types;

    QualifiedType ret_type = sema.check(type());
    if (ret_type.is_qualified())
        sema.error(type()) << "Only precision qualifiers are allowed on return types\n";

    for (auto arg : args())
        arg_types.push_back(sema.check(arg).type());

    const slang::FunctionType* fn_type = sema.function_type(ret_type.type(), normalize_args(sema, this, arg_types));

    if (!name().length())
        return fn_type;

    Symbol* symbol = sema.env()->find_symbol(name());
    if (!symbol) {
        sema.new_symbol(name(), fn_type, this);
    } else {
        // The symbol has to be a function, and has to follow the function overloading rules
        if (!symbol->is_function()) {
            sema.error(this) << "Identifier \'" << name() << "\' cannot be redefined as a function\n";
        } else {
            expect_overload(sema, this, fn_type, symbol);
        }
    }

    if (body()) {
        sema.push_env();

        // Push the arguments and their types into the environment
        for (size_t i = 0; i < num_args(); i++) {
            if (!args()[i]->name().empty())
                sema.new_symbol(args()[i]->name(), arg_types[i], args()[i]);
        }

        sema.push_env(this);
        sema.check(body());
        sema.pop_env(2);

        if (!is_void(ret_type.type()) && !body()->has_return())
            sema.warn(this) << "Function \'" << name() << "\' does not contain a return statement\n";
    }

    return fn_type;
}

slang::QualifiedType Arg::check(Sema& sema) const {
    slang::QualifiedType arg_type = sema.check(type());

    if (!name().empty())
        sema.expect_nonvoid(this, name(), arg_type.type());

    const slang::Type* type = check_array_specifier(sema, array_specifier(), arg_type.type());
    if (is_unsized(type)) {
        sema.error(this) << "Arguments to functions must be explicitly sized\n";
    }

    return QualifiedType(type, arg_type.storage_qualifier());
}

slang::QualifiedType Variable::check(Sema& sema, QualifiedType var_type) const {
    const slang::Type* type = check_array_specifier(sema, array_specifier(), var_type.type());
    if (name().empty())
        return type;

    if (init())
        type = sema.check(init(), type);

    sema.expect_nonvoid(this, name(), type);

    slang::QualifiedType full_type(type, var_type.storage_qualifier());
    if (auto symbol = sema.env()->find_symbol(name())) {
        // Arrays can be redeclared if they previously were implicitly sized
        if (is_unsized(symbol->type())) {
            if (!is_unsized(type) && full_type.subtype(symbol->full_type())) {
                symbol->push_def(type, this);
                symbol->set_type(type);
            } else {
                sema.error(this) << "Incompatible types for array redeclaration,"
                                    " expected \'" + symbol->type()->to_string() + "\'"
                                    " and got \'" + type->to_string() + "\'\n";
            }
        } else if (symbol->full_type() != full_type) {
            sema.error_redefinition(name(), symbol, this);
        }
    } else {
        sema.new_symbol(name(), full_type, this);
    }

    return full_type;
}

void LoopCond::check(Sema& sema) const {
    const slang::Type* bool_type = sema.prim_type(slang::PrimType::PRIM_BOOL);
    if (is_var()) {
        slang::QualifiedType full_type = sema.check(var(), sema.check(var_type()));
        sema.expect_type(this, full_type.type(), bool_type);
    } else {
        assert(is_expr());
        sema.check(expr(), bool_type);
    }
}

void StmtList::check(Sema& sema) const {
    for (auto stmt : stmts())
        sema.check(stmt);
}

void DeclStmt::check(Sema& sema) const {
    sema.check(decl());
}

void ExprStmt::check(Sema& sema) const {
    if (expr())
        sema.check(expr());
}

void IfStmt::check(Sema& sema) const {
    sema.check(cond(), sema.prim_type(slang::PrimType::PRIM_BOOL));
    sema.check(if_true());
    if (if_false()) sema.check(if_false());
}

void SwitchStmt::check(Sema& sema) const {
    sema.push_env(this);
    sema.check(list());
    sema.pop_env();
}

void CaseLabelStmt::check(Sema& sema) const {
    if (!is_default())
        sema.check(expr());
}

void ForLoopStmt::check(Sema& sema) const {
    sema.push_env();
    if (init()) sema.check(init());
    if (cond()) sema.check(cond());
    if (iter()) sema.check(iter());
    sema.push_env(this);
    sema.check(body());
    sema.pop_env(2);
}

void WhileLoopStmt::check(Sema& sema) const {
    sema.push_env();
    sema.check(cond());
    sema.push_env(this);
    sema.check(body());
    sema.pop_env(2);
}

void DoWhileLoopStmt::check(Sema& sema) const {
    // Push two environments, in case a variable is defined in the condition
    sema.push_env();
    sema.push_env(this);
    sema.check(body());
    sema.pop_env();
    sema.check(cond());
    sema.pop_env();
}

void BreakStmt::check(Sema& sema) const {
    if (!sema.env()->closest_loop() &&
        !sema.env()->closest_switch())
        sema.error(this) << "\'break\' statements are only allowed inside a loop or a switch\n";
}

void ContinueStmt::check(Sema& sema) const {
    if (!sema.env()->closest_loop())
        sema.error(this) << "\'continue\' statements are only allowed inside a loop\n";
}

void DiscardStmt::check(Sema& sema) const {}

void ReturnStmt::check(Sema& sema) const {
    const FunctionDecl* fn_decl = sema.env()->closest_function()->as<FunctionDecl>();
    QualifiedType ret_type = fn_decl->type()->assigned_type();
    if (value()) {
        sema.check(value(), ret_type.type());
    } else if (!is_void(ret_type.type())) {
        sema.error(this) << "Return value expected, function \'" << fn_decl->name()
                         << "\' returns \'" << ret_type.to_string() << "\'\n";
    }
}

} // namespace ast

} // namespace slang
