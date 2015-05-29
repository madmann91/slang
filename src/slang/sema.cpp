#include "slang/sema.h"
#include "slang/ast.h"

namespace slang {

namespace ast {

slang::Type* ExprList::check(Sema& sema, TypeExpectation expected) const {
    if (exprs().empty())
        return sema.error_type();

    for (auto expr : exprs())
        sema.check(expr);

    return exprs()[0]->assigned_type();
}

slang::Type* ErrorExpr::check(Sema& sema, TypeExpectation) const {
    return sema.error_type();
}

slang::Type* LiteralExpr::check(Sema& sema, TypeExpectation) const {
    switch (lit_.type()) {
        case Literal::LIT_DOUBLE: return sema.prim_type(slang::PrimType::PRIM_DOUBLE);
        case Literal::LIT_FLOAT:  return sema.prim_type(slang::PrimType::PRIM_FLOAT);
        case Literal::LIT_INT:    return sema.prim_type(slang::PrimType::PRIM_INT);
        case Literal::LIT_UINT:   return sema.prim_type(slang::PrimType::PRIM_UINT);
        default:                  return sema.error_type();
    }
}

slang::Type* IdentExpr::check(Sema& sema, TypeExpectation) const {
    if (auto type = sema.symbol_type(name())) {
        return type;
    } else {
        sema.error(this) << "Unknown identifier \'" << name() << "\'\n";
    }

    return sema.error_type();
}

slang::Type* FieldExpr::check(Sema& sema, TypeExpectation) const {
    if (auto compound = sema.check(left())->isa<slang::CompoundType>()) {
        auto field = compound->args().find(field_name());
        if (field != compound->args().end()) {
            return field->second;
        } else {
            sema.error(this) << "\'" << field_name() << "\' is not a member of \'" << compound->name() << "\'\n";
        }
    } else {
        sema.error(this) << "Expected a structure or interface type in left operand of field expression\n";
    }

    return sema.error_type();
}

slang::Type* IndexExpr::check(Sema& sema, TypeExpectation expected) const {
    ArrayType* array = sema.check(left())->isa<slang::ArrayType>();

    if (!array) {
        sema.error(this) << "Expected an array in left operand of index expression\n";
    } else if (auto prim = index()->check(sema, nullptr)->isa<slang::PrimType>()) {
        if (!prim->prim() == slang::PrimType::PRIM_INT) {
            sema.error(this) << "Expected an integer as array index\n";
        } else {
            return array->elem();
        }
    }

    return sema.error_type();
}

slang::Type* CallExpr::check(Sema& sema, TypeExpectation) const {
    if (auto type = sema.symbol_type(name())) {
        if (auto fn_type = type->isa<slang::FunctionType>()) {
            if (fn_type->num_args() != num_args()) {
                sema.error(this) << "Expected " << fn_type->num_args() << " arguments, got " << num_args() << "\n";
            } else {
                for (int i = 0; i < num_args(); i++) {
                    sema.check(args()[i], fn_type->args()[i]);
                }
                return fn_type;
            }
        } else {
            sema.error(this) << "Identifier \'" << name() << "\' is not a function\n";
        }
    } else {
        // TODO : handle constructors
        sema.error(this) << "Unknown function \'" << name() << "\'\n";
    }

    return sema.error_type();
}

slang::Type* CondExpr::check(Sema& sema, TypeExpectation expected) const {
    sema.check(cond(), sema.prim_type(slang::PrimType::PRIM_BOOL));

    slang::Type* type_true = sema.check(if_true(), expected);
    slang::Type* type_false = sema.check(if_false(), expected);
    if (type_true == type_false) {
        return type_true;
    } else {
        sema.error(this) << "Operands of ternary operator must be of the same type\n";
    }

    return sema.error_type();
}

inline bool is_numeric(slang::Type* type) {
    if (auto prim = type->isa<slang::PrimType>()) {
        switch (prim->prim()) {
            case slang::PrimType::PRIM_VOID:
            case slang::PrimType::PRIM_BVEC4:
            case slang::PrimType::PRIM_BVEC3:
            case slang::PrimType::PRIM_BVEC2:
            case slang::PrimType::PRIM_BOOL:
                break;
            default:
                return true;
        }
    }
    return false;
}

inline bool is_logic(slang::Type* type) {
    if (auto prim = type->isa<slang::PrimType>()) {
        switch (prim->prim()) {
            case slang::PrimType::PRIM_IVEC4:
            case slang::PrimType::PRIM_IVEC3:
            case slang::PrimType::PRIM_IVEC2:
            case slang::PrimType::PRIM_INT:
            case slang::PrimType::PRIM_UVEC4:
            case slang::PrimType::PRIM_UVEC3:
            case slang::PrimType::PRIM_UVEC2:
            case slang::PrimType::PRIM_UINT:
            case slang::PrimType::PRIM_BVEC4:
            case slang::PrimType::PRIM_BVEC3:
            case slang::PrimType::PRIM_BVEC2:
            case slang::PrimType::PRIM_BOOL:
                return true;
            default:
                break;
        }
    }
    return false;
}

inline bool is_integer(slang::Type* type) {
    if (auto prim = type->isa<slang::PrimType>()) {
        switch (prim->prim()) {
            case slang::PrimType::PRIM_IVEC4:
            case slang::PrimType::PRIM_IVEC3:
            case slang::PrimType::PRIM_IVEC2:
            case slang::PrimType::PRIM_INT:
            case slang::PrimType::PRIM_UVEC4:
            case slang::PrimType::PRIM_UVEC3:
            case slang::PrimType::PRIM_UVEC2:
            case slang::PrimType::PRIM_UINT:
                return true;
            default:
                break;
        }
    }
    return false;
}

inline bool is_ordered(slang::Type* type) {
    if (auto prim = type->isa<slang::PrimType>()) {
        switch (prim->prim()) {
            case slang::PrimType::PRIM_IVEC4:
            case slang::PrimType::PRIM_IVEC3:
            case slang::PrimType::PRIM_IVEC2:
            case slang::PrimType::PRIM_INT:
            case slang::PrimType::PRIM_UVEC4:
            case slang::PrimType::PRIM_UVEC3:
            case slang::PrimType::PRIM_UVEC2:
            case slang::PrimType::PRIM_UINT:
            case slang::PrimType::PRIM_VEC4:
            case slang::PrimType::PRIM_VEC3:
            case slang::PrimType::PRIM_VEC2:
            case slang::PrimType::PRIM_FLOAT:
            case slang::PrimType::PRIM_DVEC4:
            case slang::PrimType::PRIM_DVEC3:
            case slang::PrimType::PRIM_DVEC2:
            case slang::PrimType::PRIM_DOUBLE:
                return true;
            default:
                break;
        }
    }
    return false;
}

inline bool is_boolean(slang::Type* type) {
    if (auto prim = type->isa<slang::PrimType>()) {
        switch (prim->prim()) {
            case slang::PrimType::PRIM_BVEC4:
            case slang::PrimType::PRIM_BVEC3:
            case slang::PrimType::PRIM_BVEC2:
            case slang::PrimType::PRIM_BOOL:
                return true;
            default:
                break;
        }
    }
    return false;
}

inline bool is_floating(slang::Type* type) {
    if (auto prim = type->isa<slang::PrimType>()) {
        switch (prim->prim()) {
            case slang::PrimType::PRIM_VEC4:
            case slang::PrimType::PRIM_VEC3:
            case slang::PrimType::PRIM_VEC2:
            case slang::PrimType::PRIM_FLOAT:
            case slang::PrimType::PRIM_DVEC4:
            case slang::PrimType::PRIM_DVEC3:
            case slang::PrimType::PRIM_DVEC2:
            case slang::PrimType::PRIM_DOUBLE:
                return true;
            default:
                break;
        }
    }
    return false;
}

template <typename T>
void expect(Sema& sema, const ast::Node* node, T f, slang::Type* type, const std::string& msg) {
    if (!f(type)) {
        sema.error(node) << "Expected " << msg << ", but got \'" << type->to_string() << "\'\n";
    }
}

inline void expect_numeric(Sema& sema, const ast::Node* node, slang::Type* type) { expect(sema, node, is_numeric, type, "numeric type"); }
inline void expect_logic(Sema& sema, const ast::Node* node, slang::Type* type) { expect(sema, node, is_logic, type, "boolean or integer type"); }
inline void expect_integer(Sema& sema, const ast::Node* node, slang::Type* type) { expect(sema, node, is_integer, type, "integer type"); }
inline void expect_ordered(Sema& sema, const ast::Node* node, slang::Type* type) { expect(sema, node, is_ordered, type, "comparable type"); }
inline void expect_boolean(Sema& sema, const ast::Node* node, slang::Type* type) { expect(sema, node, is_boolean, type, "boolean type"); }
inline void expect_floating(Sema& sema, const ast::Node* node, slang::Type* type) { expect(sema, node, is_floating, type, "floating point type"); }

slang::Type* UnOpExpr::check(Sema& sema, TypeExpectation expected) const {
    slang::Type* op_type = sema.check(operand(), expected);
    
    // TODO : l-value for ++ --

    switch (type()) {
        case UNOP_MINUS:
        case UNOP_PLUS:
        case UNOP_INC:
        case UNOP_DEC:
        case UNOP_POST_INC:
        case UNOP_POST_DEC:
            expect_numeric(sema, this, op_type);
            return op_type;

        case UNOP_NOT:
        case UNOP_BIT_NOT:
            expect_logic(sema, this, op_type);
            return op_type;

        default: assert(0 && "Unknown operator");
    }

    return sema.error_type();
}

slang::Type* AssignOpExpr::check(Sema& sema, TypeExpectation expected) const {
    slang::Type* left_type = sema.check(left(), expected);
    slang::Type* right_type = sema.check(right(), expected);

    if (left_type != right_type) {
        sema.error(this) << "Operands must be of the same type in assignment expression\n";
        return sema.error_type();
    }

    // TODO : Make sure left_type is a l-value

    switch (type()) {
        case ASSIGN_EQUAL:
            return left_type;

        case ASSIGN_ADD:
        case ASSIGN_SUB:
        case ASSIGN_MUL:
        case ASSIGN_DIV:
            expect_numeric(sema, this, left_type);
            return left_type;

        case ASSIGN_MOD:
        case ASSIGN_LSHIFT:
        case ASSIGN_RSHIFT:
            expect_integer(sema, this, left_type);
            return left_type;

        case ASSIGN_AND:
        case ASSIGN_XOR:
        case ASSIGN_OR:
            expect_logic(sema, this, left_type);
            return left_type;

        default: assert(0 && "Unknown operator");
    }

    sema.error(this) << "Invalid type in assignment expression\n";
    return sema.error_type();
}

slang::Type* BinOpExpr::check(Sema& sema, TypeExpectation expected) const {
    slang::Type* left_type = sema.check(left(), expected);
    slang::Type* right_type = sema.check(right(), expected);

    if (left_type != right_type) {
        sema.error(this) << "Operands must be of the same type in binary expression\n";
        return sema.error_type();
    }

    switch (type()) {
        case BINOP_EQ:
        case BINOP_NEQ:
            return left_type;

        case BINOP_MUL:
        case BINOP_DIV:
        case BINOP_ADD:
        case BINOP_SUB:
            expect_numeric(sema, this, left_type);
            return left_type;

        case BINOP_MOD:
        case BINOP_LSHIFT:
        case BINOP_RSHIFT:
            expect_integer(sema, this, left_type);
            return left_type;

        case BINOP_LT:
        case BINOP_GT:
        case BINOP_LEQ:
        case BINOP_GEQ:
            expect_ordered(sema, this, left_type);
            return left_type;

        case BINOP_AND:
        case BINOP_XOR:
        case BINOP_OR:
            expect_logic(sema, this, left_type);
            return left_type;

        case BINOP_ANDAND:
        case BINOP_OROR:
            expect_boolean(sema, this, left_type);
            return left_type;

        default: assert(0 && "Unknown operator");
    }

    sema.error(this) << "Invalid type in binary expression\n";
    return sema.error_type();
}

slang::Type* InitExpr::check(Sema& sema, TypeExpectation expected) const {
    // TODO : Fix this
    return sema.error_type();
}

slang::Type* ErrorType::check(Sema& sema) const {
    return sema.error_type();
}

slang::Type* PrimType::check(Sema& sema) const {
    switch (prim()) {
#define SLANG_KEY_DATA(key, str) case PRIM_##key: return sema.prim_type(slang::PrimType::PRIM_##key);
#include "slang/keywordlist.h"
        default: assert(0 && "Unknown type");
    }
    return sema.error_type();
}

slang::Type* NamedType::check(Sema& sema) const {
    if (auto type = sema.symbol_type(name())) {
        return type;
    }
    sema.error(this) << "\'" << name() << "\' is not a valid type name\n";
    return sema.error_type();
}

inline bool compound_members(Sema& sema, const CompoundType* compound, slang::CompoundType::MemberMap& members) {
    for (auto field : compound->fields()) {
        sema.check(field);
        for (auto var : field->vars()) {
            if (members.find(var->name()) != members.end()) {
                sema.error(var) << "\'" << var->name() << "\' is already a member of \'" << compound->name() << "\'\n";
                return false;
            }
            members.insert(std::make_pair(var->name(), var->assigned_type()));
        }
    }

    return true;
}

slang::Type* StructType::check(Sema& sema) const {
    slang::CompoundType::MemberMap members;

    slang::Type* type;
    if (compound_members(sema, this, members)) {
        type = sema.struct_type(name(), members);
    } else {
        type = sema.error_type();
    }

    // Register the structure in the environment if it has a name
    if (name().length() > 0) {
        sema.new_identifier(this, name(), Symbol(this, nullptr, type));
    }

    return type;
}

slang::Type* InterfaceType::check(Sema& sema) const {
    slang::CompoundType::MemberMap members;
    if (compound_members(sema, this, members)) {
        return sema.interface_type(name(), members);
    }
    return sema.error_type();
}

slang::Type* PrecisionDecl::check(Sema& sema) const {
    slang::Type* prim = sema.check(type());
    expect_floating(sema, this, prim);
    return prim;
}

slang::Type* VariableDecl::check(Sema& sema) const {
    slang::Type* var_type = sema.check(type());
    for (auto var : vars())
        sema.check(var, var_type);
    return var_type;
}

inline void expect_overload(Sema& sema, const ast::FunctionDecl* decl,
                            slang::FunctionType* fn_a, slang::FunctionType* fn_b) {
    if (fn_a->args().size() == fn_b->args().size()) {
        for (size_t i = 0; i < fn_a->args().size(); i++) {
            if (fn_a->args()[i] != fn_b->args()[i])
                return;
        }
        sema.error(decl) << "Cannot overload function \'" << decl->name() << "\', argument types must differ\n";
    }
}

slang::Type* FunctionDecl::check(Sema& sema) const {
    slang::FunctionType::ArgList arg_types;

    slang::Type* ret_type = sema.check(type());
    for (auto arg : args())
        arg_types.push_back(sema.check(arg));

    slang::FunctionType* fn_type = sema.function_type(ret_type, arg_types);

    if (!name().length())
        return fn_type;

    Symbol* symbol = sema.env()->find_symbol(name());
    if (!symbol) {
        sema.env()->push_symbol(name(), Symbol(nullptr, nullptr, fn_type));
        symbol = sema.env()->find_symbol(name());
    } else {
        // Check that the prototype has the same type as the definition
        slang::FunctionType* type = symbol->type()->isa<slang::FunctionType>();
        if (!type) {
            sema.error(this) << "Identifier name \'" << name() << "\' cannot be redefined as a function\n";
        } else if (fn_type != type) {
            expect_overload(sema, this, fn_type, type);
        }
    }

    if (is_prototype()) {
        symbol->set_decl(this);
    } else {
        if (symbol->def()) {
            sema.error(this) << "Redefinition of function \'" << name() << "\'\n";
        } else {
            symbol->set_def(this);
        }
    }
    
    return fn_type;
}

inline bool integer_value(Sema& sema, const Expr* expr, int& result) {
    slang::Type* type = sema.check(expr);
    if (auto prim = type->isa<slang::PrimType>()) {
        if (prim->prim() == slang::PrimType::PRIM_INT ||
            prim->prim() == slang::PrimType::PRIM_UINT) {
            // TODO : reduce expressions using codegen
            auto lit = expr->as<LiteralExpr>();
            result = lit->lit().as_int();
            return true;
        }
    }
    return false;
}

inline slang::Type* array_type(Sema& sema, slang::Type* type, const ArraySpecifier* array) {
    slang::Type* cur_type = type;
    for (auto dim : array->dims()) {
        if (dim) {
            int size;
            if (integer_value(sema, dim, size))
                cur_type = sema.definite_array_type(cur_type, size);
            else
                cur_type = sema.indefinite_array_type(cur_type);
        } else {
            cur_type = sema.indefinite_array_type(cur_type);
        }
    }
    return cur_type;
}

slang::Type* Arg::check(Sema& sema) const {
    slang::Type* arg_type = sema.check(type());
    if (array_specifier())
        return array_type(sema, arg_type, array_specifier());
    return arg_type;
}

slang::Type* Variable::check(Sema& sema, slang::Type* var_type) const {
    slang::Type* type = array_specifier() ? array_type(sema, var_type, array_specifier()) : var_type;
    sema.new_identifier(this, name(), Symbol(this, nullptr, type));
    return type;
}

void StmtList::check(Sema& sema) const {
    for (auto stmt : stmts())
        sema.check(stmt);
}

void DeclStmt::check(Sema& sema) const {
    sema.check(decl());
}

void ExprStmt::check(Sema& sema) const {
    sema.check(expr());
}

void IfStmt::check(Sema& sema) const {
    sema.check(cond(), sema.prim_type(slang::PrimType::PRIM_BOOL));
    sema.check(if_true());
    sema.check(if_false());
}

void SwitchStmt::check(Sema& sema) const {
    sema.check(list());
}

void CaseLabelStmt::check(Sema& sema) const {
    if (!is_default())
        sema.check(expr());
}

void ForLoopStmt::check(Sema& sema) const {
    sema.check(body());
}

void WhileLoopStmt::check(Sema& sema) const {
    sema.check(body());
}

void DoWhileLoopStmt::check(Sema& sema) const {
    sema.check(body());
}

inline void expect_loop(Sema& sema, const ast::Stmt* stmt, const ast::Node* node) {
    if (!node->isa<LoopStmt>()) {
        sema.error(stmt) << "continue or break statements are only allowed inside a loop\n";
    }
}

void BreakStmt::check(Sema& sema) const {
    expect_loop(sema, this, sema.env()->scope());
}

void ContinueStmt::check(Sema& sema) const {
    expect_loop(sema, this, sema.env()->scope());
}

void DiscardStmt::check(Sema& sema) const {}

void ReturnStmt::check(Sema& sema) const {
    const FunctionDecl* fn_decl = sema.env()->scope()->as<FunctionDecl>();
    slang::FunctionType* fn_type = sema.symbol_type(fn_decl->name())->as<FunctionType>();
    if (value()) {
        slang::Type* type = sema.check(value());
        if (type != fn_type->ret()) {
            sema.error(this) << "Expected \'" << fn_type->ret()->to_string() << "\' as return type, got \'"
                             << type->to_string() << "\'\n";
        }
    } else {
        slang::PrimType* prim = fn_type->ret()->isa<slang::PrimType>();
        if (!prim || prim->prim() != slang::PrimType::PRIM_VOID) {
            sema.error(this) << "Function \'" << fn_decl->name() << "\' returns void, got \'"
                             << fn_type->ret()->to_string() << "\'\n";
        }
    }
}

} // namespace ast

} // namespace slang
