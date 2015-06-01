#include "slang/sema.h"
#include "slang/ast.h"

namespace slang {

namespace ast {

const slang::Type* ExprList::check(Sema& sema, TypeExpectation expected) const {
    if (exprs().empty())
        return sema.error_type();

    for (auto expr : exprs())
        sema.check(expr);

    return exprs()[0]->assigned_type();
}

const slang::Type* ErrorExpr::check(Sema& sema, TypeExpectation) const {
    return sema.error_type();
}

const slang::Type* LiteralExpr::check(Sema& sema, TypeExpectation) const {
    switch (lit_.type()) {
        case Literal::LIT_DOUBLE: return sema.prim_type(slang::PrimType::PRIM_DOUBLE);
        case Literal::LIT_FLOAT:  return sema.prim_type(slang::PrimType::PRIM_FLOAT);
        case Literal::LIT_INT:    return sema.prim_type(slang::PrimType::PRIM_INT);
        case Literal::LIT_UINT:   return sema.prim_type(slang::PrimType::PRIM_UINT);
        default:                  return sema.error_type();
    }
}

const slang::Type* IdentExpr::check(Sema& sema, TypeExpectation) const {
    if (auto symbol = sema.env()->lookup_symbol(name())) {
        return symbol->type();
    }

    sema.error(this) << "Unknown identifier \'" << name() << "\'\n";
    return sema.error_type();
}

const slang::Type* FieldExpr::check(Sema& sema, TypeExpectation) const {
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

const slang::Type* IndexExpr::check(Sema& sema, TypeExpectation expected) const {
    const slang::ArrayType* array = sema.check(left())->isa<slang::ArrayType>();

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

const slang::Type* CallExpr::check(Sema& sema, TypeExpectation) const {
    // TODO : Handle constructors

    if (auto symbol = sema.env()->lookup_symbol(name())) {
        if (!symbol->is_function()) {
            sema.error(this) << "Identifier \'" << name() << "\' is not a function\n";
            return sema.error_type();
        }

        // Check arguments
        for (size_t i = 0; i < num_args(); i++) {
            sema.check(args()[i]);
        }

        // Find the correct overloaded function to call
        for (auto def : symbol->defs()) {
            const slang::FunctionType* fn_type = def.first->as<slang::FunctionType>();

            if (fn_type->num_args() != num_args()) {
                continue;
            } else {
                bool valid_call = true;
                for (int i = 0; i < valid_call && num_args(); i++) {
                    if (args()[i]->assigned_type() != fn_type->args()[i]) {
                        valid_call = false;
                        break;
                    }
                }

                if (valid_call)
                    return fn_type->ret();
            }
        }

        sema.error(this) << "No overloaded function \'" << name()
                         << "\' was found with this signature\n";
    } else {
        sema.error(this) << "Unknown function \'" << name() << "\'\n";
    }

    return sema.error_type();
}

const slang::Type* CondExpr::check(Sema& sema, TypeExpectation expected) const {
    sema.check(cond(), sema.prim_type(slang::PrimType::PRIM_BOOL));

    const slang::Type* type_true = sema.check(if_true(), expected);
    const slang::Type* type_false = sema.check(if_false(), expected);
    if (type_true == type_false) {
        return type_true;
    } else {
        sema.error(this) << "Operands of ternary operator must be of the same type\n";
    }

    return sema.error_type();
}

inline bool is_numeric(const slang::Type* type) {
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

inline bool is_logic(const slang::Type* type) {
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

inline bool is_integer(const slang::Type* type) {
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

inline bool is_ordered(const slang::Type* type) {
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

inline bool is_boolean(const slang::Type* type) {
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

inline bool is_floating(const slang::Type* type) {
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
void expect(Sema& sema, const ast::Node* node, T f, const slang::Type* type, const std::string& msg) {
    if (!f(type)) {
        sema.error(node) << "Expected " << msg << ", but got \'" << type->to_string() << "\'\n";
    }
}

inline void expect_numeric(Sema& sema, const ast::Node* node, const slang::Type* type) { expect(sema, node, is_numeric, type, "numeric type"); }
inline void expect_logic(Sema& sema, const ast::Node* node, const slang::Type* type) { expect(sema, node, is_logic, type, "boolean or integer type"); }
inline void expect_integer(Sema& sema, const ast::Node* node, const slang::Type* type) { expect(sema, node, is_integer, type, "integer type"); }
inline void expect_ordered(Sema& sema, const ast::Node* node, const slang::Type* type) { expect(sema, node, is_ordered, type, "comparable type"); }
inline void expect_boolean(Sema& sema, const ast::Node* node, const slang::Type* type) { expect(sema, node, is_boolean, type, "boolean type"); }
inline void expect_floating(Sema& sema, const ast::Node* node, const slang::Type* type) { expect(sema, node, is_floating, type, "floating point type"); }

const slang::Type* UnOpExpr::check(Sema& sema, TypeExpectation expected) const {
    const slang::Type* op_type = sema.check(operand());
    
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

const slang::Type* AssignOpExpr::check(Sema& sema, TypeExpectation expected) const {
    const slang::Type* left_type = sema.check(left());
    const slang::Type* right_type = sema.check(right());

    if (left_type != right_type) {
        sema.error(this) << "Operands must be of the same type in assignment expression\n";
        return sema.error_type();
    }

    // TODO : Make sure left_type is a l-value
    // TODO : Rules for multiplication

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

const slang::Type* BinOpExpr::check(Sema& sema, TypeExpectation expected) const {
    const slang::Type* left_type = sema.check(left());
    const slang::Type* right_type = sema.check(right());

    if (left_type != right_type) {
        sema.error(this) << "Operands must be of the same type in binary expression\n";
        return sema.error_type();
    }

    // TODO : Rules for multiplication

    switch (type()) {
        case BINOP_EQ:
        case BINOP_NEQ:
            return sema.prim_type(slang::PrimType::PRIM_BOOL);

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
            return sema.prim_type(slang::PrimType::PRIM_BOOL);

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

const slang::Type* InitExpr::check(Sema& sema, TypeExpectation expected) const {
    // TODO : Fix this
    return sema.error_type();
}

const slang::Type* ErrorType::check(Sema& sema) const {
    return sema.error_type();
}

const slang::Type* PrimType::check(Sema& sema) const {
    switch (prim()) {
#define SLANG_KEY_DATA(key, str) case PRIM_##key: return sema.prim_type(slang::PrimType::PRIM_##key);
#include "slang/keywordlist.h"
        default: assert(0 && "Unknown type");
    }
    return sema.error_type();
}

const slang::Type* NamedType::check(Sema& sema) const {
    if (auto symbol = sema.env()->lookup_symbol(name())) {
        if (symbol->is_structure()) {
            return symbol->type();
        }
    }

    sema.error(this) << "\'" << name() << "\' is not a valid type name\n";  
    return sema.error_type();
}

inline bool compound_members(Sema& sema, const CompoundType* compound, slang::CompoundType::MemberMap& members) {
    // Creates the member list of a compound type from the AST node
    sema.push_env();
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
    sema.pop_env();

    return true;
}

const slang::Type* StructType::check(Sema& sema) const {
    slang::CompoundType::MemberMap members;

    const slang::Type* type;
    if (compound_members(sema, this, members)) {
        type = sema.struct_type(name(), members);
    } else {
        type = sema.error_type();
    }

    // Register the structure in the environment if it has a name
    if (name().length() > 0) {
        sema.new_symbol(name(), this, type);
    }

    return type;
}

const slang::Type* InterfaceType::check(Sema& sema) const {
    slang::CompoundType::MemberMap members;
    if (compound_members(sema, this, members)) {
        return sema.interface_type(name(), members);
    }
    return sema.error_type();
}

const slang::Type* PrecisionDecl::check(Sema& sema) const {
    const slang::Type* prim = sema.check(type());
    expect_floating(sema, this, prim);
    return prim;
}

const slang::Type* VariableDecl::check(Sema& sema) const {
    const slang::Type* var_type = sema.check(type());
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

static void expect_overload(Sema& sema, const ast::FunctionDecl* decl,
                            const slang::FunctionType* fn_type, Symbol* symbol) {
    // If this is not a prototype, make sure the function is not redefined
    if (!decl->is_prototype()) {
        auto range = symbol->defs().equal_range(fn_type);
        for (auto it = range.first; it != range.second; it++) {
            const ast::FunctionDecl* other_fn_decl = it->second->as<ast::FunctionDecl>();
            if (!other_fn_decl->is_prototype()) {
                sema.error(decl) << "Redefinition of function \'" << decl->name() << "\'\n";
                return;
            }
        }
    }

    // Check function overloading rules
    for (auto def : symbol->defs()) {
        const slang::FunctionType* other_fn_type = def.first->as<FunctionType>();
        const ast::FunctionDecl* other_fn_decl = def.second->as<ast::FunctionDecl>();

        // If the function has already a prototype, we are done checking
        if (other_fn_decl->is_prototype() && other_fn_type == fn_type)
            return;

        // We have to check that arguments differ
        if (!arguments_differ(fn_type, other_fn_type)) {
            sema.error(decl) << "Cannot overload function \'" << decl->name() << "\', argument types must differ\n";
            return;
        }
    }
}

inline slang::FunctionType::ArgList normalize_args(Sema& sema, const ast::FunctionDecl* fn_decl,
                                                   const slang::FunctionType::ArgList& args) {
    // Ensure there is only one void parameter
    int void_count = 0;
    for (size_t i = 0; i < args.size(); i++) {
        if (auto prim = args[i]->isa<slang::PrimType>()) {
            if (prim->prim() == slang::PrimType::PRIM_VOID) {
                void_count++;
            }
        }
        if (void_count == 1 && i != 0)
            sema.error(fn_decl) << "\'void\' must be the only parameter\n";
    }

    // Remove the void parameter
    if (args.size() == 1 && void_count == 1)
        return slang::FunctionType::ArgList();

    return args;
}

const slang::Type* FunctionDecl::check(Sema& sema) const {
    slang::FunctionType::ArgList arg_types;

    const slang::Type* ret_type = sema.check(type());
    for (auto arg : args())
        arg_types.push_back(sema.check(arg));

    const slang::FunctionType* fn_type = sema.function_type(ret_type, normalize_args(sema, this, arg_types));

    if (!name().length())
        return fn_type;

    Symbol* symbol = sema.env()->find_symbol(name());
    if (!symbol) {
        sema.new_symbol(name(), this, fn_type);
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
            if (!args()[i]->name().empty()) {
                sema.new_symbol(args()[i]->name(), args()[i], arg_types[i]);
            }
        }

        sema.push_env(this);
        sema.check(body());
        sema.pop_env();
        sema.pop_env();
    }
    
    return fn_type;
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

inline const slang::Type* array_type(Sema& sema, const slang::Type* type, const ArraySpecifier* array) {
    // Creates an array type from an element type and a list of dimensions
    const slang::Type* cur_type = type;
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

inline void expect_nonvoid(Sema& sema, const ast::Node* node, const std::string& name, const slang::Type* type) {
    assert(!name.empty());
    if (auto prim = type->isa<slang::PrimType>()) {
        if (prim->prim() == slang::PrimType::PRIM_VOID) {
            sema.error(node) << "Argument or variable \'" << name << "\' of type \'void\'\n";
        }
    }
}

const slang::Type* Arg::check(Sema& sema) const {
    const slang::Type* arg_type = sema.check(type());

    if (!name().empty())
        expect_nonvoid(sema, this, name(), arg_type);

    if (array_specifier())
        return array_type(sema, arg_type, array_specifier());

    return arg_type;
}

const slang::Type* Variable::check(Sema& sema, const slang::Type* var_type) const {
    const slang::Type* type = array_specifier() ? array_type(sema, var_type, array_specifier()) : var_type;
    if (name().empty())
        return type;

    expect_nonvoid(sema, this, name(), type);
    sema.new_symbol(name(), this, type);
    return type;
}

void LoopCond::check(Sema& sema) const {
    if (is_var()) {
        sema.check(var(), sema.check(var_type()));
    } else {
        assert(is_expr());
        sema.check(expr());
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
    sema.pop_env();
    sema.pop_env();
}

void WhileLoopStmt::check(Sema& sema) const {
    sema.check(cond());
    sema.push_env(this);
    sema.check(body());
    sema.pop_env();
}

void DoWhileLoopStmt::check(Sema& sema) const {
    sema.push_env(this);
    sema.check(body());
    sema.pop_env();
    sema.check(cond());
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
    const slang::Type* ret_type = fn_decl->type()->assigned_type();
    if (value()) {
        const slang::Type* type = sema.check(value());
        if (type != ret_type) {
            sema.error(this) << "Expected \'" << ret_type->to_string() << "\' as return type, got \'"
                             << type->to_string() << "\'\n";
        }
    } else {
        const slang::PrimType* prim = ret_type->isa<slang::PrimType>();
        if (!prim || prim->prim() != slang::PrimType::PRIM_VOID) {
            sema.error(this) << "Expected \'" << ret_type->to_string() << "\' as return type, got \'void\'\n";
        }
    }
}

} // namespace ast

} // namespace slang
