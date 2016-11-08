#include "slang/ast.h"
#include "slang/print.h"

namespace slang {

// Defined in parser.cpp
int precedence(ast::BinOpExpr::Type type);

namespace ast {

void Module::print(Printer& printer) const {
    for (auto& d : decls_) {
        d->print(printer);
        printer << "\n";
    }
}

void StorageQualifier::print(Printer& printer) const {
    switch (storage_) {
#define SLANG_KEY_QUAL_STORAGE(key, str) case key: printer.print_keyword(str); break;
#include "slang/keywordlist.h"
        default: assert(0 && "Unknown storage qualifier");
    }
}

void PrecisionQualifier::print(Printer& printer) const {
    switch (prec_) {
#define SLANG_KEY_QUAL_PREC(key, str) case key: printer.print_keyword(str); break;
#include "slang/keywordlist.h"
        default: assert(0 && "Unknown precision qualifier");
    }
}

void InterpQualifier::print(Printer& printer) const {
    switch (interp_) {
#define SLANG_KEY_QUAL_INTERP(key, str) case key: printer.print_keyword(str); break;
#include "slang/keywordlist.h"
        default: assert(0 && "Unknown interpolation qualifier");
    }
}

void SubroutineQualifier::print(Printer& printer) const {
    printer.print_keyword("subroutine") << "(";
    for (size_t i = 0; i < names_.size(); i++) {
        printer << names_[i];

        if (i != names_.size() - 1)
            printer << ", ";
    }
    printer << ")";
}

void LayoutQualifier::print(Printer& printer) const {
    printer.print_keyword("layout") << "(";
    for (auto it = layouts_.begin(); it != layouts_.end();) {
        printer << it->first;

        if (it->second) {
            printer << " = ";
            it->second->print(printer);
        }

        it++;
        if (it != layouts_.end())
            printer << ", ";
    }
    printer << ")";
}

void InvariantQualifier::print(Printer& printer) const {
    printer.print_keyword("invariant");
}

void AttributeQualifier::print(Printer& printer) const {
    printer.print_keyword("attribute");
}

void VaryingQualifier::print(Printer& printer) const {
    printer.print_keyword("varying");
}

void ArraySpecifier::print(Printer& printer) const {
    for (auto& d : dims_) {
        printer << "[";
        if (d) d->print(printer);
        printer << "]";
    }
}

void ErrorType::print(Printer& printer) const {
    printer.print_error();
}

void PrimType::print(Printer& printer) const {
    for (auto& q : quals_) {
        q->print(printer);
        printer << " ";
    }

    switch (prim_) {
#define SLANG_KEY_DATA(key, str, type, rows, cols) case key: printer.print_keyword(str); break;
#include "slang/keywordlist.h"
        default: assert(0 && "Unknown primitive type");
    }

    if (array_spec_)
        array_spec_->print(printer);
}

void NamedType::print(Printer& printer) const {
    for (auto& q : quals_) {
        q->print(printer);
        printer << " ";
    }

    printer.print_name(name_);

    if (array_spec_)
        array_spec_->print(printer);
}

void StructType::print(Printer& printer) const {
    for (auto& q : quals_) {
        q->print(printer);
        printer << " ";
    }

    printer.print_keyword("struct") << " ";
    if (name_.length()) printer.print_name(name_) << " ";
    printer << "{";
    printer.indent();
    for (size_t i = 0; i < fields_.size(); i++) {
        printer.new_line();
        fields_[i]->print(printer);
    }
    printer.unindent();
    printer.new_line();
    printer << "}";

    if (array_spec_)
        array_spec_->print(printer);
}

void InterfaceType::print(Printer& printer) const {
    for (auto& q : quals_) {
        q->print(printer);
        printer << " ";
    }

    printer.print_name(name_) << " {";
    printer.indent();
    for (size_t i = 0; i < fields_.size(); i++) {
        printer.new_line();
        fields_[i]->print(printer);
    }
    printer.unindent();
    printer.new_line();
    printer << "}";

    if (array_spec_)
        array_spec_->print(printer);
}

void PrecisionDecl::print(Printer& printer) const {
    printer.print_keyword("precision") << " ";

    if (prec_)
        prec_->print(printer);
    else
        printer.print_error();
    printer << " ";

    type_->print(printer);
    printer << ";";
}

void VariableDecl::print(Printer& printer) const {
    type_->print(printer);

    if (vars_.size() > 0)
        printer << " ";

    for (size_t i = 0; i < vars_.size(); i++) {
        vars_[i]->print(printer);
        if (i < vars_.size() - 1)
            printer << ", ";
    }
    printer << ";";
}

void FunctionDecl::print(Printer& printer) const {
    type_->print(printer);
    printer << " ";
    printer.print_name(name_) << "(";

    for (size_t i = 0; i < args_.size(); i++) {
        args_[i]->print(printer);
        if (i < args_.size() - 1)
            printer << ", ";
    }
    printer << ")";
    
    if (body_) {
        printer << " ";
        body_->print(printer);
    } else {
        printer << ";";
    }
}

void Variable::print(Printer& printer) const {
    printer << name_;

    if (array_spec_)
        array_spec_->print(printer);

    if (init_) {
        printer << " = ";
        init_->print(printer);
    }
}

void Arg::print(Printer& printer) const {
    type_->print(printer);
    if (!name_.empty()) {
        printer << " ";
        printer << name_;

        if (array_spec_)
            array_spec_->print(printer);
    }
}

void ExprList::print(Printer& printer) const {
    for (size_t i = 0; i < exprs_.size(); i++) {
        exprs_[i]->print(printer);
        if (i < exprs_.size() - 1)
            printer << ", ";
    }
}

void ErrorExpr::print(Printer& printer) const {
    printer.print_error();
}

void FieldExpr::print(Printer& printer) const {
    left_->print(printer);
    printer << "." << field_name_;
}

void IndexExpr::print(Printer& printer) const {
    left_->print(printer);
    printer << "[";
    index_->print(printer);
    printer << "]";
}

void CallExpr::print(Printer& printer) const {
    if (is_constructor())
        type_->print(printer);
    else
        expr_->print(printer);

    printer << "(";
    for (size_t i = 0; i < args_.size(); i++) {
        args_[i]->print(printer);
        if (i < args_.size() - 1)
            printer << ", ";
    }
    printer << ")";
}

void LiteralExpr::print(Printer& printer) const {
    printer.print_literal(lit_);
}

void IdentExpr::print(Printer& printer) const {
    if (assigned_type() && assigned_type()->isa<slang::CallableType>()) {
        printer.print_name(ident_);
    } else {
        printer << ident_;
    }
}

std::string UnOpExpr::op_string() const {
    switch (type_) {
        case POST_INC:
        case INC:
            return "++";
        case POST_DEC:
        case DEC:
            return "--";
        case NOT:     return "!";
        case BIT_NOT: return "~";
        case PLUS:    return "+";
        case MINUS:   return "-";
        default:
            assert(0 && "Unknown unary operation");
            return "";
    }
}

void UnOpExpr::print(Printer& printer) const {
     if (type_ == POST_INC) {
        op_->print(printer);
        printer << "++";
    } else if (type_ == POST_DEC) {
        op_->print(printer);
        printer << "--";
    } else {
        printer << op_string();

        const bool need_parens =
            !op_->isa<CallExpr>() &&
            !op_->isa<IdentExpr>() &&
            !op_->isa<LiteralExpr>();
        if (need_parens) printer << "(";
        op_->print(printer);
        if (need_parens) printer << ")";
    }
}

void CondExpr::print(Printer& printer) const {
    cond_->print(printer);
    printer << " ? ";
    if_true_->print(printer);
    printer << " : ";
    if_false_->print(printer);
}

std::string AssignOpExpr::op_string() const {
    switch (type_) {
        case ASSIGN:        return "=";
        case ASSIGN_ADD:    return "+=";
        case ASSIGN_SUB:    return "-=";
        case ASSIGN_MUL:    return "*=";
        case ASSIGN_DIV:    return "/=";
        case ASSIGN_MOD:    return "%=";
        case ASSIGN_LSHIFT: return "<<=";
        case ASSIGN_RSHIFT: return ">>=";
        case ASSIGN_AND:    return "&=";
        case ASSIGN_XOR:    return "^=";
        case ASSIGN_OR:     return "|=";
        default:
            assert(0 && "Unknown assign operation");
            return "";
    }
}

void AssignOpExpr::print(Printer& printer) const {
    left_->print(printer);
    printer << " " << op_string() << " ";
    right_->print(printer);
}

inline void print_expr(const Expr* expr, Printer& printer, int prec) {
    bool pars = printer.force_pars();

    if (auto bin_expr = expr->isa<BinOpExpr>()) {
        if (precedence(bin_expr->type()) > prec) pars = true;
    } else if (expr->isa<AssignOpExpr>() || expr->isa<CondExpr>())
        pars = true;

    if (pars) printer << "(";
    expr->print(printer);
    if (pars) printer << ")";
}

std::string BinOpExpr::op_string() const {
    switch (type_) {
        case MUL:    return "*";
        case DIV:    return "/";
        case MOD:    return "%";
        case ADD:    return "+";
        case SUB:    return "-";
        case LSHIFT: return "<<";
        case RSHIFT: return ">>";
        case LT:     return "<";
        case GT:     return ">";
        case LEQ:    return "<=";
        case GEQ:    return ">=";
        case EQ:     return "==";
        case NEQ:    return "!=";
        case AND:    return "&";
        case XOR:    return "^";
        case OR:     return "|";
        case ANDAND: return "&&";
        case XORXOR: return "^^";
        case OROR:   return "||";
        default:
            assert(0 && "Unknown binary operation");
            return "";
    }
}

void BinOpExpr::print(Printer& printer) const {
    int prec = precedence(type_);
    print_expr(left_.get(), printer, prec);
    printer << " " << op_string() << " ";
    print_expr(right_.get(), printer, prec);
}

void InitExpr::print(Printer& printer) const {
    printer << "{";
    for (size_t i = 0; i < exprs_.size(); i++) {
        exprs_[i]->print(printer);
        if (i < exprs_.size() - 1)
            printer << ", ";
    }
    printer << "}";
}

void LoopCond::print(Printer& printer) const {
    if (is_var()) {
        assert(static_cast<bool>(var_type_) && "Invalid loop condition");
        var_type_->print(printer);
        printer << " ";
        var_->print(printer);
    } else {
        assert(is_expr() && "Invalid loop condition");
        expr_->print(printer);
    }
}

void StmtList::print(Printer& printer) const {
    printer << "{";
    printer.indent();
    for (auto& s : stmts_) {
        printer.new_line();
        s->print(printer);
    }
    printer.unindent();
    printer.new_line();
    printer << "}";
}

void DeclStmt::print(Printer& printer) const {
    decl_->print(printer);
}

void ExprStmt::print(Printer& printer) const {
    if (expr_)
        expr_->print(printer);
    printer << ";";
}

void IfStmt::print(Printer& printer) const {
    printer.print_keyword("if") << " (";
    cond_->print(printer);
    printer << ") ";
    if_true_->print(printer);

    if (if_false_) {
        if (!if_true_->isa<StmtList>()) printer.new_line();
        else printer << " ";
        printer.print_keyword("else") << " ";
        if_false_->print(printer);
    }
}

void SwitchStmt::print(Printer& printer) const {
    printer.print_keyword("switch") << " (";
    expr_->print(printer);
    printer << ") ";
    list_->print(printer);
}

void CaseLabelStmt::print(Printer& printer) const {
    if (is_default()) {
        printer.print_keyword("default") << ":";
    } else {
        printer.print_keyword("case") << " ";
        expr_->print(printer);
        printer << " :";
    }
}

void ForLoopStmt::print(Printer& printer) const {
    printer.print_keyword("for") << " (";

    if (init_)
        init_->print(printer);
    else
        printer << ";";

    printer << " ";

    if (cond_) cond_->print(printer);
    printer << "; ";

    if (iter_) iter_->print(printer);
    printer << ") ";

    body_->print(printer);
}

void WhileLoopStmt::print(Printer& printer) const {
    printer.print_keyword("while") << " (";
    cond_->print(printer);
    printer << ") ";
    body_->print(printer);
}

void DoWhileLoopStmt::print(Printer& printer) const {
    printer.print_keyword("do") << " ";
    body_->print(printer);
    printer << " ";
    printer.print_keyword("while") << " (";
    cond_->print(printer);
    printer << ");";
}

void BreakStmt::print(Printer& printer) const {
    printer.print_keyword("break") << ";";
}

void ContinueStmt::print(Printer& printer) const {
    printer.print_keyword("continue") << ";";
}

void DiscardStmt::print(Printer& printer) const {
    printer.print_keyword("discard") << ";";
}

void ReturnStmt::print(Printer& printer) const {
    printer.print_keyword("return");
    if (has_value()) {
        printer << " ";
        value_->print(printer);
    }
    printer << ";";
}

} // namespace ast

} // namespace slang
