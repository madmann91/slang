#include "ast.h"

namespace slang {

namespace ast {

void StorageQualifier::print(std::ostream& out) const {
    switch (storage_) {
#define SLANG_KEY_QUAL_STORAGE(key, str) case STORAGE_##key: out << str; break;
#include "slang/keywordlist.h"
        default: assert(0 && "Unknown storage qualifier");
    }
}

void PrecisionQualifier::print(std::ostream& out) const {
    switch (prec_) {
#define SLANG_KEY_QUAL_PREC(key, str) case PREC_##key: out << str; break;
#include "slang/keywordlist.h"
        default: assert(0 && "Unknown precision qualifier");
    }
}

void InterpQualifier::print(std::ostream& out) const {
    switch (interp_) {
#define SLANG_KEY_QUAL_INTERP(key, str) case INTERP_##key: out << str; break;
#include "slang/keywordlist.h"
        default: assert(0 && "Unknown interpolation qualifier");
    }
}

void SubroutineQualifier::print(std::ostream& out) const {
    out << "subroutine";
}

void LayoutQualifier::print(std::ostream& out) const {
    out << "layout";
}

void ArraySpecifier::print(std::ostream& out) const {
    for (auto d : dims_) {
        out << "[";
        if (d) d->print(out);
        out << "]";
    }
}

void ErrorType::print(std::ostream& out) const {
    out << "<error>";
}

void PrimType::print(std::ostream& out) const {
    for (auto q : quals_) {
        q->print(out);
        out << " ";
    }

    switch (prim_) {
#define SLANG_KEY_DATA(key, str) case PRIM_##key: out << str; break;
#include "slang/keywordlist.h"
        default: assert(0 && "Unknown primitive type");
    }

    if (array_spec_)
        array_spec_->print(out);
}

void NamedType::print(std::ostream& out) const {
    for (auto q : quals_) {
        q->print(out);
        out << " ";
    }

    out << name_;

    if (array_spec_)
        array_spec_->print(out);
}

void StructType::print(std::ostream& out) const {
    for (auto q : quals_) {
        q->print(out);
        out << " ";
    }

    out << "struct " << name_ << " {\n";
    for (size_t i = 0; i < fields_.size(); i++) {
        out << "    ";
        fields_[i]->print(out);
        out << "\n";
    }
    out << "}";

    if (array_spec_)
        array_spec_->print(out);
}

void PrecisionDecl::print(std::ostream& out) const {
    out << "precision ";
    prec_->print(out);
    out << " ";
    prim_->print(out);
    out << ";";
}

void VariableDecl::print(std::ostream& out) const {
    type_->print(out);

    if (vars_.size() > 0)
        out << " ";

    for (size_t i = 0; i < vars_.size(); i++) {
        vars_[i]->print(out);
        if (i < vars_.size() - 1)
            out << ", ";
    }
    out << ";";
}

void FunctionDecl::print(std::ostream& out) const {
    type_->print(out);
    out << " " << name_;

    out << "(";
    for (size_t i = 0; i < args_.size(); i++) {
        args_[i]->print(out);
        if (i < args_.size() - 1)
            out << ", ";
    }
    out << ")";
    
    if (body_) {
        out << " ";
        body_->print(out);
    } else {
        out << ";";
    }
}

void Variable::print(std::ostream& out) const {
    out << name_;

    if (array_spec_)
        array_spec_->print(out);

    if (init_) {
        out << " = ";
        init_->print(out);
    }
}

void Arg::print(std::ostream& out) const {
    type_->print(out);
    out << " ";
    out << name_;
    if (array_spec_)
        array_spec_->print(out);
}

void ExprList::print(std::ostream& out) const {
    for (size_t i = 0; i < exprs_.size(); i++) {
        exprs_[i]->print(out);
        if (i < exprs_.size() - 1)
            out << ", ";
    }
}

void ErrorExpr::print(std::ostream& out) const {
    out << "<error>";
}

void FieldExpr::print(std::ostream& out) const {
    left_->print(out);
    out << "." << field_name_;
}

void IndexExpr::print(std::ostream& out) const {
    left_->print(out);
    out << "[";
    index_->print(out);
    out << "]";
}

void CallExpr::print(std::ostream& out) const {
    out << name_ << "(";
    for (size_t i = 0; i < args_.size(); i++) {
        args_[i]->print(out);
        if (i < args_.size() - 1)
            out << ", ";
    }
    out << ")";
}

void LiteralExpr::print(std::ostream& out) const {
    out << lit_;
}

void IdentExpr::print(std::ostream& out) const {
    out << name_;
}

void UnOpExpr::print(std::ostream& out) const {
     if (type_ == UNOP_POST_INC) {
        op_->print(out);
        out << "++";
    } else if (type_ == UNOP_POST_DEC) {
        op_->print(out);
        out << "--";
    } else {
        switch (type_) {
            case UNOP_INC:     out << "++ "; break;
            case UNOP_DEC:     out << "-- "; break;
            case UNOP_NOT:     out << "! ";  break;
            case UNOP_BIT_NOT: out << "~ ";  break;
            case UNOP_PLUS:    out << "+ ";  break;
            case UNOP_MINUS:   out << "- ";  break;
            default: out << "<unknown unop>"; break;
        }
        op_->print(out);
    }
}

void CondExpr::print(std::ostream& out) const {
    cond_->print(out);
    out << " ? ";
    if_true_->print(out);
    out << " : ";
    if_false_->print(out);
}

void AssignOpExpr::print(std::ostream& out) const {
    left_->print(out);
    switch (type_) {
        case ASSIGN_EQUAL:  out << " = ";   break;
        case ASSIGN_ADD:    out << " += ";  break;
        case ASSIGN_SUB:    out << " -= ";  break;
        case ASSIGN_MUL:    out << " *= ";  break;
        case ASSIGN_DIV:    out << " /= ";  break;
        case ASSIGN_MOD:    out << " %= ";  break;
        case ASSIGN_LSHIFT: out << " <<= "; break;
        case ASSIGN_RSHIFT: out << " >>= "; break;
        case ASSIGN_AND:    out << " &= ";  break;
        case ASSIGN_XOR:    out << " ^= ";  break;
        case ASSIGN_OR:     out << " |= ";  break;
        default: out << "<unknown assignop>"; break;
    }
    right_->print(out);
}

void BinOpExpr::print(std::ostream& out) const {
    out << "(";
    left_->print(out);
    out << " ";
    switch (type_) {
        case BINOP_MUL:    out << "*";  break;
        case BINOP_DIV:    out << "/";  break;
        case BINOP_MOD:    out << "%";  break;
        case BINOP_ADD:    out << "+";  break;
        case BINOP_SUB:    out << "-";  break; 
        case BINOP_LSHIFT: out << "<<"; break;
        case BINOP_RSHIFT: out << ">>"; break;
        case BINOP_LT:     out << "<";  break;
        case BINOP_GT:     out << ">";  break;
        case BINOP_LEQ:    out << "<="; break;
        case BINOP_GEQ:    out << ">="; break;
        case BINOP_EQ:     out << "=="; break;
        case BINOP_NEQ:    out << "!="; break;
        case BINOP_AND:    out << "&";  break;
        case BINOP_XOR:    out << "^";  break;
        case BINOP_OR:     out << "|";  break;
        case BINOP_ANDAND: out << "&&"; break;
        case BINOP_OROR:   out << "||"; break;
        default: out << "<unknown binop>";  break;
    }
    out << " ";
    right_->print(out);
    out << ")";
}

void InitExpr::print(std::ostream& out) const {
    out << "{";
    for (size_t i = 0; i < exprs_.size(); i++) {
        exprs_[i]->print(out);
        if (i < exprs_.size() - 1)
            out << ", ";
    }
    out << "}";
}

void DeclList::print(std::ostream& out) const {
    for (auto d : decls_) {
        d->print(out);
        out << "\n";
    }
}

void LoopCond::print(std::ostream& out) const {
    if (is_var()) {
        assert(static_cast<bool>(var_type_) && "Invalid loop condition");
        var_type_->print(out);
        var_->print(out);
    } else {
        assert(is_expr() && "Invalid loop condition");
        expr_->print(out);
    }
}

void StmtList::print(std::ostream& out) const {
    out << "{\n";
    for (auto s : stmts_) {
        s->print(out);
        out << "\n";
    }
    out << "}";
}

void DeclStmt::print(std::ostream& out) const {
    decl_->print(out);
}

void ExprStmt::print(std::ostream& out) const {
    if (expr_)
        expr_->print(out);
    out << ";";
}

void IfStmt::print(std::ostream& out) const {
    out << "if (";
    cond_->print(out);
    out << ") ";
    if_true_->print(out);

    if (if_false_) {
        out << " else ";
        if_false_->print(out);
    }
}

void SwitchStmt::print(std::ostream& out) const {
    out << "switch (";
    expr_->print(out);
    out << ") ";
    list_->print(out);
}

void CaseLabelStmt::print(std::ostream& out) const {
    if (is_default()) {
        out << "default:";
    } else {
        out << "case ";
        expr_->print(out);
        out << " :";
    }
}

void ForLoopStmt::print(std::ostream& out) const {
    out << "for (";

    if (init_)
        init_->print(out);
    else
        out << ";";

    out << " ";

    if (cond_) cond_->print(out);
    out << "; ";

    if (iter_) iter_->print(out);
    out << ") ";

    body_->print(out);
}

void WhileLoopStmt::print(std::ostream& out) const {
    out << "while (";
    cond_->print(out);
    out << ") ";
    body_->print(out);
}

void DoWhileLoopStmt::print(std::ostream& out) const {
    out << "do ";
    body_->print(out);
    out << " while (";
    cond_->print(out);
    out << ");";
}

void BreakStmt::print(std::ostream& out) const {
    out << "break;";
}

void ContinueStmt::print(std::ostream& out) const {
    out << "continue;";
}

void DiscardStmt::print(std::ostream& out) const {
    out << "discard;";
}

void ReturnStmt::print(std::ostream& out) const {
    out << "return";
    if (has_value()) {
        out << " ";
        value_->print(out);
    }
    out << ";";
}

} // namespace ast

} // namespace slang
