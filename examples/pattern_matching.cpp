/// This example demonstrates how to perform pattern matching on the AST
/// by replacing <code>a * b + c</code> by calls to <code>fma</code>
/// for floating point values. The code uses the Parser and Lexer class to
/// read the contents of a string. The result is printed to a stream
/// using the Printer class.

#include <iostream>
#include <sstream>
#include <string>

#include <slang/parser.h>
#include <slang/sema.h>
#include <slang/lexer.h>
#include <slang/logger.h>
#include <slang/ptr.h>
#include <slang/ast.h>
#include <slang/print.h>

using namespace slang;

void pattern_match(Ptr<ast::Module>& mod);
void pattern_match(Ptr<ast::Decl>& decl);
void pattern_match(Ptr<ast::Variable>& var);
void pattern_match(Ptr<ast::StmtList>& list);
void pattern_match(Ptr<ast::Stmt>& stmt);
void pattern_match(Ptr<ast::LoopCond>& cond);
void pattern_match(Ptr<ast::Expr>& expr);

void pattern_match(Ptr<ast::Module>& mod) {
    for (auto& decl : mod->decls()) pattern_match(decl);
}

void pattern_match(Ptr<ast::Decl>& decl) {
    // Pattern match only function declarations
    if (auto func_decl = decl->isa<ast::FunctionDecl>()) {
        // The function can be only a prototype, so there may be no body
        if (func_decl->body()) pattern_match(func_decl->body());
    } else if (auto var_decl = decl->isa<ast::VariableDecl>()) {
        for (auto& var : var_decl->vars()) pattern_match(var);
    }
}

void pattern_match(Ptr<ast::Variable>& var) {
    pattern_match(var->init());
}

void pattern_match(Ptr<ast::StmtList>& list) {
    for (auto& stmt : list->stmts()) pattern_match(stmt);
}

void pattern_match(Ptr<ast::Stmt>& stmt) {
    if (auto list = stmt->isa<ast::StmtList>()) {
        // List of statements
        for (auto& stmt : list->stmts()) pattern_match(stmt);
    } else if (auto if_stmt = stmt->isa<ast::IfStmt>()) {
        // If statements
        pattern_match(if_stmt->cond());
        pattern_match(if_stmt->if_true());
        pattern_match(if_stmt->if_false());
    } else if (auto switch_stmt = stmt->isa<ast::SwitchStmt>()) {
        // Switch statements
        pattern_match(switch_stmt->expr());
        pattern_match(switch_stmt->list());
    } else if (auto for_stmt = stmt->isa<ast::ForLoopStmt>()) {
        // For loops
        pattern_match(for_stmt->init());
        pattern_match(for_stmt->iter());
        pattern_match(for_stmt->cond());
        pattern_match(for_stmt->body());
    } else if (auto loop_stmt = stmt->isa<ast::LoopStmt>()) {
        // Other loops: while and do...while
        pattern_match(loop_stmt->cond());
        pattern_match(loop_stmt->body());
    } else if (auto expr_stmt = stmt->isa<ast::ExprStmt>()) {
        // Expressions
        pattern_match(expr_stmt->expr());
    } else if (auto decl_stmt = stmt->isa<ast::DeclStmt>()) {
        // Declarations
        pattern_match(decl_stmt->decl());
    } else if (auto ret_stmt = stmt->isa<ast::ReturnStmt>()) {
        // Return statements
        pattern_match(ret_stmt->value());
    }
}

void pattern_match(Ptr<ast::LoopCond>& cond) {
    if (cond->expr()) pattern_match(cond->expr());
}

ast::Expr* make_fma(ast::Expr* a, ast::Expr* b, ast::Expr* c) {
    // Create a call to fma with a, b and c as arguments
    auto fma = new ast::CallExpr();
    auto ident = new ast::IdentExpr();
    ident->set_ident("fma");
    fma->function().reset(ident);
    fma->push_arg(a);
    fma->push_arg(b);
    fma->push_arg(c);
    return fma;
}

ast::Expr* pattern_match(ast::Expr* expr) {
    if (auto bin_op = expr->isa<ast::BinOpExpr>()) {
        // First, descend in the tree
        pattern_match(bin_op->left());
        pattern_match(bin_op->right());

        // Look for the pattern A * B + C or C + A * B
        if (bin_op->type() == ast::BinOpExpr::ADD) {
            if (auto left = bin_op->left()->isa<ast::BinOpExpr>()) {
                if (left->type() == ast::BinOpExpr::MUL) {
                    auto a = left->left().release();
                    auto b = left->right().release();
                    auto c = bin_op->right().release();
                    return make_fma(a, b, c);
                }
            }

            if (auto right = bin_op->right()->isa<ast::BinOpExpr>()) {
                if (right->type() == ast::BinOpExpr::MUL) {
                    auto a = right->left().release();
                    auto b = right->right().release();
                    auto c = bin_op->left().release();
                    return make_fma(a, b, c);
                }
            }
        }
    } else if (auto un_op = expr->isa<ast::UnOpExpr>()) {
        // Unary operation
        pattern_match(un_op->operand());
    } else if (auto assign_op = expr->isa<ast::AssignOpExpr>()) {
        // Operation with assignment
        pattern_match(assign_op->left());
        pattern_match(assign_op->right());
    } else if (auto cond = expr->isa<ast::CondExpr>()) {
        // Ternary operator
        pattern_match(cond->cond());
        pattern_match(cond->if_true());
        pattern_match(cond->if_false());
    } else if (auto call = expr->isa<ast::CallExpr>()) {
        if (call->function()) pattern_match(call->function());
        for (auto& arg : call->args()) pattern_match(arg);
    } else if (auto list = expr->isa<ast::ExprList>()) {
        for (auto& expr : list->exprs()) pattern_match(expr);
    }

    return expr;
}

void pattern_match(Ptr<ast::Expr>& expr) {
    expr.reset(pattern_match(expr.release()));
}

int main(int argc, char** argv) {
    std::string glsl = 
R"(
float test() {
    float a = 42.0, b = 1.1;
    return 1.2 * (5.0 - a) + (1.0 + b * 2.0);
}
)";
    std::istringstream is(glsl);

    // Create a logger object to report errors
    Logger log("inline_string.glsl");
    // Create a set of keywords
    Keywords keys;
    keys.add_all_keywords();
    // Create a lexer to generate a stream of tokens
    Lexer lex(is, keys, log);
    // Create a semantic analyser (type checker) object
    Sema sema(log);
    // Create a parser object
    Parser parser([&] { return lex.lex(); }, sema, log);

    auto module = parser.parse();

    pattern_match(module);

    std::ostringstream out;
    Printer printer(out, "    ", 0, false, false);
    module->print(printer);

    std::cout << out.str();
    
    return out.str() == "float test() {\n    float a = 42.0, b = 1.1;\n    return fma(1.2, 5.0 - a, fma(b, 2.0, 1.0));\n}\n" ? 0 : 1;
}
