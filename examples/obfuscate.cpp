/// This example demonstrates how to obfuscate the code by renaming all identifiers.
/// The example goes through the AST and replaces every occurence of a symbol with
/// a fresh new symbol, generated randomly, while preserving the correctness of the code.
/// The code uses the Parser and Lexer class to read the contents of a string. The result
/// is printed to a stream using the Printer class.

#include <unordered_map>
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

struct Scope {
    std::unordered_map<std::string, std::string> names;
    std::unordered_set<std::string> new_names;
};

struct Obfuscator {
    std::vector<Scope> scopes;

    Obfuscator() { push_scope(); }

    unsigned int rnd() {
        static int c;
        c = c * 1103515245 + 12345;
        return c;
    }

    const std::string* find_name(const std::string& name) {
        for (int i = scopes.size() - 1; i >= 0; i--) {
            auto& s = scopes[i];
            auto it = s.names.find(name);
            if (it != s.names.end()) { return &it->second; }
        }
        return nullptr;
    }

    bool name_exists(const std::string& name) {
        for (int i = scopes.size() - 1; i >= 0; i--) {
            if (scopes[i].new_names.count(name) > 0) return true;
        }
        return false;
    }

    std::string new_name(const std::string& name) {
        std::string cur;
        do {
            cur += 'a' + rnd() % 5;
        } while (name_exists(cur));
        scopes.back().names[name] = cur;
        scopes.back().new_names.emplace(cur);
        return cur;
    }

    void pop_scope()  { scopes.pop_back();  }
    void push_scope() { scopes.emplace_back(); }

    void obfuscate(Ptr<ast::Module>& mod) {
        for (auto& decl : mod->decls()) obfuscate(decl);
    }

    void obfuscate(Ptr<ast::Decl>& decl) {
        // Look inside function declarations
        if (auto func_decl = decl->isa<ast::FunctionDecl>()) {
            // The function can be only a prototype, so there may be no body
            func_decl->set_name(new_name(func_decl->name()));
            push_scope();
            for (auto& arg : func_decl->args()) obfuscate(arg);
            if (func_decl->body()) obfuscate(func_decl->body());
            pop_scope();
        } else if (auto var_decl = decl->isa<ast::VariableDecl>()) {
            obfuscate(var_decl->type());
            for (auto& var : var_decl->vars()) obfuscate(var);
        }
    }

    void obfuscate(Ptr<ast::Type>& type) {
        if (auto named_type = type->isa<ast::NamedType>()) {
            auto name = find_name(named_type->name());
            if (name) named_type->set_name(*name);
        } else if (auto compound = type->isa<ast::CompoundType>()) {
            compound->set_name(new_name(compound->name()));
            for (auto& field : compound->fields()) {
                obfuscate(field->type());
                for (auto& var : field->vars()) obfuscate(var);
            }
        }
    }

    void obfuscate(Ptr<ast::Arg>& arg) {
        obfuscate(arg->type());
        arg->set_name(new_name(arg->name()));
    }

    void obfuscate(Ptr<ast::Variable>& var) {
        if (var->init()) obfuscate(var->init());
        var->set_name(new_name(var->name()));
    }

    void obfuscate(Ptr<ast::StmtList>& list) {
        push_scope();
        for (auto& stmt : list->stmts()) obfuscate(stmt);
        pop_scope();
    }

    void obfuscate(Ptr<ast::Stmt>& stmt) {
        if (auto list = stmt->isa<ast::StmtList>()) {
            // List of statements
            push_scope();
            for (auto& stmt : list->stmts()) obfuscate(stmt);
            pop_scope();
        } else if (auto if_stmt = stmt->isa<ast::IfStmt>()) {
            // If statements
            obfuscate(if_stmt->cond());
            obfuscate(if_stmt->if_true());
            obfuscate(if_stmt->if_false());
        } else if (auto switch_stmt = stmt->isa<ast::SwitchStmt>()) {
            // Switch statements
            obfuscate(switch_stmt->expr());
            obfuscate(switch_stmt->list());
        } else if (auto for_stmt = stmt->isa<ast::ForLoopStmt>()) {
            // For loops
            obfuscate(for_stmt->init());
            obfuscate(for_stmt->iter());
            obfuscate(for_stmt->cond());
            obfuscate(for_stmt->body());
        } else if (auto loop_stmt = stmt->isa<ast::LoopStmt>()) {
            // Other loops: while and do...while
            obfuscate(loop_stmt->cond());
            obfuscate(loop_stmt->body());
        } else if (auto expr_stmt = stmt->isa<ast::ExprStmt>()) {
            // Expressions
            obfuscate(expr_stmt->expr());
        } else if (auto decl_stmt = stmt->isa<ast::DeclStmt>()) {
            // Declarations
            obfuscate(decl_stmt->decl());
        } else if (auto ret_stmt = stmt->isa<ast::ReturnStmt>()) {
            // Return statements
            obfuscate(ret_stmt->value());
        }
    }

    void obfuscate(Ptr<ast::LoopCond>& cond) {
        if (cond->expr()) obfuscate(cond->expr());
    }

    void obfuscate(Ptr<ast::Expr>& expr) {
        if (auto bin_op = expr->isa<ast::BinOpExpr>()) {
            // First, descend in the tree
            obfuscate(bin_op->left());
            obfuscate(bin_op->right());
        } else if (auto un_op = expr->isa<ast::UnOpExpr>()) {
            // Unary operation
            obfuscate(un_op->operand());
        } else if (auto assign_op = expr->isa<ast::AssignOpExpr>()) {
            // Operation with assignment
            obfuscate(assign_op->left());
            obfuscate(assign_op->right());
        } else if (auto cond = expr->isa<ast::CondExpr>()) {
            // Ternary operator
            obfuscate(cond->cond());
            obfuscate(cond->if_true());
            obfuscate(cond->if_false());
        } else if (auto call = expr->isa<ast::CallExpr>()) {
            if (call->function())    obfuscate(call->function());
            if (call->constructor()) obfuscate(call->constructor());
            for (auto& arg : call->args()) obfuscate(arg);
        } else if (auto index = expr->isa<ast::IndexExpr>()) {
            obfuscate(index->left());
            obfuscate(index->index());
        } else if (auto list = expr->isa<ast::ExprList>()) {
            for (auto& expr : list->exprs()) obfuscate(expr);
        } else if (auto ident = expr->isa<ast::IdentExpr>()) {
            // Find the obfuscated version of the identifier if it exists
            auto name = find_name(ident->ident());
            if (name) ident->set_ident(*name);
        }
    }
};

int main(int argc, char** argv) {
    std::string glsl = 
R"(
struct Node {
    int left, right;
    float value;
};
float pow(float x, int i) {
    if (i == 0) return 1.0;
    else if (i % 2 == 0) return x * pow(x, i - 1);
    else return pow(x, i / 2);
}
float test() {
    float unit = 42.0;
    int array[10];
    Node element;
    for (int i = 0; i < 10; i++) {
        array[i] = pow(unit, i);
    }
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

    Obfuscator obfuscator;
    obfuscator.obfuscate(module);

    std::ostringstream out;
    Printer printer(out, "    ", 0, false, false);
    module->print(printer);

    std::cout << out.str();
    
    return out.str() ==
R"(struct a {
    int e, d;
    float c;
};
float db(float ea, int b) {
    if (b == 0) return 1.0;
    else if (b % 2 == 0) return ea * db(ea, b - 1);
    else return db(ea, b / 2);
}
float aa() {
    float ea = 42.0;
    int cb[10];
    a da;
    for (int eaa = 0; eaa < 10; eaa++) {
        cb[eaa] = db(ea, eaa);
    }
}
)" ? 0 : 1;
}
