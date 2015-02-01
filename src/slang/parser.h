#ifndef SLANG_PARSER_H
#define SLANG_PARSER_H

#include <ostream>

#include "slang/lexer.h"
#include "slang/environment.h"

#include "slang/ast.h"

namespace slang {

/// Parser object : creates the Abstract Syntax Tree (AST) for a given
/// valid SLANG program. Uses the Logger object in the lexer to report errors.
class Parser {
public:
    Parser(const Lexer& lexer);

    /// Parses the stream and produces an AST
    ast::List* parse();

private:
    template <typename T>
    class NodeLocation {
    public:
        NodeLocation(T* node, Parser* parser)
            : node_(node), parser_(parser), start_(parser->lookup_[0].loc().start())
        {}

        ~NodeLocation() {
            node_->set_location(Location(start_, parser_->prev_));
        }

        T* node() { return node_; }

        T* operator -> () { return node_; }
        const T* operator -> () const { return node_; }

    private:
        T* node_;
        Parser* parser_;
        Position start_;
    };

    template <typename T, typename... Args>
    NodeLocation<T> new_node(Args... args) {
        return NodeLocation<T>(new T(args...), this);
    }

    void lex();

    void eat(Token::Type);
    void expect(Token::Type);

    std::ostream& error();

    ast::List* parse_root();
    ast::Node* parse_declaration();

    ast::Type* parse_type();
    ast::NamedType* parse_named_type();
    ast::StructType* parse_struct_type();
    ast::PrimType* parse_prim_type();

    ast::PrecisionDecl* parse_precision_decl();
    ast::VariableDecl* parse_variable_decl(ast::Type*);
    ast::FunctionDecl* parse_function_decl(ast::Type*);
    ast::Variable* parse_variable();
    ast::Arg* parse_arg();
    ast::List* parse_body();

    ast::TypeQualifier* parse_type_qualifier();
    ast::StorageQualifier* parse_storage_qualifier();
    ast::PrecisionQualifier* parse_precision_qualifier();
    ast::InterpQualifier* parse_interp_qualifier();
    ast::LayoutQualifier* parse_layout_qualifier();
    ast::SubroutineQualifier* parse_subroutine_qualifier();

    ast::ArraySpecifier* parse_array_specifier();

    ast::Expr* parse_expr();
    ast::LiteralExpr* parse_literal_expr();
    ast::IdentExpr* parse_ident_expr();
    ast::FieldExpr* parse_field_expr(ast::Expr*);
    ast::IndexExpr* parse_index_expr(ast::Expr*);
    ast::Expr* parse_primary_expr();
    ast::Expr* parse_unary_expr();
    ast::Expr* parse_binary_expr(ast::Expr*, int);
    ast::Expr* parse_cond_expr(ast::Expr*);
    ast::Expr* parse_assign_expr();

    Lexer lexer_;
    Environment env_;
    Token lookup_[3];
    Position prev_;
};

} // namespace slang

#endif // SLANG_PARSER_H
