#include <cassert>
#include <memory>

#include "slang/parser.h"

namespace slang {

Parser::Parser(const Lexer& lexer)
    : lexer_(lexer)
{
    lookup_[0] = lexer_.lex();
    lookup_[1] = lexer_.lex();
    lookup_[2] = lexer_.lex();
    prev_ = lookup_[0].loc().start();
}

ast::List* Parser::parse() {
    return parse_root();
}

void Parser::lex() {
    prev_ = lookup_[0].loc().end();
    lookup_[0] = lookup_[1];
    lookup_[1] = lookup_[2];
    lookup_[2] = lexer_.lex();
}

void Parser::eat(Token::Type type) {
    assert(lookup_[0].type() == type);
    lex();
}

void Parser::expect(Token::Type type) {
    if (lookup_[0].type() != type)
        error() << "\'" << type << "\' expected\n";

    lex();
}

std::ostream& Parser::error() {
    return lexer_.logger().error(lookup_[0].loc().start());
}

ast::List* Parser::parse_root() {
    auto root = new_node<ast::List>();

    while (!lookup_[0].is_eof()) {
        if (lookup_[0].isa(Token::TOK_IDENT)) {
            root->push_node(parse_declaration());
        } else {
            error() << "Keyword or identifier expected\n";
            lex();
        }
    }

    return root.node();
}

ast::Node* Parser::parse_declaration() {
    // Declaration ::= FunctionDecl | FunctionDef | VariableDecl | PrecisionDecl
    if (lookup_[0].key().isa(Key::KEY_PRECISION)) {
        // PrecisionDecl
        return parse_precision_decl();
    }

    // We have to do some lookahead to determine if
    // it is a function or a variable declaration
    ast::Type* type = parse_type();

    if (lookup_[1].isa(Token::TOK_LPAREN)) {
        // FunctionDecl | FunctionDef
        return parse_function_decl(type);
    } else {
        // VariableDecl
        return parse_variable_decl(type);
    }
}

ast::PrecisionDecl* Parser::parse_precision_decl() {
    // PrecisionDecl ::= precision PrecisionQualifier PrimType ;
    auto decl = new_node<ast::PrecisionDecl>();
    eat(Token::TOK_IDENT);
    
    decl->set_precision(parse_precision_qualifier());
    decl->set_prim(parse_prim_type());

    expect(Token::TOK_SEMICOLON);

    return decl.node();
}

ast::Type* Parser::parse_type() {
    // Type ::= Qualifier* (StructType|NamedType|PrimType) (ArraySpecifier)?
    std::vector<std::unique_ptr<ast::TypeQualifier> > qualifiers;

    while (lookup_[0].key().is_qualifier()) {
        ast::TypeQualifier* qual = parse_type_qualifier();
        qualifiers.emplace_back(qual);
    }

    ast::Type* type = nullptr;
    switch (lookup_[0].key().type()) {
        case Key::KEY_UNKNOWN:
            type = parse_named_type();
            break;

        case Key::KEY_STRUCT:
            type = parse_struct_type();
            break;

#define SLANG_KEY_DATA(key, str) case Key::KEY_##key:
#include "slang/keywordlist.h"
            type = parse_prim_type();
            break;

        default:
            error() << "Expected identifier or data type\n";
            return nullptr;
    }

    // Parse optional array specification
    if (lookup_[0].isa(Token::TOK_LBRACKET)) {
        type->set_array_specifier(parse_array_specifier());
    }

    // Register qualifiers
    for (auto& q : qualifiers)
        type->push_qualifier(q.release());
    
    return type;
}

ast::NamedType* Parser::parse_named_type() {
    // NamedType ::= ident
    auto type = new_node<ast::NamedType>();

    if (lookup_[0].is_ident()) {
        type->set_name(lookup_[0].ident());
        eat(Token::TOK_IDENT);
    } else {
        error() << "Expected identifier\n";
    }

    return type.node();
}

ast::StructType* Parser::parse_struct_type() {
    // StructType ::= struct (ident)? { (Type StructField(,StructField)*;)+ } )
    auto type = new_node<ast::StructType>();
    eat(Token::TOK_IDENT);

    // Optional structure name
    if (lookup_[0].is_ident()) {
        type->set_name(lookup_[0].ident());
        eat(Token::TOK_IDENT);
    }

    expect(Token::TOK_LBRACE);
    
    while (lookup_[0].isa(Token::TOK_IDENT)) {
        ast::Type* field_type = parse_type();
        type->push_field(parse_variable_decl(field_type));
    }

    expect(Token::TOK_RBRACE);
    return type.node();
}

ast::PrimType* Parser::parse_prim_type() {
    // PrimType ::= (int|float|double|bool|(b|d)?vec{2, 3, 4})
    auto prim = new_node<ast::PrimType>();

    switch (lookup_[0].key().type()) {
#define SLANG_KEY_DATA(key, str) \
        case Key::KEY_##key: prim->set_prim(ast::PrimType::PRIM_##key); break;
#include "slang/keywordlist.h"
        default:
            error() << "Primitive data type expected\n";
            break;
    }

    eat(Token::TOK_IDENT);

    return prim.node();
}

ast::VariableDecl* Parser::parse_variable_decl(ast::Type* type) {
    // VariableDecl ::= (Variable (,Variable)*)? ;
    auto decl = new_node<ast::VariableDecl>();

    decl->set_type(type);

    if (lookup_[0].isa(Token::TOK_SEMICOLON)) {
        eat(Token::TOK_SEMICOLON);
        return decl.node();
    }

    decl->push_var(parse_variable());
    while (lookup_[0].isa(Token::TOK_COMMA)) {
        eat(Token::TOK_COMMA);
        decl->push_var(parse_variable());
    }

    expect(Token::TOK_SEMICOLON);

    return decl.node();
}

ast::FunctionDecl* Parser::parse_function_decl(ast::Type* type) {
    // FunctionDecl ::= Type ident ( (Arg)* ) ( { List } | ; )
    auto decl = new_node<ast::FunctionDecl>();

    if (lookup_[0].is_ident()) {
        decl->set_name(lookup_[0].ident());
        eat(Token::TOK_IDENT);
    } else {
        error() << "Expected identifier\n";
    }

    decl->set_type(type);

    expect(Token::TOK_LPAREN);

    // Optional argument list
    if (!lookup_[0].isa(Token::TOK_RPAREN))
    {
        decl->push_arg(parse_arg());
        while (lookup_[0].isa(Token::TOK_COMMA)) {
            eat(Token::TOK_COMMA);
            decl->push_arg(parse_arg());
        }
    }

    expect(Token::TOK_RPAREN);

    // Optional body
    if (lookup_[0].isa(Token::TOK_LBRACE)) {
        decl->set_body(parse_body());
    } else {
        expect(Token::TOK_SEMICOLON);
    }

    return decl.node();
}

ast::Variable* Parser::parse_variable() {
    // Variable = ident(ArraySpecifier)? (= Expr)?
    auto var = new_node<ast::Variable>();

    if (lookup_[0].is_ident()) {
        var->set_name(lookup_[0].ident());
    } else {
        error() << "Expected identifier\n";
    }

    eat(Token::TOK_IDENT);

    // Optional array specification
    if (lookup_[0].isa(Token::TOK_LBRACKET)) {
        var->set_array_specifier(parse_array_specifier());
    }

    // Optional initializer
    if (lookup_[0].isa(Token::TOK_ASSIGN)) {
        eat(Token::TOK_ASSIGN);
        var->set_initializer(parse_expr());
    }

    return var.node();
}

ast::Arg* Parser::parse_arg() {
    // Arg ::= Type (Name(ArraySpecifier)?)?
    auto arg = new_node<ast::Arg>();

    arg->set_type(parse_type());

    if (lookup_[0].is_ident()) {
        arg->set_name(lookup_[0].ident());
        eat(Token::TOK_IDENT);

        // Optional array specification
        if (lookup_[0].isa(Token::TOK_LBRACKET)) {
            arg->set_array_specifier(parse_array_specifier());
        }
    }

    return arg.node();
}

ast::List* Parser::parse_body() {
    auto body = new_node<ast::List>();
    eat(Token::TOK_LBRACE);
    expect(Token::TOK_RBRACE);
    return body.node();
}

ast::TypeQualifier* Parser::parse_type_qualifier() {
    // Qualifier ::= StorageQualifier | PrecisionQualifier | InterpolationQualifier | LayoutQualifier | SubroutineQualifier
    switch (lookup_[0].key().type()) {
#define SLANG_KEY_QUAL_STORAGE(key, str) case Key::KEY_##key: 
#include "slang/keywordlist.h"
            return parse_storage_qualifier();

#define SLANG_KEY_QUAL_PREC(key, str) case Key::KEY_##key: 
#include "slang/keywordlist.h"
            return parse_precision_qualifier();

#define SLANG_KEY_QUAL_INTERP(key, str) case Key::KEY_##key: 
#include "slang/keywordlist.h"
            return parse_interp_qualifier();

        case Key::KEY_LAYOUT: 
            return parse_layout_qualifier();

        case Key::KEY_SUBROUTINE:
            return parse_subroutine_qualifier();

        default:
            assert(0 && "Unknown qualifier");
            break;
    }

    return nullptr;
}

ast::StorageQualifier* Parser::parse_storage_qualifier() {
    auto storage = new_node<ast::StorageQualifier>();

    switch (lookup_[0].key().type()) {
#define SLANG_KEY_QUAL_STORAGE(key, str) \
        case Key::KEY_##key: storage->set_storage(ast::StorageQualifier::STORAGE_##key); break;
#include "slang/keywordlist.h"

        default:
            error() << "Unknown storage qualifier\n";
            break;
    }

    eat(Token::TOK_IDENT);

    return storage.node();
}

ast::PrecisionQualifier* Parser::parse_precision_qualifier() {
    auto prec = new_node<ast::PrecisionQualifier>();

    switch (lookup_[0].key().type()) {
#define SLANG_KEY_QUAL_PREC(key, str) \
        case Key::KEY_##key: prec->set_precision(ast::PrecisionQualifier::PREC_##key); break;
#include "slang/keywordlist.h"

        default:
            error() << "Unknown precision qualifier\n";
            break;
    }

    eat(Token::TOK_IDENT);

    return prec.node();
}

ast::InterpQualifier* Parser::parse_interp_qualifier() {
    auto interp = new_node<ast::InterpQualifier>();

    switch (lookup_[0].key().type()) {
#define SLANG_KEY_QUAL_INTERP(key, str) \
        case Key::KEY_##key: interp->set_interp(ast::InterpQualifier::INTERP_##key); break;
#include "slang/keywordlist.h"

        default:
            error() << "Unknown interpolation qualifier\n";
            break;
    }

    eat(Token::TOK_IDENT);

    return interp.node();
}

ast::LayoutQualifier* Parser::parse_layout_qualifier() {
    auto layout = new_node<ast::LayoutQualifier>();
    eat(Token::TOK_IDENT);
    return layout.node();
}

ast::SubroutineQualifier* Parser::parse_subroutine_qualifier() {
    auto subroutine = new_node<ast::SubroutineQualifier>();
    eat(Token::TOK_IDENT);
    return subroutine.node();
}

ast::ArraySpecifier* Parser::parse_array_specifier() {
    // ArraySpecifier ::= ([(Expr)?])+
    auto spec = new_node<ast::ArraySpecifier>();

    do {
        eat(Token::TOK_LBRACKET);

        // Optional integer expression
        if (lookup_[0].isa(Token::TOK_RBRACKET))
            spec->push_dim(nullptr);
        else
            spec->push_dim(parse_expr());

        expect(Token::TOK_RBRACKET);
    } while (lookup_[0].isa(Token::TOK_LBRACKET));

    return spec.node();
}

static ast::UnOpExpr::Type token_to_pre_unop(Token tok) {
    switch (tok.type()) {
        case Token::TOK_INC:    return ast::UnOpExpr::UNOP_INC;
        case Token::TOK_DEC:    return ast::UnOpExpr::UNOP_DEC;
        case Token::TOK_ADD:    return ast::UnOpExpr::UNOP_PLUS;
        case Token::TOK_SUB:    return ast::UnOpExpr::UNOP_MINUS;
        case Token::TOK_NEG:    return ast::UnOpExpr::UNOP_BIT_NOT;
        case Token::TOK_NOT:    return ast::UnOpExpr::UNOP_NOT;
        default: break;
    }
    return ast::UnOpExpr::UNOP_UNKNOWN;
}

static ast::UnOpExpr::Type token_to_post_unop(Token tok) {
    switch (tok.type()) {
        case Token::TOK_INC:    return ast::UnOpExpr::UNOP_POST_INC;
        case Token::TOK_DEC:    return ast::UnOpExpr::UNOP_POST_DEC;
        default: break;
    }
    return ast::UnOpExpr::UNOP_UNKNOWN;
}

static ast::BinOpExpr::Type token_to_binop(Token tok) {
    switch (tok.type()) {
        case Token::TOK_MUL:    return ast::BinOpExpr::BINOP_MUL;
        case Token::TOK_DIV:    return ast::BinOpExpr::BINOP_DIV;
        case Token::TOK_MOD:    return ast::BinOpExpr::BINOP_MOD;
        case Token::TOK_ADD:    return ast::BinOpExpr::BINOP_ADD;
        case Token::TOK_SUB:    return ast::BinOpExpr::BINOP_SUB;
        case Token::TOK_LSHIFT: return ast::BinOpExpr::BINOP_LSHIFT;
        case Token::TOK_RSHIFT: return ast::BinOpExpr::BINOP_RSHIFT;
        case Token::TOK_LT:     return ast::BinOpExpr::BINOP_LT;
        case Token::TOK_GT:     return ast::BinOpExpr::BINOP_GT;
        case Token::TOK_LEQ:    return ast::BinOpExpr::BINOP_LEQ;
        case Token::TOK_GEQ:    return ast::BinOpExpr::BINOP_GEQ;
        case Token::TOK_EQ:     return ast::BinOpExpr::BINOP_EQ;
        case Token::TOK_NEQ:    return ast::BinOpExpr::BINOP_NEQ;
        case Token::TOK_AND:    return ast::BinOpExpr::BINOP_AND;
        case Token::TOK_XOR:    return ast::BinOpExpr::BINOP_XOR;
        case Token::TOK_OR:     return ast::BinOpExpr::BINOP_OR;
        case Token::TOK_ANDAND: return ast::BinOpExpr::BINOP_ANDAND;
        case Token::TOK_OROR:   return ast::BinOpExpr::BINOP_OROR;
        default: break;
    }
    return ast::BinOpExpr::BINOP_UNKNOWN;
}

static int max_precedence = 12;

static int precedence(ast::BinOpExpr::Type type) {
    // Taken from GLSL 4.0 spec
    switch (type) {
        case ast::BinOpExpr::BINOP_MUL:    return 3;
        case ast::BinOpExpr::BINOP_DIV:    return 3;
        case ast::BinOpExpr::BINOP_MOD:    return 3;
        case ast::BinOpExpr::BINOP_ADD:    return 4;
        case ast::BinOpExpr::BINOP_SUB:    return 4;
        case ast::BinOpExpr::BINOP_LSHIFT: return 5;
        case ast::BinOpExpr::BINOP_RSHIFT: return 5;
        case ast::BinOpExpr::BINOP_LT:     return 6;
        case ast::BinOpExpr::BINOP_GT:     return 6;
        case ast::BinOpExpr::BINOP_LEQ:    return 6;
        case ast::BinOpExpr::BINOP_GEQ:    return 6;
        case ast::BinOpExpr::BINOP_EQ:     return 7;
        case ast::BinOpExpr::BINOP_NEQ:    return 7;
        case ast::BinOpExpr::BINOP_AND:    return 8;
        case ast::BinOpExpr::BINOP_XOR:    return 9;
        case ast::BinOpExpr::BINOP_OR:     return 10;
        case ast::BinOpExpr::BINOP_ANDAND: return 11;
        case ast::BinOpExpr::BINOP_OROR:   return 12;
        default: break;
    }
    assert(0 && "Unknown binary operation");
    return -1;
}

static bool left_associative(ast::BinOpExpr::Type type) {
    // Taken from GLSL 4.0 spec
    switch (type) {
        case ast::BinOpExpr::BINOP_MUL:    return true;
        case ast::BinOpExpr::BINOP_DIV:    return true;
        case ast::BinOpExpr::BINOP_MOD:    return true;
        case ast::BinOpExpr::BINOP_ADD:    return true;
        case ast::BinOpExpr::BINOP_SUB:    return true;
        case ast::BinOpExpr::BINOP_LSHIFT: return true;
        case ast::BinOpExpr::BINOP_RSHIFT: return true;
        case ast::BinOpExpr::BINOP_LT:     return true;
        case ast::BinOpExpr::BINOP_GT:     return true;
        case ast::BinOpExpr::BINOP_LEQ:    return true;
        case ast::BinOpExpr::BINOP_GEQ:    return true;
        case ast::BinOpExpr::BINOP_EQ:     return true;
        case ast::BinOpExpr::BINOP_NEQ:    return true;
        case ast::BinOpExpr::BINOP_AND:    return true;
        case ast::BinOpExpr::BINOP_XOR:    return true;
        case ast::BinOpExpr::BINOP_OR:     return true;
        case ast::BinOpExpr::BINOP_ANDAND: return true;
        case ast::BinOpExpr::BINOP_OROR:   return true;
        default: break;
    }
    assert(0 && "Unknown binary operation");
    return true;
}

static ast::AssignOpExpr::Type token_to_assignop(Token tok) {
    switch (tok.type()) {
        case Token::TOK_ASSIGN_MUL:    return ast::AssignOpExpr::ASSIGN_MUL;
        case Token::TOK_ASSIGN_DIV:    return ast::AssignOpExpr::ASSIGN_DIV;
        case Token::TOK_ASSIGN_MOD:    return ast::AssignOpExpr::ASSIGN_MOD;
        case Token::TOK_ASSIGN_ADD:    return ast::AssignOpExpr::ASSIGN_ADD;
        case Token::TOK_ASSIGN_SUB:    return ast::AssignOpExpr::ASSIGN_SUB;
        case Token::TOK_ASSIGN_LSHIFT: return ast::AssignOpExpr::ASSIGN_LSHIFT;
        case Token::TOK_ASSIGN_RSHIFT: return ast::AssignOpExpr::ASSIGN_RSHIFT;
        case Token::TOK_ASSIGN_AND:    return ast::AssignOpExpr::ASSIGN_AND;
        case Token::TOK_ASSIGN_XOR:    return ast::AssignOpExpr::ASSIGN_XOR;
        case Token::TOK_ASSIGN_OR:     return ast::AssignOpExpr::ASSIGN_OR;
        default: break;
    }
    return ast::AssignOpExpr::ASSIGN_UNKNOWN;
}

ast::Expr* Parser::parse_expr() {
    return parse_assign_expr();
}

ast::LiteralExpr* Parser::parse_literal_expr() {
    // LiteralExpr ::= lit
    auto expr = new_node<ast::LiteralExpr>();
    expr->set_literal(lookup_[0].lit());
    eat(Token::TOK_LIT);
    return expr.node();
}

ast::IdentExpr* Parser::parse_ident_expr() {
    // IdentExpr ::= ident
    auto expr = new_node<ast::IdentExpr>();
    expr->set_name(lookup_[0].ident());
    eat(Token::TOK_IDENT);
    return expr.node();
}

ast::FieldExpr* Parser::parse_field_expr(ast::Expr* left) {
    // FieldExpr ::= UnOpExpr . ident
    auto field = new_node<ast::FieldExpr>();
    eat(Token::TOK_DOT);
    field->set_left(left);
    if (lookup_[0].is_ident()) {
        field->set_field_name(lookup_[0].ident());
        eat(Token::TOK_IDENT);
    } else {
        error() << "Identifier expected\n";
    }
    return field.node();
}

ast::IndexExpr* Parser::parse_index_expr(ast::Expr* left) {
    // IndexExpr ::= UnOpExpr [Expr]
    auto index = new_node<ast::IndexExpr>();
    eat(Token::TOK_LBRACKET);
    index->set_left(left);
    index->set_index(parse_expr());
    expect(Token::TOK_RBRACKET);
    return index.node();
}

ast::Expr* Parser::parse_primary_expr() {
    // PrimExpr ::= ident | lit | (Expr)
    if (lookup_[0].isa(Token::TOK_IDENT) && lookup_[0].is_ident()) {
        return parse_ident_expr();
    } else if (lookup_[0].isa(Token::TOK_LIT)) {
        return parse_literal_expr();
    } else if (lookup_[0].isa(Token::TOK_LPAREN)) {
        eat(Token::TOK_LPAREN);
        ast::Expr* expr = parse_expr();
        expect(Token::TOK_RPAREN);
        return expr;
    } else {
        error() << "Primary expression expected\n";
        // Return error expression
        return new_node<ast::ErrorExpr>().node();
    }
}

ast::Expr* Parser::parse_unary_expr() {
    // UnOpExpr ::= ++ UnOpExpr | -- UnOpExpr | ! UnOpExpr
    //            |  ~ UnOpExpr | PrimExpr (++ | -- | . field | [Expr])*

    const ast::UnOpExpr::Type pre_type = token_to_pre_unop(lookup_[0]);

    // Prefix expression
    if (pre_type != ast::UnOpExpr::UNOP_UNKNOWN) {
        auto unop = new_node<ast::UnOpExpr>();
        unop->set_type(pre_type);
        lex();
        
        unop->set_operand(parse_unary_expr());
        return unop.node();
    }

    ast::Expr* expr = parse_primary_expr();

    // Postfix expression
    ast::UnOpExpr::Type post_type = token_to_post_unop(lookup_[0]);
    while (post_type != ast::UnOpExpr::UNOP_UNKNOWN ||
           lookup_[0].isa(Token::TOK_LBRACKET) ||
           lookup_[0].isa(Token::TOK_DOT)) {
        if (post_type != ast::UnOpExpr::UNOP_UNKNOWN) {
            auto unop = new_node<ast::UnOpExpr>();
            unop->set_type(post_type);
            lex();
            
            unop->set_operand(expr);

            expr = unop.node();
            post_type = token_to_post_unop(lookup_[0]);
        } else if (lookup_[0].isa(Token::TOK_LBRACKET)) {
            expr = parse_index_expr(expr);
        } else if (lookup_[0].isa(Token::TOK_DOT)) {
            expr = parse_field_expr(expr);
        } else {
            assert(0 && "Error in parser logic");
        }
    }

    return expr;
}

ast::Expr* Parser::parse_binary_expr(ast::Expr* left, int pred) {
    // BinOpExpr<precedence> ::= BinOpExpr<Precedence - 1> BinOp<precedence> BinOpExpr<Precedence>
    while (true) {
        const ast::BinOpExpr::Type type = token_to_binop(lookup_[0]);
        if (type == ast::BinOpExpr::BINOP_UNKNOWN)
            return left;

        // Only treat binary operators that have a smaller precedence level than the current one
        const int cur_pred = precedence(type);
        if (cur_pred > pred)
            return left;

        auto current = new_node<ast::BinOpExpr>();
        lex();

        ast::Expr* right = parse_unary_expr();

        // Check for precedence and associativity (Go into deeper operator level if needed)
        while (true) {
            ast::BinOpExpr::Type next_type = token_to_binop(lookup_[0]);
            if (next_type == ast::BinOpExpr::BINOP_UNKNOWN)
                break;

            int next_pred = precedence(next_type);
            if (next_pred >= cur_pred && (next_pred != cur_pred || left_associative(type)))
                break;

            // a + (b * c * d)
            right = parse_binary_expr(right, next_pred);
        }

        // a + (a * b * d) + e
        current->set_left(left);
        current->set_right(right);
        current->set_type(type);

        left = current.node();
    }
}

ast::Expr* Parser::parse_cond_expr(ast::Expr* left) {
    // CondExpr ::= BinOpExpr ? Expr : AssignOpExpr
    ast::Expr* binop = parse_binary_expr(left, max_precedence);

    if (lookup_[0].isa(Token::TOK_QMARK)) {
        auto cond = new_node<ast::CondExpr>();
        eat(Token::TOK_QMARK);

        cond->set_cond(binop);
        cond->set_if_true(parse_expr());
        expect(Token::TOK_COLON);
        cond->set_if_false(parse_assign_expr());

        return cond.node();
    }

    return binop;
}

ast::Expr* Parser::parse_assign_expr() {
    // AssignOpExpr ::= UnaryOpExpr (=|+=|-=|*=|/=|<<=|>>=|&=|^=||=) AssignOpExpr
    ast::Expr* left = parse_unary_expr();

    ast::AssignOpExpr::Type type = token_to_assignop(lookup_[0]);
    if (type != ast::AssignOpExpr::ASSIGN_UNKNOWN) {
        auto assign = new_node<ast::AssignOpExpr>();
        lex();
        assign->set_type(type);
        assign->set_left(left);
        assign->set_right(parse_assign_expr());
        return assign.node();
    }

    return parse_cond_expr(left);
}

} // namespace slang
