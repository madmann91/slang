#include <cassert>
#include <memory>

#include "slang/parser.h"

namespace slang {

Parser::Parser(std::function<Token()> input, Sema& sema, Logger& logger)
    : err_count_(0), input_(input), sema_(sema), logger_(logger)
{
    lookup_[0] = input();
    lookup_[1] = input();
    lookup_[2] = input();
    prev_ = lookup_[0].loc().start();
}

Ptr<ast::Module> Parser::parse() {
    return Ptr<ast::Module>(parse_module());
}

void Parser::next() {
    prev_ = lookup_[0].loc().end();
    lookup_[0] = lookup_[1];
    lookup_[1] = lookup_[2];
    lookup_[2] = input_();
}

void Parser::eat(Token::Type type) {
    assert(lookup_[0].type() == type);
    next();
}

void Parser::eat(Key::Type type) {
    assert(lookup_[0].key().type() == type);
    next();
}

void Parser::expect(Token::Type type) {
    if (lookup_[0].type() != type)
        error() << "\'" << Token::type_string(type) << "\' expected\n";

    next();
}

void Parser::expect(Key::Type type) {
    if (lookup_[0].key().type() != type)
        error() << "\'" << Key(type) << "\' expected\n";

    next();
}

std::ostream& Parser::error() {
    err_count_++;
    return logger_.error(lookup_[0].loc().start());
}

ast::Module* Parser::parse_module() {
    auto root = new_node<ast::Module>();

    while (!lookup_[0].is_eof()) {
        if (lookup_[0].isa(Token::IDENT)) {
            ast::Decl* decl = parse_decl();
            root->push_decl(decl);
            sema_.check(decl);
        } else {
            error() << "Keyword or identifier expected\n";
            next();
        }
    }

    return root.node();
}

ast::Decl* Parser::parse_decl() {
    // Declaration ::= FunctionDecl | FunctionDef | VariableDecl | PrecisionDecl
    if (lookup_[0].key().isa(Key::PRECISION)) {
        // PrecisionDecl
        return parse_precision_decl();
    }

    // We have to do some lookahead to determine if
    // it is a function or a variable declaration
    ast::Type* type = parse_type();

    if (lookup_[1].isa(Token::LPAREN)) {
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
    eat(Key::PRECISION);
    
    if (lookup_[0].key().isa(Key::LOWP) ||
        lookup_[0].key().isa(Key::HIGHP) ||
        lookup_[0].key().isa(Key::MEDIUMP))
        decl->precision().reset(parse_precision_qualifier());
    else
        error() << "Precision qualifier expected\n";

    while (lookup_[0].key().is_qualifier()) {
        eat(Token::IDENT);
        error() << "No qualifiers allowed inside precision declaration\n";
    }
    decl->type().reset(parse_type());

    expect(Token::SEMICOLON);

    return decl.node();
}

ast::Type* Parser::parse_type() {
    // Type ::= Qualifier* (StructType|NamedType|PrimType|InterfaceType) (ArraySpecifier)?
    PtrVector<ast::TypeQualifier> quals;

    while (lookup_[0].key().is_qualifier()) {
        quals.emplace_back(parse_type_qualifier());
    }

    ast::Type* type = nullptr;
    switch (lookup_[0].key().type()) {
        case Key::STRUCT:
            type = parse_struct_type();
            break;

#define SLANG_KEY_DATA(key, str, type, rows, cols) case Key::key:
#include "slang/keywordlist.h"
            type = parse_prim_type();
            break;

        case Key::UNKNOWN:
            if (lookup_[0].isa(Token::IDENT)) {
                if (lookup_[1].isa(Token::LBRACE))
                    type = parse_interface_type();
                else
                    type = parse_named_type();
                break;
            }
            // Generate an error type otherwise

        default:
            error() << "Identifier or data type expected\n";
            type = new_node<ast::ErrorType>().node();
    }

    // Parse optional array specification
    if (lookup_[0].isa(Token::LBRACKET)) {
        type->array_specifier().reset(parse_array_specifier());
    }

    // Register qualifiers
    for (auto& q : quals)
        type->push_qualifier(q.release());
    quals.clear();
    
    return type;
}

ast::NamedType* Parser::parse_named_type() {
    // NamedType ::= ident
    auto type = new_node<ast::NamedType>();

    type->set_name(lookup_[0].ident());
    eat(Token::IDENT);

    return type.node();
}

ast::StructType* Parser::parse_struct_type() {
    // StructType ::= struct (ident)? { (Type StructField(,StructField)*;)+ } )
    auto type = new_node<ast::StructType>();
    eat(Key::STRUCT);

    // Optional structure name
    if (lookup_[0].is_ident()) {
        type->set_name(lookup_[0].ident());
        eat(Token::IDENT);
    }

    expect(Token::LBRACE);
    
    while (lookup_[0].isa(Token::IDENT)) {
        ast::Type* field_type = parse_type();
        type->push_field(parse_variable_decl(field_type));
    }

    expect(Token::RBRACE);

    return type.node();
}

ast::InterfaceType* Parser::parse_interface_type() {
    // InterfaceType ::= ident { (Type InterfaceField(,InterfaceField)*;)+ } )
    auto type = new_node<ast::InterfaceType>();

    // Optional structure name
    if (lookup_[0].is_ident()) {
        type->set_name(lookup_[0].ident());
        eat(Token::IDENT);
    } else {
        error() << "Interface block name expected\n";
    }

    expect(Token::LBRACE);
    
    while (lookup_[0].isa(Token::IDENT)) {
        ast::Type* field_type = parse_type();
        type->push_field(parse_variable_decl(field_type));
    }

    expect(Token::RBRACE);

    return type.node();
}

ast::PrimType* Parser::parse_prim_type() {
    // PrimType ::= (int|float|double|bool|(b|d)?vec{2, 3, 4})
    auto prim = new_node<ast::PrimType>();

    switch (lookup_[0].key().type()) {
#define SLANG_KEY_DATA(key, str, type, rows, cols) \
        case Key::key: prim->set_prim(ast::PrimType::key); break;
#include "slang/keywordlist.h"

        default:
            assert(0 && "Invalid primitive type");
            break;
    }

    eat(Token::IDENT);

    return prim.node();
}

ast::VariableDecl* Parser::parse_variable_decl(ast::Type* type) {
    // VariableDecl ::= (Variable (,Variable)*)? ;
    auto decl = new_node<ast::VariableDecl>();

    decl->type().reset(type);

    if (lookup_[0].isa(Token::SEMICOLON)) {
        eat(Token::SEMICOLON);
        return decl.node();
    }

    decl->push_var(parse_variable());
    while (lookup_[0].isa(Token::COMMA)) {
        eat(Token::COMMA);
        decl->push_var(parse_variable());
    }

    expect(Token::SEMICOLON);

    return decl.node();
}

ast::FunctionDecl* Parser::parse_function_decl(ast::Type* type) {
    // FunctionDecl ::= Type ident ( (Arg)* ) ( { List } | ; )
    auto decl = new_node<ast::FunctionDecl>();

    if (lookup_[0].is_ident()) {
        decl->set_name(lookup_[0].ident());
        eat(Token::IDENT);
    } else {
        error() << "Function name expected\n";
    }

    decl->type().reset(type);

    expect(Token::LPAREN);

    // Optional argument list
    if (lookup_[0].isa(Token::IDENT))
    {
        decl->push_arg(parse_arg());
        while (lookup_[0].isa(Token::COMMA)) {
            eat(Token::COMMA);
            decl->push_arg(parse_arg());
        }
    }

    expect(Token::RPAREN);

    // Optional body
    if (lookup_[0].isa(Token::LBRACE)) {
        decl->body().reset(parse_compound_stmt());
    } else {
        expect(Token::SEMICOLON);
    }

    return decl.node();
}

ast::Variable* Parser::parse_variable() {
    // Variable = ident(ArraySpecifier)? (= Expr)?
    auto var = new_node<ast::Variable>();

    if (lookup_[0].is_ident()) {
        var->set_name(lookup_[0].ident());
        eat(Token::IDENT);
    } else {
        error() << "Variable name expected\n";
    }

    // Optional array specification
    if (lookup_[0].isa(Token::LBRACKET)) {
        var->array_specifier().reset(parse_array_specifier());
    }

    // Optional initializer
    if (lookup_[0].isa(Token::ASSIGN)) {
        eat(Token::ASSIGN);
        var->init().reset(parse_init_expr());
    }

    return var.node();
}

ast::Arg* Parser::parse_arg() {
    // Arg ::= Type (Name(ArraySpecifier)?)?
    auto arg = new_node<ast::Arg>();

    arg->type().reset(parse_type());

    if (lookup_[0].is_ident()) {
        arg->set_name(lookup_[0].ident());
        eat(Token::IDENT);

        // Optional array specification
        if (lookup_[0].isa(Token::LBRACKET)) {
            arg->array_specifier().reset(parse_array_specifier());
        }
    }

    return arg.node();
}

ast::TypeQualifier* Parser::parse_type_qualifier() {
    // Qualifier ::= StorageQualifier | PrecisionQualifier | InterpolationQualifier | LayoutQualifier | SubroutineQualifier
    switch (lookup_[0].key().type()) {
#define SLANG_KEY_QUAL_STORAGE(key, str) case Key::key: 
#include "slang/keywordlist.h"
            return parse_storage_qualifier();

#define SLANG_KEY_QUAL_PREC(key, str) case Key::key: 
#include "slang/keywordlist.h"
            return parse_precision_qualifier();

#define SLANG_KEY_QUAL_INTERP(key, str) case Key::key: 
#include "slang/keywordlist.h"
            return parse_interp_qualifier();

        case Key::LAYOUT: 
            return parse_layout_qualifier();

        case Key::SUBROUTINE:
            return parse_subroutine_qualifier();

#define PARSE_SIMPLE_QUAL(Key, Qual) \
        case Key: \
            { \
                auto qual = new_node<Qual>(); \
                eat(Key); \
                return qual.node(); \
            }

        PARSE_SIMPLE_QUAL(Key::INVARIANT, ast::InvariantQualifier)
        PARSE_SIMPLE_QUAL(Key::VARYING,   ast::VaryingQualifier)
        PARSE_SIMPLE_QUAL(Key::ATTRIBUTE, ast::AttributeQualifier)

#undef PARSE_SIMPLE_QUAL

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
        case Key::key: storage->set_storage(ast::StorageQualifier::key); break;
#include "slang/keywordlist.h"

        default:
            assert(0 && "Invalid storage qualifier");
            break;
    }

    eat(Token::IDENT);

    return storage.node();
}

ast::PrecisionQualifier* Parser::parse_precision_qualifier() {
    auto prec = new_node<ast::PrecisionQualifier>();

    switch (lookup_[0].key().type()) {
#define SLANG_KEY_QUAL_PREC(key, str) \
        case Key::key: prec->set_precision(ast::PrecisionQualifier::key); break;
#include "slang/keywordlist.h"

        default:
            assert(0 && "Invalid precision qualifier");
            break;
    }

    eat(Token::IDENT);

    return prec.node();
}

ast::InterpQualifier* Parser::parse_interp_qualifier() {
    auto interp = new_node<ast::InterpQualifier>();

    switch (lookup_[0].key().type()) {
#define SLANG_KEY_QUAL_INTERP(key, str) \
        case Key::key: interp->set_interp(ast::InterpQualifier::key); break;
#include "slang/keywordlist.h"

        default:
            assert(0 && "Invalid interpolation qualifier");
            break;
    }

    eat(Token::IDENT);

    return interp.node();
}

ast::LayoutQualifier* Parser::parse_layout_qualifier() {
    // LayoutQualifier ::= layout ( (ident | ident = CondExpr)+ )
    auto layout = new_node<ast::LayoutQualifier>();
    eat(Key::LAYOUT);
    expect(Token::LPAREN);

    while (lookup_[0].isa(Token::IDENT)) {
        const std::string ident = lookup_[0].ident();
        eat(Token::IDENT);

        if (lookup_[0].isa(Token::ASSIGN)) {
            eat(Token::ASSIGN);
            layout->push_layout(ident, parse_cond_expr(parse_unary_expr()));
        } else {
            // No right side
            layout->push_layout(ident, nullptr);
        }

        if (!lookup_[0].isa(Token::COMMA))
            break;

        eat(Token::COMMA);
    }

    if (layout->num_layouts() == 0)
        error() << "Empty layout qualifier\n";

    expect(Token::RPAREN);
    return layout.node();
}

ast::SubroutineQualifier* Parser::parse_subroutine_qualifier() {
    // SubroutineQualifier ::= subroutine ( ident (,ident)* )
    auto subroutine = new_node<ast::SubroutineQualifier>();
    eat(Key::SUBROUTINE);
    expect(Token::LPAREN);

    while (lookup_[0].isa(Token::IDENT)) {
        subroutine->push_name(lookup_[0].ident());
        eat(Token::IDENT);
        if (!lookup_[0].isa(Token::COMMA))
            break;
        eat(Token::COMMA);
    }

    if (subroutine->num_names() == 0)
        error() << "Empty subroutine qualifier\n";

    expect(Token::RPAREN);
    return subroutine.node();
}

ast::ArraySpecifier* Parser::parse_array_specifier() {
    // ArraySpecifier ::= ([(CondExpr)?])+
    auto spec = new_node<ast::ArraySpecifier>();

    do {
        eat(Token::LBRACKET);

        // Optional integer expression
        if (lookup_[0].isa(Token::RBRACKET))
            spec->push_dim(nullptr);
        else
            spec->push_dim(parse_cond_expr(parse_unary_expr()));

        expect(Token::RBRACKET);
    } while (lookup_[0].isa(Token::LBRACKET));

    return spec.node();
}

static ast::UnOpExpr::Type token_to_pre_unop(Token tok) {
    switch (tok.type()) {
        case Token::INC:    return ast::UnOpExpr::INC;
        case Token::DEC:    return ast::UnOpExpr::DEC;
        case Token::ADD:    return ast::UnOpExpr::PLUS;
        case Token::SUB:    return ast::UnOpExpr::MINUS;
        case Token::NEG:    return ast::UnOpExpr::BIT_NOT;
        case Token::NOT:    return ast::UnOpExpr::NOT;
        default: break;
    }
    return ast::UnOpExpr::UNKNOWN;
}

static ast::UnOpExpr::Type token_to_post_unop(Token tok) {
    switch (tok.type()) {
        case Token::INC:    return ast::UnOpExpr::POST_INC;
        case Token::DEC:    return ast::UnOpExpr::POST_DEC;
        default: break;
    }
    return ast::UnOpExpr::UNKNOWN;
}

static ast::BinOpExpr::Type token_to_binop(Token tok) {
    switch (tok.type()) {
        case Token::MUL:    return ast::BinOpExpr::MUL;
        case Token::DIV:    return ast::BinOpExpr::DIV;
        case Token::MOD:    return ast::BinOpExpr::MOD;
        case Token::ADD:    return ast::BinOpExpr::ADD;
        case Token::SUB:    return ast::BinOpExpr::SUB;
        case Token::LSHIFT: return ast::BinOpExpr::LSHIFT;
        case Token::RSHIFT: return ast::BinOpExpr::RSHIFT;
        case Token::LT:     return ast::BinOpExpr::LT;
        case Token::GT:     return ast::BinOpExpr::GT;
        case Token::LEQ:    return ast::BinOpExpr::LEQ;
        case Token::GEQ:    return ast::BinOpExpr::GEQ;
        case Token::EQ:     return ast::BinOpExpr::EQ;
        case Token::NEQ:    return ast::BinOpExpr::NEQ;
        case Token::AND:    return ast::BinOpExpr::AND;
        case Token::XOR:    return ast::BinOpExpr::XOR;
        case Token::OR:     return ast::BinOpExpr::OR;
        case Token::ANDAND: return ast::BinOpExpr::ANDAND;
        case Token::XORXOR: return ast::BinOpExpr::XORXOR;
        case Token::OROR:   return ast::BinOpExpr::OROR;
        default: break;
    }
    return ast::BinOpExpr::UNKNOWN;
}

static ast::AssignOpExpr::Type token_to_assignop(Token tok) {
    switch (tok.type()) {
        case Token::ASSIGN:        return ast::AssignOpExpr::ASSIGN;
        case Token::ASSIGN_MUL:    return ast::AssignOpExpr::ASSIGN_MUL;
        case Token::ASSIGN_DIV:    return ast::AssignOpExpr::ASSIGN_DIV;
        case Token::ASSIGN_MOD:    return ast::AssignOpExpr::ASSIGN_MOD;
        case Token::ASSIGN_ADD:    return ast::AssignOpExpr::ASSIGN_ADD;
        case Token::ASSIGN_SUB:    return ast::AssignOpExpr::ASSIGN_SUB;
        case Token::ASSIGN_LSHIFT: return ast::AssignOpExpr::ASSIGN_LSHIFT;
        case Token::ASSIGN_RSHIFT: return ast::AssignOpExpr::ASSIGN_RSHIFT;
        case Token::ASSIGN_AND:    return ast::AssignOpExpr::ASSIGN_AND;
        case Token::ASSIGN_XOR:    return ast::AssignOpExpr::ASSIGN_XOR;
        case Token::ASSIGN_OR:     return ast::AssignOpExpr::ASSIGN_OR;
        default: break;
    }
    return ast::AssignOpExpr::UNKNOWN;
}

static int max_precedence = 13;

int precedence(ast::BinOpExpr::Type type) {
    // Taken from GLSL 4.5 spec
    switch (type) {
        case ast::BinOpExpr::MUL:    return 3;
        case ast::BinOpExpr::DIV:    return 3;
        case ast::BinOpExpr::MOD:    return 3;
        case ast::BinOpExpr::ADD:    return 4;
        case ast::BinOpExpr::SUB:    return 4;
        case ast::BinOpExpr::LSHIFT: return 5;
        case ast::BinOpExpr::RSHIFT: return 5;
        case ast::BinOpExpr::LT:     return 6;
        case ast::BinOpExpr::GT:     return 6;
        case ast::BinOpExpr::LEQ:    return 6;
        case ast::BinOpExpr::GEQ:    return 6;
        case ast::BinOpExpr::EQ:     return 7;
        case ast::BinOpExpr::NEQ:    return 7;
        case ast::BinOpExpr::AND:    return 8;
        case ast::BinOpExpr::XOR:    return 9;
        case ast::BinOpExpr::OR:     return 10;
        case ast::BinOpExpr::ANDAND: return 11;
        case ast::BinOpExpr::XORXOR: return 12;
        case ast::BinOpExpr::OROR:   return 13;
        default: break;
    }
    assert(0 && "Unknown binary operation");
    return -1;
}

bool left_associative(ast::BinOpExpr::Type type) {
    // Taken from GLSL 4.5 spec
    switch (type) {
        case ast::BinOpExpr::MUL:    return true;
        case ast::BinOpExpr::DIV:    return true;
        case ast::BinOpExpr::MOD:    return true;
        case ast::BinOpExpr::ADD:    return true;
        case ast::BinOpExpr::SUB:    return true;
        case ast::BinOpExpr::LSHIFT: return true;
        case ast::BinOpExpr::RSHIFT: return true;
        case ast::BinOpExpr::LT:     return true;
        case ast::BinOpExpr::GT:     return true;
        case ast::BinOpExpr::LEQ:    return true;
        case ast::BinOpExpr::GEQ:    return true;
        case ast::BinOpExpr::EQ:     return true;
        case ast::BinOpExpr::NEQ:    return true;
        case ast::BinOpExpr::AND:    return true;
        case ast::BinOpExpr::XOR:    return true;
        case ast::BinOpExpr::OR:     return true;
        case ast::BinOpExpr::ANDAND: return true;
        case ast::BinOpExpr::XORXOR: return true;
        case ast::BinOpExpr::OROR:   return true;
        default: break;
    }
    assert(0 && "Unknown binary operation");
    return true;
}

ast::Expr* Parser::parse_expr() {
    // Expr ::= AssignExpr (, AssignExpr)*
    ast::Expr* first = parse_assign_expr();
    if (lookup_[0].isa(Token::COMMA)) {
        auto list = new_node<ast::ExprList>();
        list->push_expr(first);

        do {
            eat(Token::COMMA);
            list->push_expr(parse_assign_expr());
        } while (lookup_[0].isa(Token::COMMA));

        return list.node();
    }
    return first;
}

ast::LiteralExpr* Parser::parse_literal_expr() {
    // LiteralExpr ::= lit
    auto expr = new_node<ast::LiteralExpr>();
    expr->set_literal(lookup_[0].lit());
    eat(Token::LIT);
    return expr.node();
}

ast::IdentExpr* Parser::parse_ident_expr() {
    // IdentExpr ::= ident
    auto expr = new_node<ast::IdentExpr>();
    expr->set_ident(lookup_[0].ident());
    eat(Token::IDENT);
    return expr.node();
}

ast::FieldExpr* Parser::parse_field_expr(ast::Expr* left) {
    // FieldExpr ::= UnOpExpr . ident
    auto field = new_node<ast::FieldExpr>();
    eat(Token::DOT);
    field->left().reset(left);
    if (lookup_[0].is_ident()) {
        field->set_field_name(lookup_[0].ident());
        eat(Token::IDENT);
    } else {
        error() << "Identifier expected in field selection\n";
    }
    return field.node();
}

ast::IndexExpr* Parser::parse_index_expr(ast::Expr* left) {
    // IndexExpr ::= UnOpExpr [Expr]
    auto index = new_node<ast::IndexExpr>();
    eat(Token::LBRACKET);
    index->left().reset(left);
    index->index().reset(parse_expr());
    expect(Token::RBRACKET);
    return index.node();
}

static bool is_constructor(Sema& sema, const Token& tok) {
    assert(tok.isa(Token::IDENT));
    if (tok.key().is_data())
        return true;
    if (auto symbol = sema.env()->find_symbol(tok.ident()))
        return symbol->is_structure();
    return false;
}

ast::CallExpr* Parser::parse_call_expr(ast::Expr* callee = nullptr) {
    // CallExpr ::= (ident|Type) '(' ((void)? | AssignExpr(,AssignExpr)*) ')'
    auto call = new_node<ast::CallExpr>();

    if (callee)
        call->function().reset(callee);
    else
        call->constructor().reset(parse_type());

    expect(Token::LPAREN);

    if (lookup_[0].key().isa(Key::VOID)) {
        // No parameters
        eat(Key::VOID);
    } else if (!lookup_[0].isa(Token::RPAREN)) {
        call->push_arg(parse_assign_expr());
        while (lookup_[0].isa(Token::COMMA)) {
            eat(Token::COMMA);
            call->push_arg(parse_assign_expr());
        }
    }

    expect(Token::RPAREN);

    return call.node();
}

ast::Expr* Parser::parse_primary_expr() {
    // PrimExpr ::= CallExpr | ident | lit | (Expr)
    if (lookup_[0].isa(Token::IDENT)) {
        if (is_constructor(sema_, lookup_[0]))
            return parse_call_expr();
        else
            return parse_ident_expr();
    } else if (lookup_[0].isa(Token::LIT)) {
        return parse_literal_expr();
    } else if (lookup_[0].isa(Token::LPAREN)) {
        eat(Token::LPAREN);
        ast::Expr* expr = parse_expr();
        expect(Token::RPAREN);
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
    if (pre_type != ast::UnOpExpr::UNKNOWN) {
        auto unop = new_node<ast::UnOpExpr>();
        unop->set_type(pre_type);
        next();
        
        unop->operand().reset(parse_unary_expr());
        return unop.node();
    }

    ast::Expr* expr = parse_primary_expr();

    // Postfix expression
    ast::UnOpExpr::Type post_type = token_to_post_unop(lookup_[0]);
    while (post_type != ast::UnOpExpr::UNKNOWN ||
           lookup_[0].isa(Token::LBRACKET) ||
           lookup_[0].isa(Token::DOT) ||
           lookup_[0].isa(Token::LPAREN)) {
        if (post_type != ast::UnOpExpr::UNKNOWN) {
            auto unop = new_node<ast::UnOpExpr>();
            unop->set_type(post_type);
            next();
            
            unop->operand().reset(expr);

            expr = unop.node();
            post_type = token_to_post_unop(lookup_[0]);
        } else if (lookup_[0].isa(Token::LBRACKET)) {
            expr = parse_index_expr(expr);
        } else if (lookup_[0].isa(Token::DOT)) {
            expr = parse_field_expr(expr);
        } else if (lookup_[0].isa(Token::LPAREN)) {
            expr = parse_call_expr(expr);
        } else {
            assert(0 && "Error in parser logic");
        }
    }

    return expr;
}

ast::Expr* Parser::parse_binary_expr(ast::Expr* left, int pred) {
    // BinOpExpr<Precedence> ::= BinOpExpr<Precedence - 1> BinOp<Precedence> BinOpExpr<Precedence>
    while (true) {
        const ast::BinOpExpr::Type type = token_to_binop(lookup_[0]);
        if (type == ast::BinOpExpr::UNKNOWN)
            return left;

        // Only treat binary operators that have a smaller precedence level than the current one
        const int cur_pred = precedence(type);
        if (cur_pred > pred)
            return left;

        auto current = new_node<ast::BinOpExpr>();
        next();

        ast::Expr* right = parse_unary_expr();

        // Check for precedence and associativity (Go into deeper operator level if needed)
        while (true) {
            ast::BinOpExpr::Type next_type = token_to_binop(lookup_[0]);
            if (next_type == ast::BinOpExpr::UNKNOWN)
                break;

            int next_pred = precedence(next_type);
            if (next_pred >= cur_pred && (next_pred != cur_pred || left_associative(type)))
                break;

            // a + (b * c * d)
            right = parse_binary_expr(right, next_pred);
        }

        // a + (a * b * d) + e
        current->left().reset(left);
        current->right().reset(right);
        current->set_type(type);

        left = current.node();
    }
}

ast::Expr* Parser::parse_cond_expr(ast::Expr* left) {
    // CondExpr ::= BinOpExpr ? Expr : AssignOpExpr
    ast::Expr* binop = parse_binary_expr(left, max_precedence);

    if (lookup_[0].isa(Token::QMARK)) {
        auto cond = new_node<ast::CondExpr>();
        eat(Token::QMARK);

        cond->cond().reset(binop);
        cond->if_true().reset(parse_expr());
        expect(Token::COLON);
        cond->if_false().reset(parse_assign_expr());

        return cond.node();
    }

    return binop;
}

ast::Expr* Parser::parse_assign_expr() {
    // AssignOpExpr ::= UnaryOpExpr (=|+=|-=|*=|/=|<<=|>>=|&=|^=||=) AssignOpExpr
    ast::Expr* left = parse_unary_expr();

    ast::AssignOpExpr::Type type = token_to_assignop(lookup_[0]);
    if (type != ast::AssignOpExpr::UNKNOWN) {
        auto assign = new_node<ast::AssignOpExpr>();
        next();
        assign->set_type(type);
        assign->left().reset(left);
        assign->right().reset(parse_assign_expr());
        return assign.node();
    }

    return parse_cond_expr(left);
}

ast::Expr* Parser::parse_init_expr() {
    // InitExpr ::= AssignExpr | { InitExpr(, InitExpr)* (,)? }
    if (lookup_[0].isa(Token::LBRACE)) {
        auto init = new_node<ast::InitExpr>();
        eat(Token::LBRACE);
        if (!lookup_[0].isa(Token::RBRACE)) {
            init->push_expr(parse_init_expr());

            while (lookup_[0].isa(Token::COMMA) && !lookup_[1].isa(Token::RBRACE)) {
                eat(Token::COMMA);
                init->push_expr(parse_init_expr());
            }

            if (lookup_[0].isa(Token::COMMA))
                eat(Token::COMMA);
        }
        expect(Token::RBRACE);
        return init.node();
    }

    return parse_assign_expr();
}

ast::LoopCond* Parser::parse_loop_cond() {
    // LoopCond ::= Type Variable | Expr
    auto cond = new_node<ast::LoopCond>();

    if (lookup_[0].is_keyword()) {
        switch (lookup_[0].key().type()) {
#define SLANG_KEY_DATA(key, str, type, rows, cols) case Key::key:
#define SLANG_KEY_QUAL(key, str) case Key::key:
#include "slang/keywordlist.h"
            case Key::STRUCT:
                cond->var_type().reset(parse_type());
                cond->var().reset(parse_variable());
                break;
            default: break;
        }
    } else if (lookup_[0].is_ident() && lookup_[1].is_ident()) {
        cond->var_type().reset(parse_type());
        cond->var().reset(parse_variable());
    } else {
        cond->expr().reset(parse_expr());
    }

    return cond.node();
}

ast::Stmt* Parser::parse_stmt() {
    // Stmt ::= IfStmt | SwitchStmt | WhileLoopStmt | ForLoopStmt | DoWhileLoopStmt
    //        | CaseLabelStmt | DeclStmt | ExprStmt
    if (lookup_[0].is_keyword()) {
        switch (lookup_[0].key().type()) {
            case Key::IF:      return parse_if_stmt();
            case Key::SWITCH:  return parse_switch_stmt();

            case Key::WHILE:   return parse_while_stmt();
            case Key::FOR:     return parse_for_stmt();
            case Key::DO:      return parse_do_while_stmt();

            case Key::DEFAULT: return parse_case_stmt(true);
            case Key::CASE:    return parse_case_stmt(false);

            case Key::RETURN:  return parse_return_stmt();

#define PARSE_JUMP_STMT(Key, Stmt) \
    case Key: \
        { \
            auto stmt = new_node<Stmt>(); \
            eat(Key); \
            expect(Token::SEMICOLON); \
            return stmt.node(); \
        }

            PARSE_JUMP_STMT(Key::BREAK,    ast::BreakStmt)
            PARSE_JUMP_STMT(Key::CONTINUE, ast::ContinueStmt)
            PARSE_JUMP_STMT(Key::DISCARD,  ast::DiscardStmt)

#undef PARSE_JUMP_STMT

#define SLANG_KEY_DATA(key, str, type, rows, cols) case Key::key:
#define SLANG_KEY_QUAL(key, str) case Key::key:
#include "slang/keywordlist.h"
            case Key::STRUCT:
                return parse_decl_stmt();

            default: break;
        }
    } else if (lookup_[0].isa(Token::LBRACE)) {
        return parse_compound_stmt();
    } else if (lookup_[0].is_ident() && lookup_[1].is_ident()) {
        return parse_decl_stmt();
    }

    return parse_expr_stmt();
}

ast::StmtList* Parser::parse_compound_stmt() {
    // StmtList ::= { (Stmt)* }
    auto list = new_node<ast::StmtList>();
    eat(Token::LBRACE);
    while (lookup_[0].isa(Token::IDENT) ||
           lookup_[0].isa(Token::LIT) ||
           lookup_[0].isa(Token::LPAREN) ||
           lookup_[0].isa(Token::LBRACE) ||
           lookup_[0].isa(Token::SEMICOLON) ||
           token_to_pre_unop(lookup_[0]) != ast::UnOpExpr::UNKNOWN) {
        list->push_stmt(parse_stmt());
    }
    expect(Token::RBRACE);
    return list.node();
}

ast::IfStmt* Parser::parse_if_stmt() {
    // IfStmt ::= if ( Expr ) (Stmt | Stmt else Stmt)
    auto stmt = new_node<ast::IfStmt>();

    eat(Key::IF);
    expect(Token::LPAREN);
    stmt->cond().reset(parse_expr());
    expect(Token::RPAREN);

    stmt->if_true().reset(parse_stmt());

    if (lookup_[0].key().type() == Key::ELSE) {
        eat(Key::ELSE);
        stmt->if_false().reset(parse_stmt());
    }

    return stmt.node();
}

ast::SwitchStmt* Parser::parse_switch_stmt() {
    // SwitchStmt ::= switch ( Expr ) { (Stmt)* }
    auto stmt = new_node<ast::SwitchStmt>();
    
    eat(Key::SWITCH);
    expect(Token::LPAREN);
    stmt->expr().reset(parse_expr());
    expect(Token::RPAREN);

    stmt->list().reset(parse_compound_stmt());

    return stmt.node();
}

ast::WhileLoopStmt* Parser::parse_while_stmt() {
    // WhileLoopStmt ::= while ( LoopCond ) Stmt
    auto stmt = new_node<ast::WhileLoopStmt>();
    eat(Key::WHILE);
    expect(Token::LPAREN);
    stmt->cond().reset(parse_loop_cond());
    expect(Token::RPAREN);
    stmt->body().reset(parse_stmt());
    return stmt.node();
}

ast::ForLoopStmt* Parser::parse_for_stmt() {
    // ForLoopStmt ::= for ( (Stmt)? ; (LoopCond)? ; (Expr)? )
    auto stmt = new_node<ast::ForLoopStmt>();
    eat(Key::FOR);
    expect(Token::LPAREN);

    if (lookup_[0].isa(Token::SEMICOLON)) {
        eat(Token::SEMICOLON);
    } else { 
        stmt->init().reset(parse_stmt());
    }

    if (lookup_[0].isa(Token::SEMICOLON)) {
        eat(Token::SEMICOLON);
    } else {
        stmt->cond().reset(parse_loop_cond());
        expect(Token::SEMICOLON);
    }

    if (lookup_[0].isa(Token::RPAREN)) {
        eat(Token::RPAREN);
    } else { 
        stmt->iter().reset(parse_expr());
        expect(Token::RPAREN);
    }

    stmt->body().reset(parse_stmt());
    return stmt.node();
}

ast::DoWhileLoopStmt* Parser::parse_do_while_stmt() {
    // DoWhileLoopStmt ::= do Stmt while ( LoopCond ) ;
    auto stmt = new_node<ast::DoWhileLoopStmt>();
    eat(Key::DO);
    stmt->body().reset(parse_stmt());

    expect(Key::WHILE);
    expect(Token::LPAREN);

    stmt->cond().reset(parse_loop_cond());
    if (stmt->cond()->is_var())
        error() << "Variable declarations are not allowed in do-while loop conditions\n";

    expect(Token::RPAREN);
    expect(Token::SEMICOLON);
    return stmt.node();
}

ast::CaseLabelStmt* Parser::parse_case_stmt(bool def) {
    // CaseLabelStmt ::= (case ident | default) :
    auto stmt = new_node<ast::CaseLabelStmt>();
    if (!def) {
        eat(Key::CASE);
        stmt->expr().reset(parse_expr());
    } else {
        eat(Key::DEFAULT);
    }
    expect(Token::COLON);
    return stmt.node();
}

ast::DeclStmt* Parser::parse_decl_stmt() {
    // DeclStmt ::= Decl
    auto stmt = new_node<ast::DeclStmt>();
    stmt->decl().reset(parse_decl());
    return stmt.node();
}

ast::ExprStmt* Parser::parse_expr_stmt() {
    // ExprStmt ::= Expr ;
    auto stmt = new_node<ast::ExprStmt>();
    if (lookup_[0].isa(Token::SEMICOLON)) {
        eat(Token::SEMICOLON);
    } else {
        stmt->expr().reset(parse_expr());
        expect(Token::SEMICOLON);
    }
    return stmt.node();
}

ast::ReturnStmt* Parser::parse_return_stmt() {
    // ReturnStmt ::= return (Expr)? ;
    auto stmt = new_node<ast::ReturnStmt>();
    eat(Key::RETURN);
    if (lookup_[0].isa(Token::SEMICOLON)) {
        eat(Token::SEMICOLON);
    } else {
        stmt->value().reset(parse_expr());
        expect(Token::SEMICOLON);
    }
    return stmt.node();
}

} // namespace slang
