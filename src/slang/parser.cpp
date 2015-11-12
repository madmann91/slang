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

std::unique_ptr<ast::Module> Parser::parse() {
    return std::unique_ptr<ast::Module>(parse_module());
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
    if (lookup_[0].key().isa(Key::KEY_PRECISION)) {
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
    eat(Key::KEY_PRECISION);
    
    if (lookup_[0].key().isa(Key::KEY_LOWP) ||
        lookup_[0].key().isa(Key::KEY_HIGHP) ||
        lookup_[0].key().isa(Key::KEY_MEDIUMP))
        decl->set_precision(parse_precision_qualifier());
    else
        error() << "Precision qualifier expected\n";

    while (lookup_[0].key().is_qualifier()) {
        eat(Token::IDENT);
        error() << "No qualifiers allowed inside precision declaration\n";
    }
    decl->set_type(parse_type());

    expect(Token::SEMICOLON);

    return decl.node();
}

ast::Type* Parser::parse_type() {
    // Type ::= Qualifier* (StructType|NamedType|PrimType|InterfaceType) (ArraySpecifier)?
    PtrVector<ast::TypeQualifier> quals;

    while (lookup_[0].key().is_qualifier()) {
        quals.push_back(parse_type_qualifier());
    }

    ast::Type* type = nullptr;
    switch (lookup_[0].key().type()) {
        case Key::KEY_STRUCT:
            type = parse_struct_type();
            break;

#define SLANG_KEY_DATA(key, str, type, rows, cols) case Key::KEY_##key:
#include "slang/keywordlist.h"
            type = parse_prim_type();
            break;

        case Key::KEY_UNKNOWN:
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
        type->set_array_specifier(parse_array_specifier());
    }

    // Register qualifiers
    for (auto& q : quals)
        type->push_qualifier(q);
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
    eat(Key::KEY_STRUCT);

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
        case Key::KEY_##key: prim->set_prim(ast::PrimType::PRIM_##key); break;
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

    decl->set_type(type);

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

    decl->set_type(type);

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
        decl->set_body(parse_compound_stmt());
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
        var->set_array_specifier(parse_array_specifier());
    }

    // Optional initializer
    if (lookup_[0].isa(Token::ASSIGN)) {
        eat(Token::ASSIGN);
        var->set_init(parse_init_expr());
    }

    return var.node();
}

ast::Arg* Parser::parse_arg() {
    // Arg ::= Type (Name(ArraySpecifier)?)?
    auto arg = new_node<ast::Arg>();

    arg->set_type(parse_type());

    if (lookup_[0].is_ident()) {
        arg->set_name(lookup_[0].ident());
        eat(Token::IDENT);

        // Optional array specification
        if (lookup_[0].isa(Token::LBRACKET)) {
            arg->set_array_specifier(parse_array_specifier());
        }
    }

    return arg.node();
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

#define PARSE_SIMPLE_QUAL(Key, Qual) \
        case Key: \
            { \
                auto qual = new_node<Qual>(); \
                eat(Key); \
                return qual.node(); \
            }

        PARSE_SIMPLE_QUAL(Key::KEY_INVARIANT, ast::InvariantQualifier)
        PARSE_SIMPLE_QUAL(Key::KEY_VARYING,   ast::VaryingQualifier)
        PARSE_SIMPLE_QUAL(Key::KEY_ATTRIBUTE, ast::AttributeQualifier)

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
        case Key::KEY_##key: storage->set_storage(ast::StorageQualifier::STORAGE_##key); break;
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
        case Key::KEY_##key: prec->set_precision(ast::PrecisionQualifier::PREC_##key); break;
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
        case Key::KEY_##key: interp->set_interp(ast::InterpQualifier::INTERP_##key); break;
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
    eat(Key::KEY_LAYOUT);
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
    eat(Key::KEY_SUBROUTINE);
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
        case Token::INC:    return ast::UnOpExpr::UNOP_INC;
        case Token::DEC:    return ast::UnOpExpr::UNOP_DEC;
        case Token::ADD:    return ast::UnOpExpr::UNOP_PLUS;
        case Token::SUB:    return ast::UnOpExpr::UNOP_MINUS;
        case Token::NEG:    return ast::UnOpExpr::UNOP_BIT_NOT;
        case Token::NOT:    return ast::UnOpExpr::UNOP_NOT;
        default: break;
    }
    return ast::UnOpExpr::UNOP_UNKNOWN;
}

static ast::UnOpExpr::Type token_to_post_unop(Token tok) {
    switch (tok.type()) {
        case Token::INC:    return ast::UnOpExpr::UNOP_POST_INC;
        case Token::DEC:    return ast::UnOpExpr::UNOP_POST_DEC;
        default: break;
    }
    return ast::UnOpExpr::UNOP_UNKNOWN;
}

static ast::BinOpExpr::Type token_to_binop(Token tok) {
    switch (tok.type()) {
        case Token::MUL:    return ast::BinOpExpr::BINOP_MUL;
        case Token::DIV:    return ast::BinOpExpr::BINOP_DIV;
        case Token::MOD:    return ast::BinOpExpr::BINOP_MOD;
        case Token::ADD:    return ast::BinOpExpr::BINOP_ADD;
        case Token::SUB:    return ast::BinOpExpr::BINOP_SUB;
        case Token::LSHIFT: return ast::BinOpExpr::BINOP_LSHIFT;
        case Token::RSHIFT: return ast::BinOpExpr::BINOP_RSHIFT;
        case Token::LT:     return ast::BinOpExpr::BINOP_LT;
        case Token::GT:     return ast::BinOpExpr::BINOP_GT;
        case Token::LEQ:    return ast::BinOpExpr::BINOP_LEQ;
        case Token::GEQ:    return ast::BinOpExpr::BINOP_GEQ;
        case Token::EQ:     return ast::BinOpExpr::BINOP_EQ;
        case Token::NEQ:    return ast::BinOpExpr::BINOP_NEQ;
        case Token::AND:    return ast::BinOpExpr::BINOP_AND;
        case Token::XOR:    return ast::BinOpExpr::BINOP_XOR;
        case Token::OR:     return ast::BinOpExpr::BINOP_OR;
        case Token::ANDAND: return ast::BinOpExpr::BINOP_ANDAND;
        case Token::XORXOR: return ast::BinOpExpr::BINOP_XORXOR;
        case Token::OROR:   return ast::BinOpExpr::BINOP_OROR;
        default: break;
    }
    return ast::BinOpExpr::BINOP_UNKNOWN;
}

static ast::AssignOpExpr::Type token_to_assignop(Token tok) {
    switch (tok.type()) {
        case Token::ASSIGN:        return ast::AssignOpExpr::ASSIGN_EQUAL;
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
    return ast::AssignOpExpr::ASSIGN_UNKNOWN;
}

static int max_precedence = 13;

int precedence(ast::BinOpExpr::Type type) {
    // Taken from GLSL 4.5 spec
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
        case ast::BinOpExpr::BINOP_XORXOR: return 12;
        case ast::BinOpExpr::BINOP_OROR:   return 13;
        default: break;
    }
    assert(0 && "Unknown binary operation");
    return -1;
}

bool left_associative(ast::BinOpExpr::Type type) {
    // Taken from GLSL 4.5 spec
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
        case ast::BinOpExpr::BINOP_XORXOR: return true;
        case ast::BinOpExpr::BINOP_OROR:   return true;
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
    field->set_left(left);
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
    index->set_left(left);
    index->set_index(parse_expr());
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
        call->set_function(callee);
    else
        call->set_constructor(parse_type());

    expect(Token::LPAREN);

    if (lookup_[0].key().isa(Key::KEY_VOID)) {
        // No parameters
        eat(Key::KEY_VOID);
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
    if (pre_type != ast::UnOpExpr::UNOP_UNKNOWN) {
        auto unop = new_node<ast::UnOpExpr>();
        unop->set_type(pre_type);
        next();
        
        unop->set_operand(parse_unary_expr());
        return unop.node();
    }

    ast::Expr* expr = parse_primary_expr();

    // Postfix expression
    ast::UnOpExpr::Type post_type = token_to_post_unop(lookup_[0]);
    while (post_type != ast::UnOpExpr::UNOP_UNKNOWN ||
           lookup_[0].isa(Token::LBRACKET) ||
           lookup_[0].isa(Token::DOT) ||
           lookup_[0].isa(Token::LPAREN)) {
        if (post_type != ast::UnOpExpr::UNOP_UNKNOWN) {
            auto unop = new_node<ast::UnOpExpr>();
            unop->set_type(post_type);
            next();
            
            unop->set_operand(expr);

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
        if (type == ast::BinOpExpr::BINOP_UNKNOWN)
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

    if (lookup_[0].isa(Token::QMARK)) {
        auto cond = new_node<ast::CondExpr>();
        eat(Token::QMARK);

        cond->set_cond(binop);
        cond->set_if_true(parse_expr());
        expect(Token::COLON);
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
        next();
        assign->set_type(type);
        assign->set_left(left);
        assign->set_right(parse_assign_expr());
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
#define SLANG_KEY_DATA(key, str, type, rows, cols) case Key::KEY_##key:
#define SLANG_KEY_QUAL(key, str) case Key::KEY_##key:
#include "slang/keywordlist.h"
            case Key::KEY_STRUCT:
                cond->set_var_type(parse_type());
                cond->set_var(parse_variable());
                break;
            default: break;
        }
    } else if (lookup_[0].is_ident() && lookup_[1].is_ident()) {
        cond->set_var_type(parse_type());
        cond->set_var(parse_variable());
    } else {
        cond->set_expr(parse_expr());
    }

    return cond.node();
}

ast::Stmt* Parser::parse_stmt() {
    // Stmt ::= IfStmt | SwitchStmt | WhileLoopStmt | ForLoopStmt | DoWhileLoopStmt
    //        | CaseLabelStmt | DeclStmt | ExprStmt
    if (lookup_[0].is_keyword()) {
        switch (lookup_[0].key().type()) {
            case Key::KEY_IF:      return parse_if_stmt();
            case Key::KEY_SWITCH:  return parse_switch_stmt();

            case Key::KEY_WHILE:   return parse_while_stmt();
            case Key::KEY_FOR:     return parse_for_stmt();
            case Key::KEY_DO:      return parse_do_while_stmt();

            case Key::KEY_DEFAULT: return parse_case_stmt(true);
            case Key::KEY_CASE:    return parse_case_stmt(false);

            case Key::KEY_RETURN:  return parse_return_stmt();

#define PARSE_JUMP_STMT(Key, Stmt) \
    case Key: \
        { \
            auto stmt = new_node<Stmt>(); \
            eat(Key); \
            expect(Token::SEMICOLON); \
            return stmt.node(); \
        }

            PARSE_JUMP_STMT(Key::KEY_BREAK,    ast::BreakStmt)
            PARSE_JUMP_STMT(Key::KEY_CONTINUE, ast::ContinueStmt)
            PARSE_JUMP_STMT(Key::KEY_DISCARD,  ast::DiscardStmt)

#undef PARSE_JUMP_STMT

#define SLANG_KEY_DATA(key, str, type, rows, cols) case Key::KEY_##key:
#define SLANG_KEY_QUAL(key, str) case Key::KEY_##key:
#include "slang/keywordlist.h"
            case Key::KEY_STRUCT:
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
           token_to_pre_unop(lookup_[0]) != ast::UnOpExpr::UNOP_UNKNOWN) {
        list->push_stmt(parse_stmt());
    }
    expect(Token::RBRACE);
    return list.node();
}

ast::IfStmt* Parser::parse_if_stmt() {
    // IfStmt ::= if ( Expr ) (Stmt | Stmt else Stmt)
    auto stmt = new_node<ast::IfStmt>();

    eat(Key::KEY_IF);
    expect(Token::LPAREN);
    stmt->set_cond(parse_expr());
    expect(Token::RPAREN);

    stmt->set_if_true(parse_stmt());

    if (lookup_[0].key().type() == Key::KEY_ELSE) {
        eat(Key::KEY_ELSE);
        stmt->set_if_false(parse_stmt());
    }

    return stmt.node();
}

ast::SwitchStmt* Parser::parse_switch_stmt() {
    // SwitchStmt ::= switch ( Expr ) { (Stmt)* }
    auto stmt = new_node<ast::SwitchStmt>();
    
    eat(Key::KEY_SWITCH);
    expect(Token::LPAREN);
    stmt->set_expr(parse_expr());
    expect(Token::RPAREN);

    stmt->set_list(parse_compound_stmt());

    return stmt.node();
}

ast::WhileLoopStmt* Parser::parse_while_stmt() {
    // WhileLoopStmt ::= while ( LoopCond ) Stmt
    auto stmt = new_node<ast::WhileLoopStmt>();
    eat(Key::KEY_WHILE);
    expect(Token::LPAREN);
    stmt->set_cond(parse_loop_cond());
    expect(Token::RPAREN);
    stmt->set_body(parse_stmt());
    return stmt.node();
}

ast::ForLoopStmt* Parser::parse_for_stmt() {
    // ForLoopStmt ::= for ( (Stmt)? ; (LoopCond)? ; (Expr)? )
    auto stmt = new_node<ast::ForLoopStmt>();
    eat(Key::KEY_FOR);
    expect(Token::LPAREN);

    if (lookup_[0].isa(Token::SEMICOLON)) {
        eat(Token::SEMICOLON);
    } else { 
        stmt->set_init(parse_stmt());
    }

    if (lookup_[0].isa(Token::SEMICOLON)) {
        eat(Token::SEMICOLON);
    } else {
        stmt->set_cond(parse_loop_cond());
        expect(Token::SEMICOLON);
    }

    if (lookup_[0].isa(Token::RPAREN)) {
        eat(Token::RPAREN);
    } else { 
        stmt->set_iter(parse_expr());
        expect(Token::RPAREN);
    }

    stmt->set_body(parse_stmt());
    return stmt.node();
}

ast::DoWhileLoopStmt* Parser::parse_do_while_stmt() {
    // DoWhileLoopStmt ::= do Stmt while ( LoopCond ) ;
    auto stmt = new_node<ast::DoWhileLoopStmt>();
    eat(Key::KEY_DO);
    stmt->set_body(parse_stmt());

    expect(Key::KEY_WHILE);
    expect(Token::LPAREN);

    stmt->set_cond(parse_loop_cond());
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
        eat(Key::KEY_CASE);
        stmt->set_expr(parse_expr());
    } else {
        eat(Key::KEY_DEFAULT);
    }
    expect(Token::COLON);
    return stmt.node();
}

ast::DeclStmt* Parser::parse_decl_stmt() {
    // DeclStmt ::= Decl
    auto stmt = new_node<ast::DeclStmt>();
    stmt->set_decl(parse_decl());
    return stmt.node();
}

ast::ExprStmt* Parser::parse_expr_stmt() {
    // ExprStmt ::= Expr ;
    auto stmt = new_node<ast::ExprStmt>();
    if (lookup_[0].isa(Token::SEMICOLON)) {
        eat(Token::SEMICOLON);
    } else {
        stmt->set_expr(parse_expr());
        expect(Token::SEMICOLON);
    }
    return stmt.node();
}

ast::ReturnStmt* Parser::parse_return_stmt() {
    // ReturnStmt ::= return (Expr)? ;
    auto stmt = new_node<ast::ReturnStmt>();
    eat(Key::KEY_RETURN);
    if (lookup_[0].isa(Token::SEMICOLON)) {
        eat(Token::SEMICOLON);
    } else {
        stmt->set_value(parse_expr());
        expect(Token::SEMICOLON);
    }
    return stmt.node();
}

} // namespace slang
