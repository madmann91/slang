#include "slang/context.h"

namespace slang {

void Context::register_function(const std::string& name, Symbol::DefMap&& defs, OverloadedFunctionType::SignatureList& signs) {
    assert(defs.size() == signs.size() && defs.size() > 0);

    if (auto symbol = env_.find_symbol(name)) {
        for (auto def : defs)
            symbol->push_def(def.first, def.second);

        if (auto fn_type = symbol->type()->isa<FunctionType>())
            signs.push_back(fn_type);
        else if (auto overload = symbol->type()->isa<OverloadedFunctionType>())
            signs.insert(signs.end(), overload->signatures().begin(), overload->signatures().end());
        else
            assert(0 && "Invalid symbol type");    
    } else {
        env_.push_symbol(name, Symbol(std::forward<Symbol::DefMap>(defs)));
    }

    Symbol* symbol = env_.find_symbol(name);
    assert(symbol);
    if (signs.size() > 1)
        symbol->set_type(sema_.overloaded_function_type(signs));
    else
        symbol->set_type(signs.front());
}

void Context::register_unary(const std::string& name, GenTypeFn f) {
    Symbol::DefMap defs;
    OverloadedFunctionType::SignatureList signs;

    f(sema_, [&] (const Type* t, ast::PrimType::Prim prim) {
        const FunctionType* fn_type = sema_.function_type(t, {t});
        if (defs.find(fn_type) == defs.end()) {
            ast::FunctionDecl* decl = new ast::FunctionDecl();
            decl->push_arg(new_argument(t, prim));
            defs.emplace(fn_type, decl);
            signs.push_back(fn_type);
        }
    });

    register_function(name, std::move(defs), signs);
}


void Context::register_binary(const std::string& name, GenTypeFn f) {
    Symbol::DefMap defs;
    OverloadedFunctionType::SignatureList signs;

    f(sema_, [&] (const Type* t, ast::PrimType::Prim prim) {
        const FunctionType* fn_type = sema_.function_type(t, {t, t});
        if (defs.find(fn_type) == defs.end()) {
            ast::FunctionDecl* decl = new ast::FunctionDecl();
            decl->push_arg(new_argument(t, prim));
            decl->push_arg(new_argument(t, prim));
            defs.emplace(fn_type, decl);
            signs.push_back(fn_type);
        }
    });

    register_function(name, std::move(defs), signs);
}

void Context::register_binary(const std::string& name, GenTypeFn f, GenTypeFn g) {
    Symbol::DefMap defs;
    OverloadedFunctionType::SignatureList signs;

    f(sema_, [&] (const Type* t0, ast::PrimType::Prim prim0) {
        g(sema_, [&] (const Type* t1, ast::PrimType::Prim prim1) {
            const FunctionType* fn_type = sema_.function_type(t0, {t0, t1});
            if (defs.find(fn_type) == defs.end()) {
                ast::FunctionDecl* decl = new ast::FunctionDecl();
                decl->push_arg(new_argument(t0, prim0));
                decl->push_arg(new_argument(t1, prim1));
                defs.emplace(fn_type, decl);
                signs.push_back(fn_type);
            }
        });
    });

    register_function(name, std::move(defs), signs);
}

void Context::gentype(Sema& sema, PrimTypeFn f) {
    f(sema.prim_type(PrimType::PRIM_FLOAT),    ast::PrimType::PRIM_FLOAT);
    f(sema.prim_type(PrimType::PRIM_FLOAT, 2), ast::PrimType::PRIM_VEC2);
    f(sema.prim_type(PrimType::PRIM_FLOAT, 3), ast::PrimType::PRIM_VEC3);
    f(sema.prim_type(PrimType::PRIM_FLOAT, 4), ast::PrimType::PRIM_VEC4);
}

void Context::genitype(Sema& sema, PrimTypeFn f) {
    f(sema.prim_type(PrimType::PRIM_INT),    ast::PrimType::PRIM_INT);
    f(sema.prim_type(PrimType::PRIM_INT, 2), ast::PrimType::PRIM_IVEC2);
    f(sema.prim_type(PrimType::PRIM_INT, 3), ast::PrimType::PRIM_IVEC3);
    f(sema.prim_type(PrimType::PRIM_INT, 4), ast::PrimType::PRIM_IVEC4);
}

void Context::genutype(Sema& sema, PrimTypeFn f) {
    f(sema.prim_type(PrimType::PRIM_UINT),    ast::PrimType::PRIM_UINT);
    f(sema.prim_type(PrimType::PRIM_UINT, 2), ast::PrimType::PRIM_UVEC2);
    f(sema.prim_type(PrimType::PRIM_UINT, 3), ast::PrimType::PRIM_UVEC3);
    f(sema.prim_type(PrimType::PRIM_UINT, 4), ast::PrimType::PRIM_UVEC4);
}

void Context::genbtype(Sema& sema, PrimTypeFn f) {
    f(sema.prim_type(PrimType::PRIM_BOOL),    ast::PrimType::PRIM_BOOL);
    f(sema.prim_type(PrimType::PRIM_BOOL, 2), ast::PrimType::PRIM_BVEC2);
    f(sema.prim_type(PrimType::PRIM_BOOL, 3), ast::PrimType::PRIM_BVEC3);
    f(sema.prim_type(PrimType::PRIM_BOOL, 4), ast::PrimType::PRIM_BVEC4);
}

void Context::gendtype(Sema& sema, PrimTypeFn f) {
    f(sema.prim_type(PrimType::PRIM_DOUBLE),    ast::PrimType::PRIM_DOUBLE);
    f(sema.prim_type(PrimType::PRIM_DOUBLE, 2), ast::PrimType::PRIM_DVEC2);
    f(sema.prim_type(PrimType::PRIM_DOUBLE, 3), ast::PrimType::PRIM_DVEC3);
    f(sema.prim_type(PrimType::PRIM_DOUBLE, 4), ast::PrimType::PRIM_DVEC4);
}

Context::GenTypeFn Context::prim_type(ast::PrimType::Prim prim) {
    return [=] (Sema& sema, PrimTypeFn f) {
        ast::PrimType ast_type;
        ast_type.set_prim(prim);
        f(sema.check(&ast_type)->as<PrimType>(), prim);
    };
}

ast::Arg* Context::new_argument(const Type* type, ast::PrimType::Prim prim) {
    ast::Arg* arg = new ast::Arg();
    ast::PrimType* prim_type = new ast::PrimType();
    prim_type->set_prim(prim);
    arg->set_type(prim_type);
    arg->assign_type(type);
    return arg;
}

} // namespace slang
