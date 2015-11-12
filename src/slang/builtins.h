#ifndef SLANG_BUILTINS_H
#define SLANG_BUILTINS_H

#include <memory>

namespace slang {

class Sema;

namespace ast {
    class Module;
}

std::unique_ptr<ast::Module> parse_builtins(Sema&);

} // namespace slang

#endif // SLANG_BUILTINS_H
