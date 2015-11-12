#include <string>
#include <sstream>

#include "slang/builtins.h"
#include "slang/lexer.h"
#include "slang/preprocessor.h"
#include "slang/parser.h"
#include "slang/logger.h"
#include "slang/keyword.h"

namespace slang {

static const std::string builtins =
#include "builtins.glsl"
;

class NullBuffer : public std::streambuf
{
public:
    int overflow(int c) { return c; }
};

std::unique_ptr<ast::Module> parse_builtins(Sema& sema) {
    NullBuffer null_buffer;
    std::ostream null_stream(&null_buffer);
    std::istringstream is(builtins);

    Keywords keys;
    keys.add_all_keywords();

    Logger logger("", null_stream, null_stream);
    Lexer lexer(is, keys, logger);
    Preprocessor pp(lexer, logger);
    Parser parser([&pp]() { return pp.preprocess(); }, sema, logger);
    auto mod = parser.parse();
    assert(lexer.error_count() + pp.error_count() + parser.error_count() + sema.error_count() == 0);
    return mod;
}

} // namespace slang
