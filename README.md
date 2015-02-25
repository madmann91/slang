# Slang
A small and flexible front-end for GLSL-like languages.

## Project goal
Provide a simple GLSL-like shading language front-end, that is : a lexer, a parser, and a semantic analyser.
The system should be simple and lightweight, and well documented. A thorough test suite should also be included.

## Example

Here is a simple example describing how to parse a GLSL file :

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.cpp}
using namespace slang;

void parse_glsl(const std::string& filename) {
    // Open a stream for reading in text mode
    std::ifstream is(filename);
    if (!is) return false;

    // Generate list of available keywords (you can choose to exclude some of them at runtime)
    Keywords keys;
#define SLANG_KEY_UNKNOWN(key, str)
#define SLANG_KEY(key, str) keys.add_keyword(str, Key::KEY_##key);
#include "slang/keywordlist.h"

    // Create a logger object, error messages can be redirected
    Logger logger(filename);
    // Create a lexer object, using the previously created set of keywords
    Lexer lexer(is, keys, logger);
    // Create a parser object using this lexer
    Parser parser(lexer, logger);
    // Parse the stream (errors will be reported in the logger)
    std::unique_ptr<ast::DeclList> root(parser.parse());

    // AST can be pretty-printed to an output stream
    Printer printer(std::cout);
    root->print(printer);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Status
Currently only the lexer, parser and a basic test suite are implemented. Most of the GLSL 4.0 specification is supported.

## License
The code is distributed under the LGPL license.
