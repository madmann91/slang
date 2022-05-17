# Slang ![build-and-test](https://github.com/madmann91/slang/actions/workflows/build_and_test.yml/badge.svg)
A small, flexible and extensible front-end for GLSL.

## Project goal
This project aims to provide a simple GLSL (OpenGL shading language) front-end, that is: a lexer, a preprocessor, a parser, and a semantic analyser.
The system is designed to be simple and lightweight, and well documented. A thorough test suite is included.

## Example

Here is a simple example describing how to parse a GLSL file :

```cpp
using namespace slang;

bool parse_glsl(const std::string& filename) {
    // Open a stream for reading in text mode
    std::ifstream is(filename);
    if (!is) return false;

    // Generate list of available keywords (you can choose to exclude some of them at runtime)
    Keywords keys;
    keys.add_all_keywords();

    // Create a logger object, error messages can be redirected
    Logger logger(filename);
    // Create a lexer object, using the previously created set of keywords
    Lexer lexer(is, keys, logger);
    // Create a preprocessor that reads tokens from the lexer
    Preprocessor pp(lexer, logger);
    // Register __FILE__, __LINE__, and __VERSION__ builtin macros
    pp.register_builtin_macros();
    // Create a semantic object, that will be used during type checking
    Sema sema(logger);
    // Create a parser object that reads tokens from the preprocessor (can read directly from the lexer)
    Parser parser([&pp]() { return pp.preprocess(); }, sema, logger);

    // Parse the stream (errors will be reported in the logger)
    Ptr<ast::Module> module = parser.parse();

    // AST can be pretty-printed to an output stream
    Printer printer(std::cout);
    module->print(printer);
    
    return lexer.error_count()  == 0 &&
           pp.error_count()     == 0 &&
           parser.error_count() == 0 &&
           sema.error_count()   == 0;
}
```

## Features

* Configurable, standard-compliant lexer, preprocessor, parser and type checker
* Provided as stand-alone executable and core library
* Can parse different versions of GLSL
* Only depends on the standard library
* Builds on Windows, Mac and Linux with CMake
* Documentation through doxygen
* Precise error messages
* Colored output and syntax highlighting
* Test suite to check correctness

## Examples

### User defined macros

This [example](examples/user_macros.cpp) describes how to register custom macros into the preprocessor.
Two new macros are introduced: One with a traditional production rule (i.e. identical to a macro
declared with `#define MACRO(x) ...`), and a second macro which executes custom code.

### Pattern matching on the AST

This [example](examples/pattern_matching.cpp) describes how to perform pattern matching on the AST.
The program looks for expressions of the form `A * B + C` and replaces them by calls to `fma()`.

### Code obfuscation by renaming

This [example](examples/obfuscate.cpp) describes how to perform a simple renaming pass on the AST.
The code is obfuscated by finding new variable names on the fly. This can be used as a starting point
for a complete obfuscation tool.

## License
The code is distributed under the LGPL license.
