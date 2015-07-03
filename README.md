# Slang
A small, flexible and extensible front-end for GLSL.

## Project goal
This project aims to provide a simple GLSL (OpenGL shading language) front-end, that is : a lexer, a parser, and a semantic analyser.
The system is designed to be simple and lightweight, and well documented. A thorough test suite is included.

## Example

Here is a simple example describing how to parse a GLSL file :

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.cpp}
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
    // Create a context and register all the builtin functions into the environment
    Context context(sema);
    context.register_all();
    // Create a parser object that reads tokens from the preprocessor (you can choose to read directly from the lexer)
    Parser parser([&pp]() { return pp.preprocess(); }, sema, logger);

    // Parse the stream (errors will be reported in the logger)
    std::unique_ptr<ast::Module> module = parser.parse();

    // AST can be pretty-printed to an output stream
    Printer printer(std::cout);
    module->print(printer);
    
    return lexer.error_count() == 0 &&
           pp.error_count() == 0 &&
           parser.error_count() == 0 &&
           sema.error_count() == 0;
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Status
Currently only the lexer, preprocessor, parser and the test suite are implemented. The whole GLSL 4.0 specification
is supported. Some extensions are also implemented, to allow parsing of older GLSL versions (attribute or varying
qualifiers, for instance). The documentation can be generated using doxygen.

## License
The code is distributed under the LGPL license.
