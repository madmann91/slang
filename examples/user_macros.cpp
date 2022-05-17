/// This is an example of how to use the Preprocessor to create user macros.
///
/// There are two types of user macros:
/// \li Rule-driven macros, which work by substitution (equivalent to a "#define" command)
/// \li Built-in macros, which have a custom rule defined by a C++ function
///
/// This example demonstrates how to make use of both. The code uses the
/// Parser and Lexer class to read the contents of a string.

#include <iostream>
#include <sstream>
#include <string>

#include <slang/preprocessor.h>
#include <slang/lexer.h>
#include <slang/logger.h>

using namespace slang;

int main(int argc, char** argv) {
    std::string glsl = "MY_MACRO(41, 1)\nMY_BUILTIN_MACRO(0.666 - 0.)";
    std::istringstream is(glsl);

    // Create a logger object to report errors
    Logger log("inline_string.glsl");
    // Create a set of keywords
    Keywords keys;
    keys.add_all_keywords();
    // Create a lexer to generate a stream of tokens
    Lexer lex(is, keys, log);
    // Create a preprocessor object
    Preprocessor pp(lex, log);

    // Register a custom rule-driven macro (equivalent to "#define MY_MACRO(i, j) i + j")
    pp.register_macro("MY_MACRO", Macro({{"i", 0}, {"j", 1}},
        std::vector<Token>({
            Token(Location(), "i", keys, false, false),
            Token(Location(), Token::ADD, false, false),
            Token(Location(), "j", keys, false, false),
        })
    ));

    // Register a custom builtin macro that prints its argument on the console and returns "4.2"
    pp.register_macro("MY_BUILTIN_MACRO", Macro({{"i", 0}},
        [] (const std::vector<Macro::Arg>& args) {
            std::cout << "MY_BUILTIN_MACRO called with: ";
            for (auto& arg : args) {
                std::cout << "[";
                for (auto& tok : arg) std::cout << tok << " ";
                std::cout << "\b] ";
            }
            std::cout << "\b" << std::endl;
            auto _4_2 = Token(Location(), "4.2", Literal(4.2f, false), false, false);
            return std::vector<Token>({_4_2});
        }));

    // Preprocess the whole string
    std::ostringstream out;
    Token tok;
    do {
        tok = pp.preprocess();
        out << tok << " ";
    } while (tok.type() != Token::END);

    std::cout << out.str() << std::endl;
    
    return out.str() == "41 + 1 4.2 eof " ? 0 : 1;
}
