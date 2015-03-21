#include <iostream>
#include <fstream>
#include <cstring>

#include "slang/lexer.h"
#include "slang/parser.h"
#include "slang/preprocess.h"
#include "slang/ast.h"
#include "slang/print.h"

using namespace slang;

void usage() {
    std::cout << "slangc [options] file...\n"
              << "Available options :\n"
              << "    -h   --help : Displays this message\n"
              << "    -tok --tokenize : Generates a stream of tokens from the input and print it\n"
              << "    -pp  --preprocess : Preprocesses the input stream and prints the result\n"
              << "    -ast --syntax : Generates an AST from the input and print it (default)\n";
}

bool lexical_analysis(const std::string& filename, const Keywords& keys) {
    // Do the lexical analysis and display the stream of tokens with their location
    std::ifstream is(filename);
    if (!is) return false;

    Logger logger(filename);
    Lexer lexer(is, keys, logger);

    Token tok;
    do {
        tok = lexer.lex();
        std::cout << tok << tok.loc() << " ";
    } while (tok.type() != Token::TOK_EOF);

    std::cout << std::endl;
    return true;
}

bool preprocess(const std::string& filename, const Keywords& keys) {
    // Preprocesses the file and displays the resulting tokens
    std::ifstream is(filename);
    if (!is) return false;

    Logger logger(filename);
    Lexer lexer(is, keys, logger);
    Preprocessor pp(lexer, logger, [] (int ver, Profile p) {
        std::cout << "Version : " << ver << " " << p << std::endl;
        return true;
    });
    
    Token tok;
    do {
        tok = pp.preprocess();
        std::cout << tok << " ";
    } while (tok.type() != Token::TOK_EOF);

    std::cout << std::endl;
    return true;
}

bool syntax_analysis(const std::string& filename, const Keywords& keys) {
    // Do the syntax analysis and display the AST
    std::ifstream is(filename);
    if (!is) return false;

    Logger logger(filename);
    Lexer lexer(is, keys, logger);
    Preprocessor pp(lexer, logger, [] (int ver, Profile p) {
        std::cout << "#version " << ver << " " << p << std::endl;
        return true;
    });
    Parser parser([&pp]() { return pp.preprocess(); }, logger);
    std::unique_ptr<ast::DeclList> root = parser.parse();

    Printer printer(std::cout);
    root->print(printer);

    return true;
}

int main(int argc, char** argv) {
    if (argc < 2) {
        usage();
        return 0;
    }

    enum {
        ACTION_TOKENIZE,
        ACTION_PREPROCESS,
        ACTION_SYNTAX
    } action = ACTION_SYNTAX;

    // Generate list of available keywords
    Keywords keys;
    keys.add_all_keywords();

    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            if (!std::strcmp(argv[i], "--help") || !std::strcmp(argv[i], "-h")) {
                usage();
            } else if (!std::strcmp(argv[i], "--tokenize") || !std::strcmp(argv[i], "-tok")) {
                action = ACTION_TOKENIZE;
            } else if (!std::strcmp(argv[i], "--preprocess") || !std::strcmp(argv[i], "-pp")) {
                action = ACTION_PREPROCESS;
            } else if (!std::strcmp(argv[i], "--syntax") || !std::strcmp(argv[i], "-ast")) {
                action = ACTION_SYNTAX;
            } else {
                std::cerr << "Unknown option : " << argv[i] << std::endl;
            }
        } else {
            switch (action) {
                case ACTION_SYNTAX: syntax_analysis(argv[i], keys); break;
                case ACTION_PREPROCESS: preprocess(argv[i], keys); break;
                case ACTION_TOKENIZE: lexical_analysis(argv[i], keys); break;
            }
        }
    }

    return 0;
}
