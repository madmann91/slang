#include <iostream>
#include <fstream>
#include <cstring>

#include "slang/lexer.h"
#include "slang/parser.h"
#include "slang/ast.h"
#include "slang/print.h"

using namespace slang;

void usage() {
    std::cout << "slang [options] file...\n"
              << "Available options :\n"
              << "    -h --help : Displays this message\n"
              << "    -tok --tokenize : Generates a stream of tokens from the input and print it\n"
              << "    -ast --syntax : Generates an AST from the input and print it (default)\n";
}

bool lexical_analysis(const std::string& filename, const Keywords& keys) {
    // Do the lexical analysis and display the stream of tokens with their location
    std::ifstream is(filename);
    if (!is) return false;

    Lexer lexer(is, keys, Logger(filename));

    Token tok;
    do {
        tok = lexer.lex();
        std::cout << tok << tok.loc() << " ";
    } while (tok.type() != Token::TOK_EOF);

    std::cout << std::endl;
    return true;
}

bool syntax_analysis(const std::string& filename, const Keywords& keys) {
    // Do the syntax analysis and display the AST
    std::ifstream is(filename);
    if (!is) return false;

    Parser parser(Lexer(is, keys, Logger(filename)));
    ast::DeclList* root = parser.parse();

    Printer printer(std::cout);
    root->print(printer);
    delete root;

    return true;
}

int main(int argc, char** argv) {
    if (argc < 2) {
        usage();
        return 0;
    }

    enum {
        ACTION_TOKENIZE,
        ACTION_SYNTAX
    } action = ACTION_SYNTAX;

    // Generate list of available keywords
    Keywords keys;
#define SLANG_KEY_UNKNOWN(key, str)
#define SLANG_KEY(key, str) keys.add_keyword(str, Key::KEY_##key);
#include "slang/keywordlist.h"

    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            if (!std::strcmp(argv[i], "--help") || !std::strcmp(argv[i], "-h")) {
                usage();
            } else if (!std::strcmp(argv[i], "--tokenize") || !std::strcmp(argv[i], "-tok")) {
                action = ACTION_TOKENIZE;
            } else if (!std::strcmp(argv[i], "--syntax") || !std::strcmp(argv[i], "-ast")) {
                action = ACTION_SYNTAX;
            } else {
                std::cerr << "Unknown option : " << argv[i];
            }
        } else {
            switch (action) {
                case ACTION_SYNTAX: syntax_analysis(argv[i], keys); break;
                case ACTION_TOKENIZE: lexical_analysis(argv[i], keys); break;
            }
        }
    }

    return 0;
}
