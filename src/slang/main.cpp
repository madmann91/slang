#include <iostream>
#include <fstream>
#include <chrono>
#include <unordered_map>

#include "slang/lexer.h"
#include "slang/parser.h"
#include "slang/ast.h"

using namespace slang;

bool lexical_analysis(const std::string& filename) {
    std::ifstream is(filename);
    if (!is) return false;

    Keywords keys;
#define SLANG_KEY_UNKNOWN(key, str)
#define SLANG_KEY(key, str) keys.add_keyword(str, Key::KEY_##key);
#include "slang/keywordlist.h"

    Lexer lexer(is, keys, Logger(filename));

    Token tok;
    do {
        tok = lexer.lex();
        std::cout << tok << tok.loc() << " ";
    } while (tok.type() != Token::TOK_EOF);

    std::cout << std::endl;
    return true;
}

bool syntax_analysis(const std::string& filename) {
    auto t0 = std::chrono::high_resolution_clock::now();
    std::ifstream is(filename);
    if (!is) return false;

    Keywords keys;
#define SLANG_KEY_UNKNOWN(key, str)
#define SLANG_KEY(key, str) keys.add_keyword(str, Key::KEY_##key);
#include "slang/keywordlist.h"

    Parser parser(Lexer(is, keys, Logger(filename)));
    ast::List* root = parser.parse();
    auto t1 = std::chrono::high_resolution_clock::now();

    std::cout << "Parsing and lexing took "
              << std::chrono::duration_cast<std::chrono::milliseconds>(t1 - t0).count()
              << " ms" << std::endl;

    root->print(std::cout);
    delete root;

    return true;
}

int main(int argc, char** argv) {
    if (argc < 2) return 0;

    bool do_lex = true;
    bool do_syntax = true;
    std::string filename = argv[1];

    if (do_lex) lexical_analysis(filename);
    if (do_syntax) syntax_analysis(filename);

    return 0;
}
