#ifndef SLANG_LOGGER_H
#define SLANG_LOGGER_H

#include <iostream>
#include <string>

#include "slang/location.h"

namespace slang {

/// Utility class to report errors. Used by the lexer and the parser.
class Logger {
public:
    Logger(const std::string& filename,
           bool terminal = true,
           std::ostream& clog = std::clog,
           std::ostream& cerr = std::cerr)
        : filename_(filename), clog_(clog), cerr_(cerr), term_(terminal)
    {}

    std::ostream& error(const Location& loc) {
        cerr_ << error_mark() << "Error" << normal_mark() << " in "
              << file_mark() << filename_ << loc << normal_mark() << ": ";
        return cerr_;
    }

    std::ostream& error(const Position& pos) {
        cerr_ << error_mark() << "Error" << normal_mark() << " in "
              << file_mark() << filename_ << pos << normal_mark() << ": ";
        return cerr_;
    }

    std::ostream& warn(const Location& loc) {
        clog_ << warn_mark() << "Warning" << normal_mark() << " in "
              << file_mark() << filename_ << loc << normal_mark() << ": ";
        return clog_;
    }

    std::ostream& warn(const Position& pos) {
        clog_ << warn_mark() << "Warning" << normal_mark() << " in "
              << file_mark() << filename_ << pos << normal_mark() << ": ";
        return clog_;
    }

private:
    const char* error_mark()  const { return term_ ? "\033[1;31m" : ""; }
    const char* warn_mark()   const { return term_ ? "\033[1;33m" : ""; }
    const char* file_mark()   const { return term_ ? "\033[1;37m" : ""; }
    const char* normal_mark() const { return term_ ? "\033[0m"    : ""; }

    std::string filename_;
    std::ostream& clog_;
    std::ostream& cerr_;
    bool term_;
};

} // namespace slang

#endif // SLANG_LOGGER_H
