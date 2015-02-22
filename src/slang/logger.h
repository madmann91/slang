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
           std::ostream& clog = std::clog,
           std::ostream& cerr = std::cerr)
        : filename_(filename), clog_(clog), cerr_(cerr)
    {}

    std::ostream& error(const Location& loc) {
        cerr_ << "Error in " << filename_ << " " << loc << " : ";
        return cerr_;
    }

    std::ostream& error(const Position& pos) {
        cerr_ << "Error in " << filename_ << " " << pos << " : ";
        return cerr_;
    }

    std::ostream& warn(const Location& loc) {
        clog_ << "Warning in " << filename_ << " " << loc << " : ";
        return clog_;
    }

    std::ostream& warn(const Position& pos) {
        clog_ << "Warning in " << filename_ << " " << pos << " : ";
        return clog_;
    }

private:
    std::string filename_;
    std::ostream& clog_;
    std::ostream& cerr_;
};

} // namespace slang

#endif // SLANG_LOGGER_H
