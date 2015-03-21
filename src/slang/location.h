#ifndef SLANG_LOCATION_H
#define SLANG_LOCATION_H

#include <ostream>

namespace slang {

/// A position (line, col) in a file.
class Position {
public:
    Position() {}
    Position(int line, int col)
        : line_(line), col_(col)
    {}

    void set_line(int line) { line_ = line; }
    void set_col(int col) { col_ = col; }

    int line() const { return line_; }
    int col() const { return col_; }

    void inc_line() { line_++; }
    void inc_col() { col_++; }
    void reset_col() { col_ = 0; }

private:
    int line_, col_;
};

/// A location in a file defined by its starting and ending position.
class Location {
public:
    Location() {}
    Location(const Position& start, const Position& end)
        : start_(start), end_(end)
    {}

    const Position& start() const { return start_; }
    const Position& end() const { return end_; }

private:
    Position start_, end_;
};

inline std::ostream& operator << (std::ostream& out, const Position& pos) {
    out << "(" << pos.line() << ", " << pos.col() << ")";
    return out;
}

inline std::ostream& operator << (std::ostream& out, const Location& loc) {
    out << "(" << loc.start().line() << ", " << loc.start().col() << " - "
               << loc.end().line()   << ", " << loc.end().col()   << ")";
    return out;
}

} // namespace slang

#endif // SLANG_LOCATION_H
