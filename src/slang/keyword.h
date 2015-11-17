#ifndef SLANG_KEYWORD_H
#define SLANG_KEYWORD_H

#include <unordered_map>
#include <ostream>

namespace slang {

/// Class that represents keywords.
class Key {
public:
    enum Type {
#define SLANG_KEY(key, str) key,
#include "slang/keywordlist.h"
    };

    Key() : type_(UNKNOWN) {}
    Key(Type type) : type_(type) {}

    bool isa(Type type) const{ return type_ == type; }
    bool is_unknown() const { return type_ == UNKNOWN; }
    Type type() const { return type_; }

    bool is_qualifier() const {
        switch (type_) {
#define SLANG_KEY_QUAL(key, str) case key:
#include "slang/keywordlist.h"
                return true;
            default:
                break;
        }
        return false;
    }

    bool is_data() const {
        switch (type_) {
#define SLANG_KEY_DATA(key, str, type, rows, cols) case key:
#include "slang/keywordlist.h"
                return true;
            default:
                break;
        }
        return false;
    }

private:
    Type type_;
};

/// Map between strings and associated keywords.
class Keywords {
public:
    Keywords() {}
    Keywords(const std::unordered_map<std::string, Key> map)
        : map_(map)
    {}

    /// Add a keyword to the dictionary
    void add_keyword(const std::string& name, Key::Type type) {
        map_[name] = Key(type);
    }

    /// Adds all keywords to the dictionary
    void add_all_keywords() {
#define SLANG_KEY_UNKNOWN(key, str)
#define SLANG_KEY(key, str) add_keyword(str, Key::key);
#include "slang/keywordlist.h"
    }

    /// Adds all data type keywords to the dictionary
    void add_data_keywords() {
#define SLANG_KEY_DATA(key, str, type, rows, cols) add_keyword(str, Key::key);
#include "slang/keywordlist.h"
    }

    /// Adds all control flow keywords to the dictionary
    void add_flow_keywords() {
#define SLANG_KEY_FLOW(key, str) add_keyword(str, Key::key);
#include "slang/keywordlist.h"
    }

    /// Adds all qualifier keywords to the dictionary
    void add_qualifier_keywords() {
#define SLANG_KEY_QUAL(key, str) add_keyword(str, Key::key);
#include "slang/keywordlist.h"
    }

    bool is_keyword(const std::string& key) const {
        return map_.find(key) != map_.end();
    }

    /// Returns the keyword associated with the string (if there is no such keyword, returns UNKNOWN).
    Key keyword(const std::string& key) const {
        auto it = map_.find(key);
        if (it != map_.end())
            return it->second;
        return Key::UNKNOWN;
    }

private:
    std::unordered_map<std::string, Key> map_;
};

inline std::ostream& operator << (std::ostream& out, const Key& key) {
    switch (key.type()) {
#define SLANG_KEY(key, str) case Key::key: out << str; break;
#include "slang/keywordlist.h"
    }

    return out;
}

} // namespace slang

#endif // SLANG_KEYWORD_H
