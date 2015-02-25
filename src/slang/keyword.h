#ifndef SLANG_KEYWORD_H
#define SLANG_KEYWORD_H

#include <unordered_map>

namespace slang {

/// Class that represents keywords.
class Key {
public:
    enum Type {
#define SLANG_KEY(key, str) KEY_##key,
#include "slang/keywordlist.h"
    };

    Key() : type_(KEY_UNKNOWN) {}
    Key(Type type) : type_(type) {}

    bool isa(Type type) const{ return type_ == type; }
    bool is_unknown() const { return type_ == KEY_UNKNOWN; }
    Type type() const { return type_; }

    bool is_qualifier() const {
        switch (type_) {
#define SLANG_KEY_QUAL(key, str) case KEY_##key:
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

    void add_keyword(const std::string& name, Key::Type type) {
        map_[name] = Key(type);
    }

    bool is_keyword(const std::string& key) const {
        return map_.find(key) != map_.end();
    }

    Key keyword(const std::string& key) const {
        auto it = map_.find(key);
        if (it != map_.end())
            return it->second;
        return Key::KEY_UNKNOWN;
    }

private:
    std::unordered_map<std::string, Key> map_;
};

inline std::ostream& operator << (std::ostream& out, const Key& key) {
    switch (key.type()) {
#define SLANG_KEY(key, str) case Key::KEY_##key: out << str; break;
#include "slang/keywordlist.h"
    }

    return out;
}

} // namespace slang

#endif // SLANG_KEYWORD_H
