#ifndef SLANG_PTR_H
#define SLANG_PTR_H

#include <unordered_map>
#include <vector>
#include <memory>

namespace slang {

template <typename T>
using Ptr = std::unique_ptr<T>;

template <typename T>
class PtrVector : private std::vector<T*> {
public:
    typedef std::vector<T*> Vector;

    ~PtrVector() {
        for (auto p : *this) delete p;
    }

    using Vector::empty;
    using Vector::begin;
    using Vector::end;
    using Vector::front;
    using Vector::back;
    using Vector::push_back;
    using Vector::size;
    using Vector::clear;
    using Vector::operator [];
};

template <typename U, typename V>
class PtrMap : private std::unordered_map<U, V*> {
public:
    typedef std::unordered_map<U, V*> Map;

    ~PtrMap() {
        for (auto p : *this) delete p.second;
    }

    using Map::empty;
    using Map::begin;
    using Map::end;
    using Map::emplace;
    using Map::size;
    using Map::clear;
    using Map::operator [];
    using Map::find;
};

} // namespace slang

#endif // SLANG_PTR_H
