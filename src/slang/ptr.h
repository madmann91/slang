#ifndef SLANG_PTR_H
#define SLANG_PTR_H

#include <unordered_map>
#include <vector>
#include <memory>

namespace slang {

/// A unique pointer.
template <typename T>
using Ptr = std::unique_ptr<T>;

/// An owning pointer vector.
template <typename T>
using PtrVector = std::vector<Ptr<T> >;

/// An owning pointer map.
template <typename U, typename V>
using PtrMap = std::unordered_map<U, Ptr<V> >;

template <typename It, typename T>
struct PtrIterator {
    typedef const T*    value_type;
    typedef size_t      difference_type;
    typedef const T*&   reference;

    It it;

    PtrIterator(const It& it) : it(it) {}

    PtrIterator& operator ++ (int) { return PtrIterator(it++); }
    PtrIterator& operator ++ ()    { ++it; return *this; }

    bool operator == (const PtrIterator& other) const { return other.it == it; }
    bool operator != (const PtrIterator& other) const { return other.it != it; }

    const T* operator * () const { return it->get(); }
};

template <typename Container, typename T>
struct PtrView {
    typedef typename Container::const_iterator It;

    const Container& c;

    PtrIterator<It, T> begin() const { return c.begin(); }
    PtrIterator<It, T> end()   const { return c.end();   }

    size_t size() const { return c.size();  }
    bool empty()  const { return c.empty(); }
    const T* front() const { return c.front().get(); }
    const T* back()  const { return c.back().get();  }

    PtrView(const Container& c) : c(c) {}
};

template <typename T>
using PtrVectorView = PtrView<PtrVector<T>, T>;

template <typename U, typename V>
using PtrMapView = PtrView<PtrMap<U, V>, V>;

template <typename T>
PtrVectorView<T> make_view(const PtrVector<T>& v) {
    return PtrVectorView<T>(v);
}

template <typename U, typename V>
PtrMapView<U, V> make_view(const PtrMap<U, V>& m) {
    return PtrMapView<U, V>(m);
}

} // namespace slang

#endif // SLANG_PTR_H
