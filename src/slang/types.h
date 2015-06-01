#ifndef SLANG_TYPES_H
#define SLANG_TYPES_H

#include <vector>
#include <string>
#include <algorithm>

#include "slang/cast.h"
#include "slang/ptr.h"

namespace slang {

/// Base class for symbol types.
class Type : public Cast<Type> {
public:
    virtual ~Type() {}
    virtual bool equals(const Type* other) const = 0;
    virtual size_t hash() const = 0;
    virtual std::string to_string() const = 0;
};

/// Type expectation. Contains nullptr when no type is expected.
class TypeExpectation {
public:
    TypeExpectation() : type_(nullptr) {}
    TypeExpectation(const Type* type) : type_(type) {}

    const Type* type() const { return type_; }

    template <typename T>
    const Type* isa() const {
        if (type_)
            return type_->isa<T>();
        return nullptr;
    }

    template <typename T>
    const Type* as() const {
        if (type_)
            return type_->as<T>();
        return nullptr;
    }

private:
    const Type* type_;
};

/// Error type. For types that cannot be determined.
class ErrorType : public Type {
public:
    bool equals(const Type* other) const override { return false; }
    size_t hash() const override { return 0; }
    std::string to_string() const override { return "<error>"; }
};

/// Function type (equality based on return type and arguments).
class FunctionType : public Type {
public:
    typedef std::vector<const Type*> ArgList;

    FunctionType(const Type* ret, const ArgList& args)
        : ret_(ret), args_(args)
    {}

    bool equals(const Type* other) const override {
        if (auto fn = other->isa<FunctionType>()) {
            if (fn->ret()->equals(ret()) &&
                fn->args().size() == args().size()) {
                for (size_t i = 0; i < fn->args().size(); i++) {
                    if (args()[i] != fn->args()[i])
                        return false;
                }
                return true;
            }
        }
        return false;
    }

    size_t hash() const override {
        size_t h = 0;
        for (auto arg : args()) {
            h += arg->hash();
        }
        return ret()->hash() ^ h;
    }

    const Type* ret() const { return ret_; }
    const ArgList& args() const { return args_; }
    size_t num_args() const { return args_.size(); }

    std::string to_string() const override {
        std::string str = ret()->to_string() + " (";
        for (size_t i = 0; i < num_args(); i++) {
            str += args()[i]->to_string();
            if (i != num_args() - 1) str += ", ";
        }
        return str + ")";
    }

private:
    const Type* ret_;
    ArgList args_;
};

/// Compound type (interface or structure).
class CompoundType : public Type {
public:
    typedef std::unordered_map<std::string, const Type*> MemberMap;

    CompoundType(const std::string& name, const MemberMap& args)
        : name_(name), args_(args)
    {}
    virtual ~CompoundType() {}

    size_t hash() const override {
        std::hash<std::string> hash_string;
        size_t h = hash_string(name());
        for (auto arg : args()) {
            h += hash_string(arg.first) ^ arg.second->hash();
        }
        return h;
    }

    const MemberMap& args() const { return args_; }
    const std::string& name() const { return name_; }
    std::string to_string() const override { return name_; }

protected:
    std::string name_;
    MemberMap args_;
};

/// Structure type (equality decided from name only).
class StructType : public CompoundType {
public:
    StructType(const std::string& name, const MemberMap& args)
        : CompoundType(name, args)
    {}

    bool equals(const Type* other) const override {
        if (auto st = other->isa<StructType>()) {
            return st->name() == name_;
        }
        return false;
    }
};

/// Interface type (cannot be equal to another type).
class InterfaceType : public CompoundType {
public:
    InterfaceType(const std::string& name, const MemberMap& args)
        : CompoundType(name, args)
    {}

    bool equals(const Type* other) const override {
        return false;
    }
};

/// Primitive type (int, float, double, ...).
class PrimType : public Type {
public:
    enum Prim {
#define SLANG_KEY_DATA(key, str) PRIM_##key,
#include "slang/keywordlist.h"
    };

    PrimType(Prim prim)
        : prim_(prim)
    {}

    bool equals(const Type* other) const override {
        if (auto prim = other->isa<PrimType>()) {
            return prim->prim() == prim_;
        }
        return false;
    }

    size_t hash() const override {
        return (size_t)prim() + 1;
    }

    Prim prim() const { return prim_; }

    std::string to_string() const override {
        switch (prim()) {
#define SLANG_KEY_DATA(key, str) case PRIM_##key: return str;
#include "slang/keywordlist.h"
            default: assert(0 && "Unknown type");
        }
        return std::string();
    }

private:
    Prim prim_;
};

/// Base class for array types.
class ArrayType : public Type {
public:
    ArrayType(const Type* elem)
        : elem_(elem)
    {}
    virtual ~ArrayType() {}

    const Type* elem() const { return elem_; }

protected:
    const Type* elem_;
};

/// Array with unknown size.
class IndefiniteArrayType : public ArrayType {
public:
    IndefiniteArrayType(const Type* elem)
        : ArrayType(elem)
    {}

    bool equals(const Type* other) const override {
        if (auto def = other->isa<IndefiniteArrayType>()) {
            return def->elem() == elem();
        }
        return false;
    }

    size_t hash() const override {
        return elem()->hash();
    }

    std::string to_string() const override {
        return elem()->to_string() + "[]";
    }
};

/// Array with known size.
class DefiniteArrayType : public ArrayType {
public:
    DefiniteArrayType(const Type* elem, int size)
        : ArrayType(elem), size_(size)
    {}

    int size() const { return size_; }

    bool equals(const Type* other) const override {
        if (auto def = other->isa<DefiniteArrayType>()) {
            return def->size() == size_ && def->elem()== elem();
        }
        return false;
    }

    size_t hash() const override {
        return elem()->hash() ^ size();
    }

    std::string to_string() const override {
        return elem()->to_string() + "[" + std::to_string(size()) + "]";
    }

private:
    int size_;
};

} // namespace slang

#endif // SLANG_TYPES_H
