#ifndef SLANG_SEMA_H
#define SLANG_SEMA_H

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
};

/// Function type (equality based on return type and arguments).
class FunctionType : public Type {
public:
    bool equals(const Type* other) const {
        if (auto fn = other->isa<FunctionType>()) {
            if (fn->ret()->equals(ret()) &&
                fn->args().size() == args().size()) {
                for (size_t i = 0; i < fn->args().size(); i++) {
                    if (!args()[i]->equals(fn->args()[i]))
                        return false;
                }
                return true;
            }
        }
        return false;
    }

    void set_ret(Type* ret) { ret_.reset(ret); }
    Type* ret() { return ret_.get(); }
    const Type* ret() const { return ret_.get(); }

    const PtrVector<Type>& args() const { return args_; }
    void push_arg(Type* arg) { args_.push_back(arg); }
    int num_args() const { return args_.size(); }

private:
    PtrVector<Type> args_;
    Ptr<Type> ret_;
};

/// Compound type (interface or structure).
class CompoundType : public Type {
public:
    const PtrMap<std::string, Type>& args() const { return args_; }
    void push_arg(const std::string& name, Type* arg) { args_.emplace(name, arg); }
    int num_args() const { return args_.size(); }

private:
    PtrMap<std::string, Type> args_;
};

/// Structure type (equality decided from name only).
class StructType : public CompoundType {
public:
    bool equals(const Type* other) const {
        if (auto st = other->isa<StructType>()) {
            return st->name() == name_;
        }
        return false;
    }

    const std::string& name() const { return name_; }
    void set_name(const std::string& name) { name_ = name; }

private:
    std::string name_;
};

/// Interface type (cannot be equal to another type).
class InterfaceType : public CompoundType {
public:
    bool equals(const Type* other) const {
        return false;
    }
};

/// Primitive type (int, float, double, ...).
class PrimType : public Type {
public:
    enum Prim {
        PRIM_BVEC4,
        PRIM_BVEC3,
        PRIM_BVEC2,
        PRIM_BOOL,

        PRIM_IVEC4,
        PRIM_IVEC3,
        PRIM_IVEC2,
        PRIM_INT,

        PRIM_VEC4,
        PRIM_VEC3,
        PRIM_VEC2,
        PRIM_FLOAT,

        PRIM_DVEC4,
        PRIM_DVEC3,
        PRIM_DVEC2,
        PRIM_DOUBLE,

        PRIM_MAT4,
        PRIM_MAT3,
        PRIM_MAT2,
        PRIM_MAT4X4,
        PRIM_MAT4X3,
        PRIM_MAT4X2,
        PRIM_MAT3X4,
        PRIM_MAT3X3,
        PRIM_MAT3X2,
        PRIM_MAT2X4,
        PRIM_MAT2X3,
        PRIM_MAT2X2,

        PRIM_DMAT4,
        PRIM_DMAT3,
        PRIM_DMAT2,
        PRIM_DMAT4X4,
        PRIM_DMAT4X3,
        PRIM_DMAT4X2,
        PRIM_DMAT3X4,
        PRIM_DMAT3X3,
        PRIM_DMAT3X2,
        PRIM_DMAT2X4,
        PRIM_DMAT2X3,
        PRIM_DMAT2X2,

        PRIM_VOID
    };

    bool equals(const Type* other) const {
        if (auto prim = other->isa<PrimType>()) {
            return prim->prim() == prim_;
        }
        return false;
    }

    Prim prim() const { return prim_; }
    void set_prim(Prim prim) { prim_ = prim; }

private:
    Prim prim_;
};

class ArrayType : public Type {
public:
    const Type* elem() const { return elem_.get(); }
    void set_elem(Type* elem) { elem_.reset(elem); }

    virtual bool equals(const Type* other) const {
        if (auto array = other->isa<ArrayType>()) {
            return array->elem()->equals(elem_.get());
        }
        return false;
    }

private:
    Ptr<Type> elem_;
};

class DefiniteArrayType : public ArrayType {
public:
    int size() const { return size_; }
    void set_size(int size) { size_ = size; }

    bool equals(const Type* other) const {
        if (auto def = other->isa<DefiniteArrayType>()) {
            return def->size() == size_ && def->elem()->equals(elem());
        }
        return false;
    }

private:
    int size_;
};

} // namespace slang

#endif // SLANG_SEMA_H
