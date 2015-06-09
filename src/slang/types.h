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
    virtual bool equals(const Type*) const { return false; }
    virtual bool subtype(const Type* type) const { return this == type; }
    virtual size_t hash() const = 0;
    virtual std::string type_name() const { return ""; }
    virtual std::string type_dims() const { return ""; }
    std::string to_string() const {
        return type_name() + type_dims();
    }
};

/// Type expectation. Contains nullptr when no type is expected.
class TypeExpectation {
public:
    TypeExpectation() : type_(nullptr) {}
    TypeExpectation(const Type* type) : type_(type) {}

    const Type* type() const { return type_; }

    template <typename T>
    const T* isa() const {
        if (type_)
            return type_->isa<T>();
        return nullptr;
    }

    template <typename T>
    const T* as() const {
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
    size_t hash() const override { return 0; }
    std::string type_name() const override { return "<error>"; }
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

    std::string type_name() const override {
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
    typedef std::pair<std::string, const Type*> Member;
    typedef std::vector<Member> MemberList;

    CompoundType(const std::string& name, const MemberList& members)
        : name_(name), members_(members)
    {}
    virtual ~CompoundType() {}

    size_t hash() const override {
        std::hash<std::string> hash_string;
        size_t h = hash_string(name());
        for (auto member : members()) {
            h += hash_string(member.first) ^ member.second->hash();
        }
        return h;
    }

    const MemberList& members() const { return members_; }
    const std::string& name() const { return name_; }

    const Type* member_type(const std::string& name) const {
        auto it = std::find_if(members_.begin(), members_.end(),
            [name] (const std::pair<std::string, const Type*>& member) {
                return member.first == name;
            });
        return it != members_.end() ? it->second : nullptr;
    }

    std::string type_name() const override { return name_; }

protected:
    std::string name_;
    MemberList members_;
};

/// Structure type (equality decided from name only).
class StructType : public CompoundType {
public:
    StructType(const std::string& name, const MemberList& members)
        : CompoundType(name, members)
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
    InterfaceType(const std::string& name, const MemberList& members)
        : CompoundType(name, members)
    {}

    bool equals(const Type* other) const override {
        if (auto interface = other->isa<InterfaceType>()) {
            return interface->name() == name_;
        }
        return false;
    }
};

/// Primitive type (int, float, double, ...).
class PrimType : public Type {
public:
    enum Prim {
#define SLANG_KEY_DATA(key, str, rows, cols) PRIM_##key,
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

    bool subtype(const Type* other) const override {
        if (this == other)
            return true;

        const PrimType* prim_type = other->isa<PrimType>();
        if (!prim_type)
            return false;

        // Vector or matrices of the same size, whose components are
        // subtypes, are subtypes themselves.
        if (prim_type->cols() == cols() &&
            prim_type->rows() == rows()) {
            Prim comp1 = component();
            Prim comp2 = prim_type->component();
            switch (comp1) {
                case PRIM_UINT:
                case PRIM_INT:
                    return comp2 == PRIM_UINT || comp2 == PRIM_FLOAT || comp2 == PRIM_DOUBLE;
                case PRIM_FLOAT:
                    return comp2 == PRIM_DOUBLE;
                default:
                    break;
            }
        }

        return false;
    }

    size_t hash() const override {
        return (size_t)prim() + 1;
    }

    Prim prim() const { return prim_; }

    /// Returns the total number of components in this type.
    size_t size() const { return rows() * cols(); }
    /// Determines if this type is a vector type.
    bool is_vector() const { return cols() == 1 && rows() > 1; }
    /// Determines if this type is a matrix type.
    bool is_matrix() const { return cols() > 1 && rows() > 1; }

    /// Returns the number of rows in this type.
    size_t rows() const {
        static const std::unordered_map<Prim, size_t, HashPrim> prim_to_rows(
            {
        #define SLANG_KEY_DATA(key, str, rows, cols) {PRIM_##key, rows},
        #include "slang/keywordlist.h"
            }, 256);
        auto it = prim_to_rows.find(prim());
        assert(it != prim_to_rows.end());
        return it->second;
    }

    /// Returns the number of columns in this type.
    size_t cols() const {
        static const std::unordered_map<Prim, size_t, HashPrim> prim_to_cols(
            {
        #define SLANG_KEY_DATA(key, str, rows, cols) {PRIM_##key, cols},
        #include "slang/keywordlist.h"
            }, 256);
        auto it = prim_to_cols.find(prim());
        assert(it != prim_to_cols.end());
        return it->second;
    }

    std::string type_name() const override {
        static const std::unordered_map<Prim, std::string, HashPrim> prim_to_str(
            {
        #define SLANG_KEY_DATA(key, str, rows, cols) {PRIM_##key, str},
        #include "slang/keywordlist.h"
            }, 256);
        auto it = prim_to_str.find(prim());
        assert(it != prim_to_str.end());
        return it->second;
    }

    /// Returns the type of a component in this type.
    Prim component() const {
        switch (prim()) {
            case PRIM_VOID:    return PRIM_VOID;
            case PRIM_INT:
            case PRIM_IVEC2:
            case PRIM_IVEC3:
            case PRIM_IVEC4:   return PRIM_INT;
            case PRIM_BOOL:
            case PRIM_BVEC2:
            case PRIM_BVEC3:
            case PRIM_BVEC4:   return PRIM_BOOL;
            case PRIM_UINT:
            case PRIM_UVEC2:
            case PRIM_UVEC3:
            case PRIM_UVEC4:   return PRIM_UINT;
            case PRIM_MAT4X4:
            case PRIM_MAT4X3:
            case PRIM_MAT4X2:
            case PRIM_MAT3X4:
            case PRIM_MAT3X3:
            case PRIM_MAT3X2:
            case PRIM_MAT2X4:
            case PRIM_MAT2X3:
            case PRIM_MAT2X2:
            case PRIM_MAT4:
            case PRIM_MAT3:
            case PRIM_MAT2:
            case PRIM_FLOAT:
            case PRIM_VEC2:
            case PRIM_VEC3:
            case PRIM_VEC4:    return PRIM_FLOAT;
            case PRIM_DMAT4X4:
            case PRIM_DMAT4X3:
            case PRIM_DMAT4X2:
            case PRIM_DMAT3X4:
            case PRIM_DMAT3X3:
            case PRIM_DMAT3X2:
            case PRIM_DMAT2X4:
            case PRIM_DMAT2X3:
            case PRIM_DMAT2X2:
            case PRIM_DMAT4:
            case PRIM_DMAT3:
            case PRIM_DMAT2:
            case PRIM_DOUBLE:
            case PRIM_DVEC2:
            case PRIM_DVEC3:
            case PRIM_DVEC4:   return PRIM_DOUBLE;

            default:
                assert(0 && "Unknown primitive type");
                return prim();
        }
    }

private:
    struct HashPrim {
        size_t operator () (Prim prim) const {
            return (size_t)prim;
        }
    };

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

    std::string type_name() const override {
        return elem()->type_name();
    }

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

    std::string type_dims() const override {
        return "[]" + elem()->type_dims();
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

    bool subtype(const Type* other) const override {
        if (this == other)
            return true;

        if (auto indef = other->isa<IndefiniteArrayType>()) {
            return elem() == indef->elem() ||
                   (indef->elem()->isa<ArrayType>() && elem()->subtype(indef->elem()));
        } else if (auto def = other->isa<DefiniteArrayType>()) {
            return def->size() == size() &&
                   def->elem()->isa<ArrayType>() &&
                   elem()->subtype(def->elem());
        }

        return false;
    }

    size_t hash() const override {
        return elem()->hash() ^ size();
    }

    std::string type_dims() const override {
        return '[' + std::to_string(size_) + ']' + elem()->type_dims();
    }

private:
    int size_;
};

} // namespace slang

#endif // SLANG_TYPES_H
