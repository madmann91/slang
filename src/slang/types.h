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

    /// Tests two types for equality, by inspecting the type.
    /// This function is used by the type hashtable, you should not use it.
    virtual bool equals(const Type*) const { return false; }
    /// Determines if this type is a subtype of another one.
    virtual bool subtype(const Type* type) const { return this == type; }
    /// Computes a hash for this type.
    virtual size_t hash() const = 0;
    /// Returns the name of the type as a string (i.e. "int").
    virtual std::string type_name() const { return ""; }
    /// Returns the dimensions of the type as a string (i.e. "[1][3]").
    virtual std::string type_dims() const { return ""; }
    /// Returns the full type name.
    std::string to_string() const {
        return type_name() + type_dims();
    }
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
        PRIM_INT,
        PRIM_UINT,
        PRIM_BOOL,
        PRIM_FLOAT,
        PRIM_DOUBLE,
        PRIM_VOID
    };

    PrimType(Prim prim, int rows = 1, int cols = 1)
        : prim_(prim), rows_(rows), cols_(cols)
    {}

    bool equals(const Type* other) const override {
        if (auto prim_type = other->isa<PrimType>()) {
            return prim_type->prim() == prim() &&
                   prim_type->rows() == rows() &&
                   prim_type->cols() == cols();
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
            switch (prim()) {
                case PRIM_UINT:
                case PRIM_INT:
                    return prim_type->prim() == PRIM_UINT ||
                           prim_type->prim() == PRIM_FLOAT ||
                           prim_type->prim() == PRIM_DOUBLE;
                case PRIM_FLOAT:
                    return prim_type->prim() == PRIM_DOUBLE;
                default:
                    break;
            }
        }

        return false;
    }

    size_t hash() const override {
        return (size_t)prim() + cols() * 8 + rows() + 1;
    }

    Prim prim() const { return prim_; }

    /// Returns the total number of components in this type.
    size_t size() const { return rows() * cols(); }
    /// Determines if this type is a scalar type.
    bool is_scalar() const { return size() == 1; }
    /// Determines if this type is a vector type.
    bool is_vector() const { return cols() == 1 && rows() > 1; }
    /// Determines if this type is a matrix type.
    bool is_matrix() const { return cols() > 1 && rows() > 1; }

    /// Determines if this type can be added, subtracted, ...
    bool is_numeric() const {
        return prim() == PrimType::PRIM_INT ||
               prim() == PrimType::PRIM_UINT ||
               prim() == PrimType::PRIM_FLOAT ||
               prim() == PrimType::PRIM_DOUBLE;
    }

    /// Determines if this type is an integer type.
    bool is_integer() const {
        return prim() == PrimType::PRIM_INT ||
               prim() == PrimType::PRIM_UINT;
    }

    /// Determines if this type can be compared.
    bool is_ordered() const {
        return size() == 1 &&
               (prim() == PrimType::PRIM_INT ||
                prim() == PrimType::PRIM_UINT ||
                prim() == PrimType::PRIM_FLOAT ||
                prim() == PrimType::PRIM_DOUBLE);
    }

    /// Determines if this type is a boolean.
    bool is_boolean() const {
        return size() == 1 && prim() == PrimType::PRIM_BOOL;
    }

    /// Determines if this type is a floating point type.
    bool is_floating() const {
        return prim() == PrimType::PRIM_FLOAT ||
               prim() == PrimType::PRIM_DOUBLE;
    }

    /// Returns the number of rows in this type.
    size_t rows() const { return rows_; }
    /// Returns the number of columns in this type.
    size_t cols() const { return cols_; }

    std::string type_name() const override {
        if (rows() == 1 && cols() == 1) {
            switch (prim()) {
                case PRIM_INT:    return "int";
                case PRIM_UINT:   return "uint";
                case PRIM_BOOL:   return "bool";
                case PRIM_FLOAT:  return "float";
                case PRIM_DOUBLE: return "double";
                case PRIM_VOID:   return "void";
            }
        }

        if (rows() > 1 && cols() == 1) {
            switch (prim()) {
                case PRIM_INT:    return "ivec" + std::to_string(rows());
                case PRIM_UINT:   return "uvec" + std::to_string(rows());
                case PRIM_BOOL:   return "bvec" + std::to_string(rows());
                case PRIM_FLOAT:  return "vec"  + std::to_string(rows());
                case PRIM_DOUBLE: return "dvec" + std::to_string(rows());
                default:          assert(0 && "Invalid vector type");
            }
        }

        if (rows() > 1 && cols() > 1) {
            std::string prefix;
            if (prim() == PRIM_FLOAT) {
                prefix = "mat";
            } else if (prim() == PRIM_DOUBLE) {
                prefix = "dmat";
            } else {
                assert(0 && "Invalid matrix type");
            }
            return prefix + "mat" + std::to_string(rows()) + "x" + std::to_string(cols());
        }

        assert(0 && "Unknown primitive type");
        return "";
    }

private:
    struct HashPrim {
        size_t operator () (Prim prim) const {
            return (size_t)prim;
        }
    };

    Prim prim_;
    int rows_;
    int cols_;
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
