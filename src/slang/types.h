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

/// Base class for function types.
class CallableType : public Type {
public:
    virtual ~CallableType() {}
};

/// Function type (equality based on return type and arguments).
class FunctionType : public CallableType {
public:
    typedef std::vector<const Type*> ArgList;

    FunctionType(const Type* ret, const ArgList& args)
        : ret_(ret), args_(args)
    {}

    bool equals(const Type* other) const override;
    size_t hash() const override;
    std::string type_name() const override;

    const Type* ret() const { return ret_; }
    const ArgList& args() const { return args_; }
    size_t num_args() const { return args_.size(); }

private:
    const Type* ret_;
    ArgList args_;
};

/// Overloaded function type. Contains several function signatures.
class OverloadedFunctionType : public CallableType {
public:
    typedef std::vector<const FunctionType*> SignatureList;

    OverloadedFunctionType(const SignatureList& signs)
        : signs_(signs)
    {}

    bool equals(const Type* other) const override;
    size_t hash() const override;
    std::string type_name() const override;

    const SignatureList& signatures() const { return signs_; }
    size_t num_signatures() const { return signs_.size(); }

private:
    SignatureList signs_;
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

    size_t hash() const override;
    std::string type_name() const override;

    const MemberList& members() const { return members_; }
    size_t num_members() const { return members_.size(); }

    const std::string& name() const { return name_; }

    const Type* member_type(const std::string& name) const {
        auto it = std::find_if(members_.begin(), members_.end(),
            [name] (const std::pair<std::string, const Type*>& member) {
                return member.first == name;
            });
        return it != members_.end() ? it->second : nullptr;
    }

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

    bool equals(const Type* other) const override;
};

/// Interface type (cannot be equal to another type).
class InterfaceType : public CompoundType {
public:
    InterfaceType(const std::string& name, const MemberList& members)
        : CompoundType(name, members)
    {}

    bool equals(const Type* other) const override;
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

    bool equals(const Type* other) const override;
    bool subtype(const Type* other) const override;
    size_t hash() const override;

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

    std::string type_name() const override;

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

    std::string type_name() const override {
        return elem()->type_name();
    }

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

    bool equals(const Type* other) const override;
    size_t hash() const override;
    std::string type_dims() const override;
};

/// Array with known size.
class DefiniteArrayType : public ArrayType {
public:
    DefiniteArrayType(const Type* elem, int size)
        : ArrayType(elem), size_(size)
    {}

    bool equals(const Type* other) const override;
    bool subtype(const Type* other) const override;
    size_t hash() const override;
    std::string type_dims() const override;

    int size() const { return size_; }

private:
    int size_;
};

} // namespace slang

#endif // SLANG_TYPES_H
