#ifndef SLANG_TYPES_H
#define SLANG_TYPES_H

#include <vector>
#include <string>
#include <algorithm>
#include <unordered_set>

#include "slang/cast.h"
#include "slang/ptr.h"

namespace slang {

enum class StorageQualifier {
    STORAGE_NONE,
    STORAGE_CONST,
    STORAGE_IN,
    STORAGE_OUT,
    STORAGE_INOUT,
    STORAGE_ATTRIBUTE,
    STORAGE_UNIFORM,
    STORAGE_VARYING,
    STORAGE_BUFFER,
    STORAGE_SHARED
};

/// Base class for symbol types.
class Type : public Cast<Type> {
public:
    virtual ~Type() {}

    /// Tests two types for equality, by inspecting the type.
    /// This function is used by the type table, you should not use it.
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

/// Qualified types representation. Contains a type and the set of qualifiers attached to it.
class QualifiedType {
public:
    QualifiedType(const Type* type, StorageQualifier storage = StorageQualifier::STORAGE_NONE)
        : type_(type), storage_(storage)
    {}

    /// Returns a hash value for this type.
    size_t hash() const;
    /// Tests the given type for equality, should not be used unless you
    /// need to inspect the type (use == instead).
    bool equals(const QualifiedType&) const;
    /// Returns true if this type is a subtype of the given
    /// type and they have the same qualifiers.
    bool subtype(const QualifiedType&) const;

    /// Determines if the types and their qualifiers are the same.
    bool operator == (const QualifiedType&) const;
    /// Determines if the types and their qualifiers are different (same as !(a == b)).
    bool operator != (const QualifiedType&) const;

    /// Returns the non-qualified type.
    const Type* type() const { return type_; }
    /// Determines if this type has a qualifier or not.
    bool is_qualified() const {
        return storage() != StorageQualifier::STORAGE_NONE;
    }

    /// Returns the storage qualifier associated with this type.
    StorageQualifier storage() const { return storage_; }

    /// Returns the full type name, with its qualifiers.
    std::string to_string() const;

private:
    const Type* type_;
    StorageQualifier storage_;
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
    typedef std::vector<QualifiedType> ArgList;

    FunctionType(QualifiedType ret, const ArgList& args)
        : ret_(ret), args_(args)
    {}

    bool equals(const Type*) const override;
    size_t hash() const override;
    std::string type_name() const override;

    QualifiedType ret() const { return ret_; }
    const ArgList& args() const { return args_; }
    size_t num_args() const { return args_.size(); }

private:
    QualifiedType ret_;
    ArgList args_;
};

/// Overloaded function type. Contains several function signatures.
class OverloadedFunctionType : public CallableType {
public:
    typedef std::vector<const FunctionType*> SignatureList;

    OverloadedFunctionType(const SignatureList& signs)
        : signs_(signs)
    {}

    bool equals(const Type*) const override;
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
    typedef std::pair<std::string, QualifiedType> Member;
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

    const QualifiedType* member_type(const std::string& name) const {
        auto it = std::find_if(members_.begin(), members_.end(),
            [name] (const std::pair<std::string, QualifiedType>& member) {
                return member.first == name;
            });
        return it != members_.end() ? &it->second : nullptr;
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

    bool equals(const Type*) const override;
};

/// Interface type (cannot be equal to another type).
class InterfaceType : public CompoundType {
public:
    InterfaceType(const std::string& name, const MemberList& members)
        : CompoundType(name, members)
    {}

    bool equals(const Type*) const override;
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

    bool equals(const Type*) const override;
    bool subtype(const Type*) const override;
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

    bool equals(const Type*) const override;
    size_t hash() const override;
    std::string type_dims() const override;
};

/// Array with known size.
class DefiniteArrayType : public ArrayType {
public:
    DefiniteArrayType(const Type* elem, int size)
        : ArrayType(elem), size_(size)
    {}

    bool equals(const Type*) const override;
    bool subtype(const Type*) const override;
    size_t hash() const override;
    std::string type_dims() const override;

    int size() const { return size_; }

private:
    int size_;
};

/// Hash table containing types. Types are hashed and stored uniquely,
/// which means type equality can be checked with pointer equality.
class TypeTable {
public:
    virtual ~TypeTable() {
        for (auto type : types_)
            delete type;
    }

    /// Creates an error type. For expressions that fail typechecking.
    const ErrorType* error_type() { return new_type<ErrorType>(); }
    /// Creates a primitive type.
    const PrimType* prim_type(PrimType::Prim prim, int rows = 1, int cols = 1) {
        return new_type<PrimType>(prim, rows, cols);
    }
    /// Creates a function type from a return type and a list of arguments.
    const FunctionType* function_type(const Type* ret, const FunctionType::ArgList& args) {
        return new_type<FunctionType>(ret, args);
    }
    /// Creates an overloaded function type.
    const OverloadedFunctionType* overloaded_function_type(const OverloadedFunctionType::SignatureList& signs) {
        return new_type<OverloadedFunctionType>(signs);
    }
    /// Creates a structure type from a list of members and a name.
    const StructType* struct_type(const std::string& name,
                                  const StructType::MemberList& members) {
        return new_type<StructType>(name, members);
    }
    /// Creates an interface type type from a list of members and a name.
    const InterfaceType* interface_type(const std::string& name,
                                        const InterfaceType::MemberList& members) {
        return new_type<InterfaceType>(name, members);
    }
    /// Creates an array whose size is unknown.
    const IndefiniteArrayType* array_type(const Type* elem) {
        return new_type<IndefiniteArrayType>(elem);
    }
    /// Creates an array of known size.
    const DefiniteArrayType* array_type(const Type* elem, int dim) {
        return new_type<DefiniteArrayType>(elem, dim);
    }

private:
    template <typename T, typename... Args>
    const T* new_type(Args... args) {
        T t(std::forward<Args>(args)...);
        auto it = types_.find(&t);
        if (it != types_.end())
            return (*it)->as<T>();

        const T* pt = new T(t);
        types_.emplace(pt);
        return pt;
    }

    struct HashType {
        size_t operator () (const Type* t) const {
            return t->hash();
        }
    };

    struct EqualType {
        bool operator () (const Type* t1, const Type* t2) const {
            return t1->equals(t2);
        }
    };

    std::unordered_set<const Type*, HashType, EqualType> types_;
};

} // namespace slang

#endif // SLANG_TYPES_H
