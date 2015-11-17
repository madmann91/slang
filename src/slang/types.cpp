#include "slang/types.h"

namespace slang {

size_t TypeQualifiers::hash() const {
    return (int)storage() ^ (int)parameter();
}

bool TypeQualifiers::operator == (const TypeQualifiers& other) const {
    return storage() == other.storage() &&
           parameter() == other.parameter();
}

size_t QualifiedType::hash() const {
    return type()->hash() + qualifiers().hash();
}

bool QualifiedType::equals(const QualifiedType& other) const {
    return other.qualifiers() == qualifiers() && type()->equals(other.type());
}

bool QualifiedType::subtype(const QualifiedType& other) const {
    return other.qualifiers() == qualifiers() && type()->subtype(other.type());
}

bool QualifiedType::operator == (const QualifiedType& other) const {
    return other.type() == type() && other.qualifiers() == qualifiers();
}

bool QualifiedType::operator != (const QualifiedType& other) const {
    return !(*this == other);
}

std::string QualifiedType::to_string() const {
    std::string str;
    switch (qualifiers().parameter()) {
        case ParameterQualifier::IN:        str += "in ";        break;
        case ParameterQualifier::OUT:       str += "out ";       break;
        case ParameterQualifier::INOUT:     str += "inout ";     break;
        default: break;
    }
    switch (qualifiers().storage()) {
        case StorageQualifier::CONST:     str += "const ";     break;
        case StorageQualifier::IN:        str += "in ";        break;
        case StorageQualifier::OUT:       str += "out ";       break;
        case StorageQualifier::INOUT:     str += "inout ";     break;
        case StorageQualifier::ATTRIBUTE: str += "attribute "; break;
        case StorageQualifier::UNIFORM:   str += "uniform ";   break;
        case StorageQualifier::VARYING:   str += "varying ";   break;
        case StorageQualifier::BUFFER:    str += "buffer ";    break;
        case StorageQualifier::SHARED:    str += "shared ";    break;
        default: break;
    }
    return str + type_->to_string();
}

bool FunctionType::equals(const Type* other) const {
    if (auto fn = other->isa<FunctionType>()) {
        if (num_args() != fn->num_args())
            return false;
        for (size_t i = 0; i < num_args(); i++) {
            if (!args()[i].equals(fn->args()[i]))
                return false;
        }
        return ret().equals(fn->ret());
    }

    return false;
}

size_t FunctionType::hash() const {
    size_t h = 0;
    for (auto arg : args()) {
        h += arg.hash();
    }
    return ret().hash() ^ h;
}

std::string FunctionType::type_name() const {
    std::string str = ret().to_string() + " (";
    for (size_t i = 0; i < num_args(); i++) {
        str += args()[i].to_string();
        if (i != num_args() - 1) str += ", ";
    }
    return str + ")";
}

bool OverloadedFunctionType::equals(const Type* other) const {
    if (auto fn = other->isa<OverloadedFunctionType>()) {
        if (num_signatures() != fn->num_signatures())
            return false;

        for (size_t i = 0; i < num_signatures(); i++) {
            if (!signatures()[i]->equals(fn->signatures()[i]))
                return false;
        }

        return true;
    }

    return false;
}

size_t OverloadedFunctionType::hash() const {
    size_t h = 0;
    for (auto sign : signatures()) {
        h += sign->hash();
    }
    return h;
}

std::string OverloadedFunctionType::type_name() const {
    return "<overloaded function type>";
}

size_t CompoundType::hash() const {
    std::hash<std::string> hash_string;
    size_t h = hash_string(name());
    for (auto member : members()) {
        h += hash_string(member.first) ^ member.second.hash();
    }
    return h;
}

std::string CompoundType::type_name() const {
    return name_;
}

bool StructType::equals(const Type* other) const {
    if (auto st = other->isa<StructType>()) {
        return st->name() == name_;
    }
    return false;
}

bool InterfaceType::equals(const Type* other) const {
    if (auto interface = other->isa<InterfaceType>()) {
        return interface->name() == name_;
    }
    return false;
}

bool PrimType::equals(const Type* other) const {
    if (auto prim_type = other->isa<PrimType>()) {
        return prim_type->prim() == prim() &&
               prim_type->rows() == rows() &&
               prim_type->cols() == cols();
    }
    return false;
}

bool PrimType::subtype(const Type* other) const {
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
            case UINT:
            case INT:
                return prim_type->prim() == UINT ||
                       prim_type->prim() == FLOAT ||
                       prim_type->prim() == DOUBLE;
            case FLOAT:
                return prim_type->prim() == DOUBLE;
            default:
                break;
        }
    }

    return false;
}

size_t PrimType::hash() const {
    return (size_t)prim() + cols() * 8 + rows() + 1;
}

std::string PrimType::type_name() const {
    if (rows() == 1 && cols() == 1) {
        switch (prim()) {
            case INT:     return "int";
            case UINT:    return "uint";
            case BOOL:    return "bool";
            case FLOAT:   return "float";
            case DOUBLE:  return "double";
            case VOID:    return "void";
        }
    }

    if (rows() > 1 && cols() == 1) {
        switch (prim()) {
            case INT:     return "ivec" + std::to_string(rows());
            case UINT:    return "uvec" + std::to_string(rows());
            case BOOL:    return "bvec" + std::to_string(rows());
            case FLOAT:   return "vec"  + std::to_string(rows());
            case DOUBLE:  return "dvec" + std::to_string(rows());
            default:          assert(0 && "Invalid vector type");
        }
    }

    if (rows() > 1 && cols() > 1) {
        std::string prefix;
        if (prim() == FLOAT) {
            prefix = "mat";
        } else if (prim() == DOUBLE) {
            prefix = "dmat";
        } else {
            assert(0 && "Invalid matrix type");
        }
        return prefix + "mat" + std::to_string(rows()) + "x" + std::to_string(cols());
    }

    assert(0 && "Unknown primitive type");
    return "";
}

bool IndefiniteArrayType::equals(const Type* other) const {
    if (auto def = other->isa<IndefiniteArrayType>()) {
        return def->elem() == elem();
    }
    return false;
}

size_t IndefiniteArrayType::hash() const {
    return elem()->hash();
}

std::string IndefiniteArrayType::type_dims() const {
    return "[]" + elem()->type_dims();
}

bool DefiniteArrayType::equals(const Type* other) const {
    if (auto def = other->isa<DefiniteArrayType>()) {
        return def->size() == size_ && def->elem()== elem();
    }
    return false;
}

bool DefiniteArrayType::subtype(const Type* other) const {
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

size_t DefiniteArrayType::hash() const {
    return (elem()->hash() ^ size());
}

std::string DefiniteArrayType::type_dims() const {
    return '[' + std::to_string(size_) + ']' + elem()->type_dims();
}

} // namespace slang
