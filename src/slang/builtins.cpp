#include <string>
#include <sstream>

#include "slang/lexer.h"
#include "slang/parser.h"
#include "slang/preprocessor.h"

const std::string builtins =
R"(
#define unary(x) x
#define binary(x) x, x
#define ternary(x) x, x, x

#define gentype(f, a) \
  float f(a(float)); \
  vec2 f(a(vec2)); \
  vec3 f(a(vec3)); \
  vec4 f(a(vec4));

#define gendtype(f, a) \
  double f(a(double)); \
  dvec2 f(a(dvec2)); \
  dvec3 f(a(dvec3)); \
  dvec4 f(a(dvec4));

#define genitype(f, a) \
  int f(a(int)); \
  ivec2 f(a(ivec2)); \
  ivec3 f(a(ivec3)); \
  ivec4 f(a(ivec4));

#define genutype(f, a) \
  uint f(a(uint)); \
  uvec2 f(a(uvec2)); \
  uvec3 f(a(uvec3)); \
  uvec4 f(a(uvec4));

#define genbtype(f, a) \
  bool f(a(bool)); \
  bvec2 f(a(bvec2)); \
  bvec3 f(a(bvec3)); \
  bvec4 f(a(bvec4));

#define gentype1(f)  gentype(f, unary)
#define gendtype1(f) gendtype(f, unary)
#define genitype1(f) genitype(f, unary)
#define genutype1(f) genutype(f, unary)
#define genbtype1(f) genbtype(f, unary)

#define gentype2(f)  gentype(f, binary)
#define gendtype2(f) gendtype(f, binary)
#define genitype2(f) genitype(f, binary)
#define genutype2(f) genutype(f, binary)
#define genbtype2(f) genbtype(f, binary)

#define gentype3(f)  gentype(f, ternary)
#define gendtype3(f) gendtype(f, ternary)
#define genitype3(f) genitype(f, ternary)
#define genutype3(f) genutype(f, ternary)
#define genbtype3(f) genbtype(f, ternary)

gentype1(radians)
gentype1(degrees)
gentype1(sin)
gentype1(cos)
gentype1(tan)
gentype1(asin)
gentype1(acos)
gentype1(atan)
gentype2(atan)
gentype1(sinh)
gentype1(cosh)
gentype1(tanh)
gentype1(asinh)
gentype1(acosh)
gentype1(atanh)

gentype2(pow)
gentype1(exp)
gentype1(log)
gentype1(exp2)
gentype1(log2)
gentype1(sqrt)
gendtype1(sqrt)
gentype1(inversesqrt)
gendtype1(inversesqrt)

gentype1(abs)
genitype1(abs)
gendtype1(abs)
gentype1(sign)
genitype1(sign)
gendtype1(sign)
gentype1(floor)
gendtype1(floor)
gentype1(trunc)
gendtype1(trunc)
gentype1(round)
gendtype1(round)
gentype1(roundEven)
gendtype1(roundEven)
gentype1(ceil)
gendtype1(ceil)
gentype1(fract)
gendtype1(fract)
vec2 mod(vec2, float);
vec3 mod(vec3, float);
vec4 mod(vec4, float);
gentype2(mod)
dvec2 mod(dvec2, double);
dvec3 mod(dvec3, double);
dvec4 mod(dvec4, double);
gendtype2(mod)
float modf(float, out float);
vec2 modf(vec2, out vec2);
vec3 modf(vec3, out vec3);
vec4 modf(vec4, out vec4);
double modf(double, out double);
dvec2 modf(dvec2, out dvec2);
dvec3 modf(dvec3, out dvec3);
dvec4 modf(dvec4, out dvec4);

vec2 min(vec2, float);
vec3 min(vec3, float);
vec4 min(vec4, float);
gentype2(min)
dvec2 min(dvec2, double);
dvec3 min(dvec3, double);
dvec4 min(dvec4, double);
gendtype2(min)
ivec2 min(ivec2, int);
ivec3 min(ivec3, int);
ivec4 min(ivec4, int);
genitype2(min)
uvec2 min(uvec2, uint);
uvec3 min(uvec3, uint);
uvec4 min(uvec4, uint);
genutype2(min)

vec2 max(vec2, float);
vec3 max(vec3, float);
vec4 max(vec4, float);
gentype2(max)
dvec2 max(dvec2, double);
dvec3 max(dvec3, double);
dvec4 max(dvec4, double);
gendtype2(max)
ivec2 max(ivec2, int);
ivec3 max(ivec3, int);
ivec4 max(ivec4, int);
genitype2(max)
uvec2 max(uvec2, uint);
uvec3 max(uvec3, uint);
uvec4 max(uvec4, uint);
genutype2(max)

vec2 clamp(vec2, float, float);
vec3 clamp(vec3, float, float);
vec4 clamp(vec4, float, float);
gentype3(clamp)
dvec2 clamp(dvec2, double, double);
dvec3 clamp(dvec3, double, double);
dvec4 clamp(dvec4, double, double);
gendtype3(clamp)
ivec2 clamp(ivec2, int, int);
ivec3 clamp(ivec3, int, int);
ivec4 clamp(ivec4, int, int);
genitype3(clamp)
uvec2 clamp(uvec2, uint, uint);
uvec3 clamp(uvec3, uint, uint);
uvec4 clamp(uvec4, uint, uint);
genutype3(clamp)

vec2 mix(vec2, vec2, float);
vec3 mix(vec3, vec3, float);
vec4 mix(vec4, vec4, float);
gentype3(mix)
dvec2 mix(dvec2, dvec2, double);
dvec3 mix(dvec3, dvec3, double);
dvec4 mix(dvec4, dvec4, double);
gendtype3(mix)
float mix(float, float, bool);
vec2 mix(vec2, vec2, bvec2);
vec3 mix(vec3, vec3, bvec3);
vec4 mix(vec4, vec4, bvec4);
double mix(double, double, bool);
dvec2 mix(dvec2, dvec2, bvec2);
dvec3 mix(dvec3, dvec3, bvec3);
dvec4 mix(dvec4, dvec4, bvec4);

vec2 step(float, vec2);
vec3 step(float, vec3);
vec4 step(float, vec4);
gentype2(step)
dvec2 step(double, dvec2);
dvec3 step(double, dvec3);
dvec4 step(double, dvec4);
gendtype2(step)

vec2 smoothstep(float, float, vec2);
vec3 smoothstep(float, float, vec3);
vec4 smoothstep(float, float, vec4);
gentype3(smoothstep)
dvec2 smoothstep(double, double, dvec2);
dvec3 smoothstep(double, double, dvec3);
dvec4 smoothstep(double, double, dvec4);
gendtype3(smoothstep)

bool isnan(float);
bvec2 isnan(vec2);
bvec3 isnan(vec3);
bvec4 isnan(vec4);
bool isnan(double);
bvec2 isnan(dvec2);
bvec3 isnan(dvec3);
bvec4 isnan(dvec4);

bool isinf(float);
bvec2 isinf(vec2);
bvec3 isinf(vec3);
bvec4 isinf(vec4);
bool isinf(double);
bvec2 isinf(dvec2);
bvec3 isinf(dvec3);
bvec4 isinf(dvec4);

int floatBitsToInt(float);
ivec2 floatBitsToInt(vec2);
ivec3 floatBitsToInt(vec3);
ivec4 floatBitsToInt(vec4);
uint floatBitsToUint(float);
uvec2 floatBitsToUint(vec2);
uvec3 floatBitsToUint(vec3);
uvec4 floatBitsToUint(vec4);

float intBitsToFloat(int);
vec2 intBitsToFloat(ivec2);
vec3 intBitsToFloat(ivec3);
vec3 intBitsToFloat(ivec4);
float uintBitsToFloat(uint);
vec2 uintBitsToFloat(uvec2);
vec3 uintBitsToFloat(uvec3);
vec3 uintBitsToFloat(uvec4);

gentype3(fma)
gendtype3(fma)

float frexp(float, out int);
vec2 frexp(vec2, out ivec2);
vec3 frexp(vec3, out ivec3);
vec4 frexp(vec4, out ivec4);
double frexp(double, out int);
dvec2 frexp(dvec2, out ivec2);
dvec3 frexp(dvec3, out ivec3);
dvec4 frexp(dvec4, out ivec4);

float ldexp(float, in int);
vec2 ldexp(vec2, in ivec2);
vec3 ldexp(vec3, in ivec3);
vec4 ldexp(vec4, in ivec4);
double ldexp(double, in int);
dvec2 ldexp(dvec2, in ivec2);
dvec3 ldexp(dvec3, in ivec3);
dvec4 ldexp(dvec4, in ivec4);

float length(float);
float length(vec2);
float length(vec3);
float length(vec4);
double length(double);
double length(dvec2);
double length(dvec3);
double length(dvec4);

float distance(binary(float));
float distance(binary(vec2));
float distance(binary(vec3));
float distance(binary(vec4));
double distance(binary(double));
double distance(binary(dvec2));
double distance(binary(dvec3));
double distance(binary(dvec4));

float dot(binary(float));
float dot(binary(vec2));
float dot(binary(vec3));
float dot(binary(vec4));
double dot(binary(double));
double dot(binary(dvec2));
double dot(binary(dvec3));
double dot(binary(dvec4));

vec3 cross(vec3, vec3);
dvec3 cross(dvec3, dvec3);

gentype1(normalize)
gendtype1(normalize)
gentype3(faceforward)
gendtype3(faceforward)
gentype2(reflect)
gendtype2(reflect)

float refract(float, float, float);
vec2 refract(vec2, vec2, float);
vec3 refract(vec3, vec3, float);
vec4 refract(vec4, vec4, float);

double refract(double, double, float);
double refract(dvec2, dvec2, float);
double refract(dvec3, dvec3, float);
double refract(dvec4, dvec4, float);

#define matrix_functions(d) \
  d##mat2   matrixCompMult(binary(d##mat2)); \
  d##mat3   matrixCompMult(binary(d##mat3)); \
  d##mat4   matrixCompMult(binary(d##mat4)); \
  d##mat2x3 matrixCompMult(binary(d##mat2x3)); \
  d##mat2x4 matrixCompMult(binary(d##mat2x4)); \
  d##mat3x2 matrixCompMult(binary(d##mat3x2)); \
  d##mat3x4 matrixCompMult(binary(d##mat3x4)); \
  d##mat4x2 matrixCompMult(binary(d##mat4x2)); \
  d##mat4x3 matrixCompMult(binary(d##mat4x3)); \
  d##mat2   outerProduct(d##vec2, d##vec2); \
  d##mat3   outerProduct(d##vec3, d##vec3); \
  d##mat4   outerProduct(d##vec4, d##vec4); \
  d##mat2x3 outerProduct(d##vec3, d##vec2); \
  d##mat3x2 outerProduct(d##vec2, d##vec3); \
  d##mat2x4 outerProduct(d##vec4, d##vec2); \
  d##mat4x2 outerProduct(d##vec2, d##vec4); \
  d##mat3x4 outerProduct(d##vec4, d##vec3); \
  d##mat4x3 outerProduct(d##vec3, d##vec4); \
  d##mat2   transpose(d##mat2); \
  d##mat3   transpose(d##mat3); \
  d##mat4   transpose(d##mat4); \
  d##mat2x3 transpose(d##mat3x2); \
  d##mat3x2 transpose(d##mat2x3); \
  d##mat2x4 transpose(d##mat4x2); \
  d##mat4x2 transpose(d##mat2x4); \
  d##mat3x4 transpose(d##mat4x3); \
  d##mat4x3 transpose(d##mat3x4); \
  d##mat2   inverse(d##mat2); \
  d##mat3   inverse(d##mat3); \
  d##mat4   inverse(d##mat4);

matrix_functions()
matrix_functions(d)

float  determinant(mat2);
float  determinant(mat3);
float  determinant(mat4);
double determinant(dmat2);
double determinant(dmat3);
double determinant(dmat4);

#define relational_function(f) \
  bvec2 f(vec2, vec2); \
  bvec3 f(vec3, vec3); \
  bvec4 f(vec4, vec4); \
  bvec2 f(dvec2, dvec2); \
  bvec3 f(dvec3, dvec3); \
  bvec4 f(dvec4, dvec4); \
  bvec2 f(ivec2, ivec2); \
  bvec3 f(ivec3, ivec3); \
  bvec4 f(ivec4, ivec4); \
  bvec2 f(uvec2, uvec2); \
  bvec3 f(uvec3, uvec3); \
  bvec4 f(uvec4, uvec4);

relational_function(lessThan)
relational_function(lessThanEqual)
relational_function(greaterThan)
relational_function(greaterThanEqual)
bvec2 equal(bvec2, bvec2);
bvec3 equal(bvec3, bvec3);
bvec4 equal(bvec4, bvec4);
relational_function(equal)
bvec2 notEqual(bvec2, bvec2);
bvec3 notEqual(bvec3, bvec3);
bvec4 notEqual(bvec4, bvec4);
relational_function(notEqual)
bvec2 any(bvec2);
bvec3 any(bvec3);
bvec4 any(bvec4);
bvec2 all(bvec2);
bvec3 all(bvec3);
bvec4 all(bvec4);
bvec2 not(bvec2);
bvec3 not(bvec3);
bvec4 not(bvec4);

uint  uaddCarry(uint,  uint,  out uint);
uvec2 uaddCarry(uvec2, uvec2, out uvec2);
uvec3 uaddCarry(uvec3, uvec3, out uvec3);
uvec4 uaddCarry(uvec4, uvec4, out uvec4);
uint  usubBorrow(uint,  uint,  out uint);
uvec2 usubBorrow(uvec2, uvec2, out uvec2);
uvec3 usubBorrow(uvec3, uvec3, out uvec3);
uvec4 usubBorrow(uvec4, uvec4, out uvec4);

uint  umulExtended(uint,  uint,  out uint,  out uint);
uvec2 umulExtended(uvec2, uvec2, out uvec2, out uvec2);
uvec3 umulExtended(uvec3, uvec3, out uvec3, out uvec3);
uvec4 umulExtended(uvec4, uvec4, out uvec4, out uvec4);

int   imulExtended(int,   int,   out int,   out int);
ivec2 imulExtended(ivec2, ivec2, out ivec2, out ivec2);
ivec3 imulExtended(ivec3, ivec3, out ivec3, out ivec3);
ivec4 imulExtended(ivec4, ivec4, out ivec4, out ivec4);

int   bitfieldExtract(int, int, int);
ivec2 bitfieldExtract(ivec2, int, int);
ivec3 bitfieldExtract(ivec3, int, int);
ivec4 bitfieldExtract(ivec4, int, int);

uint  bitfieldExtract(uint, int, int);
uvec2 bitfieldExtract(uvec2, int, int);
uvec3 bitfieldExtract(uvec3, int, int);
uvec4 bitfieldExtract(uvec4, int, int);

int   bitfieldInsert(int, int, int, int);
ivec2 bitfieldInsert(ivec2, ivec2, int, int);
ivec3 bitfieldInsert(ivec3, ivec3, int, int);
ivec4 bitfieldInsert(ivec4, ivec4, int, int);

uint  bitfieldInsert(uint, uint, int, int);
uvec2 bitfieldInsert(uvec2, uvec2, int, int);
uvec3 bitfieldInsert(uvec3, uvec3, int, int);
uvec4 bitfieldInsert(uvec4, uvec4, int, int);

int  bitfieldReverse(int);
ivec2 bitfieldReverse(ivec2);
ivec3 bitfieldReverse(ivec3);
ivec4 bitfieldReverse(ivec4);

uint  bitfieldReverse(uint);
uvec2 bitfieldReverse(uvec2);
uvec3 bitfieldReverse(uvec3);
uvec4 bitfieldReverse(uvec4);

#define count_function(f) \
  int   f(int); \
  ivec2 f(ivec2); \
  ivec3 f(ivec3); \
  ivec4 f(ivec4); \
  int   f(uint); \
  ivec2 f(uvec2); \
  ivec3 f(uvec3); \
  ivec4 f(uvec4);

count_function(bitCount)
count_function(findLSB)
count_function(findMSB)

#define atomic_function(f) \
  uint f(inout uint, uint); \
  int  f(inout int, int);

atomic_function(atomicAdd)
atomic_function(atomicMin)
atomic_function(atomicMax)
atomic_function(atomicAnd)
atomic_function(atomicOr)
atomic_function(atomicXor)
atomic_function(atomicExchange)

uint atomicCompSwap(inout uint, uint, uint);
int  atomicCompSwap(inout int, int, int);
)";

using namespace slang;

class NullBuffer : public std::streambuf
{
public:
    int overflow(int c) { return c; }
};

std::unique_ptr<ast::Module> parse_builtins(Sema& sema, const Keywords& keys) {
    NullBuffer null_buffer;
    std::ostream null_stream(&null_buffer);
    std::istringstream is(builtins);

    Logger logger("", null_stream, null_stream);
    Lexer lexer(is, keys, logger);
    Preprocessor pp(lexer, logger);
    Parser parser([&pp]() { return pp.preprocess(); }, sema, logger);
    auto mod = parser.parse();
    assert(lexer.error_count() + pp.error_count() + parser.error_count() + sema.error_count() == 0);
    return mod;
}