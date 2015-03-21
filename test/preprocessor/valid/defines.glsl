#define G(x) x
#define H(x) x + 2
int j = G(H)(1);

#define SUM(a, b) (a + b)
#define PROD(a, b) (a * b)
#define PERCENT(a, b) (a * 100 / b)
#define TWO 2

#if PERCENT(5, 8) == PROD(SUM(29, TWO), TWO)
int a;
#else
int b;
#endif

#define define something
#define test #define test 32

test
test