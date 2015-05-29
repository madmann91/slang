#define BROKEN_EXP(x) 1+x ] -
#define SUM(a, b) (a + b)

#if BROKEN_EXP(33) == 78
int i;
#else
int j;
#endif

#if ANSWER == 42
#else
int k;
#endif

#if SUM(1,
#else
int l;
#endif

#if 0.99
#else
int m;
#endif

#if # {
int n;
#endif
