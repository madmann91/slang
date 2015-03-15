#define test

#ifdef test
int a;
#else
int b;
#endif

#if defined(test)
int c;
#else
int d;
#endif

#if !defined(test)
int e;
#else
int f;
#endif