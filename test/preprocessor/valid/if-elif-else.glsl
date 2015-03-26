#if (1 + 1 << 1) + 9 / 6 == 5
int a;
#else
int b;
#endif

#if 8 / 2 == 5
int c;
#elif 8 + 8 == 15
int d;
#elif 1 + 9 % 7 + (99 & 3) - 78 << 3 == (6 - 78) * 8
int e;
#else
int f;
#endif

#if (!1 ^ 5) >> 1 == 5 / 2
int g;
#else
int h;
#endif

#if (1 >= 1) && (2 < 3 + 3) && (2 >= (1 | 1)) && (8 > 5)
int i;
#else
int j;
#endif

#if 0 || 5 / 8 || 64 % 8 || 84 / 5 > 14
int k;
#else
int l;
#endif

#if 0
int m;
#if 1
int n;
#endif
#endif

#if +1 - 1 == -1 + 1
int o;
#endif

#if ~0x80000000 == 0x7FFFFFFF
int p;
#endif

#if 1
#elif 1
int q;
#endif