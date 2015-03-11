#define TEST

#ifndef TEST
    #ifdef TEST
        int a;
    #else
        int b;
    #endif
#else
    #ifdef TEST
        int c;
    #else
        int d;
    #endif
#endif
