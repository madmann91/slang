void foo();
void foo() {
    int a = 2 + 2;
}

int bar(int, int);
int bar(int a, int b) {
    return a + b;
}

int foobar(void);
int foobar(void) {
    return bar(foobar(), foobar());
}


float baz(float a, vec3 b);
float baz(float, float) {
    return baz(baz(42.0, 42.0), baz(42.0, 42.0));
}
