void foo() {
    int a = 1 + 2 * (3 + 4) - 9 / 9;
    int b = (1 - 9 * 4 % 5) * 7 / 2;
    int c = (1 & 1) + (2 | (99 ^ 5));
    bool d = (true || true) && (false || true) && true;

    struct {
        int i, j[2];
        float[2] a, b[2];
        vec2 x, y[2];
    } e = {
        1, {2, 3},
        {1.0, 2.0}, {{1.0, 2.0}, {1.555, 42.}},
        {1.0, 0.5}, {{2.0, 1.0}, {1.0, 1.0}},
    };

    float f = e.a[0] / (e.b[0][0] + e.x.x) + e.y[0].x;
    bool g = !true || !false;

    int h = -1 - (-1) + ~2 + (+2);
    int i = --h;
    int j = ++i--;
    int k = --++i;
    k++, i--;

    int m = (5 << 1 >> 2) + (2 << 1 >> 5);
    m <<= 1;
    m >>= 1;

    a += a == a;
    b -= b % m;
    b /= a *= c &= a ^= b |= a %= a + m;

    a = a >= b;
    a = a != ((b <= c) + 5);
    c = 1 >= 2 >= 3 <= 5;

    a = a >= 5 ? a : 5;
}
