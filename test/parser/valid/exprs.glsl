void foo() {
    int a = 1 + 2 * (3 + 4) - 9 / 9;
    int b = (1 - 9 * 4) * 7 / 2;
    int c = (1 & 1) + (2 | (99 ^ 5));
    bool d = (true || true) && (false || true) && true;
    struct {
        int i, j[2];
        float[2] a, b[2];
        vec2 x, y[2];
    } e;
    float f = e.a[0] / (e.b[0][0] + e.x.x) + e.y[0].x;
    bool g = !true || !false;
    int h = -1 - (-1);
    int i = --h;
    int j = ++i--;
    int k = --++i;
}