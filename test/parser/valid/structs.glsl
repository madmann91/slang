struct {
    int a;
};

struct s1 {
    int a;
};

struct s2 {
    float a;
    vec2 b;
    vec3 c;
    vec4 d;
};

lowp struct s3 {
    highp s1 a;
    s2 b;
};

struct s4 {
    struct s5 {
        struct s6 {
            struct { int d; } c;
        } b;
    } a;
};
