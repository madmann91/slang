struct {};
struct s1 {};

struct s2 {
    int a;
};

struct {
    int a;
};

struct s3 {
    float a;
    vec2 b;
    vec3 c;
    vec4 d;
};

lowp struct s4 {
    highp s3 a;
    const s2 b;
};

struct s5 {
    struct s6 {
        struct s7 {
            struct {} c;
        } b;
    } a;
};
