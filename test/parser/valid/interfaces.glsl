uniform Block1 {
    vec3 n;
    vec3 p;
} blocky_me;

in Block2 {
    int foo, bar;
} blocky_you;

layout(location = 5) uniform Block3 {
    float x, y, z;
    double w;
} blocky_them;