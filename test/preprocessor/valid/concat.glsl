#define concat(x, y) x##y

concat(hello, world)
concat(hello, 42)
concat(hello hello, world world)
concat(13, 12)
concat(13.0, 12e10)
concat(,)
concat(,world)
concat(hello,)
concat(+, +)