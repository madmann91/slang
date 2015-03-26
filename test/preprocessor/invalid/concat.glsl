#define concat(x, y) x##y

concat(1.0, 1.0)
concat(42, world)
concat(+, 1)
concat(+, -)

#define bad_concat1(x, y) ##y
#define bad_concat2(x, y) x##
#define bad_concat3(x, y) x####y