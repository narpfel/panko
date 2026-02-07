struct Inner {
    int x;
    int y;
    int z;
};

struct Outer {
    int x;
    struct Inner inner;
    int y;
};

struct Inner2 {
    char c;
};

struct Outer2 {
    int x;
    struct Inner2 inner;
    int y;
};

int main() {
    struct Inner inner_1 = {1, 2, 3};
    struct Outer outer_1 = {
        42,
        // [[compile-error: invalid implicit conversion from `struct Inner~\d+ complete` to `int`]]
        {inner_1, 5},
        27,
    };

    struct Inner2 inner_2 = {'a'};
    struct Outer2 outer_2 = {
        42,
        // [[compile-error: invalid implicit conversion from `struct Inner2~\d+ complete` to `char`]]
        {{inner_2}},
        27,
    };
}
