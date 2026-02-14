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

int main() {
    struct Inner inner = {1, 2, 3};
    // [[compile-error: invalid implicit conversion from `struct Inner~\d+ complete` to `int`]]
    struct Outer outer = {42, {inner}, 27};
}
