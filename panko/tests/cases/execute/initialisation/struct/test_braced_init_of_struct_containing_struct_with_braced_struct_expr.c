// [[known-bug]]

int memcmp(void const*, void const*, typeof(sizeof 0));

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
    struct Outer outer = {42, {inner}, 27};
    int expected[] = {42, 1, 2, 3, 27};
    return memcmp(&outer, expected, sizeof outer);
}
