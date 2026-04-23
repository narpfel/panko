// [[known-bug]]

int printf(char const*, ...);

struct T {
    int x;
    struct Inner {
        int y;
    } inner;
};

int main() {
    struct T t;
    t.x = 42;
    struct Inner inner = {.y = 27};
    t.inner = inner;
    // [[print: 42 27]]
    printf("%d %d\n", t.x, t.inner.y);

    struct T t2 = {.x = 123, .inner.y = 456};
    // [[print: 123 456]]
    printf("%d %d\n", t2.x, t2.inner.y);
}
