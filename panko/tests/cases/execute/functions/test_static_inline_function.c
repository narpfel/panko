// [[return: 42]]

static int x = 42;

static inline int f() {
    return x;
}

int main() {
    return f();
}
