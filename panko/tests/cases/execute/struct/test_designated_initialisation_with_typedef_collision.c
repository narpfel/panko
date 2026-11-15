struct T { int x; };

typedef long x;

int main() {
    struct T t = { .x = 42 };
    // [[return: 42]]
    return t.x;
}
