int printf(char const*, ...);

void f() {
    struct X xs[sizeof(struct X { int x; })];
    // [[print: 4 16]]
    printf("%zu %zu\n", sizeof(struct X), sizeof xs);
}

void g() {
    struct X { char c; int x; } xs[sizeof(struct X)];
    // [[print: 8 64]]
    printf("%zu %zu\n", sizeof(struct X), sizeof xs);
}

int main() {
    f();
    g();
}
