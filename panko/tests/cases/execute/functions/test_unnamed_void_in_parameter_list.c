int printf(char const*, ...);

typedef void T;
void const* p;
void v(typeof_unqual(*p));

int f(T) {
    return 123;
}

int g(typeof(v())) {
    return 42;
}

int h(typeof_unqual(*p)) {
    return 27;
}

int main() {
    // [[print: 123]]
    printf("%d\n", f());
    // [[print: 42]]
    printf("%d\n", g());
    // [[print: 27]]
    printf("%d\n", h());
}
