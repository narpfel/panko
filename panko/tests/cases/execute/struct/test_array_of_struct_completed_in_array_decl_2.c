// [[known-bug]]

int printf(char const*, ...);

void f() {
    typeof(typeof(typeof(struct X*)())*) funcs[sizeof(struct X { char c; int x; })] = {};
    // [[print: 8]]
    printf("%zu\n", _Lengthof funcs);
    // [[print: 8]]
    printf("%zu\n", sizeof *funcs[0]());
}

void g() {
    typeof(typeof(typeof(typeof(struct X*)())*)[sizeof(struct X { int x; })]) funcs = {};
    // [[print: 4]]
    printf("%zu\n", _Lengthof funcs);
    // [[print: 4]]
    printf("%zu\n", sizeof *funcs[0]());
}

int main() {
    f();
    g();
}
