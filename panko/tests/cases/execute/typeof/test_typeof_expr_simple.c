// [[return: 42]]

int printf(char const*, ...);
int puts(char const*);

int main() {
    unsigned long x = 42;
    typeof(x) y = 27;
    // [[print: it works]]
    puts(_Generic(x + y, typeof(x + y): "it works"));
    typeof(x + y) const* const p = &x;
    typeof(x) xs[10] = {};
    // [[print: 10]]
    printf("%zu\n", _Lengthof xs);
    // [[print: 10]]
    printf("%zu\n", _Lengthof(typeof(xs)));
    // [[print: 40]]
    printf("%zu\n", sizeof(typeof(x)[5]));
    return *p;
}
