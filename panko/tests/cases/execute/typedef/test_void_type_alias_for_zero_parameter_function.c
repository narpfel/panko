int puts(char const*);

typedef void v;

v f(v);

v f(v) {
    puts("f");
}

v g(v(v));

v g(v v(v)) {
    v();
}

v print_g(v) {
    puts("g");
}

int main() {
    // [[print: f]]
    f();
    // [[print: g]]
    g(print_g);
}
