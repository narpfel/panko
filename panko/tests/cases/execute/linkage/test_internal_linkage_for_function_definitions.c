int puts(char const*);

static void f() {
    puts("in f");
}

int main() {
    // [[print: in f]]
    f();
}
