extern int printf(char const*, ...);
int puts(char const*);

static void f() {
    puts("in f");
}

extern void g(int);
static void h(int x);
static void i();

int main() {
    // [[print: in f]]
    f();
    // [[print: in g: 5]]
    g(4);
    // [[print: in h: 10]]
    h(9);
    // [[print: in i]]
    i();
}

void g(int x) {
    printf("in g: %d\n", x + 1);
}

extern void h(int x) {
    printf("in h: %d\n", x + 1);
}

void i() {
    puts("in i");
}
