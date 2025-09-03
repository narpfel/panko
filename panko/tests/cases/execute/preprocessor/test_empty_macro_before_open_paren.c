// [[known-bug]]

int puts(char const*);

void a() {
    puts("a");
}

void b() {
    puts("b");
}

void c() {
    puts("c");
}

void d() {
    puts("d");
}

#define EMPTY
#define a() b EMPTY ()
#define b() c EMPTY ()
#define c() d EMPTY ()
#define id(x) x

int main() {
    // [[print: b]]
    a();
    // [[print: c]]
    id(a());
}
