int printf(char const*, ...);

int f();
void add_f(int);

int g();
void add_g(int);

int x = 42;

void print_variables() {
    printf("%d\n", x);
    printf("%d\n", f());
    printf("%d\n", g());
}

int main() {
    // [[print: 42]]
    // [[print: 27]]
    // [[print: 10]]
    print_variables();
    x = 123;
    // [[print: 123]]
    // [[print: 27]]
    // [[print: 10]]
    print_variables();
    add_f(10);
    // [[print: 123]]
    // [[print: 37]]
    // [[print: 10]]
    print_variables();
    add_g(5);
    // [[print: 123]]
    // [[print: 37]]
    // [[print: 15]]
    print_variables();
}
