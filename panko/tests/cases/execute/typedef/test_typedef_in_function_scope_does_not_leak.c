// [[return: 42]]

int printf(char const*, ...);

void f() {
    typedef char const name;
    name x = 'a';
    // [[print: a]]
    printf("%c\n", x);
}

int main() {
    f();
    int name = 42;
    return name;
}
