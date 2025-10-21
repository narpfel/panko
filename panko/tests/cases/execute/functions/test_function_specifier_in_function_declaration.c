// [[return: 27]]

int printf(char const*, ...);

inline int f();
_Noreturn void g();
_Noreturn void exit(int);

int main() {
    // [[print: 42]]
    printf("%d\n", f());
    g();
}

int f() {
    return 42;
}

void g() {
    exit(27);
}
