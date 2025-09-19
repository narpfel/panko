extern int printf(char const*, ...);

extern void f(int x) {
    printf("in f: %d\n", x + 1);
}

int main() {
    // [[print: in f: 5]]
    f(4);
}
