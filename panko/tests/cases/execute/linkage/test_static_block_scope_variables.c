int f() {
    static int x = 42;
    return x++;
}

int g() {
    static int x = 27;
    return x++;
}

int main() {
    int printf(char const*, ...);

    // [[print: 42 27]]
    printf("%d %d\n", f(), g());
    // [[print: 43 28]]
    printf("%d %d\n", f(), g());
    // [[print: 44 29]]
    printf("%d %d\n", f(), g());
}
