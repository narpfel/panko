// [[known-bug]]

int printf(char const*, ...);

int f(int x) {
    return x * 3;
}

int main() {
    int g = 3;

#define f(a) a*g
#define g(a) f(a)
#define id(x) x

    // this expands to either `2 * f(9)` or `2 * 9 * g`
    // [[print: 54]]
    printf("%d\n", f(2)(9));
    // [[print: 54]]
    printf("%d\n", id(f(2)(9)));
}
