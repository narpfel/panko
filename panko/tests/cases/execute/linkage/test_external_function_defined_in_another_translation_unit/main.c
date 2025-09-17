int printf(char const*, ...);

int f(int);

extern int x;

int main() {
    // [[print: 15]]
    printf("%d\n", f(10));
    x = 20;
    // [[print: 143]]
    printf("%d\n", f(123));
}
