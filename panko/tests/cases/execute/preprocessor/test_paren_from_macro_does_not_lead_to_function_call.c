// [[known-bug]]

int printf(char const*, ...);

int a() {
    return 27;
}

#define a() called

#define lparen() (
#define rparen() )

#define id(x) x

#define nested a lparen() rparen()

int main() {
    int called = 42;
    // [[print: 42]]
    printf("%d\n", id(a lparen() rparen()));
    // [[print: 27]]
    printf("%d\n", a lparen() rparen());
    // [[print: 27]]
    printf("%d\n", nested);
}
