int printf(char const*, ...);

enum E { A, B, C };

// conversion in constexpr
enum E x;
enum E y = 2;
int z = (enum E)B;

void implicit_to_int() {
    int a = y;
    int b = (enum E)B;
    // [[print: 2 1]]
    printf("%d %d\n", a, b);
}

void implicit_from_int() {
    enum E a = A;
    enum E b = 1;
    // [[print: 0 1]]
    printf("%d %d\n", a, b);
}

void explicit_to_int() {
    int a = (int)x;
    int b = (int)y;
    // [[print: 0 2]]
    printf("%d %d\n", a, b);
}

void explicit_from_int() {
    enum E a = (enum E)A;
    enum E b = (enum E)1;
    enum E c = (enum E)C;
    // [[print: 0 1 2]]
    printf("%d %d %d\n", a, b, c);
}

int main() {
    implicit_to_int();
    implicit_from_int();
    explicit_to_int();
    explicit_from_int();
    // [[print: 0 2 1]]
    printf("%d %d %d\n", x, y, z);
}
