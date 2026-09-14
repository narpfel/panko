// [[return: 2]]

int printf(char const*, ...);

enum E { A, B, C, D };

int b = B;

int main() {
    // [[print: 0 + 1 + 3 == 4]]
    printf("%d + %d + %d == %d\n", A, b, D, A + b + D);

    return C;
}
