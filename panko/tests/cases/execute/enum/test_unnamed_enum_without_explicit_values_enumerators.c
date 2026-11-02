// [[return: 2]]

int printf(char const*, ...);

enum { A, B, C, D };

int b = B;

enum: long { E, F };

int main() {
    // [[print: 0 + 1 + 3 == 4]]
    printf("%d + %d + %d == %d\n", A, b, D, A + b + D);

    // [[print: 0 1]]
    printf("%ld %ld\n", E, F);

    return C;
}
