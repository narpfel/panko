int printf(char const*, ...);

union U {
    int x[2];
    char c;
};

union U2 {
    char c;
    int x[3];
};

int main() {
    // [[print: 8 12]]
    printf("%zu %zu\n", sizeof(union U), sizeof(union U2));
}
