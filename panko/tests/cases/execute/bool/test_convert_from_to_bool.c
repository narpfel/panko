// [[return: 2]]

int printf(char const*, ...);

int main() {
    int x = 42;
    int* p = &x;
    bool from_ptr = p;
    // [[print: 1]]
    printf("%d\n", from_ptr);

    bool from_zero = 0ull;
    // [[print: 0]]
    printf("%d\n", from_zero);

    bool b = 42;
    return b + 1ul;
}
