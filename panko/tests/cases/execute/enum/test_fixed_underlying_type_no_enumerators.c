// [[return: 1]]

int printf(char const*, ...);

enum Bool: bool;
enum UnsignedLong: unsigned long;

int main() {
    enum UnsignedLong value = 123;

    // [[print: 1 8 123]]
    printf("%zu %zu %lu\n", sizeof(enum Bool), sizeof value, value);

    return (enum Bool)42;
}
