int printf(char const*, ...);

struct T {
    char c1;
    int x;
    char c2;
};

struct Nested {
    char c1;
    struct T t;
};

struct U {
    struct T t;
    struct U* p;
};

int main() {
    // [[print: 1]]
    printf("%d\n", alignof(struct T) == alignof(int));
    // [[print: 1]]
    printf("%d\n", alignof(struct Nested) == alignof(int));
    // [[print: 1]]
    printf("%d\n", alignof(struct U) == alignof(void*));
    // [[print: 12]]
    printf("%zu\n", sizeof(struct T));
    struct T value;
    // [[print: 12]]
    printf("%zu\n", sizeof value);
    // [[print: 16]]
    printf("%zu\n", sizeof(struct Nested));
    // [[print: 24]]
    printf("%zu\n", sizeof(struct U));
}
