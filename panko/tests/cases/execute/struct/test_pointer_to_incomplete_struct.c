// [[return: 8]]

int printf(char const*, ...);

typedef typeof(sizeof 0) size_t;

struct T;

size_t f(struct T* p) {
    return sizeof p;
}

int main() {
    struct T* p = (struct T*)nullptr;
    struct T* q = nullptr;
    // [[print: 1]]
    printf("%d\n", p == q);
    return (int)f(p);
}
