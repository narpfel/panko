// [[known-bug]]

int printf(char const*, ...);

struct T* ptr;

struct T {
    int xs[10];
};

int main() {
    // this is distinct from the global `struct T`
    struct T {
        int x;
    };
    // [[print: 40]]
    printf("%zu\n", sizeof *ptr);
}
