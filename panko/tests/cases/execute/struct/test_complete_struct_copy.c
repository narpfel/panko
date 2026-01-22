int printf(char const*, ...);

struct T {
    int xs[3];
    long x;
};

int main() {
    struct T t;
    typeof(int[3])* xs = (typeof(int[3])*)&t;
    (*xs)[0] = 42;
    (*xs)[1] = 27;
    (*xs)[2] = 123;

    struct T copy = t;
    xs = (typeof(int[3])*)&copy;
    // [[print: 42 27 123]]
    printf("%d %d %d\n", (*xs)[0], (*xs)[1], (*xs)[2]);
    (*xs)[1] = 5;

    struct T another_copy;
    another_copy = copy;
    xs = (typeof(int[3])*)&another_copy;
    // [[print: 42 5 123]]
    printf("%d %d %d\n", (*xs)[0], (*xs)[1], (*xs)[2]);
}
