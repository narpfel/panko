int printf(char const*, ...);

struct T {
    int x;
    int y;
    int z;
};

int main() {
    static struct T t = {10, 20, 30};
    static int* p = &t.y + -1;
    int* runtime_p = &t.y + -1;
    static int* p2 = &t.y - -1;
    int* runtime_p2 = &t.y - -1;
    // [[print: 10 30]]
    printf("%d %d\n", *p, *p2);
    // [[print: 10 30]]
    printf("%d %d\n", *runtime_p, *runtime_p2);
    // [[print: 1 1]]
    printf("%d %d\n", p == runtime_p, p2 == runtime_p2);
}
