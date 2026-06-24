int printf(char const*, ...);

struct T {
    int x;
    int y;
};

int main() {
    static struct T* p = &(static struct T){1, 2};

    // [[print: 1 2]]
    printf("%d %d\n", p->x, p->y);
}
