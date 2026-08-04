int printf(char const*, ...);

int main() {
    void* p = &(struct T { int x; int y; }){42, 27};
    struct T* p2 = p;
    // [[print: 42 27]]
    printf("%d %d\n", p2->x, p2->y);
}
