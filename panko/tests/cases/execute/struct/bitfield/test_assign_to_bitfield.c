int printf(char const*, ...);

struct T {
    unsigned int x : 20;
    long y : 20;
};

int main() {
    union {
        struct T t;
        long x;
    } u = {};
    u.t.x = 0x55555;
    u.t.y = 0xbbbbb;
    // [[print: 0x000000bbbbb55555]]
    printf("0x%016lx\n", u.x);

    unsigned assign_to_x = u.t.x = 0xabcde;
    // [[print: 0x000000bbbbbabcde 0x000abcde 0x000abcde]]
    printf("0x%016lx 0x%08x 0x%08x\n", u.x, u.t.x, assign_to_x);

    long assign_to_y = u.t.y = 0xcba98;
    // [[print: 0x000000cba98abcde 0xfffcba98 0xfffcba98]]
    printf("0x%016lx 0x%08x 0x%08x\n", u.x, (int)u.t.y, (int)assign_to_y);
}
