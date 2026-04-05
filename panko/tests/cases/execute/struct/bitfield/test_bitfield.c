// [[return: 8]]

int printf(char const*, ...);

struct T {
    unsigned int x : 20;
    long y : 20;
    unsigned int z : 20;
};

int main() {
    union {
        struct T t;
        long x;
    } u = {};
    u.t.x = 0x55555;
    u.t.y = 0xbbbbb;
    u.t.z = 0xccccc;
    // [[print: 1 1 1]]
    printf("%d %d %d\n", u.t.x > 0, u.t.y < 0, u.t.z > 0);
    // [[print: 0x0cccccbbbbb55555]]
    printf("0x%016lx\n", u.x);

    unsigned compound_assignment_result = u.t.x += 11;
    // [[print: 0x0cccccbbbbb55560 55560 55560]]
    printf("0x%016lx %x %x\n", u.x, u.t.x, compound_assignment_result);

    int preincrement_result = (int)++u.t.y;
    int uty = (int)u.t.y;
    // [[print: 0x0cccccbbbbc55560 fffbbbbc fffbbbbc]]
    printf("0x%016lx %x %x\n", u.x, uty, preincrement_result);

    return sizeof(struct T);
}
