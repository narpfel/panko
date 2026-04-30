#include <stddef.h>

struct T {
    int x;
};

union U {
    int x;
};

int main() {
    struct T t = {};
    union U u = {};

    t.does_not_exist = 42;
    u.wat = "wat";

    return
        offsetof(struct T, no_such_struct_member)
        + offsetof(union U, no_such_union_member);
}
