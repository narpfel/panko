// [[known-bug]]
// [[return: 28]]

struct T { int x; } t = {42};
struct U { int x; struct T t; } a = {123, {42}};

int main() {
    t.x = 27;
    struct U* p = &a;
    // check that member accesses can be nested
    p->t.x = p->t.x + 1;
    return a.t.x;
}
