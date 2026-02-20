// [[known-bug]]
// [[return: 28]]

struct T { int x; } t = {42};
struct U { int x; struct T t; } a = {123, {42}};

int main() {
    t.x = 27;
    struct T* p = &a.t;
    // check that member accesses can be nested
    p->x = p->x + 1;
    return a.t.x;
}
