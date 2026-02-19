// [[return: 28]]

int main() {
    struct T { int x; } t = {42};
    t.x = 27;
    struct U { int x; struct T t; } a = {123, t};
    struct U* p = &a;
    // check that member accesses can be nested
    p->t.x = p->t.x + 1;
    return a.t.x;
}
