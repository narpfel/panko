// [[known-bug]]
// [[return: 5]]

int main() {
    struct T { int x; } t = {42};
    t.x = 27;
    struct U { int x; struct T t; } a = {123, t};
    struct U* p = &a;
    p->t.x = 5;
    return a.t.x;
}
