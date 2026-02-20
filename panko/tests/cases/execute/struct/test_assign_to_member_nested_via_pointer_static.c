// [[known-bug]]
// [[return: 5]]

struct T { int x; } t = {42};
struct U { int x; struct T t; } a = {123, {42}};

int main() {
    t.x = 27;
    struct U* p = &a;
    p->t.x = 5;
    return a.t.x;
}
