// [[known-bug]]
// [[return: 5]]

struct T { int x; } t = {42};
struct { int x; struct T t; } a = {123, {42}};

int main() {
    t.x = 27;
    a.t.x = 5;
    return a.t.x;
}
