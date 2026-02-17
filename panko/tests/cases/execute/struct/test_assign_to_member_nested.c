// [[return: 5]]

int main() {
    struct T { int x; } t = {42};
    t.x = 27;
    struct { int x; struct T t; } a = {123, t};
    a.t.x = 5;
    return a.t.x;
}
