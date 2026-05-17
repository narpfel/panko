int printf(char const*, ...);

int print(int x) {
    printf("%d ", x);
    return x;
}

void f() {
    int* p1 = nullptr;
    int* p2 = nullptr;
    int* p3 = nullptr;

    // [[print: 10 5 15]]
    printf("%d\n", (p3 = &(int){(p1 = &(int){print(10)}, *p1) + (p2 = &(int){print(5)}, *p2)}, *p3));
    //
    // [[print: 10 5 15]]
    printf("%d %d %d\n", *p1, *p2, *p3);

    int a = 10, b = 20, c = 30;
    // [[print: 10 20 30]]
    printf("%d %d %d\n", a, b, c);

    // [[print: 10 5 15]]
    printf("%d %d %d\n", *p1, *p2, *p3);
}

void g() {
    int* p1 = nullptr;
    int* p2 = nullptr;
    int* p3 = nullptr;
    int x = 1, y = 2, z = 3;

    // [[print: 10 5 15]]
    printf("%d\n", (p3 = &(int){(p1 = &(int){print(10)}, *p1) + (p2 = &(int){print(5)}, *p2)}, *p3));
    // [[print: 1 2 3]]
    printf("%d %d %d\n", x, y, z);
    x = 4;
    y = 5;
    z = 6;
    // [[print: 4 5 6]]
    printf("%d %d %d\n", x, y, z);
    // [[print: 10 5 15]]
    printf("%d %d %d\n", *p1, *p2, *p3);
}

int main() {
    f();
    g();
}
