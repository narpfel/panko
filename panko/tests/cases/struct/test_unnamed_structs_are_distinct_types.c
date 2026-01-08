struct { int x; } typedef T;

T* p1;
T* p2;

void okay() {
    p1 == p2;
    T* p3 = nullptr;
    p1 == p3;
}

struct { int x; }* p4;

void should_error() {
    p1 == p4;
    struct { int x; }* p5;
    p1 == p5;
    p4 == p5;
}
