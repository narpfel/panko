int f() {
    int const a = 42;
    a = 27;
    return a;
}

int g() {
    int const* b;
    int const* c = nullptr;
    b = c;
    return 0;
}

int h() {
    int* const d;
    int* e = nullptr;
    d = e;
    return 0;
}

int i() {
    int* f;
    int* const g = nullptr;
    f = g;
    return 0;
}
