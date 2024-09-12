int f() {
    int const a = 42;
    a = 27;
    return a;
}

int g() {
    int const* b;
    // FIXME: cannot initialise pointers yet
    int const* c;
    b = c;
    return 0;
}

int h() {
    int* const d;
    // FIXME: cannot initialise pointers yet
    int* e;
    d = e;
    return 0;
}

int i() {
    int* f;
    int* const g;
    f = g;
    return 0;
}
