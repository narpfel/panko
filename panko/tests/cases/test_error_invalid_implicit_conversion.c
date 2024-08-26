int a() {
    int* p = 0;
    return p;
}

int b() {
    char const* p = 0;
    return p;
}

int* c() {
    int x = 42;
    return x;
}

char const* d() {
    int x = 42;
    return x;
}
