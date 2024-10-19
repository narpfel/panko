int main() {
    long const* p = (long*)0;
    *p = 42;
    *(p + 1) = 42;
    p[0] = 42;
    p[1] = 27;
    (p[42]) = 0;
}
