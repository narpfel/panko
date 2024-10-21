int main() {
    int a = 42;
    int b = 27;
    int const* const p = &b;
    _Generic(main, int: a, default: 27, int: a, int const*: b, default: 42) = 123;
    _Generic(&a, int: 42, long: 27);
    _Generic(42, int(): 1, void: 2, default: 3);
    return a;
}
