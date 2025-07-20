int main() {
    int a = 42;
    int* p = &a;
    return _Generic(main, int: p * 2, default: 42);
}
