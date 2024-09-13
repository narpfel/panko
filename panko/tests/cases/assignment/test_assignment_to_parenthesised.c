int main() {
    int const x, y = 27;
    int mutable;
    (mutable = x = y) = 42;
    (((mutable))) = 123;
    (x) = 42;
    return x;
}
