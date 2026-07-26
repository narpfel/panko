int main() {
    int const x = 42;
    // [[compile-error: implicit conversion from `ptr<int const>` to `ptr<int>` drops qualifiers]]
    int* p = &x;
}
