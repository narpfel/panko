int printf(char const*, ...);

int main() {
    int x = 42;
    int* p = &x;
    // [[print: 0]]
    printf("%d\n", p == nullptr);
    // [[print: 1]]
    printf("%d\n", p != nullptr);
    // [[print: 1]]
    printf("%d\n", nullptr == nullptr);
    // [[print: 0]]
    printf("%d\n", nullptr != nullptr);
    long (*null)[10] = nullptr;
    // [[print: 1]]
    printf("%d\n", null == nullptr);
    // [[print: 0]]
    printf("%d\n", null != nullptr);
}
