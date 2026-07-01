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

    // [[print: 1 0 1 0]]
    printf("%d %d %d %d\n", p != 0, 0ll == p, (void*)0u != p, p == (void*)0ul);
    // [[print: 0 1 0 1]]
    printf("%d %d %d %d\n", null != 0, 0ll == null, (void*)0 != null, null == (void*)0ul);
}
