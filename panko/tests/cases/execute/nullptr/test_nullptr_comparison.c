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
    // [[print: 1 0 0 1]]
    printf(
        "%d %d %d %d\n",
        0l == nullptr,
        -0l != nullptr,
        nullptr != (0xffff'ffff'ffff'ffff + 1),
        nullptr == (1 - 1)
    );

    typeof(nullptr) runtime_nullptr = nullptr;
    // [[print: 0 0]]
    printf("%d %d\n", p == runtime_nullptr, runtime_nullptr == p);
    // [[print: 1 1]]
    printf("%d %d\n", p != runtime_nullptr, runtime_nullptr != p);
    // [[print: 1]]
    printf("%d\n", runtime_nullptr == runtime_nullptr);
    // [[print: 0]]
    printf("%d\n", runtime_nullptr != runtime_nullptr);
    // [[print: 1]]
    printf("%d\n", null == runtime_nullptr);
    // [[print: 0]]
    printf("%d\n", null != runtime_nullptr);
}
