int printf(char const*, ...);

int main() {
    int x = 42;
    int y = 27;
    typeof(nullptr) runtime_nullptr = nullptr;

    // [[print: 42]]
    printf("%d\n", *(0 ? runtime_nullptr : &x));
    // [[print: 27]]
    printf("%d\n", *(1 ? &y : runtime_nullptr));
    // [[print: 1]]
    printf("%d\n", (1 ? runtime_nullptr : runtime_nullptr) == nullptr);
    // [[print: 1]]
    printf("%d\n", _Generic(0 ? runtime_nullptr : runtime_nullptr, typeof(nullptr): 1));
}
