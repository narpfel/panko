int printf(char const*, ...);

int main() {
    // [[print: 0]]
    printf("%d\n", (bool)nullptr);
    bool b = nullptr;
    return b;
}
