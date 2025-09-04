// [[known-bug]]
// [[return: 1]]

int main() {
    int x = 42;
    int* p = &x;
    int const* p2 = &x;
    return p == p2;
}
