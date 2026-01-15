// [[known-bug]]
// [[return: 1]]

int main() {
    // `p1` and `p2` should both be `ptr<struct ∅~1>`
    // (same id as the `StructDecl` node)
    struct { int x; }* p1 = nullptr, *p2 = nullptr;
    return p1 == p2;
}
