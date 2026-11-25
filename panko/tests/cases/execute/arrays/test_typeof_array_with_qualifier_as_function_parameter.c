// [[known-bug: qualifiers should be propagated to the element type]]

bool f(typeof(int[]) const xs) {
    int const* p = nullptr;
    xs = p;
    return xs;
}

int main() {
    int x = 42;
    return f(&x);
}
