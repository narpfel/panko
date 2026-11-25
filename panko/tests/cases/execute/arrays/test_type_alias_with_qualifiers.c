// [[known-bug: qualifiers should be propagated to the element type]]

typedef int Array[10];

bool f(Array const xs) {
    int const* p = nullptr;
    xs = p;
    return xs;
}

int main() {
    Array const xs = {};
    return f(xs);
}
