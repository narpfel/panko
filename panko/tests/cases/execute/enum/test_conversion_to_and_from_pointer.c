// [[known-bug: pointer <=> enum conversions not implemented yet]]
// [[return: 1]]

enum E { A, B, C, D, E };

int main() {
    // can roundtrip enum values through pointers (just like `int`)
    enum E value = E;
    int* ptr = (int*)value;
    enum E roundtripped = (enum E)ptr;
    return value == roundtripped;
}
