// [[known-bug: compatibility of enums with their underlying types not implemented yet]]

enum E { A, B, C };

int main() {
    enum E x = A;
    // [[compile-error: duplicate association for type `unsigned int` in _Generic selection]]
    _Generic(x, unsigned: 0, enum E: 1);
}
