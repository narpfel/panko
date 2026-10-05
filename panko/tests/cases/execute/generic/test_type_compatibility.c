// [[known-bug: `_Generic` uses type equality, not compatibility; and compatibility of enums to their underlying types not implemented]]

enum E1 { A, B, C };
enum E2 { D, E, F };

int main() {
    enum E1 x = A;
    enum E2 y = D;

    _Generic(x, enum E1: 0);
    _Generic(x, unsigned: 0);
    _Generic(y, enum E2: 0);
    _Generic(y, unsigned: 0);

    // two `enum` types are not compatible even if their underlying type is
    // compatible/the same
    return _Generic(x, enum E1: 0, enum E2: 1) + _Generic(y, enum E1: 1, enum E2: 0);
}
