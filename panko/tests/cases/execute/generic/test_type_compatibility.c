// [[known-bug: `_Generic` uses type equality, not compatibility; and compatibility of enums to their underlying types not implemented]]

int printf(char const*, ...);

enum E1 { A, B, C };
enum E2 { D, E, F };

int f(enum E1 e) { return e; }
enum E1 g();

int main() {
    enum E1 x = A;
    enum E2 y = D;

    _Generic(x, enum E1: 0);
    _Generic(x, unsigned: 0);
    _Generic(y, enum E2: 0);
    _Generic(y, unsigned: 0);

    // even works for nested types
    // [[print: 42]]
    printf("%d\n", _Generic(f, typeof(int(unsigned))*: 42));
    // [[print: 27]]
    printf("%d\n", _Generic(g, typeof(unsigned())*: 27));

    // two `enum` types are not compatible even if their underlying type is
    // compatible/the same
    return _Generic(x, enum E1: 0, enum E2: 1) + _Generic(y, enum E1: 1, enum E2: 0);
}
