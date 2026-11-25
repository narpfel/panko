// [[known-bug: it’s unclear whether `typeof(array)` as function parameter should allow qualifiers in the brackets]]
// [[return: 42]]

int f(typeof(int[const]) xs) {
    xs[0] = 41;
    return _Generic(xs, int*: 1, int const*: 0);
}

int main() {
    int x = 27;
    int value = f(&x);
    return value + x;
}
