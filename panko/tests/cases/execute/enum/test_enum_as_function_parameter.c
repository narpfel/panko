// [[known-bug: copying values of enum type not implemented]]
// [[return: 4]]

enum E { A, B, C } x;

int f(enum E e) {
    return sizeof e;
}

int main() {
    return f(x);
}
