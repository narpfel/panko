// [[known-bug: copying values of enum type and implicit conversions to integral not implemented]]

enum E { A, B, C } x;

enum E f() {
    return x;
}

int main() {
    return f();
}
