void f() {}

int returns_int() {
    return 42;
}

int main() {
    // TODO: due to the desugaring for `++`, the error message is not very good
    f()++;
    returns_int()++;
}
