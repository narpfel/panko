void f() {}

int main() {
    // TODO: due to the desugaring for `++`, the error message is not very good
    ++f();
}
