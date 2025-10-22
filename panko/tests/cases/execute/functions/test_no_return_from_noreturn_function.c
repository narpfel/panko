// [[return: 42]]

_Noreturn void exit(int);

_Noreturn void f() {
    exit(42);
    return;
}

int main() {
    f();
}
