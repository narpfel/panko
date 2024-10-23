void exit(int);

void f() {
    return;
    exit(42);
}

int main() {
    f();
}
