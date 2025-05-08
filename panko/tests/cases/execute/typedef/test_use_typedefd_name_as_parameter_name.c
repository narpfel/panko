// [[return: 42]]

int main() {
    typedef int a;
    int f(int a);
    return f(42);
}

int f(int a) {
    return a;
}
