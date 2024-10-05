// [[return: 42]]

int f(int x) {
    return x + 2;
}

int main() {
    int x = 40;
    int (*p)(int) = &f;
    return (****p)(x);
}
