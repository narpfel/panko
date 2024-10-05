// [[return: 10]]

int f(int, int, int, int, int, int, int, int, int, int, int);

int id(int n) {
    return n;
}

int main() {
    return f(1, 2, 3, 4, 5, 6, 7, 8, 9, id(10), 11);
}

int f(int a1, int a2, int a3, int a4, int a5, int a6, int a7, int a8, int a9, int a10, int a11) {
    return a10;
}
