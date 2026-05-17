// [[return: 42]]

typedef int a;
typedef int b;

int f(int a, int (*b)) {
    return a + *b;
}

int main() {
    int x = 2;
    return f(40, &x);
}
