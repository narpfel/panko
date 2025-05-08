// [[return: 84]]

int main() {
    typedef int a;
    int f(int a);
    a b = 42;
    return b + f(b);
}

int f(int a) {
    return a;
}
