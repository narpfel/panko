// [[return: 69]]

int putchar(int);

int* f(int* p) {
    putchar(65);
    putchar(10);
    return p;
}

int main() {
    int x = 42;
    // [[print: A]]
    *f(&x) += 27l;
    return x;
}
