// [[return: 69]]

#define FUNCTION(a, b, c) a - b + c

int f(int x, int y) {
    return 2 * x + y;
}

int main() {
    return FUNCTION(f(5, 20), (1 + (100, 2)), 42);
}
