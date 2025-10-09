int main() {
    int xs[2] = {};
    // okay
    _Generic(xs, int*: 0, int[1]: 1, int[2]: 2, int[3]: 3);
    _Generic(xs, int[2]: 1, int[2]: 2);
}
