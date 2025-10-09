int main() {
    int xs[2] = {};
    return _Generic(xs, int*: 0, int[2]: 1);
}
