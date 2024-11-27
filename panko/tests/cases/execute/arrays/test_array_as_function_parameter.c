// [[return: 10]]

int f(int xs[2]) {
    return xs[0] + xs[1];
}

int main() {
    int xs[2];
    xs[0] = 4;
    xs[1] = 6;
    return f(xs);
}
