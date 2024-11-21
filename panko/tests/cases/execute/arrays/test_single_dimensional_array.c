// [[return: 5]]

int main() {
    int xs[42];
    xs[0] = 1;
    xs[1] = 2;
    xs[41] = 3;
    return xs[1] + xs[41];
}
