// [[return: 1]]

int main() {
    int xs[42] = {};
    int (*p)[] = &xs;
    int (*q)[42] = &xs;
    return p == q;
}
