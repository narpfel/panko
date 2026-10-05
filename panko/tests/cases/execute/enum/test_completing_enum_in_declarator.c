// [[return: 6]]

int main() {
    enum E xs[sizeof(enum E { A, B, C, D })] = {A, B, C, D};
    return xs[0] + xs[1] + xs[2] + xs[3];
}
