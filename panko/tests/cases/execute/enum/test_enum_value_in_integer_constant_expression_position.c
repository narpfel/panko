// [[return: 42]]

int main() {
    enum E { A, B, C };
    struct T { int x:(enum E)C; };
    int xs[(enum E)B] = { [(enum E)A] = 42 };
    return xs[0];
}
