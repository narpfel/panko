// [[return: 1]]

enum E: char {
    A = 42,
    B = sizeof(A),
};

int main() {
    return B;
}
