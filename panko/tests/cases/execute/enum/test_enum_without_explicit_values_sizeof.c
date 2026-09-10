// [[return: 16]]

enum E { A, B, C };

enum E x;

int main() {
    enum E2 { X, Y, Z } e2[sizeof x];
    return sizeof e2;
}
