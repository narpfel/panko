// [[known-bug: check for nullptr-constant-ness of enum values not implemented yet]]
// [[return: 1]]

enum E { A, B, C };

int main() {
    struct T* p1 = (enum E)A;
    struct T* p2 = (enum E){};
    return p1 == p2;
}
