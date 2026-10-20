// [[known-bug: enumerator type in enumerator list is always `int`, not the correct type]]
// [[return: 1]]

enum E: char {
    A = 42,
    B = sizeof(A),
};

int main() {
    return B;
}
