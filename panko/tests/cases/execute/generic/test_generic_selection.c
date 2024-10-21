// [[return: 42]]

int main() {
    return _Generic(123,
        char: 1,
        short: 2,
        int: 42,
        int const: 3,
        unsigned: 4,
        int*: 5,
        int**: 6,
        int const*: 7
    );
}
