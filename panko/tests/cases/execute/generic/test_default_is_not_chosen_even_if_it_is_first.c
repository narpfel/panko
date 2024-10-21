// [[return: 5]]

int main() {
    return _Generic(123, default: 1, long: 2, unsigned int: 3, char: 4, int: 5);
}
