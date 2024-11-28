// [[return: 42]]

int main() {
    int xs[123][123];
    xs[122][122] = 42;
    return xs[122][122];
}
