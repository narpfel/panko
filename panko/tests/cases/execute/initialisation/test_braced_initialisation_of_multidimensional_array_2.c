// [[known-bug]]

int memcmp(void const*, void const*, typeof(sizeof 0));
int printf(char const*, ...);

int main() {
    int actual[][1][2] = {
        1, 2,
        {3},
        4,
    };
    int expected[] = {
        1, 2,
        3, 0,
        4, 0,
    };

    // [[print: 24 24]]
    printf("%zu %zu\n", sizeof actual, sizeof expected);

    return memcmp(actual, expected, sizeof actual);
}
