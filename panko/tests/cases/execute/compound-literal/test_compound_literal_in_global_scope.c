// [[return: 42]]

typeof((int){}) printf(char const*, ...);

int x = (int){42};
int y = (int){27};

int main() {
    // [[print: 27]]
    printf("%d\n", y);
    return x;
}
