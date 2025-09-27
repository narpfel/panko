// [[return: 1]]

int printf(char const*, ...);

static int x;
static int y = 32;

int main() {
    y += 10;
    // [[print: 42]]
    printf("%d\n", y);
    ++x;
    return x;
}
