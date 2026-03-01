// [[known-bug]]

int printf(char const*, ...);

struct Struct {
    int x;
};

int main() {
    struct Struct value = {42};
    struct Struct other = {27};

    // [[print: 42]]
    printf("%d\n", (true ? value : other).x);
    // [[print: 27]]
    printf("%d\n", (false ? value : other).x);
}
