// [[known-bug]]

int printf(char const*, ...);

struct {
    int x;
    struct NoDeclarator {
        int y;
    };
};

int main() {
    struct NoDeclarator s = {.y = 2};
    // [[print: 2]]
    printf("%d\n", s.y);
}
