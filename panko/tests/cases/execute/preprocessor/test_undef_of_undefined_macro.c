int printf(char const*, ...);

// `#undef`ing undefined macros is ignored
#undef x
#undef y

int main() {
    int x = 42;
    int y = 27;
#define x y
    // [[print: 27, 27]]
    printf("%d, %d\n", x, y);
#undef x
// test that double-`#undef`ing is accepted
#undef x
#undef y
    // [[print: 42, 27]]
    printf("%d, %d\n", x, y);
}
