int puts(char const*);

struct T {
    int x;
    int y;
};

static_assert(sizeof(struct T) == 2 * sizeof(int));

_Static_assert(sizeof(int) == sizeof(unsigned), "also accepts `_Static_assert`");

int main() {
    static_assert((struct T){ .x = 42 }.y == 0, "");

    // [[print: `static_assert` does not do anything at runtime]]
    puts("`static_assert` does not do anything at runtime");
}
