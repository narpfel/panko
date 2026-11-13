struct T {
    int x;
};

// [[compile-error: invalid implicit conversion from `struct T~\d+ complete` to `bool`]]
static_assert((struct T){ .x = 42 });

int main() {}
