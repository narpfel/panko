int main() {
    struct T;
    {
        struct T {
            int x;
        };
    }
    // [[compile-error: cannot declare variable `incomplete_here` with incomplete type `struct T~2`]]
    struct T incomplete_here = {.x = 42};
}
