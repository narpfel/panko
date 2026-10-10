// [[known-bug: conversions to enums with fixed underlying type `bool` not implemented]]

int printf(char const*, ...);

enum Bool: bool { False, True };

int main() {
    enum Bool from_integral = 42;
    // [[print: from_integral: 1]]
    printf("from_integral: %d\n", from_integral);

    int x = 123;
    enum Bool from_non_null_pointer = &x;
    // [[print: from_non_null_pointer: 1]]
    printf("from_non_null_pointer: %d\n", from_non_null_pointer);

    int* null = nullptr;
    enum Bool from_null_pointer = null;
    // [[print: from_null_pointer: 0]]
    printf("from_null_pointer: %d\n", from_null_pointer);

    enum Bool from_nullptr = nullptr;
    // [[print: from_nullptr: 0]]
    printf("from_nullptr: %d\n", from_nullptr);

    {
        static enum Bool from_integral = 42;
        // [[print: constexpr from_integral: 1]]
        printf("constexpr from_integral: %d\n", from_integral);

        static int x = 123;
        static enum Bool from_non_null_pointer = &x;
        // [[print: constexpr from_non_null_pointer: 1]]
        printf("constexpr from_non_null_pointer: %d\n", from_non_null_pointer);

        static enum Bool from_null_pointer = (int*)nullptr;
        // [[print: constexpr from_null_pointer: 0]]
        printf("constexpr from_null_pointer: %d\n", from_null_pointer);

        static enum Bool from_nullptr = nullptr;
        // [[print: constexpr from_nullptr: 0]]
        printf("constexpr from_nullptr: %d\n", from_nullptr);
    }
}
