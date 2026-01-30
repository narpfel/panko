int printf(char const*, ...);

struct String {
    char const* ptr;
};

struct T {
    struct String string;
    int x;
};

int main() {
    struct T t = {"hello world", 42};

    // [[print: hello world]]
    printf("%s\n", *(char const**)&t);
}
