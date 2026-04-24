int printf(char const*, ...);

struct WithAnonymousStruct {
    int x;
    struct {
        int y;
        long l;
    };
};

struct PropagatesQualifiers {
    int x;
    struct {
        int y;
        long l;
    } const;
};

struct WithStructName {
    int x;
    struct Name {
        int y;
        long l;
    };
};

struct WithMemberName {
    int x;
    struct {
        int y;
        long l;
    } s;
};

int main() {
    struct WithAnonymousStruct value = {.x = 42, .y = 27, .l = 123};
    // [[print: 42 27 123]]
    printf("%d %d %ld\n", value.x, value.y, value.l);

    // [[print: 24 8]]
    printf("%zu %zu\n", sizeof(struct WithAnonymousStruct), _Alignof(struct WithAnonymousStruct));

    // [[print: 4 4]]
    printf("%zu %zu\n", sizeof(struct WithStructName), _Alignof(struct WithStructName));

    // [[print: 24 8]]
    printf("%zu %zu\n", sizeof(struct WithMemberName), _Alignof(struct WithMemberName));
}
