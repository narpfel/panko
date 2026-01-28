// [[known-bug]]
// [[nosnapshot]]

int memcmp(void const*, void const*, typeof(sizeof 0));
int printf(char const*, ...);

struct T {
    int x;
    int y;
};

struct U {
    int x;
    struct T t;
    int y;
    int z;
};

int main() {
    struct U u = {1, {2, 3}, 0, 4};
    int ints[] = {1, 2, 3, 0, 4, 5};

    // [[print: 0]]
    printf("%d\n", memcmp(&u, ints, sizeof u));

    struct T ts[] = {{1, 2}, {3}, {4, 5}};
    // [[print: 0]]
    printf("%d\n", memcmp(ts, ints, sizeof ts));
}
