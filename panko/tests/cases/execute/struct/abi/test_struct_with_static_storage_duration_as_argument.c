// [[known-bug]]

int printf(char const*, ...);

struct Small {
    int x, y;
};

struct Medium {
    int x, y, z;
};

struct Large {
    long x, y, z;
};

void print_structs(struct Small small, struct Medium medium, struct Large large) {
    printf("%d %d\n", small.x, small.y);
    printf("%d %d %d\n", medium.x, medium.y, medium.z);
    printf("%ld %ld %ld\n", large.x, large.y, large.z);
}

struct Small small = {42, 27};
struct Medium medium = {1, 2, 3};
struct Large large = {4l, 5l, 6l};

int main() {
    // [[print: 42 27]]
    // [[print: 1 2 3]]
    // [[print: 4 5 6]]
    print_structs(small, medium, large);
}
