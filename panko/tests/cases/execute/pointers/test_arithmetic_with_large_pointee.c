int printf(char const*, ...);
void* malloc(typeof(sizeof 0));

struct Large {
    char ten_gb[1024][1024][1024][10];
};

int main() {
    // [[print: 10737418240]]
    printf("%zu\n", sizeof(struct Large));
    // [[print: 1]]
    printf("%d\n", sizeof(struct Large) == sizeof(char) * 10 * 1024 * 1024 * 1024);
    struct Large* p = malloc(20 * sizeof(struct Large));
    struct Large* third = p + 3;
    typeof(third - p) difference = third - p;
    // [[print: 3]]
    printf("%td\n", difference);
    typeof(difference) byte_difference = (char*)third - (char*)p;
    // [[print: 1]]
    printf("%d\n", byte_difference == 3 * sizeof(struct Large));
}
