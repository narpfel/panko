// [[return: 60]]
// [[arg: %zu\n]]

int printf(char const*, ...);

int main(int, char** argv) {
    // [[print: 400000]]
    printf(argv[1], sizeof(int[200][500]));
    int xs[5][3];
    return sizeof xs;
}
