// [[arg: %d\n]]
// [[arg: %ld\n]]

int printf(char const*, ...);

int main(int, char** argv) {
    // braced initialiser with scalar
    int x = {{{3}}};
    // [[print: 3]]
    printf(argv[1], x);

    // braced initialiser with scalar and implicit conversion
    long y = {{{{{-42,}}}}};
    // [[print: -42]]
    printf(argv[2], y);

    // braced initialiser with 1-d array
    int xs[5] = {-1, -5 + -2, 3, 4, {5}};
    // [[print: -1]]
    printf(argv[1], xs[0]);
    // [[print: -7]]
    printf(argv[1], xs[1]);
    // [[print: 3]]
    printf(argv[1], xs[2]);
    // [[print: 4]]
    printf(argv[1], xs[3]);
    // [[print: 5]]
    printf(argv[1], xs[4]);

    // braced initialiser with 2-d array
    int ys[2][2] = {{1, 2}, {3, {4,},},};
    // [[print: 1]]
    printf(argv[1], ys[0][0]);
    // [[print: 2]]
    printf(argv[1], ys[0][1]);
    // [[print: 3]]
    printf(argv[1], ys[1][0]);
    // [[print: 4]]
    printf(argv[1], ys[1][1]);

    // braced initialiser with braces around scalar
    int zs[2] = {{1}, 2};
    // [[print: 1]]
    printf(argv[1], zs[0]);
    // [[print: 2]]
    printf(argv[1], zs[1]);

    int as[2][2] = {{{1}, 2}, {3, 4}};
    // [[print: 1]]
    printf(argv[1], as[0][0]);
    // [[print: 2]]
    printf(argv[1], as[0][1]);
    // [[print: 3]]
    printf(argv[1], as[1][0]);
    // [[print: 4]]
    printf(argv[1], as[1][1]);

    int bs[2][2] = {{1, {3}}, {4, 5}};
    // [[print: 1]]
    printf(argv[1], bs[0][0]);
    // [[print: 3]]
    printf(argv[1], bs[0][1]);
    // [[print: 4]]
    printf(argv[1], bs[1][0]);
    // [[print: 5]]
    printf(argv[1], bs[1][1]);
}
