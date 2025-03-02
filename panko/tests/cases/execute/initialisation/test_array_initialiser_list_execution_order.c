int printf(char const*, ...);

int f(int x) {
    char s[] = {37, 100, 10, 0};
    printf(s, x);
    return x;
}

int main() {
    char s[] = {37, 100, 10, 0};

    // I don’t understand in which order the initialiser list shall be executed. The standard says
    // “The initialization shall occur in initializer list order, each initializer provided for a
    // particular subobject overriding any previously listed initializer for the same subobject”,
    // which suggests that this should print `6, 2, 4, 5`, but gcc, clang and CompCert execute it
    // in declaration order (subobject offset order), so we copy that behaviour.
    // See https://open-std.org/JTC1/SC22/WG14/www/docs/n3220.pdf page 138 6.7.11
    // paragraphs 20 and 24.
    //
    // [0][0]
    // [[print: 4]]
    // [0][1]
    // [[print: 5]]
    // [1][0]
    // [[print: 6]]
    // [1][1]
    // [[print: 2]]
    int xs[2][2] = {
        [1] = {
            f(1),
            [1] = f(2),
            [0] = f(3),
        },
        [0][0] = f(4),
        f(5),
        f(6),
    };

    // [[print: 4]]
    printf(s, xs[0][0]);
    // [[print: 5]]
    printf(s, xs[0][1]);
    // [[print: 6]]
    printf(s, xs[1][0]);
    // [[print: 2]]
    printf(s, xs[1][1]);

    // [0]
    // [[print: 20]]
    // [1]
    // [[print: 10]]
    int ys[2] = {
        [1] = f(10),
        [0] = f(20),
    };

    // [[print: 20]]
    printf(s, ys[0]);
    // [[print: 10]]
    printf(s, ys[1]);
}
