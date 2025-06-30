// [[return: 16]]

int printf(char const*, ...);

int main() {
    int xs[__LINE__] = {
        __LINE__, __LINE__,
        __LINE__,
        __LINE__,
        __LINE__,
    };

    // [[print: 6 7 7 8 9 10 0]]
    printf("%zu %d %d %d %d %d %d\n", _Lengthof(xs), xs[0], xs[1], xs[2], xs[3], xs[4], xs[5]);

    return __LINE__;
}
