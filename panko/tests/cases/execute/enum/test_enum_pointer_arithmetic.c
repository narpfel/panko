// [[known-bug: pointer arithmetic involving enum values not implemented]]

int printf(char const*, ...);

enum E { A, B, C, D };

int main() {
    int xs[] = {42, 27, 5, 123, 456};
    int* p = xs;
    enum E offset = B;
    p += offset;
    int a = p[(enum E)A];
    offset = C;
    int b = p[offset];
    offset = B;
    int c = *(p - offset);
    int d = *(offset + p);

    // [[print: 27 123 42 5]]
    printf("%d %d %d %d\n", a, b, c, d);
}
