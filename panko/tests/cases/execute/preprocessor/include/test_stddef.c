// [[return: 2]]

#include <stddef.h>

int puts(char const*);

int main() {
    int xs[2] = {};
    int* p = &xs[0];
    int const* p2 = &xs[1];
    // [[print: it works]]
    puts(_Generic(p2 - p, ptrdiff_t: "it works"));
    // [[print: it works]]
    puts(_Generic(sizeof xs, size_t: "it works"));
    // [[print: it works]]
    puts(_Generic(_Lengthof xs, size_t: "it works"));
    p = NULL;
    // [[print: null]]
    NULL ? puts("not null") : puts("null");

    nullptr_t null = nullptr;
    return null ? 1 : 2;
}
