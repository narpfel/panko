// [[return: 1]]

#include "iso646.h"
#include "stddef.h"

int main() {
    int xs[] = {1, 2};
    return _Generic(sizeof 42, size_t: 0) or _Generic(&xs[1] - &xs[0], ptrdiff_t: 1);
}
