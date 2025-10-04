// [[return: 1]]

#include "assert.h"
#include "iso646.h"
#include "stddef.h"

int main() {
    int xs[] = {1, 2};
    assert(_Generic(sizeof 42, size_t: 0) or _Generic(&xs[1] - &xs[0], ptrdiff_t: 1));
    return _Generic(sizeof 42, size_t: 0) or _Generic(&xs[1] - &xs[0], ptrdiff_t: 1);
}
