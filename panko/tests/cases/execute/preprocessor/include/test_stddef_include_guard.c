// [[return: 42]]

#include <stddef.h>

#undef NULL
int NULL = 42;

#include <stddef.h>

int main() {
    return NULL;
}
