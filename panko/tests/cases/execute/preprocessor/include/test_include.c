#define CHAR char

#include "t.h"
#include "t.h"

int main() {
    // [[print: it works]]
    puts("it works");
    // [[print: VALUE: 42]]
    printf("VALUE: %d\n", VALUE);
    // [[print: variable: 27]]
    printf("variable: %d\n", variable);
}
