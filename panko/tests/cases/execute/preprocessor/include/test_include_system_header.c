// [[return: 1]]

#include <iso646.h>

#if not true
not taken
#endif

int main() {
    int x = (1 xor 0) not_eq (0 bitand 1);
    x or_eq 0b1'1000;
    x and_eq 0xf;
    return x == 0b1001;
}
