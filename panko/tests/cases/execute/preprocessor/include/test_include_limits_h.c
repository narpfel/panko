// [[nosnapshot]]

#include <limits.h>

int printf(char const*, ...);

int main() {
    // [[print: 255 is 8 bits wide]]
    printf("%d is %d bits wide\n", UCHAR_MAX, CHAR_BIT);
}
