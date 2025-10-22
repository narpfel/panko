// [[return: 7]]
// [[stderr-re: (a*)b+a{3,}]]

#include <stddef.h>
long write(int, void const*, size_t);

int main() {
    return write(2, "aaabaaa", 7);
}
