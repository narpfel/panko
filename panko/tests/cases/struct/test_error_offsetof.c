#include <stddef.h>

int main() {
    offsetof(int, n);
    offsetof(struct Incomplete, member);
}
