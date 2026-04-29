#include <stddef.h>

int printf(char const*, ...);

struct T {
    int x;
    struct {
        int y;
        long l;
    };
    struct {
        long l;
    } nested_struct;
};

typedef struct T TypeName;

int main() {
    struct T t;
    // [[print: 0 8 16 24]]
    printf(
        "%zu %zu %zu %zu\n",
        offsetof(struct T, x),
        offsetof(typeof(t), y),
        offsetof(TypeName, l),
        offsetof(struct T, nested_struct)
    );
}
