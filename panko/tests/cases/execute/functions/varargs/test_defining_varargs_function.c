// [[nosnapshot]]

#include <stdarg.h>

int printf(char const*, ...);
int vprintf(char const*, va_list);

struct T {
    long x;
    long y;
};

int printf_wrapper(char const* fmt, ...) {
    va_list ap;
    va_start(ap);
    char const* s = va_arg(ap, char const*);
    struct T struct_from_varargs = va_arg(ap, struct T);
    printf("[%s: %ld -> %ld] ", s, struct_from_varargs.x, struct_from_varargs.y);
    vprintf(fmt, ap);
}

int main() {
    struct T t = { .x = 42, .y = 27 };
    // [[print: [hello: 42 -> 27] 123, 456, 42, 27]]
    printf_wrapper("%d, %d, %ld, %ld\n", "hello", t, 123, 456, t.x, t.y);
}
