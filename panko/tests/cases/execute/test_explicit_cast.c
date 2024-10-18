// [[arg: %p\n]]
// [[arg: %d\n]]

int printf(char const*, ...);

void f(char const* fmt) {
    printf(fmt, 123);
}

int g() {
    return 42;
}

int main(int argc, char** argv) {
    // [[print: 123]]
    (void) f(*(argv + 2));
    (void) 42;
    (void) g();
    (void) argc;
    (void) argv;

    // [[print: (nil)]]
    printf(*(argv + 1), (void*)0);
    // [[print: (nil)]]
    printf(*(argv + 1), (void*)(int const*)0);
    // [[print: 0x2a]]
    printf(*(argv + 1), (void*)42);
    // [[print: 0xffffffffffffffff]]
    printf(*(argv + 1), (void*)-1);

    // [[print: -128]]
    printf(*(argv + 2), (int)(char)128);
    // [[print: 128]]
    printf(*(argv + 2), (int)(char unsigned)128);
    // [[print: 42]]
    printf(*(argv + 2), (int)42);
    // [[print: 42]]
    printf(*(argv + 2), (int)(long)42);

}
