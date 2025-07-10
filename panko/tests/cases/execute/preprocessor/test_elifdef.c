#if false
not taken
#elifdef MACRO
also not taken
#elifndef MACRO
int puts(char const*);
#endif

#define MACRO 42

#if false
not taken
#elifdef MACRO
int printf(char const*, ...);
#elifndef MACRO
also not taken
#else
else is right out
#endif

int main() {
    // [[print: it works]]
    puts("it works");
    // [[print: MACRO: 42]]
    printf("MACRO: %d\n", MACRO);
}
