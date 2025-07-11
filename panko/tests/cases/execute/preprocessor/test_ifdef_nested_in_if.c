#if true
int printf(char const*, ...);
#else
#ifdef MACRO
not taken
#endif
also not taken
#endif

#if true
#define MACRO 42
#else
#ifndef __LINE__
not taken
#endif
also not taken
#endif

int main() {
    // [[print: MACRO: 42]]
    printf("MACRO: %d\n", MACRO);
}
