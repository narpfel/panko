// [[return: 42]]

#if defined(MACRO)
not taken
#elif defined __LINE__ && !defined(UNDEFINED)
int puts(char const*);
#endif

#define MACRO

int main() {
    // [[print: it works]]
    puts("it works");
#if (defined MACRO == 1) && (defined UNDEFINED == 0)
    return 42;
#endif
}
