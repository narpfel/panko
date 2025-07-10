#ifdef MACRO
not taken
#endif

#define MACRO 42

#ifdef MACRO
int puts(char const*);
#endif

int main() {
    // [[print: it works]]
    puts("it works");
}
