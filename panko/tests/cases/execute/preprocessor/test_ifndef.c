#ifndef MACRO
int puts(char const*);
#endif

#define MACRO 42

#ifndef MACRO
not taken
#endif

int main() {
    // [[print: it works]]
    puts("it works");
}
