#define VALUE 3

#if 3 <= 1
not taken
#elif 3 <= 2
also not taken
#elif 3 <= 3
int printf(char const*, ...);
#elif 3 <= 4
not taken the 3rd
#else
else is right out
#endif

#if VALUE <= 1
not taken
#elif VALUE <= 2
also not taken
#elif VALUE <= 3
int puts(char const*);
#elif VALUE <= 4
not taken the 3rd
#else
else is right out
#endif


int main() {
    // [[print: it works]]
    puts("it works");
    // [[print: VALUE: 3]]
    printf("VALUE: %d\n", VALUE);
}
