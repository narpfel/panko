#if true
int printf(char const*, ...);
#elif true
not taken
#elif 3 <= 3
also not taken
#elif 3 <= 4
not taken the 3rd
#else
else is right out
#endif

#if false
not taken
#elif false
also not taken
#else
int puts(char const*);
#endif

int main() {
    // [[print: printf]]
    printf("printf\n");
    // [[print: puts]]
    puts("puts");
}
