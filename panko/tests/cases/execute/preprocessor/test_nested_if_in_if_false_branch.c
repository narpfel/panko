#if false
#if true
not taken
#endif
#else
int puts(char const*);
#endif

int main() {
    // [[print: it works]]
    puts("it works");
}
