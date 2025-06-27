#if 10 > 2 && true && !false
int puts(char const*);
#else
syntax error
#endif

int main() {
    // [[print: it works]]
    puts("it works");
}
