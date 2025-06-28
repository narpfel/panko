#if 0x7fff'ffff'ffff'ffff + 1u > 0

int puts(char const*);

int main() {
    // [[print: it works]]
    puts("it works");
}

#endif
