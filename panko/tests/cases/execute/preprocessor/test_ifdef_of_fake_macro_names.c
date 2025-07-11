#ifdef __has_include
#if defined __has_embed && defined __has_c_attribute
int puts(char const*);
#endif
#endif

int main() {
    // [[print: it works]]
    puts("it works");
}
