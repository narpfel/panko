int printf(char const*, ...);
int puts(char const*);

int main() {
    char str[] = "hello";
    char copy[6];
    char* src = str;
    char* tgt = copy;
    *tgt++ = *src++;
    *tgt++ = *src++;
    *tgt++ = *src++;
    *tgt++ = *src++;
    *tgt++ = *src++;
    *tgt++ = *src++;
    // [[print: 6 6]]
    printf("%zu %zu\n", src - str, tgt - copy);
    // [[print: hello]]
    puts(str);
    // [[print: hello]]
    puts(copy);
}
