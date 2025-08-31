#if ("hello" < 1) || (0 > (0x7fff'ffff'ffff'ffff + 1))
not taken
#else
int puts(char const*);
#endif

int main() {
    puts("error");
}
