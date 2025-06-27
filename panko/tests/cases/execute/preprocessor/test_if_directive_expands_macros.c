// [[return: 22]]

#define VALUE 5
#define FUNCTION(x) (x + 27)

#if VALUE == 5 && FUNCTION(42) == 69
#define CONDITIONAL_MACRO 22
int puts(char const*);
#endif

int main() {
    // [[print: it works]]
    puts("it works");
    return CONDITIONAL_MACRO;
}
