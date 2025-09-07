// [[return: 5]]

int puts(char const*);

int main() {
    int const x = 42;
    typeof_unqual(x) y = 27;
    y = 5;
    typeof_unqual(&x) p = nullptr;
    // [[print: it works]]
    puts(_Generic(p, int const*: "it works"));
    return y;
}
