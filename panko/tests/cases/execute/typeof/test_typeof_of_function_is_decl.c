typeof(int(char const*)) puts;
typeof(puts) puts;

int main() {
    typeof(puts) puts;
    typeof(int(char const*)) puts;
    // [[print: it works]]
    puts("it works");
}
