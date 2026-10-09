int printf(char const*, ...);

enum Long: long { A, B, C };
enum Unsigned: unsigned { D, E, F };

int main() {
    // [[print: -1 1]]
    printf("%ld %d\n", B - C, B - C < 0);

    // [[print: 0xffffffff 0]]
    printf("0x%x %d\n", E - F, E - F < 0);
}
