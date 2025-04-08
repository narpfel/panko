int printf(char const*, ...);

int main() {
    // [[print: -28]]
    printf("%d\n", '\xe4');
    // [[print: -1]]
    printf("%d\n", '\xff');

    // [[print: 228]]
    printf("%u\n", u8'\xe4');
    // [[print: 255]]
    printf("%u\n", u8'\xff');

    // [[print: 228]]
    printf("%u\n", u'\xe4');
    // [[print: 255]]
    printf("%u\n", u'\xff');
    // [[print: 65535]]
    printf("%u\n", u'\xffff');
    // [[print: 255]]
    printf("%u\n", u'\xff');

    // [[print: 228]]
    printf("%u\n", U'\xe4');
    // [[print: 255]]
    printf("%u\n", U'\xff');
    // [[print: 16777215]]
    printf("%u\n", U'\xffffff');
    // [[print: 4294967295]]
    printf("%u\n", U'\xffffffff');

    // [[print: 228]]
    printf("%d\n", L'\xe4');
    // [[print: 255]]
    printf("%d\n", L'\xff');
    // [[print: -1]]
    printf("%d\n", L'\xffffffff');

    // [[print: 1]]
    printf("%d\n", U'\xffffffff' == 4294967295u);
    // [[print: 1]]
    printf("%d\n", L'\xffffffff' == (-1));
}
