int printf(char const*, ...);

int main() {
    char as_integral_value[] = {'%', 'd', '\n', 0};
    char as_char[] = {'<', '%', 'c', '>', '\n', 0};

    // [[print: 101]]
    printf(as_integral_value, 'e');
    // [[print: <e>]]
    printf(as_char, 'e');
    // [[print: 39]]
    printf(as_integral_value, '\'');
    // [[print: <'>]]
    printf(as_char, '\'');
    // [[print: 34]]
    printf(as_integral_value, '\"');
    // [[print: <">]]
    printf(as_char, '\"');
    // [[print: 34]]
    printf(as_integral_value, '"');
    // [[print: <">]]
    printf(as_char, '"');
    // [[print: 63]]
    printf(as_integral_value, '\?');
    // [[print: <?>]]
    printf(as_char, '\?');
    // [[print: 63]]
    printf(as_integral_value, '?');
    // [[print: <?>]]
    printf(as_char, '?');
    // [[print: 92]]
    printf(as_integral_value, '\\');
    // [[print: <\>]]
    printf(as_char, '\\');
    // [[print: 7]]
    printf(as_integral_value, '\a');
    // [[print: 8]]
    printf(as_integral_value, '\b');
    // [[print: 12]]
    printf(as_integral_value, '\f');
    // [[print: 10]]
    printf(as_integral_value, '\n');
    // [[print: <]]
    // [[print: >]]
    printf(as_char, '\n');
    // [[print: 13]]
    printf(as_integral_value, '\r');
    // [[print: 9]]
    printf(as_integral_value, '\t');
    // [[print: 11]]
    printf(as_integral_value, '\v');
}
