int printf(char const*, ...);

int main() {
    // [[print: panko/tests/cases/execute/preprocessor/test_file_macro.c:5]]
    printf("%s:%d\n", __FILE__, __LINE__);
}
