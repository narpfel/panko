int/* comment */puts(char const*);

/*
 * this is the `main` function’s comment
 */
int main() {
    // [[print: comment /* in */ string]]
    puts("comment /* in */ string");

    // [[print: hello world]]
    puts/*
        comment
    */("hello world");
}
