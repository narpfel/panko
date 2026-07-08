// [[known-bug: referencing global variables from libc is broken]]

// TODO: include `<stdio.h>`
typedef void FILE;

extern FILE* stdout;
extern FILE* stderr;

int fprintf(FILE*, char const*, ...);

int main() {
    // [[print: stdout]]
    fprintf(stdout, "stdout\n");
    // [[stderr-re: ^stderr\n$]]
    fprintf(stderr, "stderr\n");
}
