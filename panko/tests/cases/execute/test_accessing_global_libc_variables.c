// [[known-bug]]

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
