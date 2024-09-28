// [[arg: hello world\n]]
// [[print: hello world]]

int printf(char const*, ...);

int main(int argc, char** argv) {
    printf(*(argv + 1));
}
