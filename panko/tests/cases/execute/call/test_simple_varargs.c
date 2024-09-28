// [[return: 14]]
// [[arg: hello %d world\n]]
// [[print: hello 2 world]]

int printf(char const*, ...);

int main(int argc, char** argv) {
    return printf(*(argv + 1), argc);
}
