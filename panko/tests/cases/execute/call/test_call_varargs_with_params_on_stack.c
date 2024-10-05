// [[arg: %d, %d, %d, %d, %d, %d, %d, %d, %d, %d\n]]
// [[print: 1, 2, 3, 4, 5, 6, 7, 8, 9, 44]]

int printf(char const*, ...);

int main(int argc, char** argv) {
    printf(*(argv + 1), 1, 2, 3, 4, 5, 6, 7, 8, 9, argc + 42);
}
