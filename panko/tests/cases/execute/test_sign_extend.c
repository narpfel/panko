// [[arg: %ld\n]]

int printf(char const*, ...);

void print(char** argv, long l) {
    printf(*(argv + 1), l);
}

int main(int, char** argv) {
    // negative
    {
        char signed c = -42;
        // [[print: -42]]
        print(argv, c);
        short s = c;
        // [[print: -42]]
        print(argv, s);
        int i = c;
        // [[print: -42]]
        print(argv, i);
        int j = s;
        // [[print: -42]]
        print(argv, j);
        long l = c;
        // [[print: -42]]
        print(argv, l);
        long m = s;
        // [[print: -42]]
        print(argv, m);
        long n = i;
        // [[print: -42]]
        print(argv, n);
    }

    // positive
    {
        char signed c = 42;
        // [[print: 42]]
        print(argv, c);
        short s = c;
        // [[print: 42]]
        print(argv, s);
        int i = c;
        // [[print: 42]]
        print(argv, i);
        int j = s;
        // [[print: 42]]
        print(argv, j);
        long l = c;
        // [[print: 42]]
        print(argv, l);
        long m = s;
        // [[print: 42]]
        print(argv, m);
        long n = i;
        // [[print: 42]]
        print(argv, n);
    }
}
