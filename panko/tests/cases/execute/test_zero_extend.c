// [[arg: %lu\n]]

int printf(char const*, ...);

void print(char** argv, unsigned long l) {
    printf(*(argv + 1), l);
}

int main(int, char** argv) {
    // zero in msb
    {
        char unsigned c = 42;
        // [[print: 42]]
        print(argv, c);
        unsigned short s = c;
        // [[print: 42]]
        print(argv, s);
        unsigned int i = c;
        // [[print: 42]]
        print(argv, i);
        unsigned int j = s;
        // [[print: 42]]
        print(argv, j);
        unsigned long l = c;
        // [[print: 42]]
        print(argv, l);
        unsigned long m = s;
        // [[print: 42]]
        print(argv, m);
        unsigned long n = i;
        // [[print: 42]]
        print(argv, n);
    }

    // one in msb
    {
        char unsigned c = 250;
        // [[print: 250]]
        print(argv, c);
        unsigned short s = c;
        // [[print: 250]]
        print(argv, s);
        unsigned int i = c;
        // [[print: 250]]
        print(argv, i);
        unsigned int j = s;
        // [[print: 250]]
        print(argv, j);
        unsigned long l = c;
        // [[print: 250]]
        print(argv, l);
        unsigned long m = s;
        // [[print: 250]]
        print(argv, m);
        unsigned long n = i;
        // [[print: 250]]
        print(argv, n);
    }
}
