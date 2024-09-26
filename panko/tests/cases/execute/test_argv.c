// [[return: 12]]
// [[arg: hello world\n]]

unsigned long strlen(char const*);

int main(int argc, char** argv) {
    return strlen(*(argv + 1));
}
