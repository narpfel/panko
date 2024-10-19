// [[return: 69]]

void* calloc(unsigned long, unsigned long);

int main() {
    long* p = calloc(2, sizeof(long));
    p[0] = 42;
    p[1] = 27;
    return p[0] + 1[p];
}
