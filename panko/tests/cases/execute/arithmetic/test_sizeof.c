// [[return: 8]]

void* calloc(unsigned long, unsigned long);

int main() {
    int* p = calloc(1, sizeof *p);
    return *p + sizeof &calloc;
}
