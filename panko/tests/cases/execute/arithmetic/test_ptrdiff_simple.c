// [[return: 10]]

void* calloc(unsigned long, unsigned long);

int main() {
    int* array = calloc(42, sizeof *array);
    unsigned long ptrdiff = &array[10] - &array[0];
    return ptrdiff;
}
