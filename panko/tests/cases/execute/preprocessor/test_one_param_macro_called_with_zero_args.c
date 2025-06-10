// [[return: 42]]

#define ONE_ARG(x) -x

int main() {
    int value = -42;
    return ONE_ARG() value;
}
