int printf(char const*, ...);

void print(int xs[const]) {
    printf("%d\n", xs[0]);
    printf("%d\n", xs[1]);
}

int main() {
    int xs[] = {42, 27};
    // [[print: 42]]
    // [[print: 27]]
    print(xs);
}
