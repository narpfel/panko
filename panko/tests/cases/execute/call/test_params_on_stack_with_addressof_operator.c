// [[return: 27]]

void seven_params(int x1, int x2, int x3, int x4, int x5, int x6, int x7) {}

int main(int argc, char**) {
    // This passes one argument on the stack, which will create an argument
    // area of 8 bytes (one slot) in `main`’s stack frame.
    seven_params(1, 2, 3, 4, 5, 6, 7);

    int value = 27;
    // This was incorrectly not taking the argument area into account, so it
    // was off by 8 byte (it was actually taking the address of `argv`).
    int *p = &value;

    return *p;
}
