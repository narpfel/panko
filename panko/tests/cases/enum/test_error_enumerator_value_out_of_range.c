enum TristateBool: bool { False, True, Maybe };

enum SignedChar: signed char {
    A = 126,
    B,
    C,
    D = 2 * A,
};

int main() {
    // TODO: this emits a duplicate out-of-range error and should either be
    // suppressed or at least point to the usage loc and not the definition loc
    return Maybe;
}
