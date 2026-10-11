enum TristateBool: bool { False, True, Maybe };

int main() {
    // TODO: this emits a duplicate out-of-range error and should either be
    // suppressed or at least point to the usage loc and not the definition loc
    return Maybe;
}
