// [[return: 69]]

// this tests constructing the composite type for both decls of `xs`

int xs[];

int main() {
    xs[0] = 42;
    xs[12345] = 27;
    return xs[0] + xs[12345];
}

int xs[12346];
