enum Enum: long { A, B, C };

// [[compile-error: nonintegral fixed underlying type `enum Enum~\d+ complete`]]
enum Invalid: typeof(A) { X };

// [[compile-error: nonintegral fixed underlying type `enum Outer~\d+ complete`]]
enum Outer: char {
    D = 42,
    E = sizeof(enum Inner: typeof(D) { Y, Z }),
};

int main() {}
