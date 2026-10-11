enum Enum: long { A, B, C };

// [[compile-error: nonintegral fixed underlying type `enum Enum~\d+ complete`]]
enum Invalid: typeof(A) { X };

int main() {}
