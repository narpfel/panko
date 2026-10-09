// [[known-bug: enumerators of enums with fixed underlying type should have `enum` type, not the underlying type]]

enum Enum: long { A, B, C };

// [[compile-error: nonintegral fixed underlying type `enum Enum~\d+ complete`]]
enum Invalid: typeof(A) { X };

int main() {}
