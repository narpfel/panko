// [[known-bug: checking enumerators for previous declarations not implemented yet]]

int Value;

// [[compile-error: value name `Value` redeclared as enumerator name]]
enum E { Other, Value };
