// TODO: 6.7.4.1 sentence 10 defines the behaviour of a function type with any
// type qualifiers as undefined. We generate a nonsensical error message in this
// case that should be improved.

typedef int F();

F const f;

int const f() {}
