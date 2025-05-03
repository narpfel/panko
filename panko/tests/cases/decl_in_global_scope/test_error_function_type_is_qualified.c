typedef int F();

// 6.7.4.1 sentence 10 defines the behaviour of a function type with any
// type qualifiers as undefined. We currently accept this.

// TODO: This should error.
F const f;


F const g;

// TODO: We generate a nonsensical error message in this
// case that should be improved.
int const g() {}
