// [[compile-error: static assertion failed]]
static_assert(true == false);

// [[compile-error: static assertion failed: message]]
static_assert(42 == 27, "message");

// [[compile-error: static assertion failed: with multiple tokens]]
static_assert(nullptr, "with" " multiple" " tokens");
