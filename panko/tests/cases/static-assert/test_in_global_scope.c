static_assert(42 == 21 * 2);
static_assert(true, "true is true");
int x;
static_assert(&x, "pointer to static is not nullptr");
static_assert(!nullptr);
static_assert(sizeof(char) < sizeof(char[2]));
static_assert(42, "multiple" "string" "tokens");
