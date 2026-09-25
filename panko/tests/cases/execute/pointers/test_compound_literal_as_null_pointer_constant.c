// TODO: Should treating compound literals as (integral) constant expressions
// be allowed? GCC (with a warning) and clang (without a warning) (somewhat
// inconsistently?) allow them as constant expressions that can be used in e. g.
// initialisers for statics but not as nullptr constants.

// [[return: 1]]

int* p1 = (int){};

int main() {
    int* p2 = (int){};
    return p1 == nullptr && p2 == nullptr;
}
