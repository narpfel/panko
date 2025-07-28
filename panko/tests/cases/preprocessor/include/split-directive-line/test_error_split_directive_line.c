int MACRO = 27;

#define MACRO 42

#include "undef.h"
#include "macro.h"

int main() {
    return MACRO;
}
