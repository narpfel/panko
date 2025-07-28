#include "if_false.h"
#else
int puts(char const*);
#endif

int main() {
    puts("it doesn’t work");
}
