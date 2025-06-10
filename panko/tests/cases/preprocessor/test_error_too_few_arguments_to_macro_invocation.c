void only_arg(...);
void arg_1(...), arg_2(...), arg_3(...);

#define ONE_ARG(x) only_arg (x)
#define TWO_ARG(x, y) arg_1 (x); arg_2 (y)
#define THREE_ARG(x, y, z) arg_1 (x); arg_2 (y); arg_3 (z)

int main() {
    ONE_ARG();
    TWO_ARG(42);
    THREE_ARG(42, 27);
}
