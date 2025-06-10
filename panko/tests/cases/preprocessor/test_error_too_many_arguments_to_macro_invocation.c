void zero_args();
void only_arg(...);
void arg_1(...), arg_2(...), arg_3(...);

#define ZERO_ARGS() zero_args()
#define ONE_ARG(x) only_arg (x)
#define TWO_ARG(x, y) arg_1 (x); arg_2 (y)
#define THREE_ARG(x, y, z) arg_1 (x); arg_2 (y); arg_3 (z)

int main() {
    ZERO_ARGS(42);
    ONE_ARG(1, 2);
    TWO_ARG(3, 4, 5);
    THREE_ARG(6, 7, 8, 9);
}
