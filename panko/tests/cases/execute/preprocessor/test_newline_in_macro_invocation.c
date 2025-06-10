int printf(char const*, ...);

#define NO_ARGS() printf("hello world\n")
#define ONE_ARG(x) printf("x: %d\n", x)

int main() {
    // [[print: hello world]]
    NO_ARGS(

    );

    // [[print: x: 42]]
    ONE_ARG(

        42

    );
}
