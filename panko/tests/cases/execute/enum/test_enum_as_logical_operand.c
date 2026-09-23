int printf(char const*, ...);
void memcpy(void* dest, void const* src, typeof(sizeof 0) n);

enum E { False, True };

enum E false_value;
enum E true_value;

int main() {
    // can’t initialise values of `enum` type yet because conversions
    // from/to enums are not implemented yet
    memcpy(&true_value, &(int){True}, sizeof(enum E));

    // [[print: 1 0]]
    printf("%d %d\n", !false_value, !true_value);

    // [[print: 1 0 1]]
    printf("%d %d %d\n", false_value || true_value, false_value && true_value, true_value && true_value);

    // [[print: 42 27]]
    printf("%d %d\n", false_value ? 0 : 42, true_value ? 27 : 1);
}
