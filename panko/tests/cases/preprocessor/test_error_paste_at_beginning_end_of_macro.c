#define BEGINNING(x) ## x
#define END(x) x ##
#define BOTH() ##

int main() {
    BEGINNING(42);
    END(27);
    BOTH();
}
