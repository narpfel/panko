// [[return: 19]]

int printf(char const*, ...);

int main() {
    int a = 10;
    int b = 20;
    int c = 30;

#define a b + 1
#define b c + 3
#define c a + 5

    return a;
}
