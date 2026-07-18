int puts(char const*);

#define F(x) #x

int main() {
    // [[print: F(F(42))]]
    puts(F(F(F(42))));
}
