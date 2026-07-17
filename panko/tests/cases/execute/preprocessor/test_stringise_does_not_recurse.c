// [[known-bug: hideset tracking for stringise is incorrect]]

int puts(char const*);

#define F(x) #x

int main() {
    // [[print: F(F(42))]]
    puts(F(F(F(42))));
}
