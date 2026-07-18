int puts(char const*);

#define F(x) #42

int main() {
    puts(F(123));
}
