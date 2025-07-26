%:define CONCAT(x, y) x %:%: y
%:define STRINGISE(x) %:x

int puts(char const*);

int main() {
    // [[print: it-works]]
    CONCAT(pu, ts)(STRINGISE(it) STRINGISE(-) STRINGISE(works));
}
