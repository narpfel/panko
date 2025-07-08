// [[return: 42]]

#if -1wb > 2wbu
int puts(char const*);
#endif

#if 1wb + 2 == 3u
int main() {
#endif
    // [[print: it works]]
    puts("it works");

    return
#if !(-1wb + 1wbu)
        42
#endif
        ;
}
