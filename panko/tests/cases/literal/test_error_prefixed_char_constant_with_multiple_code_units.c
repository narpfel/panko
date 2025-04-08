int main() {
    u8'ä';
    // okay
    u'ä';
    U'ä';

    u8'ゆ';
    // okay
    u'ゆ';
    U'ゆ';

    u8'🐌';
    u'🐌';
    // okay
    U'🐌';
}
