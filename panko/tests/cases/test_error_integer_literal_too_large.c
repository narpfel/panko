int main(int, char** argv) {
    // ok
    0xffff'ffff'ffff'ffff;
    // not ok
    0x1'0000'0000'0000'0000;
}
