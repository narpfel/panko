struct T {
    // [[compile-error: signed overflow in constant expression]]
    int x:~-(int)0x8000'0000;
};
