int main() {
    const void *c_vp;
    void *vp;
    const int *c_ip;
    volatile int *v_ip;
    int *ip;
    const char *c_cp;
    _Generic(0 ? c_vp : c_ip, void const*: 0);
    // TODO: nullptr constants
    // _Generic(0 ? v_ip : 0, int volatile*: 0);
    _Generic(0 ? v_ip : nullptr, int volatile*: 0);
    _Generic(0 ? c_ip : v_ip, int const volatile*: 0);
    _Generic(0 ? vp : c_cp, void const*: 0);
    _Generic(0 ? ip : c_ip, int const*: 0);
    _Generic(0 ? vp : ip, void*: 0);
}
