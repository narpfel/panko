int main() {
    typeof(nullptr) a = nullptr;
    typeof(nullptr) b = (typeof(nullptr))nullptr;
    a = nullptr;
    b = (typeof(nullptr))nullptr;

    // conversion of null pointer constant to type `nullptr_t`
    a = 0;
    a = 2 * 0;
    a = (void*)0;
    b = (typeof(nullptr))0;
    b = (typeof(nullptr))(2 * 0);
    b = (typeof(nullptr))(void*)0;
}
