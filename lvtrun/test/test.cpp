#include <iostream>

int globa15 = 15;

int add(int a, int b) {
    return a + b;
}

int test()
{
    return globa15;
}

bool compare(int a, int b) {
    if (a != b) {
        return false;
    }
    return true;
}

int main() {
    int a = add(5, 10);
    int b = add(10, 5);
    bool res = compare(a, b);
    std::cout << "res: " << res << std::endl;
    return 0;
}
