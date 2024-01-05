int add(int a, int b) {
    return a + b;
}

int test()
{
    return 15;
}

bool compare(int a, int b) {
    if (a != b) {
        return false;
    }
    return true;
}

int main() {
    int a = 5;
    int b = 10;
    bool res = compare(a, b);
    return 0;
}
