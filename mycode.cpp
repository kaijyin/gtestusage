#include <bits/stdc++.h>
int main() {
    int n;
    std::cin >> n;
    std::vector<int> numbers(n);
    for (int i = 0; i < n; i++) {
        std::cin >> numbers[i];
    }
    int res = numbers[0];
    int now = 0;
    for (int i = 0; i < n; i++) {
        now += numbers[i];
        res = std::max(res, now);
        if (now < 0) now = 0;
    }
    std::cout << res << std::endl;
    return 0;
}
