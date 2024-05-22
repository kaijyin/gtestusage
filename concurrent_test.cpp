#include <future>
#include <iostream>
#include <map>
#include <mutex>
#include <optional>
#include <string>
#include <thread>
#include <vector>

bool is_prime(int n) {
    if (n < 2) {
        return false;
    }
    for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) {
            return false;
        }
    }
    return true;
}
void get_prime(int start, int end, std::promise<std::vector<int>> &&promise) {
    std::vector<int> result;
    for (int i = start; i < end; i++) {
        if (is_prime(i)) {
            result.push_back(i);
        }
    }
    promise.set_value(result);
}
int main() {
    unsigned int nCores = std::thread::hardware_concurrency();
    int start, end;
    std::cin >> start >> end;
    std::vector<std::thread> threads(nCores);
    std::vector<std::promise<std::vector<int>>> promises(nCores);
    std::vector<std::future<std::vector<int>>> futures(nCores);
    // 先从每个promise中获取future
    for (int i = 0; i < nCores; i++) {
        futures[i] = (promises[i].get_future());
    }
    for (int i = 0; i < nCores; i++) {
        threads[i] = std::thread(get_prime, start + i * (end - start) / nCores,
                                 start + (i + 1) * (end - start) / nCores,
                                 std::move(promises[i]));
    }
    for (int i = 0; i < nCores; i++) {
        threads[i].join();
        auto result = futures[i].get();
        for (auto num : result) {
            std::cout << num << " ";
        }
        std::cout << std::endl;
    }
    return 0;
}