#include <iostream>

#include "thread_pool.h"

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
void get_prime(int start, int end, std::promise<std::vector<int>>&& promise) {
    std::vector<int> result;
    for (int i = start; i < end; i++) {
        if (is_prime(i)) {
            result.push_back(i);
        }
    }
    promise.set_value(result);
}

int main() {
    int start, end;
    std::cin >> start >> end;

    ThreadPool pool(10);  // 创建一个包含10个线程的线程池

    std::vector<std::future<std::vector<int>>> results;

    // 分配任务到线程池
    results.reserve(10);
    for (int i = 0; i < 10; i++) {
        results.emplace_back(
            pool.enqueue([start, end, i]() -> std::vector<int> {
                std::vector<int> result;
                for (int j = start + i * (end - start) / 10;
                     j < start + (i + 1) * (end - start) / 10; j++) {
                    if (is_prime(j)) {
                        result.push_back(j);
                    }
                }
                return result;
            }));
    }

    // 获取并输出结果
    for (auto&& result : results) {
        auto primes = result.get();
        for (int prime : primes) {
            std::cout << prime << " ";
        }
        std::cout << std::endl;
    }

    return 0;
}