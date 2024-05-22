#include <iostream>
#include <iterator>
/**
 * @brief The main function of the program.
 *
 * This function reads an integer from the user, and then calculates the sum of
 * all even numbers from 0 to the input number (exclusive). The result is then
 * printed to the console.
 *
 * @return 0 indicating successful execution of the program.
 */
int main() {
    int m = 0;
    std::cin >> m;
    int result = 0;
    for (int i = 0; i < m; ++i) {
        if (i % 2 == 0) {
            result += i;
        }
    }
    std::cout << result << std::endl;
}