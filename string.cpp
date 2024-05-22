
#include <iostream>
#include <string>
class String {
private:
    std::string str_;

public:
    String() {
        str_ = "hello world";
        std::cout << "String() construct" << std::endl;
    };  //默认构造函数，无参数
    ~String() = default;
    String(const String& s)  //拷贝构造函数
    {
        str_ = s.str_;
        std::cout << "String(const String& s) construct" << std::endl;
    }
    explicit String(const std::string& s) : str_("A"){};  //参数化构造函数
    String& operator=(
        const String& s) {  //不算构造函数，重载赋值符号函数，在原有对象上操作
        std::cout << "String& operator=(const String& s) construct"
                  << std::endl;
        if (this != &s) {
            str_ = s.str_;
        }
        return *this;
    }
    std::string getStr() { return str_; }
    // 移动构造函数
    String(String&& other) noexcept : str_(std::move(other.str_)) {
        std::cout << "String(String&& other) construct" << std::endl;
    }
    String& operator=(String&& other) noexcept {
        std::cout << "String& operator=(String&& other) construct" << std::endl;
        if (this != &other) {
            str_ = std::move(other.str_);
        }
        return *this;
    }
    // 委托构造函数（这里简单示例，委托给参数化构造函数）
    explicit String(char c) : String(std::string(1, c)) {}
};
int main() {
    String s2;  //默认构造函数 输出：String() construct
    String s3 =
        s2;  //调用的不是=赋值操作符，而是拷贝构造函数,输出：String(const
             // String& s) construct
    String s4;  //输出： String() construct
    s4 = s2;  //调用重载的=赋值操作符函数,输出 String& operator=(const String&
              // s) construct
    String s5(std::move(
        s2));  //调用移动构造函数,move函数：只是把对象转换为右值引用类型(&&),输出String(String&&
               // other) construct
    std::cout << s4.getStr() << std::endl;  //输出hello world
    std::cout << s5.getStr() << std::endl;  //输出hello world
    s5 = std::move(s4);  //调用移动赋值操作符函数,输出String& operator=(String&&
                         // other) construct
    std::cout << s4.getStr()
              << std::endl;  // 输出“”，因为移动构造函数将s4的值移动到s5中
    std::cout << s5.getStr() << std::endl;  // 输出hello world
    // 1.所谓移动构造函数就是参数是右值引用的构造函数
    // 2.所谓移动赋值操作符就是参数是右值引用的重载赋值操作符函数
    return 0;
}