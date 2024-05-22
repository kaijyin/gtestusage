cmake_minimum_required(VERSION 3.10) # 设置CMake的最小版本要求

project(Gtest_TEST_USAGE) # 设置项目名称

#生成compile.json文件，方便clangd识别
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# 设置CMake构建类型为Debug
set(CMAKE_BUILD_TYPE Debug)

# 设置C++标准
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_COMPILER "clang++")
#设置C标准
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED True)
set(CMAKE_C_COMPILER "clang")

# 添加code.cpp到库中（如果code.cpp足够大，也可以考虑编译成库）
add_library(code_lib code.cpp)

# 添加测试可执行文件
add_executable(code_test code_test.cpp)
add_executable(format_test format_test.cpp)

# 寻找Google Test库
find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

# 链接Google Test到测试可执行文件
target_link_libraries(code_test ${GTEST_LIBRARIES} pthread code_lib)

# 启用测试
enable_testing()
add_test(NAME code_test COMMAND code_test)