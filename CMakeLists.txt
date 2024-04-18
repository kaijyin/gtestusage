cmake_minimum_required(VERSION 3.10) # 设置CMake的最小版本要求

project(Gtest_TEST_USAGE) # 设置项目名称

# 设置C++标准
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# 寻找Google Test库
find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

# 添加code.cpp到库中（如果code.cpp足够大，也可以考虑编译成库）
add_library(code_lib code.cpp)

# 添加测试可执行文件
add_executable(code_test code_test.cpp)

# 链接Google Test到测试可执行文件
target_link_libraries(code_test ${GTEST_LIBRARIES} pthread code_lib)

# 启用测试
enable_testing()
add_test(NAME code_test COMMAND code_test)