cmake_minimum_required(VERSION 3.14)
project(sprite-tool LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(sprite-tool
    src/main.cpp
    src/config.cpp
    src/sprite.cpp
    src/launch.cpp
    src/push.cpp
    src/pull.cpp
    src/watch.cpp
)
target_include_directories(sprite-tool PRIVATE src)
