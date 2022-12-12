cmake_minimum_required(VERSION 3.16)

project(day11 LANGUAGES CXX)

add_executable(day11 main.cpp)
target_compile_options(day11
  PRIVATE
  -Wall -Wextra -Wshadow -Wnon-virtual-dtor -pedantic
)
