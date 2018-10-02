# 
# Copyright (C) 2018 E.S.R.Labs
# 
# Licensed under the Apache License, Version 2.0 (the License);
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#      http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an AS IS BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

cmake_minimum_required(VERSION 3.3)

project(midl-tests)

find_package(Threads)

# Use sccache if available in PATH
find_program(SCCACHE sccache)
if(SCCACHE)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ${SCCACHE})
endif()

# Compiler flags
add_definitions(-Wall -Wextra -Wno-unused-parameter -Wno-sign-compare -g -std=c++11)

# googletest
include_directories(Mindroid.cpp/googletest Mindroid.cpp/googletest/include)
add_library(googletest Mindroid.cpp/googletest/src/gtest-all.cc)

# Tinyxml2
file(GLOB TINYXML2 Mindroid.cpp/src/tinyxml2/*.cpp)
include_directories(Mindroid.cpp/src/tinyxml2)
add_library(tinyxml2 ${TINYXML2})

# Mindroid
include_directories(Mindroid.cpp Mindroid.cpp/src)
file(GLOB_RECURSE MINDROID Mindroid.cpp/src/mindroid/*/*.cpp Mindroid.cpp/tests/mindroid/testing/*.cpp)
include_directories(Mindroid.cpp/tests)
add_library(mindroid ${MINDROID})

# Tests
file(GLOB_RECURSE TESTS src/*.cpp gen/*.cpp)
include_directories(src gen)
add_executable(midl-tests ${TESTS})
target_link_libraries(midl-tests mindroid tinyxml2 googletest ${CMAKE_THREAD_LIBS_INIT})
