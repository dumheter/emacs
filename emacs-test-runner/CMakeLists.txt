cmake_minimum_required(VERSION 3.16)

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

# Link the C runtime statically so startup does not load extra DLLs.
set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

project(emacs-test-runner LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

add_executable(emacs-test-runner main.c)

# Keep the executable at build/emacs-test-runner[.exe] for every generator.
set_target_properties(emacs-test-runner PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY "$<1:${CMAKE_BINARY_DIR}>")

if(MSVC AND NOT CMAKE_C_COMPILER_ID STREQUAL "Clang")
  target_compile_options(emacs-test-runner PRIVATE /W4 /WX /utf-8)
else()
  target_compile_options(emacs-test-runner PRIVATE
    -Wall -Wextra -Wpedantic -Werror
    -Wshadow -Wconversion -Wsign-conversion -Wformat=2
    -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations
    -Wundef -Wcast-qual -Wwrite-strings -Wpointer-arith
    -Wnull-dereference -Wvla)
endif()

if(WIN32)
  target_compile_definitions(emacs-test-runner PRIVATE
    _CRT_SECURE_NO_WARNINGS WIN32_LEAN_AND_MEAN UNICODE _UNICODE
    _WIN32_WINNT=0x0A00)
else()
  set(THREADS_PREFER_PTHREAD_FLAG ON)
  find_package(Threads REQUIRED)
  target_link_libraries(emacs-test-runner PRIVATE Threads::Threads)
endif()
