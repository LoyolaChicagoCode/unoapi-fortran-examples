cmake_minimum_required(VERSION 3.10)

project(Fortran-OpenMP-Examples)

# Enable Fortran
enable_language(Fortran)
     
# Bin directory
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# Find OpenMP
find_package(OpenMP REQUIRED)

# Add the executable
add_executable(aoc aoc.f90)
target_link_libraries(aoc PRIVATE OpenMP::OpenMP_Fortran)

add_executable(vdp vdp.f90)
target_link_libraries(vdp PRIVATE OpenMP::OpenMP_Fortran)

