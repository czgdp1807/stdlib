if (NOT TARGET "test-drive::test-drive")
  find_package("test-drive" REQUIRED)
endif()

macro(ADDTEST name)
    add_executable(test_${name} test_${name}.f90)
    target_link_libraries(test_${name} "${PROJECT_NAME}" "test-drive::test-drive")
    add_test(NAME ${name}
             COMMAND $<TARGET_FILE:test_${name}> ${CMAKE_CURRENT_BINARY_DIR}
             WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endmacro(ADDTEST)

list(
  APPEND fyppFlags
  "-I${PROJECT_SOURCE_DIR}/src"
)

add_subdirectory(array)
add_subdirectory(optval)
add_subdirectory(quadrature)
# add_subdirectory(sorting)
add_subdirectory(ascii)
add_subdirectory(string)
add_subdirectory(linalg)
add_subdirectory(selection)
add_subdirectory(specialfunctions)
add_subdirectory(math)
