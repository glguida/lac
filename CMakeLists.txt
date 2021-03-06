project(laconic)
cmake_minimum_required(VERSION 2.8)

set(CMAKE_C_FLAGS_DEBUG "-g -Wall -O0 ${CMAKE_C_FLAGS_DEBUG}")
set(CMAKE_C_FLAGS_RELEASE "-Wall -O3 ${CMAKE_C_FLAGS_RELEASE}")
set(CMAKE_C_FLAGS_GCOV "-fprofile-arcs -ftest-coverage")
set(CMAKE_EXE_LINKER_FLAGS_GCOV "-fprofile-arcs -ftest-coverage")
set(CMAKE_C_FLAGS_GPROF "-pg")
set(CMAKE_EXE_LINKER_FLAGS_GPROF "-pg")

#set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS_GPROF}")
#set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_LINKER_FLAGS_GPROF}")

#set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_DEBUG}")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_RELEASE}")


set(laconic_VERSION 0.1)

set(laconic_C_SRCS
 lac.c laconic.c ext_types.c env.c map.c ty_string.c ty_int.c
)

set(laconic_FILES)

include(FindPkgConfig)
pkg_check_modules(GC REQUIRED bdw-gc)
link_directories(${GC_LIBRARY_DIRS})
find_library(LIBGC gc PATHS "${GC_LIBRARY_DIRS}")
include_directories(${GC_INCLUDE_DIRS})
set(laconic_LINK_FLAGS "${laconic_LINK_FLAGS} ${GC_LDFLAGS}")

find_library(READLINE_LIBRARY NAMES libreadline)
find_path(READLINE_INCLUDE_DIRS readline.h)
link_directories(${READLINE_LIBRARY_DIRS})
include_directories(READLINE_INCLUDE_DIRS)

find_library(SIGSEGV_LIBRARY NAMES libsigsegv)
find_path(SIGSEGV_INCLUDE_DIRS sigsegv.h)
link_directories(${SIGSEGV_LIBRARY_DIRS})
include_directories(SIGSEGV_INCLUDE_DIRS)

find_package(BISON 3.0)
find_package(FLEX)

FLEX_TARGET(sexpr_lex sexpr_lex.l ${CMAKE_CURRENT_BINARY_DIR}/sexpr_lex.c)
BISON_TARGET(sexpr_parse
             sexpr_parse.y
	     ${CMAKE_CURRENT_BINARY_DIR}/sexpr_parse.c)
ADD_FLEX_BISON_DEPENDENCY(sexpr_lex sexpr_parse)
SET_PROPERTY(SOURCE ${CMAKE_CURRENT_BINARY_DIR}/sexpr_lex.c
             PROPERTY COMPILE_FLAGS -Wno-unused-function)

add_definitions(-DLAC_SYSDIR="${CMAKE_INSTALL_PREFIX}/share/lac")
include_directories(${CMAKE_CURRENT_SOURCE_DIR}
                    ${CMAKE_CURRENT_BINARY_DIR})

add_executable(laconic ${laconic_C_SRCS}
                       ${BISON_sexpr_parse_OUTPUTS}
                       ${FLEX_sexpr_lex_OUTPUTS})
target_link_libraries(laconic sigsegv gc readline curses)
set_target_properties(laconic PROPERTIES
	LINKER_LANGUAGE C
	SOVERSION ${laconic_VERSION}
	LINK_FLAGS "${laconic_LINK_FLAGS}")


INSTALL(TARGETS laconic RUNTIME DESTINATION bin)
INSTALL(FILES sys.lac DESTINATION share/lac)
