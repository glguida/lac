project(laconic)

set(CMAKE_C_FLAGS_DEBUG "-g -Wall -O0 -fno-inline ${CMAKE_C_FLAGS_DEBUG}")
set(MAKE_C_FLAGS_RELEASE "-Wall -O3 ${CMAKE_C_FLAGS_RELEASE}")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_RELEASE}")

set(laconic_VERSION 0.1)

set(laconic_C_SRCS
 laconic.c ext_types.c env.c map.c ty_string.c ty_int.c
)

set(laconic_FILES)

include(FindPkgConfig)
pkg_check_modules(GC REQUIRED bdw-gc)
link_directories(${GC_LIBRARY_DIRS})
find_library(LIBGC gc PATHS "${GC_LIBRARY_DIRS}")
include_directories(${GC_INCLUDE_DIRS})
set(laconic_LINK_FLAGS "${laconic_LINK_FLAGS} ${GC_LDFLAGS}")
message("B IS ${laconic_LINK_FLAGS}")

find_library(READLINE_LIBRARY NAMES libreadline)
find_path(READLINE_INCLUDE_DIRS readline.h)
link_directories(${READLINE_LIBRARY_DIRS})
include_directories(READLINE_INCLUDE_DIRS)

find_library(SIGSEGV_LIBRARY NAMES libsigsegv)
find_path(SIGSEGV_INCLUDE_DIRS sigsegv.h)
link_directories(${SIGSEGV_LIBRARY_DIRS})
include_directories(SIGSEGV_INCLUDE_DIRS)

find_package(BISON)
find_package(FLEX)

FLEX_TARGET(scanner lexer.l ${CMAKE_CURRENT_BINARY_DIR}/lexer.c)
BISON_TARGET(parser sexpr.y ${CMAKE_CURRENT_BINARY_DIR}/sexpr.c)
ADD_FLEX_BISON_DEPENDENCY(scanner parser)

add_definitions(-DLAC_SYSDIR=\\"${CMAKE_INSTALL_PREFIX}/share/lac\\")
include_directories(${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BIN_DIR})

add_executable(laconic ${laconic_C_SRCS}
                       ${BISON_parser_OUTPUTS}
                       ${FLEX_scanner_OUTPUTS})
target_link_libraries(laconic sigsegv gc readline curses)
set_target_properties(laconic PROPERTIES
	LINKER_LANGUAGE C
	SOVERSION ${laconic_VERSION}
	LINK_FLAGS "${laconic_LINK_FLAGS}")


INSTALL(TARGETS laconic RUNTIME DESTINATION bin)
INSTALL(FILES sys.lac DESTINATION share/lac)
