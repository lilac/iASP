set(CMAKE_SYSTEM_NAME Windows)
set(CMAKE_C_COMPILER i586-mingw32msvc-gcc)
set(CMAKE_CXX_COMPILER i586-mingw32msvc-g++)
SET(CMAKE_RC_COMPILER i586-mingw32msvc-windres)

SET(CMAKE_FIND_ROOT_PATH /usr/mingw32 $ENV{HOME}/local/mingw32 $ENV{HOME}/local/mingw32/opt/boost)

SET(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
SET(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
SET(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
