# Build dependencies configuration

# MZSCHEME=mzscheme-6.1.1

# RACKET_VERSION=6.1.1
RACKET_VERSION=6.8

#RACKET=racket-$(RACKET_VERSION)
#RACO=raco-$(RACKET_VERSION)

RACKET_BIN=/usr/local/racket-$(RACKET_VERSION)/bin
RACKET=$(RACKET_BIN)/racket
RACO=$(RACKET_BIN)/raco

# The Steinberg VST SDK 2.4 can not be included in this distribution.
# Get it at: http://www.steinberg.net/en/company/developer.html

VST_DIR := $(HOME)/kmook/vst/vstsdk2.4
# VST_DIR := $(HOME)/vstsdk2.4
# VST_DIR := C:/home/tom/vstsdk2.4


# MinGW prefix for compiling Windows DLL/EXE

## Debian-hosted mingw32msvc
MINGW := i586-mingw32msvc-

## Cygwin-hosted mingw
#MINGW := i686-pc-mingw32-

## Native MinGW.  See build-mingw.bat
# set PATH=C:\MinGW\bin;%PATH%
# set PATH=C:\MinGW\msys\1.0\bin;%PATH%
# set PATH=C:\Progra~1\Racket\;%PATH%
# MINGW := mingw32-

