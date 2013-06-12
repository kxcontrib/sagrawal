uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')

ifeq ($(uname_S),Linux)
        DYLIBFLAGS = -shared -fPIC
endif
ifeq ($(uname_S),Darwin)
        DYLIBFLAGS = -bundle -undefined dynamic_lookup
endif


all: install
 
install:
	gcc -O2 lcs.c utils.c -std=c99 -Wall $(DYLIBFLAGS) -o lcs.so
 
clean:
	rm -rf *.o *.so lcs
