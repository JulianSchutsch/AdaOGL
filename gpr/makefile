.PHONY: all
.PHONY: clean

all:
ifeq ($(OS),Windows_NT)
	gnatmake -Pps.gpr
else
	gcc -c -o bsdsockets_constants.o ../src/network/linux/bsdsockets_constants.c
	gcc -c -o cxlib.o ../src/xlib/cxlib.c
	gnatmake -Pps.gpr -largs bsdsockets_constants.o cxlib.o
endif

clean:
	$(RM) ../build/*.o
	$(RM) ../build/*.ali
ifneq ($(OS),Windows_NT)
	$(RM) bsdsockets_constants.o
	$(RM) cxlib.o
endif
