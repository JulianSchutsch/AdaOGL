.PHONY: all
.PHONY: clean

all: client

client:
	$(MAKE) client --directory=buildcfg

server:
	$(MAKE) server --directory=buildcfg

clean:
	$(MAKE) clean --directory=buildcfg