
MODE=debug
# debug or release

VERSION := $(strip $(shell scripts/getcfgvar.sh version))

DEBUG_LIBS=nums.cmxa
RELEASE_LIBS=nums.cmxa

DEBUG_FLAGS=-inline 0 -g
RELEASE_FLAGS=-inline 40 -noassert -unsafe -nodynlink -ccopt -O9

SPACE :=
SPACE +=
COMMA = ,
DIRS=$(shell scripts/lsdirs.sh src)
IDIRS=$(subst $(SPACE),$(COMMA),$(DIRS))

all: $(MODE)

debug:
	ocamlbuild -pp 'm4 -P' -ocamlopt "ocamlopt.opt -pp 'm4 -P' -S $(DEBUG_FLAGS) $(DEBUG_LIBS)" -Is $(IDIRS) hcpl.native
	cp hcpl.native hcpl

release:
	ocamlbuild -pp 'm4 -P' -ocamlopt "ocamlopt.opt -pp 'm4 -P' -S $(RELEASE_FLAGS) $(RELEASE_LIBS)" -Is $(IDIRS) hcpl.native
	cp hcpl.native hcpl

package: clean
	-scripts/rmbackups.sh
	mkdir hcpl-$(VERSION)
	cp -r scripts src lib data tests examples TODO README hcpl-$(VERSION)
	cat Makefile | sed s/MODE=debug/MODE=release/ > hcpl-$(VERSION)/Makefile
	tar czf hcpl-$(VERSION).tar.gz hcpl-$(VERSION)
	rm -r hcpl-$(VERSION)

configure:
	scripts/configure.sh
	touch .configure

.configure:
	scripts/configure.sh
	touch .configure

install: .configure all
	scripts/install.sh

test: all
	scripts/run-tests.sh

debug-test: debug
	scripts/run-tests.sh

release-test: release
	scripts/run-tests.sh

benchmark: release
	scripts/run-benchmarks.sh

clean:
	ocamlbuild -clean
	-rm hcpl
	-rm *.out
	-rm *.log
	-rm -r hcpl-*
	-rm uninstall.sh
	-rm .configure
