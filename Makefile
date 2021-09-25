VERSION := $(strip $(shell scripts/getcfgvar.sh version))

all:
	dune build
	cp _build/default/src/hcpl.exe hcpl
	chmod u+w hcpl

package: clean
	-scripts/rmbackups.sh
	mkdir hcpl-$(VERSION)
	cp -r scripts src lib data tests examples TODO README README.bin hcpl-$(VERSION)
	cat Makefile | sed s/MODE=debug/MODE=release/ > hcpl-$(VERSION)/Makefile
	tar czf hcpl-$(VERSION).tar.gz hcpl-$(VERSION)
	rm -r hcpl-$(VERSION)

binpackage: release
	-rm -r hcpl-*
	-scripts/rmbackups.sh
	mkdir hcpl-$(VERSION)-bin
	cp -r hcpl scripts lib data tests examples hcpl-$(VERSION)-bin
	cp README.bin hcpl-$(VERSION)-bin/README
	tar czf hcpl-$(VERSION)-bin.tar.gz hcpl-$(VERSION)-bin
	rm -r hcpl-$(VERSION)-bin

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
	dune clean
	-rm hcpl
	-rm *.out
	-rm *.log
	-rm -r hcpl-*
	-rm uninstall.sh
	-rm .configure
