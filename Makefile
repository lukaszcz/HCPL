
MODE=debug
# debug or release

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
	ocamlbuild -pp 'm4 -P' -ocamlopt "ocamlopt.opt -pp 'm4 -P' -S $(DEBUG_FLAGS) $(DEBUG_LIBS)" -Is $(IDIRS) ipl.native
	cp ipl.native ipl

release:
	ocamlbuild -pp 'm4 -P' -ocamlopt "ocamlopt.opt -pp 'm4 -P' -S $(RELEASE_FLAGS) $(RELEASE_LIBS)" -Is $(IDIRS) ipl.native
	cp ipl.native ipl

package: clean
	-scripts/rmbackups.sh
	mkdir ipl-`date +"%d-%m-%y"`
	cp -r scripts src lib tests TODO ipl-`date +"%d-%m-%y"`
	cat Makefile | sed s/MODE=debug/MODE=release/ > ipl-`date +"%d-%m-%y"`/Makefile
	tar czf ipl-`date +"%d-%m-%y"`.tar.gz ipl-`date +"%d-%m-%y"`
	rm -r ipl-`date +"%d-%m-%y"`

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
	-rm ipl
	-rm *.out
	-rm *.log
	-rm -r ipl-*
