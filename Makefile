
MODE=release
# debug or release

DEBUG_LIBS=nums.cmxa
RELEASE_LIBS=nums.cmxa

DEBUG_FLAGS=-inline 0 -g
RELEASE_FLAGS=-inline 100 -noassert -unsafe -nodynlink -ccopt -O9

SPACE :=
SPACE +=
COMMA = ,
DIRS=$(shell scripts/lsdirs.sh src)
IDIRS=$(subst $(SPACE),$(COMMA),$(DIRS))

all: $(MODE)

debug:
	ocamlbuild -ocamlopt "ocamlopt.opt -S $(DEBUG_FLAGS) $(DEBUG_LIBS)" -Is $(IDIRS) ipl.native
	cp ipl.native ipl

release:
	ocamlbuild -ocamlopt "ocamlopt.opt -S $(RELEASE_FLAGS) $(RELEASE_LIBS)" -Is $(IDIRS) ipl.native
	cp ipl.native ipl

clean:
	ocamlbuild -clean
	-rm ipl
	-rm *.out
	-rm benchmarks.log
