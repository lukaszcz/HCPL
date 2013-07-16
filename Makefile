
MODE=debug
# debug or release

DEBUG_LIBS=nums.cma
RELEASE_LIBS=nums.cmxa

DEBUG_FLAGS=-g
RELEASE_FLAGS=-inline 100 -noassert -unsafe -nodynlink -ccopt -O9

SPACE :=
SPACE +=
COMMA = ,
DIRS=$(shell scripts/lsdirs.sh src)
IDIRS=$(subst $(SPACE),$(COMMA),$(DIRS))

all: $(MODE)

debug:
	ocamlbuild -ocamlc "ocamlc.opt $(DEBUG_FLAGS) $(DEBUG_LIBS)" -Is $(IDIRS) ipl.byte
	cp ipl.byte ipl

release:
	ocamlbuild -ocamlopt "ocamlopt.opt -S $(RELEASE_FLAGS) $(RELEASE_LIBS)" -Is $(IDIRS) ipl.native
	cp ipl.native ipl

clean:
	ocamlbuild -clean
	-rm ipl
	-rm *.out
	-rm benchmarks.log
