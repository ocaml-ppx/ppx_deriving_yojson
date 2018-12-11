build:
	dune build

test:
	dune runtest

doc:
	dune build @doc

clean:
	dune clean

.PHONY: build test doc clean

VERSION      := $$(opam query --version)
NAME_VERSION := $$(opam query --name-version)
ARCHIVE      := $$(opam query --archive)

release:
	git tag -a v$(VERSION) -m "Version $(VERSION)."
	git push origin v$(VERSION)
	opam publish prepare $(NAME_VERSION) $(ARCHIVE)
	opam publish submit $(NAME_VERSION)
	rm -rf $(NAME_VERSION)

.PHONY: release all-supported-ocaml-versions

all-supported-ocaml-versions:
	dune build @install @runtest --workspace dune-workspace.dev
