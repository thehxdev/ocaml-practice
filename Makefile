PROJECT := practice

build:
	@dune build

run:
	@dune exec $(PROJECT)

utop:
	@dune utop

clean:
	@dune clean
