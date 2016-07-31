all: src/parser.rs target/debug/mincamlasm
	./target/debug/mincamlasm

target/debug/mincamlasm: src/*.rs
	multirust run stable cargo build

src/parser.rs: src/parser.lalrpop
	lalrpop ./src/parser.lalrpop
clean:
	rm -rf src/parser.rs

