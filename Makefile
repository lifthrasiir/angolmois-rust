SRC = angolmois.rs
BIN = angolmois
RUSTC = rustc
RUSTDOC = rustdoc
RUSTSDL = rust-sdl
RUSTFLAGS = -O -L $(RUSTSDL)


.PHONY: all clean

all: $(BIN)

$(BIN): $(SRC)
	$(RUSTC) $(RUSTFLAGS) $(SRC) -o $(BIN)

doc:
	$(RUSTDOC) $(SRC) --output-dir . --output-style doc-per-crate

clean:
	rm -rf $(BIN)

