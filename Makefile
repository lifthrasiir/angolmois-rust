SRC = angolmois.rs
BIN = angolmois
RUSTC ?= rustc
RUSTDOC ?= rustdoc
RUSTSDL ?= rust-sdl
RUSTFLAGS ?= -O
RUSTPKGFLAGS ?= -O --crate-type=rlib

LIBSDL = $(RUSTSDL)/libsdl.dummy
LIBSDLIMAGE = $(RUSTSDL)/libsdl_image.dummy
LIBSDLMIXER = $(RUSTSDL)/libsdl_mixer.dummy


.PHONY: all clean

all: $(BIN)

$(BIN): $(SRC) $(LIBSDL) $(LIBSDLIMAGE) $(LIBSDLMIXER)
	$(RUSTC) $(RUSTFLAGS) -L $(RUSTSDL) $(SRC) -o $(BIN)

$(LIBSDL): $(RUSTSDL)/src/sdl/lib.rs
	$(RUSTC) $(RUSTPKGFLAGS) $< --out-dir $(RUSTSDL) && touch $@

$(LIBSDLIMAGE): $(RUSTSDL)/src/sdl_image/lib.rs $(LIBSDL)
	$(RUSTC) $(RUSTPKGFLAGS) -L $(RUSTSDL) $< --out-dir $(RUSTSDL) && touch $@

$(LIBSDLMIXER): $(RUSTSDL)/src/sdl_mixer/lib.rs $(LIBSDL)
	$(RUSTC) $(RUSTPKGFLAGS) -L $(RUSTSDL) $< --out-dir $(RUSTSDL) && touch $@

doc:
	$(RUSTDOC) -L $(RUSTSDL) $(SRC)

clean:
	rm -rf $(BIN)
	cd $(RUSTSDL) && rm -f *.so *.dll *.rlib *.dylib *.dummy

