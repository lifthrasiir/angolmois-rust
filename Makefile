SRC = angolmois.rs
BIN = angolmois
RUSTC ?= rustc
RUSTDOC ?= rustdoc
RUSTSDL ?= rust-sdl
RUSTFLAGS ?= -O
RUSTPKGFLAGS ?= -O

LIBSDL = $(RUSTSDL)/libsdl_rust.rlib
LIBSDLIMAGE = $(RUSTSDL)/libsdl_image_rust.rlib
LIBSDLMIXER = $(RUSTSDL)/libsdl_mixer_rust.rlib


.PHONY: all clean

all: $(BIN)

$(BIN): $(SRC) $(LIBSDL) $(LIBSDLIMAGE) $(LIBSDLMIXER)
	$(RUSTC) $(RUSTFLAGS) -L $(RUSTSDL) $(SRC) -o $(BIN)

$(LIBSDL): $(RUSTSDL)/src/sdl/lib.rs
	$(RUSTC) $(RUSTPKGFLAGS) $< --out-dir $(dir $@)

$(LIBSDLIMAGE): $(RUSTSDL)/src/sdl_image/lib.rs $(LIBSDL)
	$(RUSTC) $(RUSTPKGFLAGS) -L $(RUSTSDL) $< --out-dir $(dir $@)

$(LIBSDLMIXER): $(RUSTSDL)/src/sdl_mixer/lib.rs $(LIBSDL)
	$(RUSTC) $(RUSTPKGFLAGS) -L $(RUSTSDL) $< --out-dir $(dir $@)

doc:
	$(RUSTDOC) -L $(RUSTSDL) $(SRC)

clean:
	rm -rf $(BIN)
	cd $(RUSTSDL) && rm -f *.so *.dll *.rlib *.dylib

