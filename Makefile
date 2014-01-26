SRC = angolmois.rs
BIN = angolmois
RUSTC ?= rustc
RUSTDOC ?= rustdoc
RUSTPKG ?= rustpkg
RUSTSDL ?= rust-sdl
RUSTFLAGS ?= -O
RUSTPKGFLAGS ?= -O

TRIPLE = $(shell rustc -v | grep host: | cut -b7-)
LIBSDL = $(RUSTSDL)/build/$(TRIPLE)/sdl
LIBSDLIMAGE = $(RUSTSDL)/build/$(TRIPLE)/sdl_image
LIBSDLMIXER = $(RUSTSDL)/build/$(TRIPLE)/sdl_mixer


.PHONY: all clean

all: $(BIN)

$(BIN): $(SRC) $(LIBSDL) $(LIBSDLIMAGE) $(LIBSDLMIXER)
	$(RUSTC) $(RUSTFLAGS) -L $(LIBSDL) -L $(LIBSDLIMAGE) -L $(LIBSDLMIXER) $(SRC) -o $(BIN)

$(LIBSDL):
	cd $(RUSTSDL) && $(RUSTPKG) build $(RUSTPKGFLAGS) sdl

$(LIBSDLIMAGE):
	cd $(RUSTSDL) && $(RUSTPKG) build $(RUSTPKGFLAGS) sdl_image

$(LIBSDLMIXER):
	cd $(RUSTSDL) && $(RUSTPKG) build $(RUSTPKGFLAGS) sdl_mixer

doc:
	$(RUSTDOC) -L $(LIBSDL) -L $(LIBSDLIMAGE) -L $(LIBSDLMIXER) $(SRC)

clean:
	rm -rf $(BIN) $(RUSTSDL)/bin $(RUSTSDL)/lib $(RUSTSDL)/build $(RUSTSDL)/.rust

