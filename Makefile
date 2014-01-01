SRC = angolmois.rs
ifneq (,$(findstring MINGW,$(shell uname -s)))
EXE ?= .exe
else
EXE ?=
endif
BIN = angolmois$(EXE)
RUSTC ?= rustc$(EXE)
RUSTDOC ?= rustdoc$(EXE)
RUSTSDL ?= rust-sdl
RUSTFLAGS ?= -O


.PHONY: all clean

all: $(BIN)

$(BIN): $(SRC)
	$(RUSTC) $(RUSTFLAGS) -L . $(SRC) -o $(BIN)

$(RUSTSDL)/libsdl.dummy:
	cd $(RUSTSDL) && ./configure && $(MAKE) RUSTC=$(RUSTC)

doc:
	$(RUSTDOC) $(SRC) --output-dir . --output-style doc-per-crate

clean:
	rm -rf $(BIN)

