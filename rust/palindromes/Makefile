

# Rust parameters
ARCH=`uname -s`-`uname -r`-`uname -m`
SRC=src
BUILD=build
RUSTC=rustc -Z debug-info --out-dir $(BUILD) -L $(BUILD) 
MAIN_FILE_SRC=$(SRC)/main.rs
PALINDROMES_MODULE_SRC=$(SRC)/palindromes.rs
TESTS_MODULE_SRC=$(SRC)/tests.rs
MAIN_BINARY=$(BUILD)/palindromes-$(ARCH)

all: build run

clean:
	rm -fr $(BUILD) || true

build: clean
	mkdir $(BUILD) || true
	$(RUSTC) --lib $(PALINDROMES_MODULE_SRC) 
	$(RUSTC) --lib $(TESTS_MODULE_SRC) 
	$(RUSTC) $(MAIN_FILE_SRC) -o $(MAIN_BINARY)

run:
	./$(MAIN_BINARY)

