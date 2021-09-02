# Go parameters
GOCMD=go
GOBUILD=$(GOCMD) build
GOCLEAN=$(GOCMD) clean
GOINSTALL=$(GOCMD) install
GOTEST=$(GOCMD) test
GODEP=$(GOTEST) -i
GOFMT=gofmt -w -tabs=false -tabwidth=2 .

ARCH=`uname -s`-`uname -r`-`uname -m`
MAIN_FILE_PREFIX=main
MAIN_FILE=$(MAIN_FILE_PREFIX).go
BINARY=palindromes

all: build

clean:
	rm $(MAIN_BINARY) || true

build: clean
	$(GOCMD) build -o $(BINARY) $(MAIN_FILE)

fmt:
	$(GOFMT)

test:
	go test ./pkg
