

# Go parameters
GOCMD=go
GOBUILD=$(GOCMD) build
GOCLEAN=$(GOCMD) clean
GOINSTALL=$(GOCMD) install
GOTEST=$(GOCMD) test
GODEP=$(GOTEST) -i
GOFMT=gofmt -w -tabs=false -tabwidth=2 .

ARCH=`uname -p`
MAIN_FILE_PREFIX=main
MAIN_FILE=$(MAIN_FILE_PREFIX).go
MAIN_BINARY=$(MAIN_FILE_PREFIX)_$(ARCH)

all: build run

clean:
	rm $(MAIN_BINARY) || true

build: clean
	$(GOCMD) build $(MAIN_FILE)
	mv $(MAIN_FILE_PREFIX) $(MAIN_BINARY)

fmt:
	$(GOFMT)

run:
	./$(MAIN_BINARY)

