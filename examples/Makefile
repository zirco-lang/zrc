EXAMPLES := $(wildcard */)

.PHONY: all test clean $(EXAMPLES)

all: $(EXAMPLES)
test: $(EXAMPLES)
	for dir in $(EXAMPLES); do \
		$(MAKE) -C $$dir test || exit 1; \
	done

# Build each example by invoking its Makefile
$(EXAMPLES):
	$(MAKE) -C $@ build

clean:
	for dir in $(EXAMPLES); do \
		$(MAKE) -C $$dir clean; \
	done