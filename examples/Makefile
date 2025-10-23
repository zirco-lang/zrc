EXAMPLES := $(wildcard */)

.PHONY: all test clean $(EXAMPLES)

all: $(EXAMPLES)

$(EXAMPLES):
	$(MAKE) -C $@

test: $(EXAMPLES:%=%_test)
	@echo "All examples passed!"

clean: $(EXAMPLES:%=%_clean)

%_test:
	$(MAKE) -C $* test

%_clean:
	$(MAKE) -C $* clean
