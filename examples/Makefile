EXAMPLES := $(wildcard */)

.PHONY: all test clean $(EXAMPLES)

all: $(EXAMPLES)

$(EXAMPLES):
	$(MAKE) -C $@

test: $(EXAMPLES:%=%_test)
	@echo "All examples passed!"

clean: $(EXAMPLES:%=%_clean)

lint: $(EXAMPLES:%=%_lint)
	@echo "All examples linted and OK!"

%_test:
	$(MAKE) -C $* test

%_clean:
	$(MAKE) -C $* clean

%_lint:
	$(MAKE) -C $* lint