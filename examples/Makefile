EXAMPLES := $(wildcard */)

V ?= 0

ifeq ($(V),1)
Q := 
ECHO := @true
else
Q := @
ECHO := @echo
MAKEFLAGS += --no-print-directory
endif

.PHONY: all test clean $(EXAMPLES)

all: $(EXAMPLES)

$(EXAMPLES):
	$(ECHO) "  BUILD  $@"
	$(Q)$(MAKE) -C $@

test: $(EXAMPLES:%=%_test)
	@echo "All examples passed!"

clean: $(EXAMPLES:%=%_clean)

lint: $(EXAMPLES:%=%_lint)
	@echo "All examples linted and OK!"

%_test:
	$(ECHO) "  TEST   $*"
	$(Q)$(MAKE) -C $* test

%_clean:
	$(ECHO) "  CLEAN  $*"
	$(Q)$(MAKE) -C $* clean

%_lint:
	$(ECHO) "  ZIRCOP $*"
	$(Q)$(MAKE) -C $* lint
