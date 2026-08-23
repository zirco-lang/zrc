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

.PHONY: all test clean lint zrc-only $(EXAMPLES)

all: $(EXAMPLES)

$(EXAMPLES):
	$(ECHO) "  BUILD  $@"
	$(Q)$(MAKE) -C $@ all

test: $(EXAMPLES:%=%_test)
	@echo "All examples passed!"

clean: $(EXAMPLES:%=%_clean)

lint: $(EXAMPLES:%=%_lint)
	@echo "All examples linted and OK!"

zrc-only: $(EXAMPLES:%=%_zrc-only)
	@echo "Evaluated profiles on examples!"

%_test:
	$(ECHO) "  TEST   $*"
	$(Q)$(MAKE) -C $* test

%_clean:
	$(ECHO) "  CLEAN  $*"
	$(Q)$(MAKE) -C $* clean

%_lint:
	$(ECHO) "  LINT   $*"
	$(Q)$(MAKE) -C $* lint

%_zrc-only:
	$(ECHO) "  PROF   $*"
	$(Q)$(MAKE) -C $* zrc-only