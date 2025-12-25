V ?= 0

ifeq ($(V),1)
Q := 
ECHO := @true
else
Q := @
ECHO := @echo
MAKEFLAGS += --no-print-directory
endif

ZRC ?= ../../target/debug/zrc
ZIRCOP ?= ../../target/debug/zircop
ZRFLAGS ?= -I../../include/
ZIRCOPFLAGS ?= -I../../include/
CC ?= clang
LDFLAGS ?= -lc
OUTDIR ?= ./out

ifeq ($(shell uname),Darwin)
CFLAGS += -mmacosx-version-min=26.0
endif

ZR_SOURCES ?= $(wildcard *.zr)
ZR_OUTPUTS ?= $(ZR_SOURCES:%.zr=$(OUTDIR)/%.o)

all: $(ZR_OUTPUTS)
	$(ECHO) "  CCLD   run"
	$(Q)$(CC) $(CFLAGS) -o $(OUTDIR)/run $(ZR_OUTPUTS) $(LDFLAGS)

$(OUTDIR)/%.o: %.zr
	$(Q)mkdir -p $(OUTDIR)
	$(ECHO) "  ZRC    $<"
	$(Q)$(ZRC) --emit object $(ZRFLAGS) -o $@ $<

.PHONY: clean
clean:
	$(Q)rm -rf $(OUTDIR)

.PHONY: test
test: all
	$(ECHO) -n "  CHECK: "
	$(Q)V=$V ../test_harness.sh $(OUTDIR)/run

.PHONY: lint
lint:
	$(Q)failed=0; \
	for file in $(ZR_SOURCES); do \
	  [ $(V) -eq 0 ] && echo "  ZIRCOP $$file"; \
	  $(ZIRCOP) $(ZIRCOPFLAGS) $$file || failed=1; \
	done; \
	exit $$failed
