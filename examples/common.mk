V ?= 0

ifeq ($(V),1)
Q := 
ECHO := @true
else
Q := @
ECHO := @echo
MAKEFLAGS += --no-print-directory
endif

LIBZR_DIR ?= ../../libzr/dist/
ZRC ?= ../../target/debug/zrc
ZIRCOP ?= ../../target/debug/zircop
ZRFLAGS ?= -I../../include/
ZIRCOPFLAGS ?= -I../../include/
CC ?= clang
LDFLAGS ?= -lc $(LIBZR_DIR)/libzr.a
OUTDIR ?= ./out

ifeq ($(shell uname),Darwin)
CFLAGS += -mmacosx-version-min=26.0
endif

ZR_SOURCES ?= $(wildcard *.zr)
ZR_OUTPUTS ?= $(ZR_SOURCES:%.zr=$(OUTDIR)/%.o)

libzr: $(LIBZR_DIR)/libzr.a
$(LIBZR_DIR)/libzr.a:
	@echo "Please build libzr first in $(LIBZR_DIR)"
	@exit 1

all: $(OUTDIR)/run

$(OUTDIR)/run: libzr $(ZR_OUTPUTS)
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
test: $(OUTDIR)/run
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
