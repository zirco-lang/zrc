V ?= 0

ifeq ($(V),1)
Q := 
ECHO := @true
else
Q := @
ECHO := @echo
MAKEFLAGS += --no-print-directory
endif

ZRC ?= ../target/debug/zrc
ZIRCOP ?= ../target/debug/zircop
ZRFLAGS ?= -I./include/ -I../include/
ZIRCOPFLAGS ?= -I./include/ -I../include/
CC ?= clang
OUTDIR ?= ./dist
SRCDIR ?= ./src

ifeq ($(shell uname),Darwin)
CFLAGS += -mmacosx-version-min=26.0
endif

ZR_SOURCES ?= $(wildcard $(SRCDIR)/*.zr) $(wildcard $(SRCDIR)/math/*.zr)
ZR_OUTPUTS ?= $(ZR_SOURCES:$(SRCDIR)/%.zr=$(OUTDIR)/%.o)

C_SOURCES ?= $(wildcard $(SRCDIR)/*.c) $(wildcard $(SRCDIR)/math/*.c)
C_OUTPUTS ?= $(C_SOURCES:$(SRCDIR)/%.c=$(OUTDIR)/%.o)

# make .so and .a files
all: out $(ZR_OUTPUTS) $(C_OUTPUTS)
	$(ECHO) "  CCLD   libzr.so"
	$(Q)$(CC) $(CFLAGS) -shared -o $(OUTDIR)/libzr.so $(ZR_OUTPUTS) $(C_OUTPUTS)
	$(ECHO) "  AR     libzr.a"
	$(Q)ar rcs $(OUTDIR)/libzr.a $(ZR_OUTPUTS) $(C_OUTPUTS)

all-opt: ZRFLAGS += -O3
all-opt: all

$(OUTDIR)/%.o: $(SRCDIR)/%.zr
	$(ECHO) "  ZRC    $<"
	$(Q)$(ZRC) --emit object $(ZRFLAGS) -o $@ $<

$(OUTDIR)/%.o: $(SRCDIR)/%.c
	$(ECHO) "  CC     $<"
	$(Q)$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: out
out:
	$(Q)mkdir -p $(OUTDIR)
	$(Q)mkdir -p $(OUTDIR)/math

.PHONY: clean
clean:
	$(ECHO) "  CLEAN  out"
	$(Q)rm -rf $(OUTDIR)

.PHONY: lint
lint:
	$(Q)failed=0; \
	for file in $(ZR_SOURCES); do \
	  [ $(V) -eq 0 ] && echo "  ZIRCOP $$file"; \
	  $(ZIRCOP) $(ZIRCOPFLAGS) $$file || failed=1; \
	done; \
	exit $$failed
