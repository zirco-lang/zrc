MACOSX_DEPLOYMENT_TARGET ?= 13.0

ZRC ?= ../target/debug/zrc
ZIRCOP ?= ../target/debug/zircop
ZRFLAGS ?= -I./include/
ZIRCOPFLAGS ?= -I./include/
OUTDIR ?= ./dist
SRCDIR ?= ./src

ZR_SOURCES ?= $(wildcard $(SRCDIR)/*.zr)
ZR_OUTPUTS ?= $(ZR_SOURCES:$(SRCDIR)/%.zr=$(OUTDIR)/%.o)

# make .so and .a files
all: out $(ZR_OUTPUTS)
	clang -shared -o $(OUTDIR)/libzr.so $(ZR_OUTPUTS)
	ar rcs $(OUTDIR)/libzr.a $(ZR_OUTPUTS)

all-opt: ZRFLAGS += -O3
all-opt: all

$(OUTDIR)/%.o: $(SRCDIR)/%.zr
	$(ZRC) --emit object $(ZRFLAGS) -o $@ $<

.PHONY: out
out:
	@mkdir -p $(OUTDIR)

.PHONY: clean
clean:
	rm -rf $(OUTDIR)

.PHONY: lint
lint:
	@failed=0; \
	for file in $(ZR_SOURCES); do \
	  $(ZIRCOP) $(ZIRCOPFLAGS) $$file || failed=1; \
	done; \
	exit $$failed
