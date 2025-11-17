MACOSX_DEPLOYMENT_TARGET ?= 13.0

ZRC ?= ../../target/debug/zrc
ZIRCOP ?= ../../target/debug/zircop
ZRFLAGS ?= -I../../include/
ZIRCOPFLAGS ?= -I../../include/
LDFLAGS ?= -lc
OUTDIR ?= ./out

ZR_SOURCES ?= $(wildcard *.zr)
ZR_OUTPUTS ?= $(ZR_SOURCES:%.zr=$(OUTDIR)/%.o)

all: $(ZR_OUTPUTS)
	clang -o $(OUTDIR)/run $(ZR_OUTPUTS) $(LDFLAGS)

$(OUTDIR)/%.o: %.zr
	@mkdir -p $(OUTDIR)
	$(ZRC) --emit object $(ZRFLAGS) -o $@ $<

.PHONY: clean
clean:
	rm -rf $(OUTDIR)

.PHONY: test
test: all
	../test_harness.sh $(OUTDIR)/run

.PHONY: lint
lint:
	@failed=0; \
	for file in $(ZR_SOURCES); do \
	  $(ZIRCOP) $(ZIRCOPFLAGS) $$file || failed=1; \
	done; \
	exit $$failed