ZRC ?= ../../target/debug/zrc
ZRFLAGS ?= 
LDFLAGS ?= -lc
OUTDIR ?= ./out

ZR_SOURCES := $(wildcard *.zr)
ZR_OUTPUTS := $(ZR_SOURCES:%.zr=$(OUTDIR)/%.o)

all: $(ZR_OUTPUTS)

$(OUTDIR)/%.o: %.zr
	@mkdir -p $(OUTDIR)
	$(ZRC) --emit object $(ZRFLAGS) -o $@ $<

.PHONY: build
build: $(ZR_OUTPUTS)
	clang -o $(OUTDIR)/run $(ZR_OUTPUTS) $(LDFLAGS)

.PHONY: clean
clean:
	rm -rf $(OUTDIR)

# Test that the example builds and runs correctly
.PHONY: test
test: build
	@echo "Testing struct construction example..."
	@./$(OUTDIR)/run > $(OUTDIR)/output.txt 2>&1
	@if grep -q "=== Struct Construction Examples ===" $(OUTDIR)/output.txt && \
	    grep -q "Point { x: 0, y: 0 }" $(OUTDIR)/output.txt && \
	    grep -q "Color { r: 255, g: 0, b: 0 }" $(OUTDIR)/output.txt && \
	    grep -q "Distance squared" $(OUTDIR)/output.txt; then \
		echo "✓ Test passed: Example runs correctly"; \
		cat $(OUTDIR)/output.txt; \
		exit 0; \
	else \
		echo "✗ Test failed: Expected output not found"; \
		cat $(OUTDIR)/output.txt; \
		exit 1; \
	fi
