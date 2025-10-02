ZRC ?= ../../target/debug/zrc
ZRFLAGS ?= 
OUTDIR ?= ./out

ZR_SOURCES := $(wildcard *.zr)

.PHONY: all build clean test

# For now, we only test AST parsing since type checker is not yet implemented
all: test

build:
	@echo "Note: Full compilation not yet supported (type checker implementation pending)"
	@echo "Use 'make test' to verify parser functionality"

clean:
	rm -rf $(OUTDIR)

# Test that the parser correctly handles struct construction syntax
test:
	@echo "Testing struct construction parser..."
	@mkdir -p $(OUTDIR)
	@$(ZRC) --emit ast main.zr > $(OUTDIR)/ast_output.txt 2>&1
	@if grep -q "new Point { x: (0), y: (0) }" $(OUTDIR)/ast_output.txt && \
	    grep -q "new Color { r: (255), g: (0), b: (0) }" $(OUTDIR)/ast_output.txt; then \
		echo "✓ Parser test passed: All struct construction patterns parsed correctly"; \
		exit 0; \
	else \
		echo "✗ Parser test failed: Expected struct construction patterns not found in AST"; \
		cat $(OUTDIR)/ast_output.txt; \
		exit 1; \
	fi
