ZRC ?= ../../target/debug/zrc
ZRFLAGS ?= 
LDFLAGS ?= -lc
OUTDIR ?= ./out

# Only compile main.zr - point.zr is included via preprocessor
ZR_SOURCES := main.zr
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

.PHONY: test
test: build
	set +e; \
	./$(OUTDIR)/run $$(xargs < test/args.txt) < test/stdin.txt > test/stdout.actual 2> test/stderr.actual; \
	expected_exitcode=`cat test/exitcode.txt`; \
	exitcode=$$?; \
	status=0; \
	if [ $$exitcode -ne $$expected_exitcode ]; then \
		echo "Expected exit code $$expected_exitcode but got $$exitcode"; \
		status=1; \
	fi; \
	diff -u test/stdout.txt test/stdout.actual || (echo "stdout mismatch"; status=1; ); \
	diff -u test/stderr.txt test/stderr.actual || (echo "stderr mismatch"; status=1; ); \
	set -e; \
	rm test/stdout.actual test/stderr.actual; \
	exit $$status
