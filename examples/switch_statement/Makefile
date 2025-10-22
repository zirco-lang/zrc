ZRC ?= ../../target/debug/zrc
ZRFLAGS ?= -I../../include/
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

.PHONY: test
test: build
	set +e; \
	if [ -f test/args.txt ]; then args=$$(xargs < test/args.txt); else args=""; fi; \
	if [ -f test/stdin.txt ]; then stdin_file=test/stdin.txt; else stdin_file=/dev/null; fi; \
	./$(OUTDIR)/run $$args < $$stdin_file > test/stdout.actual 2> test/stderr.actual; \
	exitcode=$$?; \
	if [ -f test/exitcode.txt ]; then expected_exitcode=$$(cat test/exitcode.txt); else expected_exitcode=0; fi; \
	status=0; \
	if [ $$exitcode -ne $$expected_exitcode ]; then \
		echo "Expected exit code $$expected_exitcode but got $$exitcode"; \
		status=1; \
	fi; \
	if [ -f test/stdout.txt ]; then \
		diff -u test/stdout.txt test/stdout.actual || { echo "stdout mismatch"; status=1; }; \
	fi; \
	if [ -f test/stderr.txt ]; then \
		diff -u test/stderr.txt test/stderr.actual || { echo "stderr mismatch"; status=1; }; \
	fi; \
	set -e; \
	rm test/stdout.actual test/stderr.actual; \
	exit $$status