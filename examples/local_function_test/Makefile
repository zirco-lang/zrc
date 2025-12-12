.PHONY: build clean test run

OUTDIR=./target
MAINFILE=main.zr
OUTFILE=$(OUTDIR)/run
ZRC=../../target/release/zrc
INCLUDE=-I ../../include

build:
	@mkdir -p $(OUTDIR)
	$(ZRC) $(MAINFILE) --emit object -o $(OUTDIR)/main.o $(INCLUDE) -O0
	gcc $(OUTDIR)/main.o -o $(OUTFILE)

run: build
	$(OUTFILE)

clean:
	rm -rf $(OUTDIR)

.PHONY: test
test: build
	set +e; \
	if [ -f test/args.txt ]; then args=$$(xargs < test/args.txt); else args=""; fi; \
	if [ -f test/stdin.txt ]; then stdin_file=test/stdin.txt; else stdin_file=/dev/null; fi; \
	./$(OUTDIR)/run $$args < $$stdin_file > test/stdout.actual 2> test/stderr.actual; \
	if [ -f test/exitcode.txt ]; then expected_exitcode=$$(cat test/exitcode.txt); else expected_exitcode=0; fi; \
	exitcode=$$?; \
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
