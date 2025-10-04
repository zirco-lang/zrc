.PHONY: build test clean

build:
	../../target/debug/zrc -o ./out/main.o --emit object main.zr
	clang -o ./out/run ./out/main.o -lc

test: build
	set +e; \
	././out/run $$(xargs < test/args.txt) < test/stdin.txt > test/stdout.actual 2> test/stderr.actual; \
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

clean:
	rm -f ./out/*
