#!/usr/bin/env bash
# Invoked as ./test_harness.sh $BINARY

set +e

[[ -f test/args.txt ]] && args="$(xargs < test/args.txt)" || args=""

[[ -f test/stdin.txt ]] && stdin_file="test/stdin.txt" || stdin_file="/dev/null"

echo "[harness] running $1 $args < $stdin_file"
$1 $args < $stdin_file > test/stdout.actual 2> test/stderr.actual
exit_code=$?

[[ -f test/expected_exit_code.txt ]] && expected_exit_code=$(<test/expected_exit_code.txt) || expected_exit_code=0

status=0

if [[ $exit_code -ne $expected_exit_code ]]; then
    echo "FAIL: Exit code mismatch: expected $expected_exit_code, got $exit_code"
    status=1
fi

if [[ -f test/stdout.txt ]]; then
    diff -u test/stdout.txt test/stdout.actual
    if [[ $? -ne 0 ]]; then
        echo "FAIL: stdout mismatch"
        status=1
    fi
fi

if [[ -f test/stderr.txt ]]; then
    diff -u test/stderr.txt test/stderr.actual
    if [[ $? -ne 0 ]]; then
        echo "FAIL: stderr mismatch"
        status=1
    fi
fi

[[ -f test/stdout.actual ]] && rm test/stdout.actual
[[ -f test/stderr.actual ]] && rm test/stderr.actual

[[ $status -eq 0 ]] && echo "PASS"

exit $status