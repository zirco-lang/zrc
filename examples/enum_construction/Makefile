enum_construction: main.ll

main.ll: main.zr
	zrc main.zr > main.ll

clean:
	rm -f main.ll out/run

build: main.ll
	mkdir -p out
	clang main.ll -o out/run

test: build
	./out/run
	@echo "✓ Enum construction example completed successfully"

.PHONY: clean build test enum_construction
