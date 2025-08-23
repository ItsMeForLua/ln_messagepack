LEAN ?= lake
SRC_DIR ?= ln_messagepack

build:
	@echo "Building ln_messagepack"
	@$(LEAN) build

bld: build

tests:
	@echo "Starting tests for ln_messagepack"
	@$(LEAN) exec tests

tst: tests

bench:
	@echo "Starting benchmarks for ln_messagepack"
	@$(LEAN) exec bench >> logbench.txt

bnc: bench

clean:
	@echo "Running lake clean..."
	@$(LEAN) clean
	@echo "Done"

cln: clean

backup:
	@echo "Running backup for ln_messagepack"
	@echo "Copying project to ~/.backups"
	@$ cp ../ln_messagepack -r ~/.backups/ln_messagepack

bcp: backup