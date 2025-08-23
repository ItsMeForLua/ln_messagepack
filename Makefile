
.PHONY: all build test bench clean graphs help install-r-deps

all: build

build:
	@echo "Building ln_messagepack..."
	lake build

test: build
	@echo "Running tests..."
	lake exe test

bench: build
	@echo "Starting benchmarks for ln_messagepack"
	lake exe bench


graphs: benchmark_results.csv
	@echo " Generating plots from benchmark data..."
	@if command -v Rscript >/dev/null 2>&1; then \
		Rscript plot_benchmarks.r; \
	else \
		echo "× Error: R is not installed. Please install R to generate plots."; \
		echo "On Ubuntu/Debian: sudo apt-get install r-base"; \
		echo "On macOS: brew install r"; \
		echo "On Arch: sudo pacman -S r"; \
		exit 1; \
	fi

# Install required R dependencies
install-r-deps:
	@echo " Installing R dependencies..."
	@if command -v Rscript >/dev/null 2>&1; then \
		Rscript -e "if (!require('ggplot2', quietly = TRUE)) install.packages('ggplot2', repos='https://cran.rstudio.com/')"; \
		Rscript -e "if (!require('dplyr', quietly = TRUE)) install.packages('dplyr', repos='https://cran.rstudio.com/')"; \
		Rscript -e "if (!require('tidyr', quietly = TRUE)) install.packages('tidyr', repos='https://cran.rstudio.com/')"; \
		Rscript -e "if (!require('scales', quietly = TRUE)) install.packages('scales', repos='https://cran.rstudio.com/')"; \
		echo "✓ R dependencies installed successfully!"; \
	else \
		echo "× Error: R is not installed. Please install R first."; \
		exit 1; \
	fi

benchmark-suite: bench graphs
	@echo "Complete benchmark suite finished!"
	@echo "CSV data: benchmark_results.csv"
	@echo "Graphs available in: graphs/"

clean:
	@echo "Cleaning build artifacts..."
	lake clean
	rm -f benchmark_results.csv
	rm -rf graphs/

clean-all: clean
	@echo "Cleaning all generated files..."
	rm -f *.csv
	rm -rf graphs/

benchmark_results.csv:
	@echo "× No benchmark data found. Running benchmarks first..."
	$(MAKE) bench

help:
	@echo "ln_messagepack Makefile Commands:"
	@echo ""
	@echo "Building:"
	@echo "  build           - Build the project using Lake"
	@echo "  clean           - Clean build artifacts and benchmark data"
	@echo "  clean-all       - Clean everything including generated plots"
	@echo ""
	@echo "Testing:"
	@echo "  test            - Run test suite"
	@echo ""
	@echo "Benchmarking:"
	@echo "  bench           - Run performance benchmarks (generates CSV)"
	@echo "  graphs          - Generate plots from existing benchmark data"
	@echo "  benchmark-suite - Run benchmarks AND generate graphs"
	@echo ""
	@echo "R Dependencies:"
	@echo "  install-r-deps  - Install required R packages for plotting"
	@echo ""
	@echo "Usage Examples:"
	@echo "  make build                  # Build project"
	@echo "  make benchmark-suite        # Full benchmark with graphs"
	@echo "  make bench && make graphs   # Run benchmarks then create plots"
	@echo "  make install-r-deps         # Install R dependencies first"
	@echo ""
