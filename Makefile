.PHONY: deps
deps:
	Rscript -e 'renv::restore()'

.PHONY: plan
plan:
	snakemake -n -s Snakefile

.PHONY: run
run:
	snakemake -s Snakefile --cores 8

.PHONY: lint
lint:
	R --vanilla -e 'source("renv/activate.R"); styler::style_dir(".", filetype = "R")'
	snakemake -s Snakefile -c8 --touch