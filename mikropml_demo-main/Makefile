.SECONDARY:

raw_data/baxter.% :
	wget https://github.com/riffomonas/raw_data/archive/refs/tags/0.3.zip
	unzip 0.3.zip
	mv raw_data-0.3 raw_data
	rm 0.3.zip

SEEDS = $(shell seq 1 1 100)
L2_GENUS_RDS = $(patsubst %,processed_data/l2_genus_%.Rds,$(SEEDS))

$(L2_GENUS_RDS) : code/run_split.R code/genus_process.R\
																code/l2_genus.R\
																raw_data/baxter.metadata.tsv\
																raw_data/baxter.cons.taxonomy\
																raw_data/baxter.subsample.shared
	./code/run_split.R $@ code/l2_genus.R

processed_data/l2_genus_performance.tsv : code/combine_models.R $(L2_GENUS_RDS)
	$^



L2_GENUS_FIT_RDS = $(patsubst %,processed_data/l2_genus_fit_%.Rds,$(SEEDS))

$(L2_GENUS_FIT_RDS) : code/run_split.R code/genus_process.R\
											code/l2_genus_fit.R\
											raw_data/baxter.metadata.tsv\
											raw_data/baxter.cons.taxonomy\
											raw_data/baxter.subsample.shared
	./code/run_split.R $@ code/l2_genus_fit.R

processed_data/l2_genus_fit_performance.tsv : code/combine_models.R\
											$(L2_GENUS_FIT_RDS)
	$^



RF_GENUS_RDS = $(patsubst %,processed_data/rf_genus_%.Rds,$(SEEDS))

$(RF_GENUS_RDS) : code/run_split.R code/genus_process.R\
																code/rf_genus.R\
																raw_data/baxter.metadata.tsv\
																raw_data/baxter.cons.taxonomy\
																raw_data/baxter.subsample.shared
	./code/run_split.R $@ code/rf_genus.R

processed_data/rf_genus_performance.tsv : code/combine_models.R $(RF_GENUS_RDS)
	$^

RF_GENUS_FIT_RDS = $(patsubst %,processed_data/rf_genus_fit_%.Rds,$(SEEDS))

$(RF_GENUS_FIT_RDS) : code/run_split.R code/genus_process.R\
																code/rf_genus_fit.R\
																raw_data/baxter.metadata.tsv\
																raw_data/baxter.cons.taxonomy\
																raw_data/baxter.subsample.shared
	./code/run_split.R $@ code/rf_genus_fit.R

processed_data/rf_genus_fit_performance.tsv : code/combine_models.R\
										$(RF_GENUS_FIT_RDS)
	$^
