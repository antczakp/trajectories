# Run this script once to persist the synthetic demo dataset so it is
# bundled with the installed package.  Requires the package to be loaded
# (devtools::load_all()) so that generate_sample_data() is available.

sample_data <- Shiny:::generate_sample_data()
saveRDS(sample_data, "inst/extdata/expression_data.rds")
message("Saved inst/extdata/expression_data.rds (", nrow(sample_data), " rows)")
