# Run once (after devtools::load_all()) to persist synthetic demo data.

sample_expr  <- Shiny:::generate_sample_data()
sample_annot <- Shiny:::generate_sample_annotation()

saveRDS(sample_expr,  "inst/extdata/expression_data.rds")
saveRDS(sample_annot, "inst/extdata/annot.rds")

message("Saved expression_data.rds (", nrow(sample_expr), " rows)")
message("Saved annot.rds (", nrow(sample_annot), " rows)")
