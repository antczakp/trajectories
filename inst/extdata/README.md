# Expression Data & Annotation

## expression_data.rds

Place your expression dataset here as `expression_data.rds`.

Long-format `data.frame` with five columns:

| Column       | Type      | Description                                   |
|--------------|-----------|-----------------------------------------------|
| `gene`       | character | Gene / protein identifier                     |
| `type`       | character | Biological group (must match `annot$type`)    |
| `day`        | numeric   | Timepoint — e.g. 1, 5, 14, 30, 37            |
| `organ`      | character | Tissue / organ (must match `annot$organ`)     |
| `expression` | numeric   | Expression value (one row per replicate)      |

The app computes medians internally, so replicate rows are expected.

## annot.rds

Place your sample annotation here as `annot.rds`.

`data.frame` with at minimum these three columns (additional columns are ignored):

| Column  | Type      | Description                          |
|---------|-----------|--------------------------------------|
| `type`  | character | Group / condition label              |
| `day`   | numeric   | Timepoint                            |
| `organ` | character | Tissue / organ                       |

Unique values of each column populate the sidebar selectors at app start.

## Saving the files

```r
saveRDS(your_expr_df,  "inst/extdata/expression_data.rds")
saveRDS(your_annot_df, "inst/extdata/annot.rds")
```

If either file is absent the app falls back to built-in synthetic data.
