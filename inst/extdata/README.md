# Expression Data

Place your expression dataset here as `expression_data.rds`.

## Expected format

A long-format `data.frame` with four columns:

| Column       | Type      | Description                              |
|--------------|-----------|------------------------------------------|
| `gene`       | character | Gene / protein identifier                |
| `group`      | character | Biological group (e.g. "male"/"female")  |
| `day`        | numeric   | Timepoint — one of 1, 5, 14, 30, 37     |
| `expression` | numeric   | Expression value (single replicate row)  |

The app computes medians internally across all rows sharing the same
`gene × group × day` combination, so replicate rows are expected.

## Saving the file

```r
saveRDS(your_long_df, "inst/extdata/expression_data.rds")
```

If this file is absent the app falls back to a built-in synthetic dataset
so development can proceed without real data.
