#' Load expression dataset
#'
#' Reads inst/extdata/expression_data.rds when present; falls back to a
#' synthetic demo dataset so the app runs out of the box.
#'
#' Expected format (long): columns gene, type, day, organ, expression.
#'
#' @noRd
load_expression_data <- function() {
  data_path <- app_sys("extdata", "Dataset.rds")
  if (file.exists(data_path)) {
    readRDS(data_path)
  } else {
    generate_sample_data()
  }
}

#' Load annotation file
#'
#' Reads inst/extdata/annot.rds when present; falls back to a synthetic
#' annotation derived from the demo expression data.
#'
#' Expected format: columns type, day, organ (one row per sample or per
#' unique combination).
#'
#' @noRd
load_annotation_data <- function() {
  annot_path <- app_sys("extdata", "Annotation.rds")
  if (file.exists(annot_path)) {
    readRDS(annot_path)
  } else {
    generate_sample_annotation()
  }
}

#' Generate synthetic expression data for demo / development
#' @noRd
generate_sample_data <- function() {
  set.seed(42)

  genes <- c(
    "Act5C", "GAPDH1",  "Hsp70Aa", "Akt1",    "InR",
    "foxo",  "S6k",     "thor",    "Sir2",    "Sod2",
    "Cat",   "Jafrac1", "CG9040",  "CG5931",  "CG8517",
    "eIF4E", "raptor",  "Atg1",    "Atg8a",   "Ref(2)P",
    "p62",   "dTOR",    "Rheb",    "TSC1",    "TSC2"
  )
  types  <- c("control", "mutant_A", "mutant_B")
  days   <- c(1, 5, 14, 30, 37)
  organs <- c("head", "gut", "body")
  n_reps <- 4

  grid <- expand.grid(
    gene      = genes,
    type      = types,
    day       = days,
    organ     = organs,
    replicate = seq_len(n_reps),
    stringsAsFactors = FALSE
  )

  baselines <- stats::setNames(stats::runif(length(genes), 3, 10), genes)

  grid$expression <- vapply(seq_len(nrow(grid)), function(i) {
    base         <- baselines[grid$gene[i]]
    trend        <- grid$day[i] * stats::runif(1, -0.05, 0.10)
    type_effect  <- switch(grid$type[i],
      mutant_A = stats::runif(1,  0.3, 1.0),
      mutant_B = stats::runif(1, -1.0, 0.0),
      0
    )
    organ_effect <- switch(grid$organ[i],
      head = stats::runif(1, 0.0, 0.5),
      gut  = stats::runif(1, -0.5, 0.0),
      0
    )
    noise <- stats::rnorm(1, 0, 0.8)
    max(0, base + trend + type_effect + organ_effect + noise)
  }, numeric(1))

  grid[, c("gene", "type", "day", "organ", "expression")]
}

#' Generate synthetic annotation for demo / development
#' @noRd
generate_sample_annotation <- function() {
  expand.grid(
    type   = c("control", "mutant_A", "mutant_B"),
    day    = c(1, 5, 14, 30, 37),
    organ  = c("head", "gut", "body"),
    stringsAsFactors = FALSE
  )
}

#' Parse gene/protein names from a file upload and/or a text area
#'
#' @param file_path Path to an uploaded file (CSV or plain-text, one gene per
#'   line). May be NULL.
#' @param text_input Character scalar from a textAreaInput.
#'
#' @return Character vector of unique, trimmed gene names.
#' @noRd
parse_gene_input <- function(file_path = NULL, text_input = "") {
  genes <- character(0)

  if (!is.null(file_path) && file.exists(file_path)) {
    ext <- tolower(tools::file_ext(file_path))
    if (ext == "csv") {
      tbl      <- utils::read.csv(file_path, stringsAsFactors = FALSE)
      gene_col <- if ("gene" %in% tolower(names(tbl))) {
        names(tbl)[tolower(names(tbl)) == "gene"][1]
      } else {
        names(tbl)[1]
      }
      genes <- c(genes, as.character(tbl[[gene_col]]))
    } else {
      genes <- c(genes, readLines(file_path, warn = FALSE))
    }
  }

  if (nzchar(trimws(text_input))) {
    genes <- c(genes, strsplit(text_input, "\n")[[1]])
  }

  unique(trimws(genes[nzchar(trimws(genes))]))
}

#' Match a user-supplied gene list against the dataset
#'
#' Matching is case-insensitive; the original capitalisation from the dataset
#' is preserved in the result.
#'
#' @param data  Long-format expression data frame.
#' @param gene_list Character vector of gene names to look up.
#' @import gprofiler2
#'
#' @return A list with elements \code{matched} and \code{unmatched}.
#' @noRd
match_genes <- function(data, gene_list) {
  available   <- unique(data$id)
  matched <- gprofiler2::gconvert(gene_list,organism="dmelanogaster")
  query <- unique(matched$target)
  upper_avail <- toupper(available)
  upper_query <- toupper(trimws(query))

  matched   <- available[upper_avail %in% upper_query]
  unmatched <- gene_list[!upper_query %in% upper_avail]

  list(matched = matched, unmatched = unmatched)
}

#' Compute per-gene median expression
#'
#' @param data  Long-format expression data frame (pre-filtered to the desired
#'   types, days, and organ).
#' @param genes Character vector of gene names (already matched).
#' @import dplyr
#' @import tidyr
#'
#' @return Data frame with columns gene, type, day, median_expression.
#' @noRd
compute_median_expression <- function(data, genes, annot) {
  df <- data[data$id %in% genes, ]
  df %>%
    tidyr::pivot_longer(-id,names_to="ID",values_to="expression") %>%
    left_join(annot) %>%
    group_by(id,type,day) %>%
    summarise(
      median_expression = stats::median(expression, na.rm=T)
    )
}

#' Pivot median data to a wide table suitable for DT display
#'
#' Rows = gene; columns = \code{<type> – Day <day>}.
#'
#' @param median_data Output of \code{compute_median_expression()}.
#' @noRd
build_expression_table <- function(median_data) {
  days  <- sort(unique(median_data$day))
  types <- sort(unique(median_data$type))
  genes <- sort(unique(median_data$id))

  result <- data.frame(Gene = genes, stringsAsFactors = FALSE)

  for (tp in types) {
    for (d in days) {
      col_name <- paste0(tp, " – Day ", d)
      vals <- vapply(genes, function(g) {
        v <- median_data$median_expression[
          median_data$id == g &
          median_data$type  == tp &
          median_data$day   == d
        ]
        if (length(v) == 0L) NA_real_ else round(v[1], 3)
      }, numeric(1))
      result[[col_name]] <- vals
    }
  }

  result
}

#' Fixed qualitative colour palette mapped to gene names
#' @noRd
gene_colors <- function(genes) {
  palette <- c(
    "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
    "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF",
    "#AEC7E8", "#FFBB78", "#98DF8A", "#FF9896", "#C5B0D5",
    "#F7B6D2", "#C49C94", "#DBD8E3", "#B5CF6B", "#6BAED6"
  )
  n <- length(genes)
  cols <- if (n <= length(palette)) {
    palette[seq_len(n)]
  } else {
    grDevices::hcl.colors(n, palette = "Dark 2")
  }
  stats::setNames(cols, genes)
}
