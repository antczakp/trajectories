#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Load dataset once per session (falls back to synthetic data if no RDS
  # file is found in inst/extdata/)
  expr_data <- load_expression_data()

  # ---- Reactive: parse submitted gene list --------------------------------
  gene_list_r <- shiny::eventReactive(input$submit_genes, {
    genes <- parse_gene_input(
      file_path  = input$gene_file$datapath,
      text_input = input$gene_text
    )
    shiny::validate(
      shiny::need(length(genes) > 0, "Please enter at least one gene name.")
    )
    genes
  })

  # ---- Reactive: match genes against dataset ------------------------------
  matched_r <- shiny::reactive({
    match_genes(expr_data, gene_list_r())
  })

  # ---- Reactive: compute medians for matched genes ------------------------
  median_data_r <- shiny::reactive({
    genes <- matched_r()$matched
    shiny::validate(
      shiny::need(
        length(genes) > 0,
        "None of the supplied names were found in the dataset."
      )
    )
    compute_median_expression(expr_data, genes)
  })

  # ---- Output: match summary panel ----------------------------------------
  output$match_summary <- shiny::renderUI({
    shiny::req(matched_r())
    m          <- matched_r()
    n_matched  <- length(m$matched)
    n_unmatched <- length(m$unmatched)

    matched_block <- tags$p(
      shiny::icon(if (n_matched > 0) "circle-check" else "circle-xmark"),
      tags$strong(n_matched),
      if (n_matched == 1) "gene matched." else "genes matched.",
      class = if (n_matched > 0) "text-success mb-1" else "text-danger mb-1"
    )

    unmatched_block <- if (n_unmatched > 0) {
      tags$div(
        class = "mt-2",
        tags$p(
          shiny::icon("triangle-exclamation"),
          tags$strong(n_unmatched),
          if (n_unmatched == 1) "name not found:" else "names not found:",
          class = "text-warning mb-1"
        ),
        tags$p(
          class = "text-muted small",
          paste(m$unmatched, collapse = ", ")
        )
      )
    }

    tagList(matched_block, unmatched_block)
  })

  # ---- Output: plotly trajectory ------------------------------------------
  output$trajectory_plot <- plotly::renderPlotly({
    df     <- median_data_r()
    genes  <- sort(unique(df$gene))
    groups <- sort(unique(df$group))
    colors <- gene_colors(genes)

    line_dashes <- c("solid", "dash", "dot", "dashdot")
    group_dash  <- stats::setNames(
      line_dashes[seq_along(groups)],
      groups
    )

    p <- plotly::plot_ly()

    for (g in genes) {
      for (grp in groups) {
        sub <- df[df$gene == g & df$group == grp, ]
        if (nrow(sub) == 0L) next

        trace_name <- if (length(groups) > 1L) {
          paste0(g, " – ", grp)
        } else {
          g
        }

        p <- plotly::add_trace(
          p,
          data        = sub,
          x           = ~day,
          y           = ~median_expression,
          type        = "scatter",
          mode        = "lines+markers",
          name        = trace_name,
          legendgroup = g,
          line        = list(
            color = colors[g],
            dash  = group_dash[grp],
            width = 2.5
          ),
          marker = list(
            color = colors[g],
            size  = 7,
            line  = list(color = "white", width = 1)
          ),
          hovertemplate = paste0(
            "<b>", g, "</b> (", grp, ")<br>",
            "Day %{x}<br>",
            "Median: %{y:.3f}<extra></extra>"
          )
        )
      }
    }

    plotly::layout(
      p,
      xaxis = list(
        title     = "Day",
        tickvals  = c(1, 5, 14, 30, 37),
        ticktext  = c("1", "5", "14", "30", "37"),
        gridcolor = "#e8e8e8"
      ),
      yaxis = list(
        title     = "Median Expression",
        gridcolor = "#e8e8e8"
      ),
      legend = list(
        title         = list(text = "<b>Gene (click to toggle)</b>"),
        tracegroupgap = 6,
        bgcolor       = "rgba(255,255,255,0.85)"
      ),
      hovermode  = "x unified",
      plot_bgcolor  = "#fafafa",
      paper_bgcolor = "#ffffff"
    )
  })

  # ---- Output: expression table -------------------------------------------
  output$expression_table <- DT::renderDataTable({
    wide <- build_expression_table(median_data_r())

    DT::datatable(
      wide,
      rownames  = FALSE,
      selection = "none",
      options   = list(
        pageLength = 20,
        scrollX    = TRUE,
        dom        = "ftp",
        columnDefs = list(
          list(className = "dt-left",  targets = 0),
          list(className = "dt-right", targets = seq_len(ncol(wide) - 1))
        )
      ),
      class = "stripe hover compact"
    ) |>
      DT::formatRound(columns = seq_len(ncol(wide))[-1], digits = 3)
  })
}
