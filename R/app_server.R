#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @noRd
app_server <- function(input, output, session) {
  expr_data <- load_expression_data()
  annot     <- load_annotation_data()

  # Populate filter selectors from annotation --------------------------------
  types_avail  <- sort(unique(annot$type))
  days_avail   <- sort(unique(annot$day))
  organs_avail <- sort(unique(annot$organ))

  shiny::updateSelectInput(session, "group_a",
    choices  = types_avail,
    selected = "Normal Diet ctrl"
  )
  shiny::updateSelectInput(session, "group_b",
    choices  = types_avail,
    selected = "Normal Diet klf"
  )
  shiny::updateSelectizeInput(session, "sel_days",
    choices  = days_avail,
    selected = days_avail,
    server   = FALSE
  )
  shiny::updateSelectInput(session, "sel_organ",
    choices  = organs_avail,
    selected = "Gut"
  )

  # ---- Reactive: filter expression data by current selector state ----------
  filtered_data_r <- shiny::reactive({
    shiny::req(input$group_a, input$group_b, input$sel_days, input$sel_organ)

    selected_types <- unique(c(input$group_a, input$group_b))
    selected_days  <- as.numeric(input$sel_days)

    ann_sel <- annot %>% filter(
      type %in% selected_types,
      day %in% selected_days,
      organ == input$sel_organ
    )

    shiny::validate(
      shiny::need(nrow(ann_sel) > 0, "No Samples selected - please adjust the filters.")
    )

    data.frame(expr_data[,c("id",ann_sel$ID)])
  })

  # ---- Reactive: parse submitted gene list ---------------------------------
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

  # ---- Reactive: match genes against the (unfiltered) dataset --------------
  # Matching uses the full dataset so that gene names are always found
  # regardless of which filters are active.
  matched_r <- shiny::reactive({
    match_genes(expr_data, gene_list_r())
  })

  # ---- Reactive: compute medians on filtered + matched data ----------------
  median_data_r <- shiny::reactive({
    genes <- matched_r()$matched
    nms <- matched_r()$names
    shiny::validate(
      shiny::need(
        length(genes) > 0,
        "None of the supplied names were found in the dataset."
      )
    )
    data <- filtered_data_r()
    shiny::validate(
      shiny::need(
        nrow(data) > 0,
        "No data available for the current filter selection."
      )
    )
    compute_median_expression(data, genes, annot, nms)
  })

  # ---- Reactive: compute stats on filtered + matched data ----------------
  stat_data_r <- shiny::reactive({
    genes <- matched_r()$matched
    nms <- matched_r()$names
    shiny::validate(
      shiny::need(
        length(genes) > 0,
        "None of the supplied names were found in the dataset."
      )
    )
    data <- filtered_data_r()
    shiny::validate(
      shiny::need(
        nrow(data) > 0,
        "No data available for the current filter selection."
      )
    )
    compute_stats_expression(data, genes, annot, nms)
  })

  # ---- Output: match summary panel -----------------------------------------
  output$match_summary <- shiny::renderUI({
    shiny::req(matched_r())
    m           <- matched_r()
    n_matched   <- length(m$matched)
    n_unmatched <- length(m$unmatched)

    matched_block <- tags$p(
      shiny::icon(if (n_matched > 0) "circle-check" else "circle-xmark"),
      tags$strong(n_matched),
      if (n_matched == 1L) "gene matched." else "genes matched.",
      class = if (n_matched > 0) "text-success mb-1" else "text-danger mb-1"
    )

    unmatched_block <- if (n_unmatched > 0) {
      tags$div(
        class = "mt-2",
        tags$p(
          shiny::icon("triangle-exclamation"),
          tags$strong(n_unmatched),
          if (n_unmatched == 1L) "name not found:" else "names not found:",
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

  # ---- Output: plotly trajectory -------------------------------------------
  output$trajectory_plot <- plotly::renderPlotly({
    df    <- median_data_r()
    .df <<- df
    genes <- sort(unique(df$id))
    types <- sort(unique(df$type))
    colors <- gene_colors(genes)

    # Central tendency selected in the sidebar (median vs mean)
    stat_choice <- if (identical(input$central_stat, "mean")) "mean" else "median"
    stat_col    <- paste0(stat_choice, "_expression")
    stat_label  <- if (stat_choice == "mean") "Mean of standardised" else "Median of standardised"
    df$value    <- df[[stat_col]]

    line_dashes <- c("solid", "dash", "dot", "dashdot")
    type_dash   <- stats::setNames(
      line_dashes[seq_along(types)],
      types
    )

    p <- plotly::plot_ly()

    for (g in genes) {
      for (tp in types) {
        sub <- df[df$id == g & df$type == tp, ]
        if (nrow(sub) == 0L) next

        trace_name <- if (length(types) > 1L) {
          paste0(g, " - ", tp)
        } else {
          g
        }

        # Error bars: SD (symmetric) for the mean, IQR whiskers for the median
        err_y <- if (stat_choice == "median") {
          list(
            type       = "data",
            symmetric  = FALSE,
            array      = sub$q3_expression - sub$value,
            arrayminus = sub$value - sub$q1_expression,
            color      = colors[g],
            thickness  = 1.5,
            width      = 4
          )
        } else {
          list(
            type      = "data",
            array     = sub$sd_expression,
            color     = colors[g],
            thickness = 1.5,
            width     = 4
          )
        }

        p <- plotly::add_trace(
          p,
          data        = sub,
          x           = ~day,
          y           = ~value,
          customdata  = ~datatype,
          type        = "scatter",
          mode        = "lines+markers",
          name        = trace_name,
          legendgroup = g,
          line        = list(
            color = colors[g],
            dash  = type_dash[tp],
            width = 2.5
          ),
          marker = list(
            color = colors[g],
            size  = 7,
            line  = list(color = "white", width = 1)
          ),
          error_y = err_y,
          hovertemplate = paste0(
            "<b>", g, "</b> (", tp, ")<br>",
            "Day %{x}<br>",
            stat_label, ": %{y:.3f}<br>",
            "Data type: %{customdata}<extra></extra>"
          )
        )
      }
    }

    plotly::layout(
      p,
      xaxis = list(
        title     = "Day",
        tickvals  = sort(unique(df$day)),
        gridcolor = "#e8e8e8"
      ),
      yaxis = list(
        title     = paste0(stat_label, " Expression"),
        gridcolor = "#e8e8e8"
      ),
      legend = list(
        title         = list(text = "<b>Gene (click to toggle)</b>"),
        tracegroupgap = 6,
        bgcolor       = "rgba(255,255,255,0.85)"
      ),
      hovermode     = "x unified",
      plot_bgcolor  = "#fafafa",
      paper_bgcolor = "#ffffff"
    )
  })

  # ---- Output: plotly trajectory statistics----------------------------------
  output$trajectory_plot_stat <- plotly::renderPlotly({
    df    <- stat_data_r()
    .df <<- df
    genes <- sort(unique(df$id))
    colors <- gene_colors(genes)

    p <- plotly::plot_ly()

    for (g in genes) {
        sub <- df[df$id == g, ]
        if (nrow(sub) == 0L) next
        trace_name <- g

        # Pack p-value + datatype into a per-point array for the hover
        sub$cd <- lapply(seq_len(nrow(sub)), function(i) {
          list(sub$p_value[i], sub$datatype[i])
        })

        p <- plotly::add_trace(
          p,
          data        = sub,
          x           = ~day,
          y           = ~statistic,
          customdata  = ~cd,
          type        = "scatter",
          mode        = "lines+markers",
          name        = trace_name,
          legendgroup = g,
          line        = list(
            color = colors[g],
            width = 2.5
          ),
          marker = list(
            color = colors[g],
            size  = 7,
            line  = list(color = "white", width = 1)
          ),
          hovertemplate = paste0(
            "<b>", g, "</b><br>",
            "Day %{x}<br>",
            "Statistic: %{y:.3f}<br>",
            "P-value: %{customdata[0]:.3f}<br>",
            "Data type: %{customdata[1]}<extra></extra>"
          )
        )
    }

    plotly::layout(
      p,
      xaxis = list(
        title     = "Day",
        tickvals  = sort(unique(df$day)),
        gridcolor = "#e8e8e8"
      ),
      yaxis = list(
        title     = "Statistics",
        gridcolor = "#e8e8e8"
      ),
      legend = list(
        title         = list(text = "<b>Gene (click to toggle)</b>"),
        tracegroupgap = 6,
        bgcolor       = "rgba(255,255,255,0.85)"
      ),
      hovermode     = "x unified",
      plot_bgcolor  = "#fafafa",
      paper_bgcolor = "#ffffff"
    )
  })

  # ---- Output: expression table --------------------------------------------
  output$expression_table <- DT::renderDataTable({
    value_col <- if (identical(input$central_stat, "mean")) {
      "mean_expression"
    } else {
      "median_expression"
    }
    wide <- build_expression_table(median_data_r(), value_col)

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
