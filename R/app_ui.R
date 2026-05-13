#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import waiter
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page_sidebar(
      title = "D. melanogaster Gene Trajectory Viewer",
      theme = bslib::bs_theme(bootswatch = "flatly", version = 5),

      # ---- Sidebar --------------------------------------------------------
      sidebar = bslib::sidebar(
        width = 320,
        open  = TRUE,

        bslib::card(
          full_screen = FALSE,
          bslib::card_header(shiny::icon("dna"), " Gene / Protein Input"),
          bslib::card_body(
            shiny::fileInput(
              "gene_file",
              "Upload a gene list (CSV or plain-text, one per line):",
              accept      = c(".csv", ".txt", "text/plain", "text/csv"),
              buttonLabel = shiny::icon("folder-open"),
              placeholder = "No file selected"
            ),
            tags$div(
              style = "text-align:center; color:#888; margin: 4px 0 8px;",
              tags$em("— or type genes below —")
            ),
            shiny::textAreaInput(
              "gene_text",
              label       = NULL,
              placeholder = "Act5C\nGAPDH1\nHsp70Aa\n...",
              rows        = 9
            ),
            shiny::actionButton(
              "submit_genes",
              "Load Genes",
              icon  = shiny::icon("magnifying-glass"),
              class = "btn-primary w-100 mt-1"
            )
          )
        ),

        bslib::card(
          full_screen = FALSE,
          bslib::card_header(shiny::icon("circle-info"), " Match Summary"),
          bslib::card_body(
            shiny::uiOutput("match_summary")
          )
        )
      ),

      # ---- Main panel -----------------------------------------------------
      bslib::navset_card_underline(
        title = NULL,

        bslib::nav_panel(
          title = tagList(shiny::icon("chart-line"), " Trajectory Plot"),
          plotly::plotlyOutput("trajectory_plot", height = "560px")
        ),

        bslib::nav_panel(
          title = tagList(shiny::icon("table"), " Expression Table"),
          bslib::card_body(
            tags$p(
              class = "text-muted small mb-2",
              "Median expression per gene × group across all timepoints."
            ),
            DT::dataTableOutput("expression_table")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Visualising Trajectories in D. melanogaster"
    )
  )
}
