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
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    # fluidPage(
    #   golem::golem_welcome_page() # Remove this line to start building your UI
    # )
    page_fillable(
      id = "page",
      lang = "en",
      fillable_mobile = F,
      #autoWaiter(color="#FFFFFF",html=bs5_spinner()),
      #waiterPreloader(color = "#FFF", html=bs5_spinner()),
      navset_card_tab(
        title = "Main Page",
        id = "nav",
        sidebar = sidebar(
          width = 320,
          padding = 0,
          id = "sidebar",
          open = T,
          title = tags$h1("Selectors", style = "margin: 11px 10px 0px 10px;", class="sidebar-title"),
          accordion(
              class="accordion-flush",
              #width="100"%,
              accordion_panel(
                #width="100"%,
                "Drosophila IDs",
                selectizeInput(
                  inputId = "selector",
                  label = "Select Gene/Protein",
                  choices = NULL,
                  selected = NULL,
                  multiple = T
                )
              )
            )
        ),
        nav_panel(
          "Trajectory",
          layout_columns(
            col_widths = 12,
            row_heights = "75vh",
            card(
              class="border-0",
              tags$h4("Selected Protein Trajectory"),
              plotOutput("trajectory")
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Visualising Trajectories in D. melanogaster"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
