#' @title UI Dispatcher
#' @noRd
#' @export
app_ui <- function() {
  page(
    theme = "light",
    color = "azure",
    base = "slate",
    radius = 1,
    layout = "boxed",
    show_theme_button = FALSE,
    title = "WDI Historical Data",
    body = div(
      class = "page-body",
      div(
        class = "container-xl",
        row(
          col12(
            card(
              title = "Controls",
              selectInput("countries_input", "Country", choices = countries_by_region, selected = "GBR"),
              selectInput("indicators_input", "Indicator", choices = indicators_by_topic, selected = NULL),
              sliderInput("years_input", "Years", min = 1960, max = 2025, value = c(2015, 2020), fill = TRUE),
              checkboxInput("scale_input", "Scale to billions", value = FALSE, description = NULL, inline = FALSE)
            ),
            card(
              title = "Output",
              d3po_output("d3po_boxplot", width = "100%", height = "500px")
            )
          )
        )
      )
    ),
    footer = footer(
      left = "Tabler",
      right = tags$span("v1.4.0")
    )
  )
}
