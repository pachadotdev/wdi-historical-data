#' @title UI Dispatcher
#' @noRd
#' @importFrom RPostgres dbGetQuery
#' @import d3po
#' @export
app_server <- function(input, output, session) {
  d_content <- reactive({
    con <- open_con()

    query1 <- sprintf("select broad_topic_tidy from indicators where indicator_code = '%s'", input$indicators_input)
    print(query1)

    d_topic <- dbGetQuery(con, query1)

    query2 <- sprintf(
      "select year, revision, value from %s
      where year between %s and %s
      and indicator_code = '%s'
      and iso3 = '%s'",
      d_topic, min(input$years_input), max(input$years_input), input$indicators_input, input$countries_input
    )
    print(query2)

    d_res <- dbGetQuery(con, query2)

    print(head(d_res))

    close_con(con)

    d_res$color <- "#4299e1"

    if (isTRUE(input$scale_input)) {
      d_res$value <- d_res$value / 1000000000
    }

    return(d_res)
  })

  output$d3po_boxplot <- render_d3po({
    d3po(d_content()) |>
      po_box(daes(x = year, y = value, color = color))
  })

  syncUrl(session)
}

#' @title UI Dispatcher
#' @noRd
#' @importFrom RPostgres dbConnect Postgres dbIsValid dbDisconnect
#' @export
open_con <- function() {
  dbConnect(
    Postgres(),
    user = Sys.getenv("LOCAL_SQL_USR"),
    password = Sys.getenv("LOCAL_SQL_PWD"),
    dbname = "wdi",
    host = "localhost"
  )
}

close_con <- function(con) {
  if (!is.null(con) && dbIsValid(con)) {
    dbDisconnect(con)
  }
}
