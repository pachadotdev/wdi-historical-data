lapply(
  c("data.table", "RPostgres"),
  function(x) {
    if (!requireNamespace(x, quietly = TRUE)) install.packages(x, repos = "https://cran.r-project.org")
  }
)

library(data.table)
library(RPostgres)

con <- dbConnect(
  Postgres(),
  user = Sys.getenv("LOCAL_SQL_USR"),
  password = Sys.getenv("LOCAL_SQL_PWD"),
  dbname = "wdi",
  host = "localhost"
)

topics <- unlist(dbGetQuery(con, "select distinct broad_topic_tidy from indicators"))
name_topics <- unlist(dbGetQuery(con, "select distinct broad_topic from indicators"))

indicators_by_topic <- list()

for (i in seq_along(topics)) {
  indicators_by_topic[[i]] <- unlist(dbGetQuery(con,
    sprintf("select indicator_code from indicators where broad_topic_tidy = '%s'", topics[i])))

  names(indicators_by_topic[[i]]) <- paste(
    unlist(dbGetQuery(con,
      sprintf("select indicator_name from indicators where broad_topic_tidy = '%s'", topics[i]))),
    unlist(dbGetQuery(con,
      sprintf("select indicator_code from indicators where broad_topic_tidy = '%s'", topics[i]))),
      sep = " - "
  )
}

names(indicators_by_topic) <- name_topics

regions <- unlist(dbGetQuery(con, "select distinct region from countries"))

countries_by_region <- list()

for (i in seq_along(regions)) {
  countries_by_region[[i]] <- unlist(dbGetQuery(con,
    sprintf("select iso3 from countries where region = '%s'", regions[i])))

  names(countries_by_region[[i]]) <- unlist(dbGetQuery(con,
    sprintf("select country_name from countries where region = '%s'", regions[i])))
}

names(countries_by_region) <- regions

usethis::use_data(indicators_by_topic, overwrite = T)
usethis::use_data(countries_by_region, overwrite = T)

dbDisconnect(con)
