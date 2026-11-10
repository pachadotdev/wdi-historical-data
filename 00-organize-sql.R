lapply(
  c("data.table", "RPostgres", "rvest", "wbstats", "janitor"),
  function(x) {
    if (!requireNamespace(x, quietly = TRUE)) install.packages(x, repos = "https://cran.r-project.org")
  }
)

library(data.table)
library(RPostgres)
library(rvest)
library(wbstats)

con <- dbConnect(
  Postgres(),
  user = Sys.getenv("LOCAL_SQL_USR"),
  password = Sys.getenv("LOCAL_SQL_PWD"),
  dbname = "wdi",
  host = "localhost"
)

# dbSendQuery(con, "drop table wdi_raw")

# See https://datatopics.worldbank.org/world-development-indicators/wdi-archives.html
# links of the form
# https://databank.worldbank.org/data/download/Archive/WDI_excel_2023_12_18.zip

fhistorical <- "WDI-links.txt"

if (!file.exists(fhistorical)) {
  historical <- read_html("https://datatopics.worldbank.org/world-development-indicators/wdi-archives.html")
  historical <- html_attr(html_nodes(historical, "a"), "href")
  historical <- grep("Archive/WDI|archive/WDI", historical, value = TRUE)
  writeLines(historical, fhistorical)
} else{
  historical <- readLines(fhistorical)
}

historical_dates <- gsub(".*_excel_", "", historical)
historical_dates <- gsub(".*WDI_", "", historical_dates)
historical_dates <- gsub("\\.zip", "", historical_dates)
historical_dates <- substr(historical_dates, 1, 7)
historical_dates <- gsub("_", "-", historical_dates)
historical_dates <- paste0(historical_dates, "-01")
historical_dates <- as.Date(historical_dates)

historical <- data.table(
  link = historical,
  date = historical_dates
)

setorder(historical, -date)

historical <- historical$link

for (i in seq_along(historical)) {
  # i = 1
  
  fzip <- file.path("data", basename(historical[i]))
  message(fzip)

  rev <- as.integer(substr(gsub("[A-z]|[a-z]|[[:punct:]]", "", fzip), 1, 6))

  if (!("processed_revisions" %in% dbListTables(con))) {
    dbExecute(con, "create table processed_revisions (revision integer primary key)")
  }

  d <- nrow(dbGetQuery(con, "select revision from processed_revisions where revision = $1 limit 1", params = list(rev)))

  if (d > 0) {
    message(paste("skipping", rev, "(already processed)"))
    next
  }
  
  dzip <- gsub("\\.zip", "", fzip)

  # See https://datatopics.worldbank.org/world-development-indicators/wdi-archives.html

  if (!file.exists(fzip)) {
    download.file(historical[i], destfile = fzip, mode = "wb")
  }

  if (!file.exists(dzip)) {
    unzip(fzip, exdir = dzip)
  }

  finp <- list.files(dzip, full.names = TRUE)
  csv <- FALSE
  gdf <- any(grepl("GDF", finp))

  if (length(finp) != 1L && isFALSE(gdf)) {
    finp <- grep("WDIData|WDIdata", finp, value = TRUE)
    csv <- TRUE
  } else {
    if (isTRUE(gdf)) {
      finp <- grep("GDF", finp, value = TRUE)
    } else {
      finp <- grep("WDI", finp, value = TRUE)
    }
  }

  y <- as.integer(substr(rev, 1, 4))

  if (isFALSE(csv) && isFALSE(gdf)) {
    if (y <= 2016) {
      wdi_raw <- setDT(readxl::read_xlsx(finp, sheet = "Data"))
    } else {
      wdi_raw <- setDT(readxl::read_xlsx(finp))
    }
  } else if (isTRUE(gdf)) {
    sht <- grep("Data|WDI_GDF_Data", readxl::excel_sheets(finp), value = TRUE)
    wdi_raw <- setDT(readxl::read_xlsx(finp, sheet = sht, n_max = 1))
    wdi_cols <- ncol(wdi_raw)
    wdi_raw <- setDT(readxl::read_xlsx(finp, sheet = sht, col_types = c(rep("text", 4), rep("numeric", wdi_cols - 4))))
  } else {
    wdi_raw <- fread(finp, header = TRUE)
  }

  if (i == 1L) {
    indicators <- setDT(readxl::read_xlsx(finp, sheet = "Series"))
    colnames(indicators) <- janitor::make_clean_names(colnames(indicators))

    wdi_topics <- unique(indicators$topic)
    wdi_broad_topic <- unique(gsub("\\:.*", "", wdi_topics))
    wdi_broad_topic <- tolower(gsub("\\&", "and", wdi_broad_topic))
    wdi_broad_topic <- gsub("\\s+", "_", wdi_broad_topic)
    wdi_broad_topic <- ifelse(wdi_broad_topic == "na", "uncategorized", wdi_broad_topic)

    indicators$broad_topic <- gsub("\\:.*", "", indicators$topic)
    indicators$broad_topic_tidy <- tolower(gsub("\\&", "and", indicators$broad_topic))
    indicators$broad_topic_tidy <- gsub("\\s+", "_", indicators$broad_topic_tidy)

    indicators$broad_topic <- ifelse(indicators$broad_topic == "NA", "Uncategorized", indicators$broad_topic)
    indicators$broad_topic_tidy <- ifelse(indicators$broad_topic_tidy == "na", "uncategorized", indicators$broad_topic_tidy)

    dbWriteTable(con, "indicators", indicators, overwrite = TRUE)

    countries <- setDT(readxl::read_excel(finp, sheet = "Country"))
    colnames(countries) <- janitor::make_clean_names(colnames(countries))
    countries <- countries[, .(country_code, short_name, region, income_group)]
    setnames(countries, c("country_code", "short_name"), c("iso3", "country_name"))
    countries <- countries[!is.na(region)]

    dbWriteTable(con, "countries", countries, overwrite = TRUE)
  } else {
    indicators <- setDT(dbGetQuery(con, "select * from indicators"))
    countries <- setDT(dbGetQuery(con, "select * from countries"))
    wdi_broad_topic <- unique(indicators$broad_topic_tidy)
  }

  colnames(wdi_raw) <- janitor::make_clean_names(colnames(wdi_raw))

  # year_cols <- paste0("x", 1960:2012)
  year_cols <- grep("^[x]", colnames(wdi_raw), value = TRUE)

  if (any("series_code" %in% colnames(wdi_raw))) {
    wdi_raw <- wdi_raw[, c("country_code", "series_code", year_cols), with = FALSE]
    setnames(wdi_raw, "series_code", "indicator_code")
  } else {
    wdi_raw <- wdi_raw[, c("country_code", "indicator_code", year_cols), with = FALSE]
  }

  wdi_raw <- melt(
    wdi_raw,
    id.vars = c("country_code", "indicator_code"),
    measure.vars = year_cols,
    variable.name = "year",
    value.name = "value"
  )

  wdi_raw[, value := suppressWarnings(as.numeric(value))]

  wdi_raw[, year := as.integer(gsub("x", "", year))]

  setnames(wdi_raw, "country_code", "iso3")

  wdi_raw[, revision := rev]

  setcolorder(wdi_raw, "revision", after = "year")

  wdi_raw <- wdi_raw[!is.na(value)]
  wdi_raw <- wdi_raw[iso3 %in% countries$iso3]
  
  print(wdi_raw)

  for (tpc in wdi_broad_topic) {
    print(tpc)
    idc <- indicators[broad_topic_tidy == tpc]
    idc <- idc$series_code
    dbWriteTable(con, tpc, wdi_raw[indicator_code %in% idc], append = TRUE, overwrite = FALSE, row.names = FALSE)
  }

  rm(wdi_raw)

  dbExecute(con, "insert into processed_revisions (revision) values ($1) on conflict do nothing", params = list(rev))

  gc()
}

tbls <- dbListTables(con)
tbls <- tbls[!(tbls %in% c("indicators", "countries", "processed_revisions"))]

for (tpc in tbls) {
  message(paste("adding indexes for", tpc))
  dbExecute(con, sprintf("create index if not exists %s_idx_iso3 on %s (iso3)", tpc, tpc))
  dbExecute(con, sprintf("create index if not exists %s_idx_year on %s (year)", tpc, tpc))
  dbExecute(con, sprintf("create index if not exists %s_idx_indicator on %s (indicator_code)", tpc, tpc))
  # dbExecute(con, sprintf("create index if not exists %s_idx_revision on %s (revision)", tpc, tpc))

  # has_uq <- nrow(dbGetQuery(con, "select 1 from pg_constraint where conname = $1", params = list(paste0(tpc, "_uq")))) > 0

  # if (!has_uq) {
  #   dbExecute(con, sprintf("alter table %s add constraint %s_uq unique (revision, iso3, indicator_code, year)", tpc, tpc))
  # }
}

# indicators_indexes <- dbGetQuery(con, "select indexname from pg_indexes where tablename = 'indicators'")$indexname

# if (!("indicators_idx_broad_topic" %in% indicators_indexes)) {
#   dbExecute(con, "create index indicators_idx_broad_topic on indicators (broad_topic_tidy)")
# }

# if (!("indicators_idx_series_code" %in% indicators_indexes)) {
#   dbExecute(con, "create index indicators_idx_series_code on indicators (series_code)")
# }

# indicators_cols <- dbGetQuery(con, "select column_name from information_schema.columns where table_name = 'indicators'")$column_name

# if ("series_code" %in% indicators_cols && !("indicator_code" %in% indicators_cols)) {
#   dbExecute(con, "alter table indicators rename column series_code TO indicator_code")
# }

# has_series_uq <- nrow(dbGetQuery(con, "select 1 from pg_constraint where conname = 'uq_series_code'")) > 0

# if (!has_series_uq) {
#   dbExecute(con, "alter table indicators add constraint uq_series_code unique (indicator_code)")
# }

# dbExecute(con, "create index indicators_idx_broad_topic on indicators (broad_topic_tidy)")
# dbExecute(con, "create index indicators_idx_series_code on indicators (series_code)")

dbExecute(con, "alter table indicators rename column series_code TO indicator_code")
dbExecute(con, "alter table indicators add constraint uq_indicator_code unique (indicator_code)")

dbExecute(con, "alter table countries add constraint uq_iso3 unique (iso3)")

for (tb in tbls) {
  fk_name <- sprintf("fk_%s_indicator", tb)

  has_fk <- nrow(dbGetQuery(con, "select 1 from pg_constraint where conname = $1", params = list(fk_name))) > 0

  if (!has_fk) {
    sql_fk <- sprintf(
      "alter table %s add constraint %s foreign key (indicator_code) references indicators (indicator_code)", 
      tb, 
      fk_name
    )

    message(paste("adding", fk_name))
    dbExecute(con, sql_fk)
  }

  fk_name <- sprintf("fk_%s_country", tb)

  has_fk <- nrow(dbGetQuery(con, "select 1 from pg_constraint where conname = $1", params = list(fk_name))) > 0

  if (!has_fk) {
    sql_fk <- sprintf(
      "alter table %s add constraint %s foreign key (iso3) references countries (iso3)", 
      tb, 
      fk_name
    )

    message(paste("adding", fk_name))
    dbExecute(con, sql_fk)
  }
}

dbDisconnect(con)
