lapply(
  c("data.table", "RPostgres", "rvest"),
  function(x) {
    if (!requireNamespace(x, quietly = TRUE)) install.packages(x, repos = "https://cran.r-project.org")
  }
)

library(data.table)
library(RPostgres)
library(rvest)

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

# Create indicator lookup table if it doesn't exist
if (!any("wdi_indicators_raw" %in% dbListTables(con))) {
  dbExecute(con, "
    CREATE TABLE wdi_indicators_raw (
      id serial PRIMARY KEY,
      indicator_code text UNIQUE NOT NULL
    )
  ")
}

for (i in seq_along(historical)) {
  # i = 22
  
  fzip <- file.path("data", basename(historical[i]))
  message(fzip)

  rev <- as.integer(substr(gsub("[A-z]|[a-z]|[[:punct:]]", "", fzip), 1, 6))

  if (any("wdi_raw" %in% dbListTables(con))) {
    d <- nrow(dbGetQuery(con, "select revision from wdi_raw where revision = $1 limit 1", params = list(rev)))
  } else {
    d <- 0L
  }

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

  # Upsert new indicator codes and replace string column with integer id
  new_codes <- data.table(indicator_code = unique(wdi_raw$indicator_code))
  dbWriteTable(con, "wdi_tmp_codes", new_codes, temporary = TRUE, overwrite = TRUE, row.names = FALSE)
  dbExecute(con, "
    INSERT INTO wdi_indicators_raw (indicator_code)
    SELECT indicator_code FROM wdi_tmp_codes
    ON CONFLICT (indicator_code) DO NOTHING
  ")
  indicators_map <- setDT(dbGetQuery(con, "SELECT id AS indicator_id, indicator_code FROM wdi_indicators_raw"))
  wdi_raw[indicators_map, on = "indicator_code", indicator_id := i.indicator_id]
  wdi_raw[, indicator_code := NULL]

  setcolorder(wdi_raw, "indicator_id", after = "revision")

  wdi_raw <- wdi_raw[!is.na(value)]
  print(wdi_raw)

  # dbWriteTable(con, "wdi_raw", wdi_raw, append = TRUE)

  N <- 2000000L
  starts <- seq(1L, nrow(wdi_raw), by = N)

  for (j in seq_along(starts)) {
    message(sprintf("Writing fragment %s of %s", j, length(starts)))
    end <- min(starts[j] + N - 1L, nrow(wdi_raw))
    dbWriteTable(con, "wdi_raw", wdi_raw[starts[j]:end], append = TRUE, overwrite = FALSE, row.names = FALSE)
  }

  rm(wdi_raw)

  if (i == 1L) {
    dbExecute(con, "create index idx_iso3 on wdi_raw (iso3)")
    dbExecute(con, "create index idx_year on wdi_raw (year)")
    dbExecute(con, "create index idx_indicator_id on wdi_raw (indicator_id)")
    dbExecute(con, "create index idx_revision on wdi_raw (revision)")
  }

  gc()
}

dbDisconnect(con)
