# World Development Indicators (WDI) - World Bank

World Development Indicators (WDI) is the World Bank's premier compilation of cross-country
comparable data on development.

I did not create this data nor I maintain the original series.

I downloaded all the data revisions from [the WB Data Topics](https://datatopics.worldbank.org/world-development-indicators/wdi-archives.html) and organized it into a PostgreSQL database to simplify my own research (e.g., having all organized into a long
table rather than different spreadsheets with changing formats).

How to use it?

1. Download all of the ZIP parts from the [releases page](https://github.com/pachadotdev/wdi-historical-data/releases/tag/sql-dump).

2. Unzip *wdi.zip* (e.g., `7z x wdi.zip`)

3. Restore the database with

```bash
psql -h localhost -p 5432 -U REPLACE_USER -v ON_ERROR_STOP=1 -c \
    \"drop database if exists wdi; create database wdi owner REPLACE_USER;\";

pg_restore -h localhost -p REPLACE_PORT -U REPLACE_USER -d wdi \
    --no-owner --no-acl -v wdi.dump
```

4. Use it from R/Python/etc. Here is an example from R.

```r
library(RPostgres)
library(data.table)

con <- dbConnect(
  Postgres(),
  user = Sys.getenv("LOCAL_SQL_USR"),
  password = Sys.getenv("LOCAL_SQL_PWD"),
  dbname = "wdi",
  host = "localhost"
)

# GDP indicators
wdi_indicator_map <- c(
  "NY.GDP.MKTP.CD",
  "NY.GDP.MKTP.KD",
  "NY.GDP.PCAP.CD",
  "NY.GDP.PCAP.KD"
)

wdi_gdp <- setDT(dbGetQuery(
  con,
  sprintf(
    "select w.year, w.iso3, w.revision, i.indicator_code, w.value
    from wdi_raw as w
    inner join wdi_indicators_raw as i on i.id = w.indicator_id
    where i.indicator_code in (%s)",
    paste0("'", wdi_indicator_map, "'", collapse = ", ")
  )
))

dcast(wdi_gdp, year + iso3 + revision ~ indicator_code, value.var = "value")

# > dcast(wdi_gdp, year + iso3 + revision ~ indicator_code, value.var = "value")
# Key: <year, iso3, revision>
#           year   iso3 revision NY.GDP.MKTP.CD NY.GDP.MKTP.KD NY.GDP.PCAP.CD
#          <int> <char>    <int>          <num>          <num>          <num>
#       1:  1960    AFE   202107    19291929320   154443036828       147.4504
#       2:  1960    AFE   202109    19291929320   154443036828       147.4504
#       3:  1960    AFE   202110    19342484576             NA       147.8368
#       4:  1960    AFE   202111    19342484576             NA       147.8368
#       5:  1960    AFE   202112    19299444453             NA       147.5078
#      ---                                                                   
# 1403252:  2024    ZWE   202507    44187704410    23634169921      2656.4094
# 1403253:  2024    ZWE   202510    44187704410    23634169921      2656.4094
# 1403254:  2024    ZWE   202512    41539411516    23582865676      2497.2033
# 1403255:  2024    ZWE   202601    41539411516    23582865676      2497.2033
# 1403256:  2024    ZWE   202602    41539411516    23582865676      2497.2033
#          NY.GDP.PCAP.KD
#                   <num>
#       1:       1180.425
#       2:       1180.425
#       3:             NA
#       4:             NA
#       5:             NA
#      ---               
# 1403252:       1420.803
# 1403253:       1420.803
# 1403254:       1417.719
# 1403255:       1417.719
# 1403256:       1417.719
```

The data in long form simplifies plots to show how these revisions change in terms of time coverage and
how the values change.

```r
library(tinyplot)

last10_revisions <- sort(unique(wdi_gdp$revision))
last10_revisions <- tail(last10_revisions, 10)

wdi_gdp <- wdi_gdp[year >= 2000]
wdi_gdp <- wdi_gdp[revision %in% last10_revisions]
wdi_gdp <- wdi_gdp[indicator_code == "NY.GDP.MKTP.KD"]
wdi_gdp <- wdi_gdp[iso3 == "GBR"]

wdi_gdp <- wdi_gdp[, year := as.numeric(year)]

tpar(bg = "white")

tinyplot(
    value ~ year,
    data = wdi_gdp,
    type = "box",
    main = "GDP (constant 2015 US$) - Different WDI revisions for the UK",
    sub = "Source: World Bank",
    file = "wdi.png", width = 8, height = 5
)
```

<figure>
<img src="wdi.png" alt="wdi-plot"/>
</figure>
