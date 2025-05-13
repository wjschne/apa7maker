library(duckdb)
library(dplyr)
con <- dbConnect(duckdb(), dbdir = ":memory:")
duckdb_read_csv(
  con,
  name = "author",
  "author.csv",
  col.types = c(
    author_id = "INTEGER",
    author_name = "VARCHAR",
    author_url = "VARCHAR",
    email = "VARCHAR",
    phone = "VARCHAR",
    fax = "VARCHAR",
    degrees = "VARCHAR",
    orcid = "VARCHAR",
    deceased = "BOOLEAN"
  )
)
dbReadTable(con, "author")
duckdb_register(con, "author", readxl::read_excel("dbdata.xlsx", sheet = "author", col_types = ))
duckdb_register(con, "affiliation", readxl::read_excel("dbdata.xlsx", sheet = "affiliation"))
tbl(con, "author")
tbl(con, "affiliation")
