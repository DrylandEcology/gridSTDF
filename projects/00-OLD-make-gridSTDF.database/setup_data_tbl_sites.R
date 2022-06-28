rm(list = ls(all = TRUE))
library(RPostgres)
library(DBI)
library(RSQLite)

db_user = 'mynonsuperuser'
db_password = 'grid-stdf-2022'
db_name = 'gridSTDF_2022'

con <- DBI::dbConnect(RPostgres::Postgres(), dbname = db_name,
                      user=db_user, password=db_password) #dbname = db, host=host_db, port=db_port, user=db_user, password=db_password
dbListTables(con)

# add  data to tblSites from weatherDB
con2 <- DBI::dbDriver('SQLite')
weatherDB <- dbConnect(con2, 
                       "gridSTDF/Data/dbWeatherData_WesternUS_gridMET_historical.sqlite3")
sites <- dbReadTable(weatherDB, 'sites')


# Insert!
names <- paste0("(", paste0(c('id', 'latitude', 'longitude', 'weather_label'), collapse = ", "), ")")

for(r in 1:296006) {
  
  if(r %in% seq(1, nrow(sites), 1000)) print(r)
  
  values <- paste0(apply(sites[r,], 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
  values <- stringr::str_replace_all(values, "'NA'", "NULL")
  
  cmd <- paste("INSERT INTO tbl_sites", names, " VALUES ", values)
  
  dbSendQuery(con, cmd, as.is=TRUE)
  
}
