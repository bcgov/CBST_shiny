##Kiri Daust
## setup postgres database for CBST app
library(RPostgreSQL)
library(data.table)
library(sf)
library(readxl)
library(RPostgres)
library(pool)

##function to fix column names for postgres
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}


pl_mig <- fread("./input_data/Pl_migrated_height_list_1.csv")
seedlots <- as.data.table(read_excel("./input_data/Seedlots BECv10 for Forsite March 2019.xlsx"))

dbCon <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "kiri_assign",
  host = Sys.getenv("DB_HOST"),
  port = 5432,
  user = "postgres",
  password = Sys.getenv("DB_PWD")
)

setnames(pl_mig,c("plant_bec","seed_bec","htp")) ##postgres doesn't like capitalised names
dbWriteTable(dbCon,"cbst_pl",pl_mig,row.names = F)
dbExecute(dbCon,"create index on cbst_pl(plant_bec)")##create index on plant_bec

##now load seedlots
setnames(seedlots,dbSafeNames(names(seedlots)))
names(seedlots)
dbWriteTable(dbCon,"cbst_seedlot",seedlots,row.names = F)
dbExecute(dbCon,"create index on cbst_seedlot(bec)")

poolClose(dbCon)


