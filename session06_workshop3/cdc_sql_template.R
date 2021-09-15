library(tidyverse)
library(RSQLite)
library(dbplyr)
library(DBI)
library(janitor)

# more details and examples on connecting to an SQL database can be found at 
# https://mam2022.netlify.app/reference/reference_sql/

# set up a connection to sqlite database. 
# make sure you copy the database file in the same directory-- 

cdc_db <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "cdc_data.db"
)

# browse the tables in the database using DBI::dbListTables()
DBI::dbListTables(cdc_db)

# We can easily set these tables up as database objects using dplyr
cdc_data <- dplyr::tbl(cdc_db, "cdc")

cdc_data

# ************************
# You need to calculate Covid death % by age group, sex 
# and presence of co-morbidities (query1) and Covid death % 
# by age group, sex and ICU admission (query2) 


# rather than loading the entire file in memory, you will use dplyr+SQL,
# to generate a smaller dataset that you will use for your calculations and
# thn visualisations


query1 <-   cdc_data %>%
  # dplyr commands like 
  # select, filter, group_by, summarise...
  #
  
  
# what kind of a thing is query1?
class(query1)


# Generate actual SQL commands: We can either use dbplyr::sql_render() or dplyr::show_query()
dbplyr::sql_render(query1)

# execute query and retrieve results in a tibble (dataframe). 
query1_tibble <- query1 %>% 
  collect() # collect runs the SQL query and returns the output of your dplyr pipe sequence