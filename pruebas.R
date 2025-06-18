#Instalación de package----

install.packages("readxl")

#Instalación de library----

library(readxl)
library(RSQLite)
library(dbplyr)
library(dplyr)
library(jsonlite)

#Leyendo información excel---

df_excel_h1 <- read_excel(here("data","otra_data","excel_data.xlsx"),
  sheet = "Sheet1")

df_excel_h2 <- read_excel(here("data","otra_data","excel_data.xlsx"),
  sheet = "Sheet2")

#Respuesta 1----
mean(df_excel_h2$X12)

#Respuesta 2----
cor(df_excel_h1$X5,df_excel_h2$X8)

#Leyendo base de dao SQL----

sqlite <- dbDriver("SQLite")

db <- dbConnect(sqlite, here("data","otra_data","sqlite_data.db"))

dbListTables(db)

#Cargando tabla de la base de datos

tabla1 <- as.data.frame(tbl(db, "table1"))

view(tabla1)

tabla1_id8 <- subset(tabla1, ID == 8)

#Respuesta 3

cor(tabla1_id8$S2,tabla1_id8$S3)

#Leyendo archivo JSON----

tabla_json <- read_json(here("data","otra_data","table2.json"),
  simplifyVector = TRUE)

#Left join 

inner <- inner_join(df_excel_h2, tabla_json, by = "ID")

#Respuesta 4

mean(inner$J2)

#Respuesta 5

cor(inner$X2, inner$J4)










