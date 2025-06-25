#Instalación de package----

install.packages("readxl")

#Instalación de library----

library(readxl)
library(RSQLite)
library(dbplyr)
library(dplyr)
library(jsonlite)
library(here)
library(readr)
library(lubridate)
library(tidyr)
library(stringr)

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

#Leyendo datos----

complaints <- read_csv(here("data","otra_data","data_complaints.csv"))

class(complaints$`Date received`)

#Transformando columna a formato fecha----

complaints$`Date received` <- as.Date(mdy(complaints$`Date received`))

class(complaints$`Date received`)

#Respuesta 1----

complaints %>% filter(Product == "Student loan", `Date received` >= "2018-01-01") %>% 
  group_by(State) %>% 
  summarize(N=n()) %>% 
  arrange(desc(N))

#Respuesta 2----

complaints$`Date sent to company` <- as.Date(mdy(complaints$`Date sent to company`))

class(complaints$`Date sent to company`)

complaints_mean <- complaints %>% 
  mutate(dif_dias = `Date sent to company` - `Date received`) %>% 
  mutate(dif_dias2 = dif_dias) %>% 
  separate(dif_dias2, 
    into = c("valor","dias"), sep = " ") %>% 
  mutate(valor = as.numeric(valor)) %>% 
  group_by(`Submitted via`) %>%
  summarize(mean_times = mean(valor,na.rm=TRUE)) %>% 
  mutate(mean_times = round(mean_times,1)) 

complaints_mean

#Respuesta 3----

student <- complaints %>% filter(Product ==  "Credit card or prepaid card")

str_count(student$`Consumer complaint narrative`,"student") %>% sum(na.rm=TRUE)

#Respuesta 4----

student_loan <- complaints %>% filter(Product == "Student loan"  )

student_loan <- student_loan %>% mutate(charlength =
  nchar(`Consumer complaint narrative`))

student_loan %>% group_by(Issue) %>% summarise(charlength =
  mean(charlength,na.rm=T)) %>% arrange(desc(charlength))

#Respuesta 5----

credit_card <- complaints %>% filter(Product == "Credit card or prepaid card"  )

str_count(credit_card$`Consumer complaint narrative`,"account") %>% sum(na.rm=TRUE)

















