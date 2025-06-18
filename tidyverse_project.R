
#Instalación de packages----

install.packages("ProjectTemplate")
install.packages("here")

#Carga de packages----

library(ProjectTemplate)
library(here)
library(readr)

#Creando template del proyecto----

create.project(project.name = "tidyverse_project",
               template = "minimal")

#Viendo ubicación del diectorio----

here()

#Datos cobertura sanitaria----
##Leyendo datos----

#Revisando primeras filas

read_lines(file = here("data","raw_data","healthcare-coverage.csv"), n_max = 10 )

#Leyendo archivo desde la tercera fila ya que las dos primeras son título y
#subtitulo

coverage <- read_csv(here("data","raw_data","healthcare-coverage.csv"), skip = 2 )

coverage

tail(coverage, n = 30)

#Hay notas al final del archivo por lo que no se van a leer

coverage <- read_csv(here("data","raw_data","healthcare-coverage.csv"), skip = 2,
  n_max  = which(coverage$Location == "Notes")-1)

tail(coverage)

##Tipos de datos----

glimpse(coverage)

#Datos gastos sanitarios----
##Leyendo datos----

#Revisando primeras filas

read_lines(file = here("data","raw_data","healthcare-spending.csv"), n_max = 10 )

#Leyendo archivo desde la tercera fila ya que las dos primeras son título y
#subtitulo

spending <- read_csv(here("data","raw_data","healthcare-spending.csv"), skip = 2 )

spending

tail(spending, n = 30)

#Hay notas al final del archivo por lo que no se van a leer

spending <- read_csv(here("data","raw_data","healthcare-spending.csv"), skip = 2,
  n_max  = which(spending$Location == "Notes")-1)

tail(spending)

##Tipos de datos----

glimpse(spending)

#Guardando los datos----

here()

save(coverage, spending, file = here::here("data", "raw_data", "case_study_1.rda"))




