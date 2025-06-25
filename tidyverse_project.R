
#Instalación de packages----

install.packages("ProjectTemplate")
install.packages("here")

#Carga de packages----

library(ProjectTemplate)
library(here)
library(readr)
library(tidyverse)
library(visdat)
library(skimr)

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

#Cargando los datos----

load(here::here("data","raw_data", "case_study_1.rda"))

coverage

#Datos de los estados----

library(datasets)
data(state)
state.name

##Agregando información sobre Washington DC----

state.abb <- c(state.abb, "DC")
state.region <- as.factor(c(as.character(state.region), "South"))
state.name <- c(state.name, "District of Columbia")
state_data <- tibble(Location = state.name,
  abb = state.abb,
  region = state.region)
state_data

#Revisando datos coverage----

names(coverage)

#Pivot longer coverage----

coverage <- coverage %>%
  mutate(across(starts_with("20"), 
  as.integer)) %>%  ## Convert all year-based columns to integer
  pivot_longer(-Location,         ## Use all columns BUT 'Location'
  names_to = "year_type", 
  values_to = "tot_coverage")

coverage

#Separando columna año coverage----

coverage <- coverage %>% 
  separate(year_type, sep="__", 
  into = c("year", "type"), 
  convert = TRUE)

coverage

#Agregando abreviatura a nivel estatal coverage----

coverage <- coverage %>%
  left_join(state_data, by = "Location")

coverage

#Revisando datos spending----

names(spending)

#Pivot longer spending----

spending <- spending %>%
  pivot_longer(-Location, 
  names_to = "year", 
  values_to = "tot_spending")

#Separando columna año spending----

spending <- spending %>% 
  separate(year, sep="__", 
  into = c("year", "name"), 
  convert = TRUE) %>% 
  select(-name)

spending

#Uniendo coverage y spending----

hc <- inner_join(coverage, spending, 
  by = c("Location", "year"))

hc

#Limpiando datos de la unión----

hc <- hc %>% 
  filter(Location != "United States")

table(hc$type)

#Agregando columna----

pop <- hc %>% 
  filter(type == "Total") %>% 
  select(Location, year, tot_coverage)

pop

hc <- hc %>% 
  filter(type != "Total") %>% 
  left_join(pop, by = c("Location", "year")) %>% 
  rename(tot_coverage = tot_coverage.x, 
  tot_pop = tot_coverage.y)

hc

#Calculando proporción de cobertura----

hc <- hc %>% 
  mutate(prop_coverage = tot_coverage/tot_pop) 

hc

#Transformando columna tot_spending de millones a dolares y agregando columna----

hc <- hc %>% 
  mutate(spending_capita = (tot_spending*1e6) / tot_pop)

hc

#Guardando los datos----

save(hc, file = here::here("data", "tidy_data", "case_study_1_tidy.rda"))

#Cargando los datos----

load(here::here("data", "tidy_data", "case_study_1_tidy.rda"))

#Viendo datos faltantes con Viz----

vis_dat(hc)

hc

#Viendo % datos faltantes con Viz----

vis_miss(hc)

#Viendo que observaciones tienen datos faltantes----

hc %>%
  filter(is.na(tot_coverage))

#Viendo tipo de variables----

skim(hc)

#Agrupando por años----

hc %>% 
  group_by(year) %>%
  skim()

#Gráficos exploratorios----

hc %>%
  filter(type == "Employer", 
    year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
    y = prop_coverage)) +
  geom_point() + 
  labs(x = "spending per capita",
    y = "coverage proportion")

hc %>%
  filter(type == "Employer", 
    year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
    y = prop_coverage)) + 
  geom_point() + 
  labs(x = "spending per capita",
    y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red")

hc %>%
  filter(type == "Employer", 
         year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage)) + 
  geom_point() + 
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
            nudge_x = 150)

hc %>%
  filter(type == "Employer", 
    year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
    y = prop_coverage,
    color = region)) + 
  geom_point() + 
  labs(x = "spending per capita",
    y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
    nudge_x = 150, 
    show.legend = FALSE)

hc %>%
  filter(type == "Employer") %>% 
  ggplot(aes(x = spending_capita, 
    y = prop_coverage,
    color = region)) + 
  geom_point() + 
  facet_wrap(~year) +
  labs(x = "spending per capita",
    y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
    nudge_x = 150, 
    show.legend = FALSE)

hc %>%
  filter(year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
    y = prop_coverage,
    color = region)) + 
  geom_point() + 
  facet_wrap(~type) +
  labs(x = "spending per capita",
    y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
    nudge_x = 150, 
    show.legend = FALSE)

hc %>%
  filter(year == "2014") %>% 
  ggplot(aes(x = spending_capita, 
    y = prop_coverage,
    color = region)) + 
  geom_point() + 
  facet_wrap(~type) +
  labs(x = "spending per capita",
    y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
    nudge_x = 150, 
    show.legend = FALSE)

#Guardando gráficos entre cobertura y gasto----

pdf(here::here("figures", "exploratory", "2013and2014_spending_and_coverage.pdf"))

hc %>%
  filter(type == "Employer") %>% 
  ggplot(aes(x = spending_capita, 
    y = prop_coverage,
    color = region)) + 
  geom_point() + 
  facet_wrap(~year) +
  labs(x = "spending per capita",
    y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
    nudge_x = 150, 
    show.legend = FALSE)

dev.off()

pdf(here::here("figures", "exploratory", "2013_coverage_type.pdf"))

hc %>%
  filter(year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
    y = prop_coverage,
    color = region)) + 
  geom_point() + 
  facet_wrap(~type) +
  labs(x = "spending per capita",
    y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
    nudge_x = 150, 
    show.legend = FALSE)

dev.off()

pdf(here::here("figures", "exploratory", "2014_coverage_type.pdf"))

hc %>%
  filter(year == "2014") %>% 
  ggplot(aes(x = spending_capita, 
    y = prop_coverage,
    color = region)) + 
  geom_point() + 
  facet_wrap(~type) +
  labs(x = "spending per capita",
    y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
    nudge_x = 150, 
    show.legend = FALSE)

dev.off()

#Gráficos gastos por regiones----

hc %>% 
  ggplot(aes(x = region, 
             y = spending_capita)) + 
  geom_boxplot() +
  labs(y = "spending per capita")

hc %>% 
  filter(type == "Employer") %>%
  ggplot(aes(x = region, 
             y = spending_capita)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.2) +
  labs(y = "spending per capita")

#Gráficos con grid----

hc %>% 
  ggplot(aes(x = spending_capita, 
    y = prop_coverage,
    color = region)) + 
  geom_point() + 
  labs(x = "spending per capita",
    y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  facet_grid(year~type)























































