library(dplyr) # /!\ BUG if plyr (or memisc?) loaded before => detach("package:plyr", unload = TRUE)
library("openxlsx")
#source https://www.insee.fr/fr/statistiques/8331297
#keep what is necesary on the sheet
data <- read.xlsx("C:/Users/ZBOOK/Downloads/estim-pop-dep-sexe-gca-1975-2025.xlsx", sheet=2, startRow = 5)

data <- data %>%
  rename(departement="X1", Name="X2", Population="Total")
head(data)

data <- data %>%
  mutate(region = case_when(
    departement %in% c("75", "77", "78", "91", "92", "93", "94", "95") ~ 1,
    departement %in% c("59", "62", "80", "02", "60") ~ 2,
    departement %in% c("08", "10", "51", "52", "54", "55", "57", "67", "68", "88", "21", "25", "39", "58", "70", "71", "89", "90") ~ 3,
    departement %in% c("09", "12", "19", "23", "24", "31", "32", "33", "40", "46", "47", "48", "64", "65", "81", "82", "87") ~ 4,
    departement %in% c("01", "03", "07", "11", "13", "15", "26", "30", "34", "38", "42", "43", "48", "63", "66", "69", "73", "74", "83", "84") ~ 5,
    TRUE ~ 6
  ))
# Calculer la population totale par région
population_par_region <- data %>%
  group_by(region) %>%
  summarise(total_population = sum(Population, na.rm = TRUE))

print(population_par_region)
