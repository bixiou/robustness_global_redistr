######################################################################################################################################################################################################################
#chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/definition-stl-ab-31122011.pdf?__blob=publicationFile
#Germany defines urbanity as degrees of density 
#Rural grid cells:  Grid cells outside urban clusters 
#Urban clusters: clusters of contiguous2 grid cells of 1km2 with a density of at least 300 inhabitants per km2 and a minimum population of 5 000.  
#High-density cluster (or city centre): Contiguous3 grid cells of 1km2 with a density of at least 1 500 inhabitants per km2 and a minimum population of 5000
#This code assesses urbanity level and region index to zipcode
######################################################################################################################################################################################################################

library("openxlsx")
library(dplyr)
library(tidyr)

# Charger les données
#source https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/Archiv/GVAuszugQ/AuszugGV4QAktuell.html
data2 <- read.xlsx(".../robustness_global_redistr/data_ext/source_zipcode/AuszugGV4QAktuell.xlsx", sheet = 2, startRow = 6, colNames = TRUE)

# Renommer les colonnes
colnames(data2) <- c(
  "data_type", "text_code", "country", "regional_code", "district", "municipality_code",
  "municipality", "municipality_name", "area_km2", "population_total", "population_male",
  "population_female", "population_density", "zipcode", "longitude", "latitude", "region_key",
  "region_description", "density_type", "urbanization_description"
)

# Convertir zipcode en entier
data2$zipcode <- as.integer(as.character(data2$zipcode))

# Supprimer les lignes où zipcode est NA
data2 <- data2 %>%
  filter(!is.na(zipcode))

# Définir les groupes régionaux basés sur les codes Länder
Northern <- c("01", "02", "03", "04")  # Schleswig-Holstein, Hamburg, Niedersachsen, Bremen
Western <- c("05", "07", "10")  # Nordrhein-Westfalen, Rheinland-Pfalz, Saarland
Central <- c("06", "16")  # Hessen, Thüringen
Eastern <- c("11", "12", "13", "14", "15")  # Berlin, Brandenburg, Sachsen, Sachsen-Anhalt, Mecklenburg-Vorpommern
Southern <- c("08", "09")  # Baden-Württemberg, Bayern

data2 <- data2 %>%
  mutate(
    RegionSTAR = case_when(
      country %in% Northern ~ "Others",
      country %in% Western ~ "Others",
      country %in% Central ~ "Others",
      country %in% Eastern ~ "Eastern",
      country %in% Southern ~ "Southern",
      TRUE ~ NA
    )
  ) %>%
  select(zipcode, density_type, RegionSTAR, population_total)  # Garder les colonnes essentielles

total_by_region <- data2 %>%
  group_by(RegionSTAR) %>%
  summarise(region_total = sum(population_total, na.rm = TRUE))

print(total_by_region)

total_by_urbanity <- data2 %>%
  group_by(density_type) %>%
  summarise(urbanity_total = sum(population_total, na.rm = TRUE))

print(total_by_urbanity)

data3 <- data2 %>%
  select(zipcode,density_type, region="RegionSTAR")


# Créer un fichier CSV avec les colonnes souhaitées
result <- data3 %>%
  group_by(zipcode) %>%  # Regrouper par code postal
  summarise(urbanity = min(as.integer(density_type), na.rm=TRUE),  # Trouver la valeur minimale de urbanity par zipcode
            region = first(region)) %>%  # Garder la région associée
  ungroup()  # Retirer le groupement pour éviter des erreurs par la suite

data_add <- read.csv("".../robustness_global_redistr/data_ext/source_zipcode/zipcode_DE.csv"")

# 1. Identifier les zipcodes manquants
missing_zipcodes <- setdiff(data_add$zipcode, result$zipcode)

# 2. Extraire les urbanity des zipcodes manquants depuis data_add
missing_data <- data_add %>%
  filter(zipcode %in% missing_zipcodes) %>%
  select(zipcode, urbanity) %>%
  mutate(region = NA)  # Region sera remplie ensuite

# 3. Fusionner avec result et trier par zipcode
result_extended <- bind_rows(result, missing_data) %>%
  arrange(zipcode) %>%
  fill(region, .direction = "downup")  # Remplit d'abord vers le bas, puis vers le haut si besoin



# Enregistrer le fichier CSV
write.csv(result_extended, "C:/Users/ZBOOK/Downloads/zipcode_DE_complete.csv", row.names = FALSE)
