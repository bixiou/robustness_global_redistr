library(dplyr) # /!\ BUG if plyr (or memisc?) loaded before => detach("package:plyr", unload = TRUE)
library("openxlsx")

data <- read.csv("C:/Users/ZBOOK/Downloads/DECENNIALDHC2020.P1-Data.csv", skip=1)

data <- data %>%
  select(Geographic.Area.Name, X...Total) %>%  
  mutate(zipcode = substr(Geographic.Area.Name, nchar(Geographic.Area.Name) - 4, nchar(Geographic.Area.Name))) %>%
  select(zipcode, X...Total)

data_urb <- read.csv("C:/Users/ZBOOK/Downloads/zipcode_US.csv")

data$zipcode <- as.integer(data$zipcode)
data_urb$zipcode <- as.integer(data_urb$zipcode)

merge_1 <- merge(x=data, y=data_urb, by="zipcode", all.x=TRUE)

population_by_urbanity <- merge_1 %>%
  group_by(urbanity) %>%
  summarise(total_population = sum(X...Total, na.rm = TRUE))

print(population_by_urbanity)

data4 <- read.xlsx("C:/Users/ZBOOK/Downloads/co-est2024-pop.xlsx", startRow = 5)

# Renommer les colonnes
colnames(data4) <- c("County/State", "x2", "x3", "x4", "x5", "x6", "Population")

# Garder uniquement le texte après la virgule dans la colonne "County/State"
data4$`County/State` <- sub(".*,\\s*", "", data4$`County/State`)

# Ne garder que la première (County/State) et la dernière colonne (Population)
data4_cleaned <- data4[, c(1, ncol(data4))]

# Regrouper par "County/State" (qui contient maintenant les états), puis sommer les populations
data4_summed <- data4_cleaned %>%
  group_by(`County/State`) %>%
  summarise(Total_Population = sum(Population, na.rm = TRUE))

# Ajouter directement la colonne de région à partir des états
data4_summed$Region <- case_when(
  data4_summed$"County/State" %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont", 
                              "New Jersey", "New York", "Pennsylvania") ~ "Northeast",
  data4_summed$"County/State" %in% c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa", "Kansas", "Minnesota", 
                              "Missouri", "Nebraska", "North Dakota", "South Dakota") ~ "Midwest",
  data4_summed$"County/State" %in% c("Delaware", "District of Columbia", "Florida", "Georgia", "Maryland", "North Carolina", 
                              "South Carolina", "Virginia", "West Virginia", "Alabama", "Kentucky", "Mississippi", 
                              "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas") ~ "South",
  data4_summed$"County/State" %in% c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming", 
                              "Alaska", "California", "Hawaii", "Oregon", "Washington") ~ "West",
  TRUE ~ "Other"  # Pour les états qui ne sont pas inclus
)

# Somme des populations par région
data4_region_sum <- data4_summed %>%
  group_by(Region) %>%
  summarise(Total_Pop = sum(Total_Population, na.rm = TRUE))

# Afficher le résultat
print(data4_region_sum)

# Somme totale des populations dans le tableau data4_cleaned
total_population2 <- sum(data4$Population, na.rm = TRUE)

# Afficher la somme totale
print(total_population2)

datacode <- read.csv("C:/Users/ZBOOK/Downloads/zipcode_US.csv")
datastate <- read.csv ("C:/Users/ZBOOK/Downloads/us_zip_codes_to_county.csv")

# Garder uniquement les deux premières colonnes (ZIP et COUNTY)
datastate_cleaned <- datastate[, c("ZIP", "COUNTY")]

# Garder uniquement la première occurrence de chaque ZIP
datastate_cleaned <- datastate_cleaned[!duplicated(datastate_cleaned$ZIP), ]

# Extraire tous les chiffres sauf les trois derniers et remplacer par "0" s'il y a moins de trois chiffres
datastate_cleaned$state <- sapply(datastate_cleaned$COUNTY, function(county) {
  if (is.na(county)) {
    return(NA)  # Retourne NA si la valeur est manquante
  }
  county_length <- nchar(county)
  if (county_length <= 3) {
    return("0")
  } else {
    return(substr(county, 1, county_length - 3))
  }
})

# Supprimer la colonne COUNTY si nécessaire (optionnel)
datastate_cleaned <- datastate_cleaned[, c("ZIP", "state")]

# Ajouter la colonne 'region' directement dans datastate
datastate_cleaned$region <- case_when(
  # Northeast Region
  datastate_cleaned$state %in% c("9", "23", "25", "33", "44", "50", "34", "36", "42") ~ "Northeast",
  
  # Midwest Region
  datastate_cleaned$state %in% c("17", "18", "26", "39", "55", "19", "20", "27", "29", "31", "38", "46") ~ "Midwest",
  
  # South Region
  datastate_cleaned$state %in% c("10", "11", "12", "13", "24", "37", "45", "51", "54","1", "21", "28", "47", "5", "22", "40", "48") ~ "South",
  
  # West Region
  datastate_cleaned$state %in% c("4", "8", "16", "30", "32", "35", "49", "56", "2", "6", "15", "41", "53") ~ "West",
  
  # Autres cas possibles (par défaut, si un état n'est pas dans l'une des régions ci-dessus)
  TRUE ~ "Unknown"
)

merge_1 <- merge(x=datacode, y=datastate_cleaned, by.x="zipcode", by.y="ZIP", all.x=TRUE)

# Fonction pour attribuer les valeurs de state et region
assign_state_region <- function(df) {
  for (i in 2:nrow(df)) {
    if (df$zipcode[i] >= 1000 && is.na(df$state[i])) {
      if (substr(df$zipcode[i], 1, 2) == substr(df$zipcode[i-1], 1, 2)) {
        df$state[i] <- df$state[i-1]
        df$region[i] <- df$region[i-1]
      }
    }
  }
  return(df)
}

# Appliquer la fonction
merge_1 <- assign_state_region(merge_1)

write.csv(merge_1, "us_25_zipcode.csv", row.names = FALSE)

# Filtrer les lignes où 'region' est NA
datastate_na_region <- merge_1[is.na(merge_1$region), ]

# Sauvegarder ce sous-ensemble dans un fichier CSV, en gardant toutes les colonnes
write.csv(datastate_na_region, "us_zipcode_with_na_region.csv", row.names = FALSE)



