##################################################################################################################################################################
#This code computes population counts per level of urbanity in the US                                                                        
#Source : 2020 Census
#Urbanity of zipcodes comes from a code from this github https://github.com/bixiou/oecd_climate/tree/main/code_oecd/zipcodes that must be found searched again
#This code then assigns regions to zipcodes - but the method is sub-optimal and can be modofied the next time
##################################################################################################################################################################

#Population per region come directly from this source (2024 estimate)
#https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-total.html
#".../robustness_global_redistr/data_ext/source_zipcode/NST-EST2024-POP.xlsx"

library(dplyr) # /!\ BUG if plyr (or memisc?) loaded before => detach("package:plyr", unload = TRUE)
library("openxlsx")

# source : https://data.census.gov/table?q=All+5-digit+ZIP+Code+Tabulation+Areas+within+United+States+Populations+and+People
data <- read.csv("../../data_ext/source_zipcode/DECENNIALDHC2020.P1-Data.csv", skip=1)

data <- data %>%
  select(Geographic.Area.Name, X...Total) %>%  
  mutate(zipcode = substr(Geographic.Area.Name, nchar(Geographic.Area.Name) - 4, nchar(Geographic.Area.Name))) %>%
  select(zipcode, X...Total)

#source : https://github.com/bixiou/oecd_climate/tree/main/code_oecd/zipcodes
data_urb <- read.csv("../../data_ext/source_zipcode/zipcode_US.csv")

data$zipcode <- as.integer(data$zipcode)
data_urb$zipcode <- as.integer(data_urb$zipcode)

merge_1 <- merge(x=data, y=data_urb, by="zipcode", all.x=TRUE)

population_by_urbanity <- merge_1 %>%
  group_by(urbanity) %>%
  summarise(total_population = sum(X...Total, na.rm = TRUE))

print(population_by_urbanity)

datacode <- read.csv("../../data_ext/source_zipcode/zipcode_US.csv")

# source : https://redivis.com/datasets/b36a-8fmm08tgf
#The file is >100 Mo so you won't find it on the github but it is often updated and easily accessible
datastate <- read.csv ("us_zip_codes_to_county.csv")

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

# Allocates regions to the fex missing zipcodes - not optimal ut right because zipcodes are split regularly
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
