######################################################################################################################################################################################################################
################################# POLAND, SPAIN AND SWITZERLAND ZIPCODE #####################################################################################################################################################################

'
This script processes Polish postal code data to assign regional and urbanity 
labels. It combines shapefile data with NUTS classifications, filters for Poland, 
and merges NUTS3 codes to get NUTS2 regions.

Postal codes are grouped into 2 or 4 macro-regions, and DGURBA (degree of 
urbanization) is standardized as "urbanity".

The final output consists of two CSVs containing zipcode, urbanity, 
and regional classifications.


Le file est organisé de la manière suivante:

Upload data (ligne 40)
Poland zipcode (ligne 56)
Spain zipcode (ligne 213)
Switzerland zipocode (ligne 342)
....
'



#Definition of Degurba in POland 



#'-----------------------------------------------------------------------------#

library(sf)
library(tidyverse)
library(stringr)

# Charger les données
#source https://gisco-services.ec.europa.eu/tercet/NUTS-2024/PCODE_2024_PT.zip

################################################################################
#                                 Upload data  (Poland, Spain, Suisse)                                #
################################################################################
# Upload data
zipcode <- st_read("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/PCODE_2024_PT/PCODE_2024_PT.shp")


#see data
glimpse(zipcode)
unique(zipcode$CNTR_ID)

# Counts the number of unique postal codes (POSTCODE) for each country (CNTR_ID)
tapply(zipcode$POSTCODE, zipcode$CNTR_ID, function(x) length(unique(x)))


#For Poland and Spain
# A separate dataset with NUTS2 regions is needed, so I’ll merge the NUTS data 
#with the Zipcode data using NUTS3_2024 as the key 
#------------------------------------------------------------------------------#
#Upload NUTS dataset
#Source https://ec.europa.eu/eurostat/documents/345175/629341/NUTS2021-NUTS2024.xlsx/2b35915f-9c14-6841-8197-353408c4522d?t=1717505289640
library(openxlsx)
nuts <- read.xlsx("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/NUTS2021-NUTS2024.xlsx", sheet = 3, startRow = 1, colNames = TRUE)





#------------------------------------------------------------------------------#
################################################################################
#                              Poland zipcode                                  #
################################################################################

# 1. Filter only rows where the country is Poland (CNTR_ID == "PL")
zipcode_poland  <- zipcode %>%
  filter(CNTR_ID == "PL") %>%
  
  # 2. Remove single quotes from the NUTS3_2024 column
  mutate(NUTS3_2024 = gsub("'", "", NUTS3_2024)) %>%
  
  # 3. Select only the relevant columns for analysis
  select(POSTCODE, NUTS3_2024, LAU_NAT, DGURBA)


#------------------------------------------------------------------------------#
#NUTS dataset for Poland
nuts_poland<- nuts %>%
  filter(Country.Code == "PL") 

#Clean and structure the NUTS dataset

# Rename columns
names(nuts_poland)[5:7] <- c("NUTS1", "NUTS2", "NUTS3")

# Step 1: Fill NUTS1 values downward (applies to all rows regardless of level)
nuts_poland = nuts_poland %>%
  fill(NUTS1, .direction = "down")

# Step 2: Controlled filling of NUTS2
# - Create a temporary column (NUTS2_aux) that keeps original NUTS2 values
# - Fill it downward, so it carries the last known NUTS2 value
# - Only apply this filled value to rows where NUTS3 is NOT NA

nuts_poland = nuts_poland %>%
  mutate(NUTS2_aux = ifelse(!is.na(NUTS2), NUTS2, NA)) %>%      # Keep only valid NUTS2 values
  fill(NUTS2_aux, .direction = "down") %>%                      # Fill down with last valid value
  mutate(
    NUTS2 = ifelse(is.na(NUTS2) & !is.na(NUTS3), NUTS2_aux, NUTS2)  # Replace only if NUTS3 is present
  ) %>%
  select(-NUTS2_aux)  # drop the helper column

# Select only the columns: Code.2024, NUTS1, NUTS2, NUTS3
nuts_poland <- nuts_poland %>%
  select(`Code.2024`, NUTS1, NUTS2, NUTS3)

#------------------------------------------------------------------------------#
#Merge ZIP code data with NUTS region names
zipcode_poland <- zipcode_poland %>%
  left_join(
    nuts_poland,
    by = c("NUTS3_2024" = "Code.2024")
  )

#------------------------------------------------------------------------------#
#See https://www.economist.com/graphic-detail/2018/11/21/imperial-borders-still-shape-politics-in-poland-and-romania
# Two Regions

#  Define the regions classified as "East" based on NUTS2 names
region_east <- c(
  "Małopolskie", "Lubelskie", "Podkarpackie", "Podlaskie",
  "Warszawski stołeczny", "Łódzkie", "Świętokrzyskie", "Mazowiecki regionalny"
)

# Divide Poland into two macro-regions: East vs. West (based on NUTS2)
zipcode_poland_2 <- zipcode_poland %>%
  mutate(region2 = ifelse(NUTS2 %in% region_east, "East", "West"))



# Clean and select
zipcode_poland_2 <- zipcode_poland_2 %>%
  mutate(
    zipcode = str_replace_all(POSTCODE, "['-]", ""),  # Remove quotes and dashes from CODE
    urbanity = DGURBA                             # Rename DGURBA to urbanity
  ) %>%
  select(zipcode, urbanity, region2) %>%          # Keep only relevant columns
  st_drop_geometry()                              # Drop geometry column


zipcode_poland_2 <- zipcode_poland_2 %>%
  mutate(
    region2 = ifelse(region2 == "East", 1, 2)       # Recode region: 1 = East, 2 = West
  )

#----------------------------------
# Four Regions
zipcode_poland_4 <-  zipcode_poland %>%
  mutate(
    region4 = case_when(
      NUTS2 %in% c(
        "Warszawski stołeczny",
        "Łódzkie",
        "Świętokrzyskie",
        "Mazowiecki regionalny"
      ) ~ "East-Centre",
      NUTS2 %in% c(
        "Małopolskie",
        "Lubelskie",
        "Podkarpackie",
        "Podlaskie"
      ) ~ "East",
      NUTS2 %in% c(
        "Wielkopolskie",
        "Zachodniopomorskie",
        "Kujawsko-pomorskie",
        "Pomorskie",
        "Warmińsko-mazurskie"
      ) ~ "North-West",
      NUTS2 %in% c(
        "Lubuskie",
        "Dolnośląskie",
        "Opolskie",
        "Śląskie"
      ) ~ "South-West",
      TRUE ~ NA_character_
    )
  )



# Clean and select
zipcode_poland_4 <- zipcode_poland_4 %>%
  mutate(
    zipcode = str_replace_all(POSTCODE, "['-]", ""),  # Remove quotes and dashes from CODE
    urbanity = DGURBA                             # Rename DGURBA to urbanity
  ) %>%
  select(zipcode, urbanity, region4) %>%          # Keep only relevant columns
  st_drop_geometry()                              # Drop geometry column



zipcode_poland_4 <- zipcode_poland_4 %>%
  mutate(
      region4 = case_when(
      region4 == "East" ~ 1,
      region4 == "East-Centre" ~ 2,
      region4 == "North-West" ~ 3,
      region4 == "South-West" ~ 4,
      TRUE ~ NA_real_  #  Assign NA to any unexpected or missing values
    )
  )




#CSV POLOGNE
write.csv(zipcode_poland_2, "zipcode_PL_region2_teste2.csv", row.names = FALSE, quote = FALSE)

write.csv(zipcode_poland_4, "zipcode_PL_region4.csv", row.names = FALSE, quote = FALSE)



#-----------------Calcul QUOTA 2 REGIONS----------------------------------------
library(readxl)  # ou readxl

# Leia o arquivo .xls
quota_reg <- read_excel("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/PL/population_size_and_structure_31_12_2023/tabela14.xls",  skip = 4, col_names = TRUE
)

quota_reg <- quota_reg |>
  rename(
    region = `Wyszczególnienie\nSpecification`,
    code_region = `Identyfikator terytorialny\nCode`,
    pop_2023 = `Ogółem \nTotal`
  ) |>
  select(region, code_region, pop_2023)

quota_reg <- quota_reg |> 
  slice(-1, -2)

library(stringr)

quota_reg <- quota_reg |> 
  # Keep only rows where code_region is exactly "PL" followed by two alphanumeric characters (e.g., PL21, PL92)
  filter(str_detect(code_region, "^PL\\w{2}$"))

#Pop total 2023
sum(quota_reg$pop_2023, na.rm = TRUE)


# Define the East regions in lowercase (to match cleaned names)
region_east <- c(
  "małopolskie", "lubelskie", "podkarpackie", "podlaskie",
  "warszawski stołeczny", "łódzkie", "świętokrzyskie", "mazowiecki regionalny"
)

quota_reg <- quota_reg |>
  # Clean the region names: remove "REGION ", convert to lowercase
  mutate(region_clean = str_to_lower(str_remove(region, "^REGION\\s"))) |>
  
  # Create region_2: "East" if in list, otherwise "West"
  mutate(region_2 = if_else(region_clean %in% region_east, "East", "West")) |>
  
  # Optionally remove the helper column
  select(-region_clean)


proportion_pop <- quota_reg |> 
  group_by(region_2) |> 
  summarise(total_pop = sum(pop_2023, na.rm = TRUE)) |> 
  mutate(proportion = total_pop / sum(total_pop))

print(proportion_pop )













################################################################################
#                              SPAIN ZIPCODE                                   #
################################################################################

zipcode_spain  <- zipcode %>%
  filter(CNTR_ID == "ES") %>%
  
  # 2. Remove single quotes from the NUTS3_2024 column
  mutate(NUTS3_2024 = gsub("'", "", NUTS3_2024)) %>%
  
  # 3. Select only the relevant columns for analysis
  select(POSTCODE, NUTS3_2024, LAU_NAT, DGURBA, NSI_CODE_2)



#------------------------------------------------------------------------------#
#NUTs dataset for Spain

nuts_spain<- nuts %>%
  filter(Country.Code == "ES") 

# Rename columns
names(nuts_spain)[5:7] <- c("NUTS1", "NUTS2", "NUTS3")

# Step 1: Fill NUTS1 values downward (applies to all rows regardless of level)
nuts_spain = nuts_spain %>%
  fill(NUTS1, .direction = "down")

# Step 2: Controlled filling of NUTS2
# - Create a temporary column (NUTS2_aux) that keeps original NUTS2 values
# - Fill it downward, so it carries the last known NUTS2 value
# - Only apply this filled value to rows where NUTS3 is NOT NA

nuts_spain = nuts_spain %>%
  mutate(NUTS2_aux = ifelse(!is.na(NUTS2), NUTS2, NA)) %>%      # Keep only valid NUTS2 values
  fill(NUTS2_aux, .direction = "down") %>%                      # Fill down with last valid value
  mutate(
    NUTS2 = ifelse(is.na(NUTS2) & !is.na(NUTS3), NUTS2_aux, NUTS2)  # Replace only if NUTS3 is present
  ) %>%
  select(-NUTS2_aux)  # drop the helper column

# Select only the columns: Code.2024, NUTS1, NUTS2, NUTS3
nuts_spain <- nuts_spain %>%
  select(`Code.2024`, NUTS1, NUTS2, NUTS3)

# keep only rows where Code.2024 has exactly "ES" + 3 digits
nuts_spain <- nuts_spain %>%
  filter(grepl("^ES\\d{3}$", Code.2024))


#Merge ZIP code data with NUTS region names
zipcode_spain <- zipcode_spain %>%
  left_join(
    nuts_spain,
    by = c("NUTS3_2024" = "Code.2024")
  )
# Check for NAs generated after the join
sum(is.na(zipcode_spain$NUTS2))

#----------------------------------
# Remove trailing spaces only
zipcode_spain$NUTS2 <- sub("\\s+$", "", zipcode_spain$NUTS2)


#5 regions
zipcode_spain_5 <- zipcode_spain %>%
  mutate(
    region = case_when(
      NUTS2 %in% c(
        "La Rioja", "Castilla y León", "Cantabria", "Galicia", "Principado de Asturias"
      ) ~ "North-West",
      
      NUTS2 %in% c(
        "Cataluña", "Aragón", "Illes Balears", "País Vasco", "Comunidad Foral de Navarra"
      ) ~ "North",
      
      NUTS2 %in% c("Comunidad de Madrid") ~ "Centre",
     
      NUTS2 %in% c(
        "Comunitat Valenciana", "Región de Murcia", "Castilla-La Mancha"
      ) ~ "Eastern",
      
      NUTS2 %in% c(
        "Canarias", "Extremadura", "Andalucía", "Ciudad de Ceuta", "Ciudad de Melilla"
      ) ~ "Southern",
      
      TRUE ~ NA_character_
    )
  )

# Check for NAs generated after the join
sum(is.na(zipcode_spain_5$region))

# Clean and select
zipcode_spain_5 <- zipcode_spain_5 %>%
  mutate(
    zipcode = str_replace_all(POSTCODE, "['-]", ""),  # Remove quotes and dashes from CODE
    urbanity = DGURBA                             # Rename DGURBA to urbanity
  ) %>%
  select(zipcode, urbanity, region) %>%          # Keep only relevant columns
  st_drop_geometry()                              # Drop geometry column



zipcode_spain_5 <- zipcode_spain_5 %>%
  mutate(
      region = case_when(
        region == "North-West" ~ 1,
        region == "North" ~ 2,
        region == "Centre" ~ 3,
        region == "Eastern" ~ 4,
        region == "Southern" ~ 5,
        TRUE ~ NA_real_  # Caso haja algum valor inesperado
      )
    )



#CSV SPAIN
write.csv(zipcode_spain_5, "zipcode_ES_region5.csv", row.names = FALSE, quote = FALSE)

#--------------URBANITY QUOTA POP 2017 ET POP 2024 CHECK-----------------------#
#https://www.ine.es/jaxiT3/Tabla.htm?t=29005
pop <-  read.xlsx("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/ES/29005.xlsx",
                                       startRow = 8, sheet = 1)

names(pop)[1] <- "city_raw"
# Renomear colunas de ano para formato pop_XXXX
names(pop) <- gsub("^X?(\\d{4})$", "pop_\\1", names(pop))


pop <- pop %>%
  separate(city_raw, into = c("city_code", "city_name"), sep = " ", extra = "merge")

pop <- pop %>%
  filter(!is.na(pop_2024), !is.na(pop_2017))


#-----------prepare zipcode_ES to merged


zipcode_spain %>%
  group_by(NSI_CODE_2) %>%                              # Group data by municipality code
  summarise(n_dgurba = n_distinct(DGURBA)) %>%          # Count how many distinct DGURBA values exist for each code
  filter(n_dgurba > 1)                                  # Keep only those with more than one value (i.e., inconsistent)

#there is no different dgurba by NSI_CODE_2, so I'ill drop the NSI_CODE_2 duplicated (one NSI_CODE_2 can have more the one Postalcode)



zipcode_spain_2 <- zipcode_spain %>%
  select(-geometry) %>%                             # Drop the geometry column (spatial info)
  group_by(NSI_CODE_2) %>%                          # Group by municipality code
  summarise(
    POSTCODE = first(POSTCODE),                     # Keep first postcode
    NUTS3_2024 = first(NUTS3_2024),                 # Keep first NUTS3 value
    LAU_NAT = first(LAU_NAT),                       # Municipality name
    DGURBA = first(DGURBA),                         # Urbanisation level
    NUTS1 = first(NUTS1),                           # NUTS1 region
    NUTS2 = first(NUTS2),                           # NUTS2 region
    NUTS3 = first(NUTS3),                           # NUTS3 region
    .groups = "drop"                                # Ungroup the result
  )


merged_data <- full_join(zipcode_spain_2, pop, by = c("NSI_CODE_2" = "city_code"))


merged_data <- merged_data %>%
  mutate(
    pop_2024 = as.numeric(pop_2024),
    pop_2017 = as.numeric(pop_2017)
  )

merged_data %>% 
  filter(is.na(POSTCODE)) %>%  ## Keep only rows where DGURBA is missing
  summarise(total_missing_dgurba = sum(pop_2024, na.rm = TRUE))# Sum population, ignoring NA


merged_data %>%
  filter(!is.na(DGURBA)) %>%                           # Keep only rows where DGURBA is not missing
  summarise(total_with_dgurba = sum(pop_2024, na.rm = TRUE)) # Sum population for those rows

merged_data_valid <- merged_data %>%
  filter(!is.na(DGURBA))

pop_total_dgurba <- sum(merged_data_valid$pop_2024, na.rm = TRUE)

pop_por_dgurba <- merged_data_valid %>%
  group_by(DGURBA) %>%
  summarise(pop_dgurba = sum(pop_2024, na.rm = TRUE)) %>%
  mutate(share_2024 = pop_dgurba / pop_total_dgurba)


pop_por_dgurba <- merged_data_valid %>%
  group_by(DGURBA) %>%
  summarise(
    pop_2024 = sum(pop_2024, na.rm = TRUE),
    pop_2017 = sum(pop_2017, na.rm = TRUE)
  ) %>%
  mutate(
    share_2024 = pop_2024 / sum(pop_2024),
    share_2017 = pop_2017 / sum(pop_2017)
  )



#----------------CALCUL QUOTA REGION SPAIN------------------------------------
#SOURCE POP 2021 / https://ine.es/jaxiT3/Tabla.htm?t=


pop_2021 <-  read.xlsx("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/ES/2915.xlsx",
                  startRow = 8, cols = 1:2, sheet = 1)



# Renome and clean region names
pop_2021 <- pop_2021 |> 
  # Rename columns
  rename(region = `xml:space="preserve">`, pop_2021 = `2021`) |> 
  
  # Clean region names: remove XML prefix, numbers, extra spaces
  mutate(region = str_remove(region, '^xml:space="preserve">\\s*')) |> 
  mutate(region = str_remove(region, "^\\d{2}\\s+")) |> 
  mutate(region = str_trim(region)) |> 
  
  # Optional: remove total row (if desired)
  filter(region != "Total")


pop_2021<- pop_2021 |>
  filter(!is.na(pop_2021))



#5 regions
pop_2021 <- pop_2021 %>%
  mutate(
    region5 = case_when(
      region %in% c(
        "Rioja, La", "Castilla y León", "Cantabria", "Galicia", "Asturias, Principado de"
      ) ~ "North-West",
      
      region %in% c(
        "Cataluña", "Aragón", "Balears, Illes", "País Vasco", "Navarra, Comunidad Foral de"
      ) ~ "North",
      
      region %in% c("Madrid, Comunidad de") ~ "Centre",
      
      region %in% c(
        "Comunitat Valenciana", "Murcia, Región de", "Castilla - La Mancha"
      ) ~ "Eastern",
      
      region %in% c(
        "Canarias", "Extremadura", "Andalucía", "Ceuta", "Melilla"
      ) ~ "Southern",
      
      TRUE ~ NA_character_
    )
  )


pop_2021 <- pop_2021 %>%
  mutate(
    region_code = case_when(
      region5 == "North-West" ~ 1,
      region5 == "North" ~ 2,
      region5 == "Centre" ~ 3,
      region5 == "Eastern" ~ 4,
      region5 == "Southern" ~ 5,
      TRUE ~ NA_real_  # Caso haja algum valor inesperado
    )
  )

pop_region5 <- pop_2021|>
  # Group data by the macro-region column "region5"
  group_by(region_code) |>
  
  # Sum the population for each region5 group
  summarise(total_pop = sum(pop_2021, na.rm = TRUE)) |>
  
  # Calculate the proportion of each region's population relative to the national total
  mutate(proportion = total_pop / sum(total_pop))

print(pop_region5)












################################################################################
#                          Switzerland zipcode                                 #
################################################################################


zipcode_switz <- zipcode %>%
  filter(CNTR_ID == "CH") %>%
  
  # 2. Remove single quotes from the NUTS3_2024 column
  mutate(NUTS3_2024 = gsub("'", "", NUTS3_2024)) %>%
  
  # 3. Select only the relevant columns for analysis
  select(POSTCODE, NUTS3_2024, LAU_NAT, DGURBA,NSI_CODE_2)



# 1. Criar a coluna 'lau_code' na base principal (somente números de NSI_CODE_2)
zipcode_switz  <- zipcode_switz  %>%
  mutate(lau_code = str_extract(NSI_CODE_2, "\\d+"))


#https://www.agvchapp.bfs.admin.ch/fr/typologies/results?SnapshotDate=06.04.2025&SelectedTypologies%5B0%5D=HR_REGCH
cantons <- read.xlsx("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/CH/Niveaux_géographiques.xlsx",
                     startRow = 2, # correspond à A2
                     cols = 1:7,   # correspond à colonnes A à G
                     sheet = 1)    # ou mettre le nom de la feuille si connue





# Pad commune numbers with leading zeros to 4 digits (e.g., "23" becomes "0023")
cantons <- cantons %>%
  mutate(Numéro.de.la.commune = str_pad(as.character(Numéro.de.la.commune), width = 4, pad = "0"))

# 2. Selecionar apenas as colunas desejadas da base lau_code
cantons <- cantons %>%
  select(lau_code = Numéro.de.la.commune, lau_label =  Nom.de.la.commune , canton = Canton)

# 3. Fazer o merge (left_join)
zipcode_switz  <- zipcode_switz  %>%
  left_join(cantons, by = "lau_code")

sum(is.na(zipcode_switz$canton))
sum(is.na(zipcode_switz$lau_label))
sum(is.na(zipcode_switz$lau_code))



#https://ec.europa.eu/eurostat/documents/345175/6787248/Population_data.zip
other_lau <- read.xlsx("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/CH/CH_final.xlsx")
other_lau <- other_lau %>%
  select(lau_code = LAU_CODE, lau_label = LAU_LABEL, canton = KANTON)



# Faz o join com a base auxiliar que tem os valores corretos de 'canton'
zipcode_switz <- zipcode_switz %>%
  left_join(other_lau, by = "lau_code", suffix = c("", "_new")) %>%
  mutate(
    # Atualiza a coluna 'canton' somente onde está NA, usando a nova coluna
    canton = if_else(is.na(canton), canton_new, canton)
  ) %>%
  # Remove a coluna temporária criada no join
  select(-canton_new)



zipcode_switz%>%
  filter(is.na(canton)) %>%
  select(POSTCODE, LAU_NAT, lau_code)



zipcode_switz <- zipcode_switz %>%
  mutate(canton = case_when(
    POSTCODE == "6000" ~ "LU",  # Vierwaldstättersee (LU)
    POSTCODE == "8872" ~ "SG",  # Walensee (SG)
    POSTCODE == "6283" ~ "LU",  # Baldeggersee
    POSTCODE == "8640" ~ "SG",  # Zürichsee (SG)
    POSTCODE == "3855" ~ "BE",  # Brienzersee
    POSTCODE == "3235" ~ "BE",  # Bielersee (BE)
    TRUE ~ canton  # mantém o valor atual se não for um dos listados
  ))

zipcode_switz  <- zipcode_switz  %>%
  select(POSTCODE, DGURBA, canton)


zipcode_switz  <- zipcode_switz  %>%
  mutate(canton_nome = case_when(
    canton == "GE" ~ "Genève",
    canton == "ZH" ~ "Zürich",
    canton == "VD" ~ "Vaud",
    canton == "BE" ~ "Bern",
    canton == "LU" ~ "Luzern",
    canton == "UR" ~ "Uri",
    canton == "SZ" ~ "Schwyz",
    canton == "OW" ~ "Obwalden",
    canton == "NW" ~ "Nidwalden",
    canton == "GL" ~ "Glarus",
    canton == "ZG" ~ "Zug",
    canton == "FR" ~ "Fribourg",
    canton == "SO" ~ "Solothurn",
    canton == "BS" ~ "Basel-Stadt",
    canton == "BL" ~ "Basel-Landschaft",
    canton == "SH" ~ "Schaffhausen",
    canton == "AR" ~ "Appenzell Ausserrhoden",
    canton == "AI" ~ "Appenzell Innerrhoden",
    canton == "SG" ~ "St. Gallen",
    canton == "GR" ~ "Graubünden",
    canton == "AG" ~ "Aargau",
    canton == "TG" ~ "Thurgau",
    canton == "TI" ~ "Ticino",
    canton == "NE" ~ "Neuchâtel",
    canton == "JU" ~ "Jura",
    canton == "VS" ~ "Valais",
    TRUE ~ NA_character_
  ))

zipcode_switz  <- zipcode_switz  %>%
  mutate(macroregion = case_when(
    canton_nome %in% c(
      "Zürich", "Bern", "Luzern", "Uri", "Schwyz", "Obwalden", "Nidwalden",
      "Glarus", "Zug", "Solothurn", "Basel-Stadt", "Basel-Landschaft",
      "Schaffhausen", "Appenzell Ausserrhoden", "Appenzell Innerrhoden",
      "St. Gallen", "Graubünden", "Aargau", "Thurgau"
    ) ~ "Suisse alémanique",
    
    canton_nome %in% c(
      "Fribourg", "Vaud", "Valais", "Neuchâtel", "Genève", "Jura"
    ) ~ "Suisse française",
    
    canton_nome == "Ticino" ~ "Suisse italienne",
    
    TRUE ~ NA_character_
  ))



zipcode_switz  <- zipcode_switz  %>%
  mutate(macroregion_num = case_when(
    macroregion == "Suisse alémanique" ~ 1,
    macroregion == "Suisse française" ~ 2,
    macroregion == "Suisse italienne" ~ 3,
    TRUE ~ NA_real_
  ))


# Check for NAs generated after the join
sum(is.na(zipcode_switz$macroregion_num))




zipcode_switz  <- zipcode_switz  %>%
  st_drop_geometry() %>%  # REMOVE o vínculo com geometria
  select(
    zipcode = POSTCODE,
    urbanity = DGURBA,
    region = macroregion_num
  )



# Sauvegarder en CSV tous les pays européens de l'enquete
write.csv(zipcode_switz , "zipcode_CH_region_gh .csv", row.names = FALSE, quote = FALSE)















################################################################################
#                           Italy zipcode                                      #
################################################################################

# Initial code from:  https://github.com/bixiou/oecd_climate/blob/main/code_oecd/zipcodes/Italy_rural.R


setwd("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire")

#source data2 : https://www.istat.it/wp-content/uploads/2024/05/Classificazioni-statistiche-Anni_2024-2025.zip
# Prepare data for merging
data2 <- read.xlsx("Classificazioni-statistiche-Anni_2024-2025/Classificazioni statistiche-e-dimensione-dei-comuni_31_12_2024.xlsx", 
                   sheet = "Comuni 31-12-2024",
                   colNames = TRUE)

data2 <- rename(data2,Population = `Popolazione.residente.al.31/12/2023`)
data2 <- rename(data2, Codice.Comune = `Codice.Istat.del.Comune.(numerico)`)
data2$Codice.Comune <- as.integer(data2$Codice.Comune)
data2$Codice.Comune <- as.integer(data2$Codice.Comune)

data2$Population <- as.integer(data2$Population)

# # Take figures for 2011 as it corresponds more to the actual figures
# sum(as.integer(data2$Popolazione.legale.2011..09.10.2011.[data2$Grado.di.urbanizzazione==1]), na.rm=T)/ sum(as.integer(data2$Popolazione.legale.2011..09.10.2011.), na.rm=T)
# sum(as.integer(data2$Popolazione.legale.2011..09.10.2011.[data2$Grado.di.urbanizzazione==2]), na.rm=T)/ sum(as.integer(data2$Popolazione.legale.2011..09.10.2011.), na.rm=T)
# sum(as.integer(data2$Popolazione.legale.2011..09.10.2011.[data2$Grado.di.urbanizzazione==3]), na.rm=T)/ sum(as.integer(data2$Popolazione.legale.2011..09.10.2011.), na.rm=T)

data2$Istat <- data2$Codice.Comune

data3 <- read.csv("IT_zipcode_pop.csv", sep=",")

# Merge
merge_1 <- merge(x=data3, y=data2[,c("Istat", "Grado.di.urbanizzazione")], all.x=T)
sum(as.integer(merge_1$Abitanti[merge_1$Grado.di.urbanizzazione==3]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)
sum(as.integer(merge_1$Abitanti[merge_1$Grado.di.urbanizzazione==2]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)
sum(as.integer(merge_1$Abitanti[merge_1$Grado.di.urbanizzazione==1]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)
sum(as.integer(merge_1$Abitanti[is.na(merge_1$Grado.di.urbanizzazione)]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)


summary(merge_1$Abitanti[is.na(merge_1$Grado.di.urbanizzazione)])
summary(merge_1$Abitanti[merge_1$Grado.di.urbanizzazione==2])
sum(as.integer(merge_1$Abitanti[merge_1$Grado.di.urbanizzazione==3 | (is.na(merge_1$Grado.di.urbanizzazione) & merge_1$Abitanti<23000)]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)

sum(as.integer(merge_1$Abitanti[is.na(merge_1$Grado.di.urbanizzazione) & merge_1$Abitanti<23000]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)

# Set names as in our taxonomy

# Assign the two CP which are related to two different regions to a region (the one w/ more inhabitants)
# Insignificant btw, < 2k
merge_1$Regione[merge_1$CAP == "12071"] <- "PIE"
merge_1$Regione[merge_1$CAP == "18025"] <- "LIG"

merge_1$Region.quota <- ""
merge_1$Region.quota[merge_1$Regione %in% c("VDA", "LIG", "LOM", "PIE")] <- "North-West"
merge_1$Region.quota[merge_1$Regione %in% c("EMR", "FVG", "TAA", "VEN", "TOS", "LAZ", "MAR", "UMB")] <- "North-East"
merge_1$Region.quota[merge_1$Regione %in% c("ABR",  "CAM" , "MOL" )] <- "Center"
merge_1$Region.quota[merge_1$Regione %in% c("PUG", "BAS", "CAL", "SAR", "SIC")] <- "South"


merge_1$Region.quota2 <- ""
merge_1$Region.quota2[merge_1$Regione %in% c("VDA", "LIG", "LOM", "PIE","EMR", "FVG", "TAA", "VEN", "TOS", "LAZ",  "MAR", "UMB")] <- "North"
merge_1$Region.quota2[merge_1$Regione %in% c("PUG", "BAS", "CAL", "SAR", "SIC", "ABR",  "CAM" , "MOL" )] <- "South"




merge_1$Urban.rural.quota <- ""
merge_1$Urban.rural.quota[merge_1$Grado.di.urbanizzazione==1] <- "Cities"
merge_1$Urban.rural.quota[merge_1$Grado.di.urbanizzazione==2] <- "Small_Cities"
merge_1$Urban.rural.quota[merge_1$Grado.di.urbanizzazione==3] <- "Rural"

# Check quotas
sum(as.integer(merge_1$Abitanti[merge_1$Region.quota=="North-West"]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)
sum(as.integer(merge_1$Abitanti[merge_1$Region.quota=="North-East"]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)
sum(as.integer(merge_1$Abitanti[merge_1$Region.quota=="Center"]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)
sum(as.integer(merge_1$Abitanti[merge_1$Region.quota=="South"]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)

sum(as.integer(merge_1$Abitanti[merge_1$Region.quota2=="North"]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)
sum(as.integer(merge_1$Abitanti[merge_1$Region.quota2=="South"]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)

# Pop oK
check <- merge_1 %>%
  group_by(CAP) %>%
  summarise(pop.tot = sum(Abitanti, na.rm=T)) %>%
  ungroup()


# Not for category
## /!\
# Focus on subsample of duplicates
merge_1_pop <- merge_1 %>%
  group_by(CAP) %>%
  summarise(CAP.pop = sum(Abitanti, na.rm=T)) %>%
  ungroup()
merge_1 <- merge(merge_1, merge_1_pop, by="CAP")

duplicate.urbanity <- merge_1 %>%
  group_by(CAP) %>%
  summarise(dup.urbanity = length(unique(Urban.rural.quota))) %>%
  ungroup()
merge_1 <- merge(merge_1, duplicate.urbanity, by="CAP")

# Assign region if no duplicates
merge_1$Urbanity.CAP <- ""
merge_1$Urbanity.CAP[merge_1$dup.urbanity == 1] <- merge_1$Urban.rural.quota[merge_1$dup.urbanity == 1]

# Get share of each rural/urban category for each outcode
merge_1_urbanity <- merge_1[merge_1$dup.urbanity > 1,] %>%
  group_by(CAP, Urban.rural.quota) %>%
  summarise(CAP, Urban.rural.quota, share = sum(Abitanti, na.rm = T)/CAP.pop) %>%
  ungroup()

# Transform data
merge_1_urbanity <- merge_1_urbanity[!duplicated(merge_1_urbanity),]
merge_1_urbanity[merge_1_urbanity==""] <- "NA"
merge_1_urbanity <- merge_1_urbanity %>%
  pivot_wider(names_from = Urban.rural.quota, values_from = share)

merge_1_urbanity$Urbanity.CAP <- ""

# Replace NA with 0
merge_1_urbanity$Small_Cities[is.na(merge_1_urbanity$Small_Cities)] <- 0
merge_1_urbanity$Rural[is.na(merge_1_urbanity$Rural)] <- 0
merge_1_urbanity$Cities[is.na(merge_1_urbanity$Cities)] <- 0
merge_1_urbanity["NA"][is.na(merge_1_urbanity["NA"])] <- 0

# Assign value with greatest share
merge_1_urbanity$Urbanity.CAP[(merge_1_urbanity['NA'] > merge_1_urbanity$Rural & merge_1_urbanity['NA'] > merge_1_urbanity$Cities & merge_1_urbanity['NA'] > merge_1_urbanity$Small_Cities)] <- "Small_Cities"
merge_1_urbanity$Urbanity.CAP[(merge_1_urbanity$Cities > merge_1_urbanity$Rural & merge_1_urbanity$Cities > merge_1_urbanity$Small_Cities & merge_1_urbanity$Cities > merge_1_urbanity['NA'])] <- "Cities"
merge_1_urbanity$Urbanity.CAP[(merge_1_urbanity$Rural > merge_1_urbanity$Cities & merge_1_urbanity$Rural > merge_1_urbanity$Small_Cities & merge_1_urbanity$Rural > merge_1_urbanity['NA'])] <- "Rural"
merge_1_urbanity$Urbanity.CAP[(merge_1_urbanity$Small_Cities > merge_1_urbanity$Rural & merge_1_urbanity$Small_Cities > merge_1_urbanity$Cities & merge_1_urbanity$Small_Cities > merge_1_urbanity['NA'])] <- "Small_Cities"

# Merging
merge_1_urbanity <- merge_1_urbanity %>%
  select(CAP, Urbanity.CAP)

merge_1 <- merge(x=merge_1, y=merge_1_urbanity, by="CAP", all.x = T)

# Assign values of non-duplicates
merge_1$Urbanity.CAP.y[is.na(merge_1$Urbanity.CAP.y)] <- merge_1$Urbanity.CAP.x[is.na(merge_1$Urbanity.CAP.y)]
merge_1 <- merge_1 %>%
  rename(Urbanity.CAP = Urbanity.CAP.y)

sum(merge_1$Abitanti[merge_1$Urbanity.CAP == "Rural"], na.rm=T)/sum(merge_1$Abitanti, na.rm=T)
sum(merge_1$Abitanti[merge_1$Urbanity.CAP == "Small_Cities"], na.rm=T)/sum(merge_1$Abitanti, na.rm=T)
sum(merge_1$Abitanti[merge_1$Urbanity.CAP == "Cities"], na.rm=T)/sum(merge_1$Abitanti, na.rm=T)
sum(merge_1$Abitanti[merge_1$Urbanity.CAP == ""], na.rm=T)/sum(merge_1$Abitanti, na.rm=T)

# Assign to rural values with inhbaitants < max of rural
merge_1$Urbanity.CAP[merge_1$Urbanity.CAP == "" & merge_1$Abitanti < 23036] <- "Rural"
merge_1$Urbanity.CAP[merge_1$Urbanity.CAP == "" & merge_1$Abitanti >= 23036] <- "Small_Cities"

data.CAP.list <- merge_1 %>%
  select(CAP, Urbanity.CAP, Region.quota)

# Generate additional ZIP for big cities

# Select zip for which we need to generate additional numbers
# Two different format ends with 'xx' or 'x'
data.CAP.list.xx <- data.CAP.list[substr(data.CAP.list$CAP, 5, 5) == 'x',]
data.CAP.list.x <- data.CAP.list.xx[substr(data.CAP.list.xx$CAP, 4, 5) != 'xx',]
data.CAP.list.xx <- data.CAP.list.xx[substr(data.CAP.list.xx$CAP, 4, 5) == 'xx',]

data.CAP.list.x$CAP <- substr(data.CAP.list.x$CAP, 1, 4)
data.CAP.list.xx$CAP <- substr(data.CAP.list.xx$CAP, 1, 3)

generate_zip <- function(vector, zipsize){
  interval.start <- as.numeric(vector[[1]])*zipsize
  interval.end <- (as.numeric(vector[[1]])+1)*zipsize-1
  
  zip <- c(interval.start:interval.end)
  
  df <- data.frame(zip, vector[[2]], vector[[3]]) %>%
    setNames(c("CAP", "Urbanity.CAP", "Region.quota"))
  
  return(df)
}

# Generate the zip
result.x <- apply(data.CAP.list.x, 1, generate_zip, zipsize=10)
result.x <- rbindlist(result.x, fill=T)

result.xx <- apply(data.CAP.list.xx, 1, generate_zip, zipsize=100)
result.xx <- rbindlist(result.xx, fill=T)
# Remove two zip generated that are duplicates of already existing CAP (with different information)
result.xx <- result.xx[!(result.xx$CAP %in% c(9170,34170)),]

# Select the remaining zip and append the two datasets
data.CAP.list.nox <- data.CAP.list[substr(data.CAP.list$CAP, 5, 5) != 'x',]
final <- data.CAP.list.nox[!duplicated(data.CAP.list.nox),]
final_2 <- data.CAP.list.nox[!duplicated(data.CAP.list.nox),]
final$CAP <- as.integer(final$CAP)
final_2$CAP <- as.integer(final_2$CAP)
final <- rbind(final, result.x) %>%
  rbind(result.xx)
final_2 <- rbind(final_2, result.x) %>%
  rbind(result.xx)


final$CAP <- sprintf("%05d", final$CAP)
final_2$CAP <- sprintf("%05d", final_2$CAP)

# Renommer les colonnes
final <- final %>%
  rename(zipcode = CAP,
         urbanity = Urbanity.CAP,
         Region = Region.quota)

# Renommer les colonnes
final_2 <- final_2 %>%
  rename(zipcode = CAP,
         urbanity = Urbanity.CAP,
         Region = Region.quota)


# Recoder urbanity
final$urbanity[final$urbanity == "Cities"] <- 1
final$urbanity[final$urbanity == "Small_Cities"] <- 2
final$urbanity[final$urbanity == "Rural"] <- 3
final$urbanity <- as.integer(final$urbanity)

# Recoder 4 Region
final$Region[final$Region == "North-West"] <- 1
final$Region[final$Region == "North-East"] <- 2
final$Region[final$Region == "Center"] <- 3
final$Region[final$Region == "South"] <- 4



# Recoder urbanity
final_2$urbanity[final_2$urbanity == "Cities"] <- 1
final_2$urbanity[final_2$urbanity == "Small_Cities"] <- 2
final_2$urbanity[final_2$urbanity == "Rural"] <- 3
final_2$urbanity <- as.integer(final_2$urbanity)

# Recoder 2 Region
# Recoder 4 Region
final_2$Region[final_2$Region == "North-West"] <- 1
final_2$Region[final_2$Region == "North-East"] <- 1
final_2$Region[final_2$Region == "Center"] <- 2
final_2$Region[final_2$Region == "South"] <- 2



final$Region <- as.integer(final$Region)
final_2$Region <- as.integer(final_2$Region)

write.csv(final,"IT_zipcode_pop_2023_region4.csv", row.names=F)
write.csv(final_2,"IT_zipcode_pop_2023_region2.csv", row.names=F)












################################################################################
#                          Japan zipcode                                 #
################################################################################

library(dplyr)
library(stringr)


# Source (population by municipality, year 2020): https://www.e-stat.go.jp/en/regional-statistics/ssdsview/municipality
# Source (zipcode, updated 3 july 2024) : https://www.post.japanpost.jp/zipcode/dl/roman-zip.html

data1 <- read.csv("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/JA/source_JA_zipcode.csv", sep = ",", fileEncoding = "Shift-JIS", header = F) # Zipcode 
data.pop2 <- read.csv2("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/JA/source_JA_population.csv", sep=",", fileEncoding = "cp932", header = F, skip = 9) #Population by Municipality in 2020

### PART 1 Municipality

data.pop2 <- data.pop2 %>%
  rename(Year = V2, Area.Code = V3,
         Area = V4, Pop = V6) %>% select(Year, Area.Code, Area, Pop)
data.pop2$Pop <- gsub(",", "", data.pop2$Pop)
data.pop2$Pop.int <- as.integer(data.pop2$Pop)


data.pop2$Prefecture<- toupper(gsub("-"," ",word(data.pop2$Area, 1)))

# NB : "ken Takizawashi" is a "shi
data.pop2$Municipality.type <- gsub(".*-","",data.pop2$Area)

# Take two last words of the name
data.pop2$Municipality.short2 <- toupper(gsub("-", " ", data.pop2$Area %>%
                                                word(2)))

data.pop2$Municipality.short3 <- toupper(gsub("-", " ", data.pop2$Area %>%
                                                word(3)))

data.pop2$Municipality.short <- apply(cbind(data.pop2$Municipality.short2, data.pop2$Municipality.short3), 1, 
                                      function(x) paste(x[!is.na(x)], collapse = " "))

# Do some cleaning for the matching
data.pop2$Municipality.short[data.pop2$Municipality.short == "MINOH SHI"] <- "MINO SHI"

city.w.arrondissement <- c("CHIBA SHI","FUKUOKA SHI", "HAMAMATSU SHI", "HIROSHIMA SHI",
                           "KAWASAKI SHI", "KITAKYUSHU SHI", "KOBE SHI", 
                           "KUMAMOTO SHI", "KYOTO SHI", "NAGOYA SHI", "NIIGATA SHI",
                           "OKAYAMA SHI", "OSAKA SHI", "SAGAMIHARA SHI", "SAITAMA SHI",
                           "SAPPORO SHI", "SENDAI SHI", "SHIZUOKA SHI","YOKOHAMA SHI")

# SAKAI SHI, pb: 2 SAKAI SHI
# So need to account for prefecture as well
data.pop2.unique <- data.pop2[!(data.pop2$Municipality.short %in% city.w.arrondissement) & (data.pop2$Area!="Osaka-fu Sakai-shi"),]
sum(data.pop2.unique$Pop.int[data.pop2.unique$Municipality.type=="cho"])/sum(data.pop2.unique$Pop.int)
word(data.pop2$Municipality.short, 1)






### PART 2 POSTAL CODES

data1 <- data1 %>% 
  select(V1, V5, V6, V7)

data1 <- data1 %>%
  rename(Postal.Code = V1, Prefecture = V5,
         Municipality = V6, Town = V7)

data1$region <- ""

data1$Region[data1$Prefecture %in% c("HOKKAIDO", "AKITA KEN", "AOMORI KEN", 
                                     "FUKUSHIMA KEN", "IWATE KEN", "MIYAGI KEN",
                                     "YAMAGATA KEN")] <- "North"

data1$Region[data1$Prefecture %in% c("HIROSHIMA KEN", "OKAYAMA KEN", "SHIMANE KEN", 
                                     "TOTTORI KEN", "YAMAGUCHI KEN", "EHIME KEN", 
                                     "KAGAWA KEN", "KOCHI KEN", "TOKUSHIMA KEN", 
                                     "FUKUOKA KEN", "KAGOSHIMA KEN", "KUMAMOTO KEN",
                                     "MIYAZAKI KEN", "NAGASAKI KEN", "OITA KEN", 
                                     "OKINAWA KEN", "SAGA KEN")] <- "South"

data1$Region[data1$Prefecture %in% c("CHIBA KEN", "GUMMA KEN", "IBARAKI KEN", 
                                     "KANAGAWA KEN", "SAITAMA KEN", "TOCHIGI KEN",
                                     "TOKYO TO")] <- "Kanto"

data1$Region[data1$Prefecture %in% c("HYOGO KEN", "KYOTO FU", "MIE KEN", "NARA KEN",
                                     "OSAKA FU", "SHIGA KEN", "WAKAYAMA KEN")] <- "Kansai"

data1$Region[data1$Prefecture %in% c("AICHI KEN", "FUKUI KEN", "GIFU KEN",
                                     "ISHIKAWA KEN", "NAGANO KEN", "NIIGATA KEN",
                                     "SHIZUOKA KEN", "TOYAMA KEN", "YAMANASHI KEN")] <- "Chubu"

data1$Municipality.short <- (data1$Municipality)

data1$Municipality.short[data1$Municipality.short == "KUMAMOTO SHI CHUO KU"] <- "KUMAMOTO SHI CHUUOU KU"

table(!(data1$Municipality.short %in% data.pop2$Municipality.short))
df <- data1[!(data1$Municipality.short %in% data.pop2$Municipality.short),]
data1 <- data1[(data1$Municipality.short %in% data.pop2$Municipality.short),]

df$N_word <- sapply(df$Municipality.short,function(x)length(unlist(gregexpr(" ",x)))+1)

# Correcting some typos
df$Municipality.short[df$Municipality=="MIYAKEJIMA MIYAKE MURA"] <- "MIYAKE MURA"
df$Municipality.short[df$Municipality=="HACHIJOJIMA HACHIJO MACHI"] <- "HACHIJO MACHI"
df$Municipality.short[df$Municipality=="NISHIYATSUSHIRO GUN ICHIKAWAMISATO"] <- "ICHIKAWAMISATO CHO"
df$Municipality.short[df$Municipality.short=="MATSURA SHI"] <- "MATSUURA SHI"
df$Municipality.short[df$Municipality.short=="TAKIZAWA SHI"] <- "TAKIZAWASHI"
df$Municipality.short[df$Municipality.short=="NIHOMMATSU SHI"] <- "NIHONMATSU SHI"
df$Municipality.short[df$Municipality.short=="OTAWARA SHI"] <- "OTAWARA SHI"
df$Municipality.short[df$Municipality.short=="KATSURA SHI"] <- "KATSUURA SHI"
df$Municipality.short[df$Municipality.short=="KOZUSHIMA MURA"] <- "KOUZUSHIMA MURA"
df$Municipality.short[df$Municipality.short=="TOYOKA SHI"] <- "TOYOOKA SHI"
df$Municipality.short[df$Municipality.short=="SETOCHI SHI"] <- "SETOUCHI SHI"
df$Municipality.short[df$Municipality.short=="NISHINOMOTE SHI"] <- "NISHINOOMOTE SHI"
df$Municipality.short[df$Municipality.short=="MINAMIKYUSHU SHI"] <- "MINAMIKYUSYU SHI"

# A bunch of them only have the two last words instead of all last 4
df$Municipality.short[df$N_word == 4] <- paste(word(df$Municipality[df$N_word==4], 3), word(df$Municipality[df$N_word==4], 4))



# Fix 21 remaining that still do not match
unique(df$Municipality.short[!(df$Municipality.short %in% data.pop2$Municipality.short)])
df$Municipality.short[df$Municipality.short=="NAMPORO CHO"] <- "NANPORO CHO"
df$Municipality.short[df$Municipality.short=="TOMA CHO"] <- "TOHMA CHO"
df$Municipality.short[df$Municipality.short=="TAKINOE CHO"] <- "TAKINOUE CHO"
df$Municipality.short[df$Municipality.short=="TOYORA CHO"] <- "TOYOURA CHO"
df$Municipality.short[df$Municipality.short=="HIRO CHO"] <- "HIROO CHO"
df$Municipality.short[df$Municipality.short=="HAPPO CHO"] <- "HAPPOU CHO"
df$Municipality.short[df$Municipality.short=="YAMATSURI MACH"] <- "YAMATSURI MACHI"
df$Municipality.short[df$Municipality.short=="OTAWARA SHI"] <- "OHTAWARA SHI"
df$Municipality.short[df$Municipality.short=="OI CHO"] <- "OHI CHO"
df$Municipality.short[df$Municipality.short=="FUJIKAWAGUCHIKO MAC"] <- "FUJIKAWAGUCHIKO MACHI"
df$Municipality.short[df$Municipality.short=="KOMI MACHI"] <- "KOUMI MACHI"
df$Municipality.short[df$Municipality.short=="TOYOKA MURA"] <- "TOYOOKA MURA"
df$Municipality.short[df$Municipality.short=="YAMANOCHI MACHI"] <- "YAMANOUCHI MACHI"
df$Municipality.short[df$Municipality.short=="WANOCHI CHO"] <- "WANOUCHI CHO"
df$Municipality.short[df$Municipality.short=="AMPACHI CHO"] <- "ANPACHI CHO"
df$Municipality.short[df$Municipality.short=="CHIHAYAAKASAKA MU"] <- "CHIHAYAAKASAKA MURA"
df$Municipality.short[df$Municipality.short=="NACHIKATSURA CHO"] <- "NACHIKATSUURA CHO"
df$Municipality.short[df$Municipality.short=="KOTORA CHO"] <- "KOTOURA CHO"
df$Municipality.short[df$Municipality.short=="KATSURA CHO"] <- "KATSUURA CHO"
df$Municipality.short[df$Municipality.short=="SANAGOCHI SON"] <- "SANAGOUCHI SON"
df$Municipality.short[df$Municipality.short=="SETOCHI CHO"] <- "SETOUCHI CHO"
unique(df$Municipality.short[!(df$Municipality.short %in% data.pop2$Municipality.short)])


df$N_word <- NULL
data1 <- rbind(data1, df)


merge_1 <- merge(data1, data.pop2[,c("Prefecture", "Municipality.short", "Pop.int")], by=c("Prefecture", "Municipality.short"))

# When take first number need to add a 0 for numbers less than 1,000,000. Otherwise taking first 3 numbers create duplicates
merge_1$small.zip <- ""
merge_1$small.zip[merge_1$Postal.Code >= 1000000] <- substr(merge_1$Postal.Code[merge_1$Postal.Code >= 1000000], 1, 5)
merge_1$small.zip[merge_1$Postal.Code %in% c(100000:999999)] <- paste("0",substr(merge_1$Postal.Code[merge_1$Postal.Code %in% c(100000:999999)], 1, 4), sep="")
merge_1$small.zip[merge_1$Postal.Code < 100000] <- paste("00",substr(merge_1$Postal.Code[merge_1$Postal.Code < 100000], 1, 3), sep="")

duplicate.region <- merge_1 %>%
  group_by(small.zip) %>%
  summarise(dup.region = length(unique(Region))) %>%
  ungroup()
table(duplicate.region$dup.region) # ok!


pop.small.zip <- merge_1 %>%
  group_by(small.zip) %>%
  summarise(pop.max = max(Pop.int)) %>%
  ungroup()

# Assign urban, if small zip contains at least one zip w/ more than 100k (old definition)
merge_1$Rural.urban <- ""
merge_1$Rural.urban[merge_1$small.zip %in% pop.small.zip$small.zip[pop.small.zip$pop.max>=100000]] <- "Urban"
merge_1$Rural.urban[merge_1$small.zip %in% pop.small.zip$small.zip[pop.small.zip$pop.max<100000]] <- "Rural"


# merge_3 <- merge_1

# merge_2 <- merge_1[,c("Prefecture", "Municipality.short", "Rural.urban")]
# merge_2 <- merge_2[!duplicated(merge_2),]

# Check pop in rural and urban old definition
# merge_2 <- merge(x=data.pop2.unique, y=merge_2, by=c("Prefecture", "Municipality.short"), all.x = T)
# table(is.na(merge_2$Rural.urban))


# merge_2$Rural.urban[duplicated(merge_2[c("Prefecture", "Municipality.short")])] <- "Urban"
# merge_2 <- merge_2[!duplicated(merge_2[c("Prefecture", "Municipality.short")]), ]

# sum(merge_2$Pop.int[merge_2$Rural.urban == "Urban"])/sum(merge_2$Pop.int)
# sum(merge_2$Pop.int[merge_2$Rural.urban == "Rural"])/sum(merge_2$Pop.int)

duplicate.urbanity <- merge_1 %>%
  group_by(small.zip) %>%
  summarise(dup.urbanity = length(unique(Region))) %>%
  ungroup()

View(merge_1[merge_1$small.zip %in% duplicate.urbanity$small.zip[duplicate.urbanity$dup.urbanity>1],])


# Fix dup small codes
merge_1$small.zip[merge_1$Municipality.short== "KISOSAKI CHO"] <- "49808"
merge_1$small.zip[merge_1$Postal.Code== 3840097] <- "37716"
merge_1$small.zip[merge_1$Postal.Code== 3890121] <- "37903"


# Copy of merge_1 for following analysis
merge_final <- merge_1 %>% 
  mutate(Municipality.type2 = word(Municipality.short, -1))



# EXPORT OLD VERSION OF JAPAN ZIPCODE (5 digits and old definition of urbanity)
# merge_1 <- merge_1 %>%
# select(small.zip, Region, Rural.urban)

# merge_1 <- merge_1[!duplicated(merge_1),]

# write.csv(merge_1,"Japan_zipcode2.csv", row.names=F)



### POPULATION BY REGION AND BY URBAN/RURAL WITH OLD DEFINITION OF URBANITY

# merge_4 <- merge_3 %>% distinct(Municipality.short, .keep_all = TRUE)

# region <- merge_4 %>% 
# group_by(Region) %>% 
# summarise(Tot_Pop_Region= sum(Pop.int, na.rm = TRUE)) %>% 
# mutate(Proportion = Tot_Pop_Region / sum(Tot_Pop_Region))

# urb_rur <- merge_4 %>% 
# group_by(Rural.urban) %>% 
# summarise(Tot_Pop_Urb_Rur= sum(Pop.int, na.rm = TRUE)) %>% 
# mutate(Proportion = Tot_Pop_Urb_Rur / sum(Tot_Pop_Urb_Rur))

# merge_3.5 <- merge_3 %>% distinct(Municipality.short, .keep_all = TRUE)

# merge_4 <- data.pop2.unique %>% 
# left_join(merge_3.5 %>% select(Region, Rural.urban, Municipality.short), by = "Municipality.short")

# sum(merge_4$Pop.int)

# 6 NA in Region and Rural.Urban

# merge_4$Region[c(1012 : 1017)] <- "Chubu"

# merge_4$Rural.urban[c(1017, 1016, 1015)] <- "Rural"
# merge_4$Rural.urban[c(1014, 1013, 1012 )] <- "Urban"

# region <- merge_4 %>% 
# group_by(Region) %>% 
# summarise(Tot_Pop_Region= sum(Pop.int, na.rm = TRUE)) %>% 
# mutate(Proportion = Tot_Pop_Region / sum(Tot_Pop_Region))

# urb_rur <- merge_4 %>% 
# group_by(Rural.urban) %>% 
# summarise(Tot_Pop_Urb_Rur= sum(Pop.int, na.rm = TRUE)) %>% 
# mutate(Proportion = Tot_Pop_Urb_Rur / sum(Tot_Pop_Urb_Rur))

# sum(region$Tot_Pop_Region)
# sum(urb_rur$Tot_Pop_Urb_Rur)

# Check the difference between 2020 and 2025


# Column urbanity using the municipality_type (ku and shi for urban, cho, machi, mura, son for rural)
# Sources : http://www.tt.rim.or.jp/~ishato/tiri/code/code.htm , https://fr.wikipedia.org/wiki/Syst%C3%A8me_d%27adressage_japonais#:~:text=Depuis%20la%20r%C3%A9forme%20de%201998,agit%20d%27un%20code%20postal.
merge_final <- merge_final %>%
  mutate(urbanity = case_when(
    Municipality.type2 %in% c("KU", "SHI", "KEN", "TAKIZAWASHI") ~ "urban",
    Municipality.type2 %in% c("CHO", "MACHI", "MURA", "SON") ~ "rural",
    TRUE ~ NA_character_  
  ))



# ZIPCODE with 7 digits
merge_final$small.zip3 <- sprintf("%07d", merge_final$Postal.Code)# zipcode with 7 digits



# ZIPCODE with 6 digits
merge_final$small.zip2 <- ""  #zipcode with 6 digits
# Case 1 : Postal.Code >= 1 000 000 → keeping the first 6 digits
merge_final$small.zip2[merge_final$Postal.Code >= 1e6] <- substr(merge_final$Postal.Code[merge_final$Postal.Code >= 1e6], 1, 6)
# Case 2 : Postal.Code entre 100000 et 999999 → keeping the first 5 and add a “0” in front of them
merge_final$small.zip2[merge_final$Postal.Code >= 1e5 & merge_final$Postal.Code < 1e6] <- paste0("0", substr(merge_final$Postal.Code[merge_final$Postal.Code >= 1e5 & merge_final$Postal.Code < 1e6], 1, 5))
# Case 3 : Postal.Code < 100000 → keeping the first 4 and add “00” in front of them
merge_final$small.zip2[merge_final$Postal.Code < 1e5] <- paste0("00", substr(merge_final$Postal.Code[merge_final$Postal.Code < 1e5], 1, 4))


# Keeping only 7 columns
merge_final2 <- merge_final %>% 
  select(small.zip2, small.zip3, urbanity, Region, Municipality.short, Town, Pop.int)

# Removing duplicate lines except for small.zip2, Municipality, Town and Pop.int
merge_final2 <- merge_final2[!duplicated(merge_final2[ , setdiff(names(merge_final2), c("small.zip2", "Municipality.short", "Town", "Pop.int"))]), ]


# Obtaining the full zipcode that are associated to two town which are not of the same urbanity
duplicate_small.zip3 <- merge_final2[duplicated(merge_final2[c("small.zip3")]) | 
                                       duplicated(merge_final2[c("small.zip3")], fromLast = TRUE), ]

### Applying changes to merge_final2 based on "duplicate_small.zip3"
# Zipcode of OISHIDA MACHI TAKANOSU(OKAKUSAYAMA) seems wrong, true zipcode is 999-4101 
merge_final2[merge_final2$Town == "TAKANOSU(OKAKUSAYAMA)", c("small.zip3", "small.zip2")] <- c("9994101 ", "999410")


# Removing duplicated zip code lines for rural towns : https://www.zipcode-jp.com/modules/zipcode/viewzip.php?zcd=4110000 
# Town == IKANIKEISAIGANAIBAAI means “if not specified", these zipcode always end by "00"
zipcode_to_remove <- duplicate_small.zip3[duplicate_small.zip3$urbanity == "rural", ]

zipcode_to_remove <- zipcode_to_remove %>% 
  filter(Town != "TAKANOSU(OKAKUSAYAMA)")

merge_final2 <- anti_join(merge_final2, zipcode_to_remove)




# Keeping only the first occurence of same lines
merge_final2 <- merge_final2[!duplicated(merge_final2[, !(names(merge_final2) %in% c("Pop.int", "Municipality.short", "Town", "small.zip3"))]), ]


# Zipcode in duplicate with different urbanity (there are 35)
duplicate_small.zip2 <- merge_final2[duplicated(merge_final2[c("small.zip2")]) | 
                                       duplicated(merge_final2[c("small.zip2")], fromLast = TRUE), ]

# Removing column except small.zip2, urbanity and Region
merge_final2 <- merge_final2 %>% 
  select(small.zip2, urbanity, Region)

### Determining the maximum population of which we don't know if it is urban or rural because of the duplicate small.zip2
pop_duplicated <- duplicate_small.zip2[!duplicated(duplicate_small.zip2$Pop.int), ]
sum(pop_duplicated$Pop.int)
sum(pop_duplicated$Pop.int[pop_duplicated$urbanity == "urban"])
# 4 645 683 people are concerned with 4 057 611 leaving in urban zone


merge_final2 <- merge_final2 %>% 
  rename(zipcode = small.zip2,
         urbanity = urbanity,
         region = Region)





# Finding the municipalities concerned
municipalites_concerned <- merge_final %>%
  filter(small.zip2 %in% duplicate_small.zip2$small.zip2) %>%
  distinct(Municipality.short)

# Counting the separate zipcodes in merge_final for these municipalities
zipcodes_municipality <- merge_final %>%
  semi_join(municipalites_concerned, by = "Municipality.short") %>%
  group_by(Municipality.short) %>%
  summarise(n_zipcodes = n_distinct(small.zip2)) %>%
  arrange(desc(n_zipcodes))
sum(zipcodes_municipality$n_zipcodes) # 823 zipcodes are concerned



merge_final2 <- merge_final2 %>%
  mutate(urbanity = case_when(
    urbanity == "urban" ~ 1,
    urbanity == "rural" ~ 2,
    TRUE ~ as.numeric(urbanity) 
  ))

merge_final2 <- merge_final2 %>%
  mutate(region = case_when(
    region == "Chubu" ~ 1,
    region == "Kansai" ~ 2,
    region == "Kanto" ~ 3,
    region == "North" ~ 4,
    region == "South" ~ 5,
    TRUE ~ as.numeric(urbanity) 
  ))



### Dealing with the 35 duplicated zipcode

duplicate_small.zip2 <- duplicate_small.zip2 %>% 
  distinct(small.zip2, .keep_all = TRUE) %>% 
  select(small.zip2, urbanity, Region)

# Extracting the first 5 digits of small.zip3
merge_final <- merge_final %>%
  mutate(zip3_prefix = str_sub(small.zip3, 1, 6))

# Keeping lines where the prefix is in merge_final2 small.zip2
duplicate7 <- merge_final %>%
  filter(zip3_prefix %in% duplicate_small.zip2$small.zip2) %>% 
  select(small.zip3, urbanity, Region)

duplicate7 <- duplicate7 %>% 
  rename(zipcode = small.zip3,
         urbanity = urbanity,
         region = Region)


duplicate7 <- duplicate7 %>%
  mutate(urbanity = case_when(
    urbanity == "urban" ~ 1,
    urbanity == "rural" ~ 2,
    TRUE ~ as.numeric(urbanity) 
  ))

duplicate7 <- duplicate7 %>%
  mutate(region = case_when(
    region == "Chubu" ~ 1,
    region == "Kansai" ~ 2,
    region == "Kanto" ~ 3,
    region == "North" ~ 4,
    region == "South" ~ 5,
    TRUE ~ as.numeric(urbanity) 
  ))

# Removing the duplicated zipcode lines
merge_final2 <- merge_final2 %>%
  filter(!(zipcode %in% duplicate_small.zip2$small.zip2))

# adding the 7 digits zipcode
merge_final2 <- rbind(duplicate7, merge_final2)

merge_final2 <- merge_final2[!duplicated(merge_final2), ]

# Removing the rural version of duplicated 7 digits zipcode (has made before)
merge_final2 <- merge_final2[!(
  (merge_final2$zipcode == "0350000" & merge_final2$urbanity == "2") |
    (merge_final2$zipcode == "7360000" & merge_final2$urbanity == "2") |
    (merge_final2$zipcode == "6100300" & merge_final2$urbanity == "2") |
    (merge_final2$zipcode == "6390200" & merge_final2$urbanity == "2") |
    (merge_final2$zipcode == "4110000" & merge_final2$urbanity == "2")
), ]

# Checking there is no more duplicated zipcode.
merge_final2[duplicated(merge_final2[c("zipcode")]) | 
               duplicated(merge_final2[c("zipcode")], fromLast = TRUE), ]


write.csv(merge_final2,"C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/JA/zipcode_JA.csv", row.names=F, quote = FALSE)



### POPULATION RURAL/URBAN AND REGION USING THE 5 DIGITS ZIPCODE

merge_final3 <- merge_final %>% distinct(Municipality.short, .keep_all = TRUE)

merge_final4 <- data.pop2.unique %>% 
  left_join(merge_final3 %>% select(Region, urbanity, Municipality.short), by = "Municipality.short")

unique(merge_final4$Municipality.short[!(merge_final4$Municipality.short %in% merge_final3$Municipality.short)])


sum(merge_final4$Pop.int)

# 6 NA in Region and Rural.Urban

merge_final4$Region[c(1012 : 1017)] <- "Chubu"

merge_final4$urbanity[c(1012 : 1017 )] <- "urban"

region <- merge_final4 %>% 
  group_by(Region) %>% 
  summarise(Tot_Pop_Region= sum(Pop.int, na.rm = TRUE)) %>% 
  mutate(Proportion = Tot_Pop_Region / sum(Tot_Pop_Region))

urb_rur <- merge_final4 %>% 
  group_by(urbanity) %>% 
  summarise(Tot_Pop_Urb_Rur= sum(Pop.int, na.rm = TRUE)) %>% 
  mutate(Proportion = Tot_Pop_Urb_Rur / sum(Tot_Pop_Urb_Rur))

sum(region$Tot_Pop_Region)
sum(urb_rur$Tot_Pop_Urb_Rur)


### END Japon













################################################################################
#                          Germany zipcode                                 #
################################################################################

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
data2 <- read.xlsx("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/DE/AuszugGV4QAktuell.xlsx", sheet = 2, startRow = 6, colNames = TRUE)

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
#Data from 2020 computations to complete missing data
#source : https://github.com/bixiou/oecd_climate/tree/main/code_oecd/zipcodes
data_add <- read.csv("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/DE/zipcode_DE.csv")

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
write.csv(result_extended, "zipcode_DE_complete.csv", row.names = FALSE, quote = FALSE)












################################################################################
#                          Great Britain zipcode                              #
################################################################################

library(dplyr)
library(tidyr)

# Source: https://www.doogal.co.uk/postcodedownloads.php
data <- read.csv2("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/GB/postcodes.csv", sep=',')

rural <- c("Rural village", "Rural hamlet and isolated dwellings", "Rural town and fringe", "Rural town and fringe in a sparse setting", "Rural hamlet and isolated dwellings in a sparse setting", "Rural village in a sparse setting")
rural <- c(rural,"", "Accessible rural area", "Remote rural area", "Very remote rural area", "Very remote small town", "Accessible small town", "Remote small town")
urban_town <- c("Urban city and town", "Urban city and town in a sparse setting")
urban_large <- c("Urban major conurbation", "Urban minor conurbation", "Large urban area", "Other urban area")

sum(data$Population[data$Rural.urban %in% urban_town], na.rm=T)/sum(data$Population, na.rm=T)
sum(data$Population[data$Rural.urban %in% c(rural)], na.rm=T)/sum(data$Population, na.rm=T)
sum(data$Population[data$Rural.urban %in% urban_large], na.rm=T)/sum(data$Population, na.rm=T)


## PART 2

#data <- read.csv2(file="/Users/Bluebii/Library/Mobile Documents/com~apple~CloudDocs/TRAVAIL/Jobs/Stantcheva_2020:21/OECD/oecd_climate/xlsx/UK/UK_zip_pop.csv", header=T, sep=",")

# View(a)
# data.england <- data[data$Country=="England",]

southern.england <- c("South West", "South East", "East of England")
midlands <- c("West Midlands", "East Midlands")
northern.england <- c("North West", "Yorkshire and The Humber", "North East")

sum(data$Population[data$Region == "London"], na.rm = T)/sum(data$Population, na.rm = T)
sum(data$Population[data$Region %in% southern.england], na.rm = T)/sum(data$Population, na.rm = T)
sum(data$Population[data$Region %in% midlands | data$Country=="Wales"], na.rm = T)/sum(data$Population, na.rm = T)
sum(data$Population[data$Region %in% northern.england], na.rm = T)/sum(data$Population, na.rm = T)
sum(data$Population[data$Country %in% c("Scotland", "Northern Ireland")], na.rm = T)/sum(data$Population, na.rm = T)

data$Rural.urban.quota[data$Rural.urban %in% urban_town] <-"City_Town"
data$Rural.urban.quota[data$Rural.urban %in% rural] <-"Rural"
data$Rural.urban.quota[data$Rural.urban %in% urban_large] <-"Large_urban"

data$Region.quota[data$Region =="London"] <- "London"
data$Region.quota[data$Region %in% southern.england] <- "Southern England"
data$Region.quota[data$Region %in% midlands | data$Country == "Wales"] <- "Central UK"
data$Region.quota[data$Region %in% northern.england] <- "Northern England"
data$Region.quota[data$Country %in% c("Scotland", "Northern Ireland")] <- "Northern UK"

data.outcode <- data %>%
  select(Population, Postcode, Region.quota, Rural.urban.quota)

data.outcode <- data.outcode %>%
  subset(Region.quota != "")
# Get the outcode
data.outcode$outcode <- sub(" .*", "", data.outcode$Postcode)

# Then since there are more then 2Mio postcodes, we want info at the outcode levels
# When there are multiples regions or rural/urban areas for the same outcode,
# we assign the one with the greatest share of population


# Step 1: Get total pop by outcode
data.outcode.pop <- data.outcode %>%
  group_by(outcode) %>%
  summarise(pop.outcode = sum(Population, na.rm=T)) %>%
  ungroup()
data.outcode <- merge(data.outcode, data.outcode.pop, by="outcode")

######################
## Step 2: Urbanity ##
######################
# Focus on subsample of duplicates
duplicate.urbanity <- data.outcode %>%
  group_by(outcode) %>%
  summarise(dup.urbanity = length(unique(Rural.urban.quota))) %>%
  ungroup()

# Assign region if no duplicates
data.outcode$Urbanity.outcode <- ""
data.outcode <- merge(data.outcode, duplicate.urbanity, by="outcode")
data.outcode$Urbanity.outcode[data.outcode$dup.urbanity == 1] <- data.outcode$Rural.urban.quota[data.outcode$dup.urbanity == 1]

# Get share of each rural/urban category for each outcode
data.outcode.urbanity <- data.outcode[data.outcode$dup.urbanity > 1,] %>%
  group_by(outcode, Rural.urban.quota) %>%
  summarise(outcode, Rural.urban.quota, share = sum(Population, na.rm = T)/pop.outcode) %>%
  ungroup()

# Transform data
data.outcode.urbanity <- data.outcode.urbanity[!duplicated(data.outcode.urbanity),]
data.outcode.urbanity <- data.outcode.urbanity %>%
  pivot_wider(names_from = Rural.urban.quota, values_from = share)

data.outcode.urbanity$Urbanity.outcode <- ""
# If no population at all, we assign it to rural, except for 3 exceptions (not related to rural in original data)
data.outcode.urbanity$Urbanity.outcode[(is.na(data.outcode.urbanity$Large_urban) & is.na(data.outcode.urbanity$Rural) & is.na(data.outcode.urbanity$City_Town)) == T] <- "Rural"
data.outcode.urbanity$Urbanity.outcode[data.outcode.urbanity$outcode %in% c("DH98", "NG80", "WV98")] <- "City_Town"

# Replace NA with 0
data.outcode.urbanity$Large_urban[is.na(data.outcode.urbanity$Large_urban)] <- 0
data.outcode.urbanity$Rural[is.na(data.outcode.urbanity$Rural)] <- 0
data.outcode.urbanity$City_Town[is.na(data.outcode.urbanity$City_Town)] <- 0

# Assign value with greatest share
data.outcode.urbanity$Urbanity.outcode[(data.outcode.urbanity$Large_urban > data.outcode.urbanity$Rural & data.outcode.urbanity$Large_urban > data.outcode.urbanity$City_Town)] <- "Large_urban"
data.outcode.urbanity$Urbanity.outcode[(data.outcode.urbanity$Rural > data.outcode.urbanity$Large_urban & data.outcode.urbanity$Rural > data.outcode.urbanity$City_Town)] <- "Rural"
data.outcode.urbanity$Urbanity.outcode[(data.outcode.urbanity$City_Town > data.outcode.urbanity$Rural & data.outcode.urbanity$City_Town > data.outcode.urbanity$Large_urban)] <- "City_Town"

# Merging
data.outcode.urbanity <- data.outcode.urbanity %>%
  select(outcode, Urbanity.outcode)

data.outcode <- merge(x=data.outcode, y=data.outcode.urbanity, by="outcode", all.x = T)

# Assign values of non-duplicates
data.outcode$Urbanity.outcode.y[is.na(data.outcode$Urbanity.outcode.y)] <- data.outcode$Urbanity.outcode.x[is.na(data.outcode$Urbanity.outcode.y)]
data.outcode <- data.outcode %>%
  rename(Urbanity.outcode = Urbanity.outcode.y)

sum(data.outcode$Population[data.outcode$Urbanity.outcode == "City_Town"], na.rm=T)/sum(data.outcode$Population, na.rm=T)
sum(data.outcode$Population[data.outcode$Urbanity.outcode == "Rural"], na.rm=T)/sum(data.outcode$Population, na.rm=T)
sum(data.outcode$Population[data.outcode$Urbanity.outcode == "Large_urban"], na.rm=T)/sum(data.outcode$Population, na.rm=T)


#############
## Step 3: Region ##
#############

# Focus on subsample of duplicates
duplicate.region <- data.outcode %>%
  group_by(outcode) %>%
  summarise(dup.region = length(unique(Region.quota))) %>%
  ungroup()

# Assign region if no duplicates
data.outcode$Region.outcode <- ""
data.outcode <- merge(data.outcode, duplicate.region, by="outcode")
data.outcode$Region.outcode[data.outcode$dup.region == 1] <- data.outcode$Region.quota[data.outcode$dup.region == 1]
# Get share of each region for each outcode
data.outcode.region <- data.outcode[data.outcode$dup.region > 1,] %>%
  group_by(outcode, Region.quota) %>%
  summarise(outcode, Region.quota, share = sum(Population, na.rm = T)/pop.outcode) %>%
  ungroup()

# Transform data
data.outcode.region <- data.outcode.region[!duplicated(data.outcode.region),]
data.outcode.region <- data.outcode.region %>%
  pivot_wider(names_from = Region.quota, values_from = share)

data.outcode.region$Region.outcode <- ""
# For values with only NA and NaN, we assign the NaN values
# with the following order of priority : London >> Central UK >> Rest
outcode.central <- c("CH28", "CH29", "CH34", "CH70", "NP5", "NP6", "PE17", "S19", "S30", "S31")
data.outcode.region$Region.outcode[data.outcode.region$outcode %in% outcode.central] <- "Central UK"
data.outcode.region$Region.outcode[data.outcode.region$outcode == "WD2"] <- "London"

# Replace NA with 0 to sum
region <- c("London", "Southern England", "Northern England", "Northern UK", "Central UK")
data.outcode.region[,region] <- replace(data.outcode.region[,region], is.na(data.outcode.region[region]), 0)

# Assign value with greatest share
data.outcode.region$Region.outcode[(data.outcode.region["Southern England"] > data.outcode.region["London"] & data.outcode.region["Southern England"] > data.outcode.region["Northern England"] & data.outcode.region["Southern England"] > data.outcode.region["Northern UK"] & data.outcode.region["Southern England"] > data.outcode.region["Central UK"])] <- "Southern England"
data.outcode.region$Region.outcode[(data.outcode.region["Northern England"] > data.outcode.region["London"] & data.outcode.region["Northern England"] > data.outcode.region["Southern England"] & data.outcode.region["Northern England"] > data.outcode.region["Northern UK"] & data.outcode.region["Northern England"] > data.outcode.region["Central UK"])] <- "Northern England"
data.outcode.region$Region.outcode[(data.outcode.region["Northern UK"] > data.outcode.region["London"] & data.outcode.region["Northern UK"] > data.outcode.region["Southern England"] & data.outcode.region["Northern UK"] > data.outcode.region["Northern England"] & data.outcode.region["Northern UK"] > data.outcode.region["Central UK"])] <- "Northern UK"
data.outcode.region$Region.outcode[(data.outcode.region["Central UK"] > data.outcode.region["London"] & data.outcode.region["Central UK"] > data.outcode.region["Southern England"] & data.outcode.region["Central UK"] > data.outcode.region["Northern England"] & data.outcode.region["Central UK"] > data.outcode.region["Northern UK"])] <- "Central UK"
data.outcode.region$Region.outcode[(data.outcode.region["London"] > data.outcode.region["Southern England"] & data.outcode.region["London"] > data.outcode.region["Northern England"] & data.outcode.region["London"] > data.outcode.region["Northern UK"] & data.outcode.region["London"] > data.outcode.region["Central UK"])] <- "London"

# Merging
data.outcode.region <- data.outcode.region %>%
  select(outcode, Region.outcode)

data.outcode <- merge(x=data.outcode, y=data.outcode.region, by="outcode", all.x = T)
# Assign values of non-duplicates
data.outcode$Region.outcode.y[is.na(data.outcode$Region.outcode.y)] <- data.outcode$Region.outcode.x[is.na(data.outcode$Region.outcode.y)]
data.outcode <- data.outcode %>%
  rename(Region.outcode = Region.outcode.y)


sum(data.outcode$Population[data.outcode$Region.outcode == "London"], na.rm = T)/sum(data.outcode$Population, na.rm = T)
sum(data.outcode$Population[data.outcode$Region.outcode == "Southern England"], na.rm = T)/sum(data.outcode$Population, na.rm = T)
sum(data.outcode$Population[data.outcode$Region.outcode == "Central UK"], na.rm = T)/sum(data.outcode$Population, na.rm = T)
sum(data.outcode$Population[data.outcode$Region.outcode == "Northern England"], na.rm = T)/sum(data.outcode$Population, na.rm = T)
sum(data.outcode$Population[data.outcode$Region.outcode == "Northern UK"], na.rm = T)/sum(data.outcode$Population, na.rm = T)

## Final Export
final <- data.outcode %>%
  select(outcode, Urbanity.outcode, Region.outcode)

final <- final[!duplicated(final),]


final$Urbanity.outcode[final$Urbanity.outcode=="Rural"] <- 1
final$Urbanity.outcode[final$Urbanity.outcode=="City_Town"] <- 2
final$Urbanity.outcode[final$Urbanity.outcode=="Large_urban"] <- 3

final$Region.outcode[final$Region.outcode == "London"] <- 1
final$Region.outcode[final$Region.outcode == "Southern England"] <- 2
final$Region.outcode[final$Region.outcode == "Central UK"] <- 3
final$Region.outcode[final$Region.outcode == "Northern England"] <- 4
final$Region.outcode[final$Region.outcode == "Northern UK"] <- 5
final$Region.outcode <- as.integer(final$Region.outcode)

final <- final %>%
  rename(
    zipcode = outcode,
    urbanity = Urbanity.outcode,
    region = Region.outcode
  )
write.csv(final,"UK_zipcode.csv", row.names=F, quote = FALSE)













################################################################################
#                         UNITED STATE OF AMERICA zipcode                              #
################################################################################

##################################################################################################################################################################
#This code computes population counts per level of urbanity in the US                                                                        
#Source : 2020 Census
#Urbanity of zipcodes comes from a code from this github https://github.com/bixiou/oecd_climate/tree/main/code_oecd/zipcodes that must be found searched again
#This code then assigns regions to zipcodes - but the method is sub-optimal and can be modofied the next time
##################################################################################################################################################################

#Population per region come directly from this source (2024 estimate)
#https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-total.html
#"../robustness_global_redistr/data_ext/source_zipcode/NST-EST2024-POP.xlsx"

library(dplyr) # /!\ BUG if plyr (or memisc?) loaded before => detach("package:plyr", unload = TRUE)
library("openxlsx")

# source : https://data.census.gov/table?q=All+5-digit+ZIP+Code+Tabulation+Areas+within+United+States+Populations+and+People
data <- read.csv("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/US/DECENNIALDHC2020.P1-Data.csv", skip=1)

data <- data %>%
  select(Geographic.Area.Name, X...Total) %>%  
  mutate(zipcode = substr(Geographic.Area.Name, nchar(Geographic.Area.Name) - 4, nchar(Geographic.Area.Name))) %>%
  select(zipcode, X...Total)

#source : https://github.com/bixiou/oecd_climate/tree/main/code_oecd/zipcodes
data_urb <- read.csv("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/US/zipcode_US.csv")

data$zipcode <- as.integer(data$zipcode)
data_urb$zipcode <- as.integer(data_urb$zipcode)

merge_1 <- merge(x=data, y=data_urb, by="zipcode", all.x=TRUE)

population_by_urbanity <- merge_1 %>%
  group_by(urbanity) %>%
  summarise(total_population = sum(X...Total, na.rm = TRUE))

print(population_by_urbanity)

datacode <- read.csv("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/US/zipcode_US.csv")

# source : https://redivis.com/datasets/b36a-8fmm08tgf
#The file is >100 Mo so you won't find it on the github but it is often updated and easily accessible
datastate <- read.csv ("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/US/us_zip_codes_to_county.csv")

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

# Allocates regions to the fex missing zipcodes - not optimal but right because zipcodes are split regularly
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

write.csv(merge_1, "us_25_zipcode.csv", row.names = FALSE, quote = FALSE)


final_data <- merge_1 %>%
  transmute(
    zipcode,
    urbanity,
    region = case_when(
      region == "Northeast" ~ 1,
      region == "Midwest"   ~ 2,
      region == "South"     ~ 3,
      region == "West"      ~ 4,
      TRUE                  ~ 0  # pour NA, "Unknown", ou tout autre cas
    )
  )


write.csv(final_data, "zipcode_US_region.csv", row.names = FALSE, quote = FALSE)











################################################################################
#                          France (calcul)                             #
################################################################################
############################################################################################
#This code computes the urbanity distribution of French population per degree of urbanity  # 
#Data provided in 2015, rarely updated, population described per zipcode                   #
############################################################################################


library(dplyr) # /!\ BUG if plyr (or memisc?) loaded before => detach("package:plyr", unload = TRUE)
library("openxlsx")

#source https://public.opendatasoft.com/explore/dataset/correspondance-code-insee-code-postal/table/
#data from 2015 but they are the best with this scale of precision according to INSEE
data <-read.csv("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/FR/correspondance-code-insee-code-postal.csv", sep=';')
data <- as.data.frame(data)

data <- data %>% 
  select(Code.INSEE, Code.Postal, Population) %>%  
  rename(CODGEO =  Code.INSEE, Code_Postal= Code.Postal, Population_commune = Population)

data <- data %>%
  mutate(Code_Postal = as.integer(substr(Code_Postal, 1, 5)))

#urbanity per zipcode data comes from a code that was created in 2020 accessible athe followong address
#https://github.com/bixiou/oecd_climate/blob/main/code_oecd/FR_commune.R
#source : https://github.com/bixiou/robustness_global_redistr/blob/main/data_ext/zipcode_FR.csv
data2 <-read.csv("C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/FR/zipcode_FR.csv")

data2 <- data2 %>%
  mutate(zipcode=as.integer(zipcode)
  )

merge_1 <- merge(data,data2,by.x="Code_Postal",by.y="zipcode",all.x=TRUE)

population_by_urbanity <- merge_1 %>%
  group_by(urbanity) %>%
  summarise(total_population = sum(Population_commune, na.rm = TRUE))

print(population_by_urbanity)

na_urbanity <- merge_1 %>% filter(is.na(urbanity))
print(na_urbanity)

#REGIONS FRANCE
#This code computes the population in the 5 regions of France : North/East, Ile de France, SouthEast, SouthWest, and the rest. #
#Latest data from 2025, INSEE - updated yearly.                                                                                #
################################################################################################################################

#source : https://www.insee.fr/fr/statistiques/8331297
#keep what is necesary on the sheet

data <- read.xlsx(
  "C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/FR/estim-pop-dep-sexe-gca-1975-2025.xlsx",
  sheet = 2,
  startRow = 5
)[1:105, 1:8]

data <- data %>%
  rename(departement="X1", Name="X2", Population="Total")
head(data)




data <- data %>%
  mutate(region = case_when(
    departement %in% c("75", "77", "78", "91", "92", "93", "94", "95") ~ 1,
    departement %in% c("08", "10", "51", "52", "54", "55", "57", "67", "68", "88", "21", "25", "39", "58", "70", "71", "89", "90","59", "62", "80", "02", "60") ~ 2,
    departement %in% c("09", "12", "19", "23", "24", "31", "32", "33", "40", "46", "47", "48", "64", "65", "81", "82", "87") ~ 3,
    departement %in% c("01", "03", "07", "11", "13", "15", "26", "30", "34", "38", "42", "43", "48", "63", "66", "69", "73", "74", "83", "84") ~ 4,
    TRUE ~ 5
  ))

# Strings a excluir
to_remove <- c(
  "France métropolitaine ",
  "DOM",
  "France métropolitaine et DOM",
  "Source : Insee - Estimations de population (résultats précoces arrêtés fin 2024)"
)

# 1) Base R
data<- data[ ! data$departement %in% to_remove, ]


data <- data %>%
  filter( ! departement %in% to_remove )



# Compute population per region
population_par_region <- data %>%
  group_by(region) %>%
  summarise(total_population = sum(Population, na.rm = TRUE))

print(population_par_region)

#-------------
#-------------
#-------------
#-------------
#-------------
#-------------
#-------------


#base utilisé dans l'enquete 
zipcode_fr <- read.csv(
  "C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/FR/zipcode_FR_region5.csv")

#base https://www.data.gouv.fr/fr/datasets/base-officielle-des-codes-postaux/
zipcode_fr_2 <- read.csv(
  "C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/FR/019HexaSmal.csv",
  fileEncoding = "latin1",
  sep = ";"
)

zipcode_fr_2 <- zipcode_fr_2 %>%
  select(Code_postal, Nom_de_la_commune, X.Code_commune_INSEE)

zipcode_fr_2 <- zipcode_fr_2 %>%
  rename(zipcode = Code_postal)




#Fusioner les deux base :
zipcode_fr_merged <- zipcode_fr %>%
  left_join(zipcode_fr_2, by = "zipcode")

#https://www.insee.fr/fr/statistiques/fichier/8290591/ensemble.zip
pop_fr =  read.csv(
  "C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/FR/ensemble/donnees_communes.csv",
  fileEncoding = "latin1",
  sep = ";"
)


pop_fr<- pop_fr %>%
  select(REG, RÃ.gion, COM, PTOT)

pop_fr <- pop_fr %>%
  rename( X.Code_commune_INSEE = COM)


#t supprimer les doublons et ne garder qu’une seule ligne par combinaison unique de zipcode et X.Code_commune_INSEE
zipcode_fr_merged_unique <- zipcode_fr_merged %>%
  distinct(zipcode, X.Code_commune_INSEE, .keep_all = TRUE)


#Fusioner avec pop, à partir de code commune
zipcode_fr_pop <- zipcode_fr_merged_unique %>%
  left_join(pop_fr, by = "X.Code_commune_INSEE")




total_population <- zipcode_fr_pop %>%
  summarise(total_ptot = sum(PTOT, na.rm = TRUE))

print(total_population)




#------


# Verificar duplicatas de code commune
dublons_commune <- zipcode_fr_pop %>%
  group_by(X.Code_commune_INSEE) %>%
  filter(n() > 1)

# Verificar duplicatas de zipcode
dublons_zipcode <- zipcode_fr_pop %>%
  group_by(zipcode) %>%
  filter(n() > 1)

# Verificar duplicatas da combinação code commune + zipcode
dublons_commune_zipcode <- zipcode_fr_pop %>%
  group_by(X.Code_commune_INSEE, zipcode) %>%
  filter(n() > 1)

# Ver quantos há em cada caso
n_distinct(dublons_commune$X.Code_commune_INSEE)   # Número de communes duplicadas
n_distinct(dublons_zipcode$zipcode)                # Número de zipcodes duplicados
nrow(dublons_commune_zipcode)                      # Número total de duplicatas da combinação


# Número de duplicatas por variável ou combinação
sum(duplicated(zipcode_fr_pop$X.Code_commune_INSEE))
sum(duplicated(zipcode_fr_pop$zipcode))
sum(duplicated(zipcode_fr_pop %>% select(X.Code_commune_INSEE, zipcode)))



library(dplyr)
library(data.table)

dt <- as.data.table(zipcode_fr_pop)

# marca o "master zip" em cada commune
dt[, is_master := zipcode == min(zipcode, na.rm = TRUE),
   by = X.Code_commune_INSEE]

# agrega direto só com as linhas master
zip_dt <- dt[is_master == TRUE,
             .(
               population  = sum(PTOT, na.rm = TRUE),
               urbanity    = first(urbanity),
               region = first(region)
             ),
             by = zipcode]

# espiar resultado
head(zip_dt)




library(data.table)

# Supondo que zip_dt já seja data.table:
# Proporção por urbanity
urbanity_prop <- zip_dt[
  , .(pop_total = sum(population, na.rm = TRUE)),        # soma de pop por urbanity
  by = urbanity
][
  , proporcao := pop_total / sum(pop_total)              # divide pelo total geral
]

# Proporção por região
region_prop <- zip_dt[
  , .(pop_total = sum(population, na.rm = TRUE)),        
  by = region
][
  , proporcao := pop_total / sum(pop_total)
]

# Exibir resultados
urbanity_prop
region_prop

#-----------------------
#-----------------------
#-----------------------
#-----------------------
#-----------------------
#-----------------------


zipcode_fr <- zipcode %>%
  filter(CNTR_ID == "FR") %>%
  
  # 2. Remove single quotes from the NUTS3_2024 column
  mutate(NUTS3_2024 = gsub("'", "", NUTS3_2024)) %>%
  
  # 3. Select only the relevant columns for analysis
  select(POSTCODE, NUTS3_2024, LAU_NAT,NSI_CODE_2, DGURBA)



#base https://www.data.gouv.fr/fr/datasets/base-officielle-des-codes-postaux/
zipcode_fr_2 <- read.csv(
  "C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/FR/019HexaSmal.csv",
  fileEncoding = "latin1",
  sep = ";"
)

zipcode_fr_2 <- zipcode_fr_2 %>%
  rename(POSTCODE= Code_postal)

# 1) Transformer POSTCODE dans zipcode_fr_2 :
zipcode_fr_2 <- zipcode_fr_2 %>%
  mutate(
    POSTCODE = str_pad(
      as.character(POSTCODE), # convertir l’entier en caractère
      width = 5,               # s’assurer d’avoir 5 caractères
      side = "left",           # remplissage à gauche
      pad = "0"                # avec des “0”
    )
  )


zipcode_fr_2 <- zipcode_fr_2 %>%
  select(POSTCODE, Nom_de_la_commune, X.Code_commune_INSEE)




#Fusioner les deux base :
zipcode_fr_merged <- zipcode_fr %>%
  left_join(zipcode_fr_2, by = "POSTCODE")

#https://www.insee.fr/fr/statistiques/fichier/8290591/ensemble.zip
pop_fr =  read.csv(
  "C:/Users/raque/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/Trabalhos/Adrien Fabre - Questionnaire/FR/ensemble/donnees_communes.csv",
  fileEncoding = "latin1",
  sep = ";"
)


pop_fr<- pop_fr %>%
  select(REG, RÃ.gion, COM, PTOT)

pop_fr <- pop_fr %>%
  rename( X.Code_commune_INSEE = COM)


#t supprimer les doublons et ne garder qu’une seule ligne par combinaison unique de zipcode et X.Code_commune_INSEE
zipcode_fr_merged_unique <- zipcode_fr_merged %>%
  distinct(POSTCODE, X.Code_commune_INSEE, .keep_all = TRUE)


#Fusioner avec pop, à partir de code commune
zipcode_fr_pop <- zipcode_fr_merged_unique %>%
  left_join(pop_fr, by = "X.Code_commune_INSEE")




total_population <- zipcode_fr_pop %>%
  summarise(total_ptot = sum(PTOT, na.rm = TRUE))

print(total_population)




#------


# Verificar duplicatas de code commune
dublons_commune <- zipcode_fr_pop %>%
  group_by(X.Code_commune_INSEE) %>%
  filter(n() > 1)

# Verificar duplicatas de zipcode
dublons_zipcode <- zipcode_fr_pop %>%
  group_by(POSTCODE) %>%
  filter(n() > 1)

# Verificar duplicatas da combinação code commune + zipcode
dublons_commune_zipcode <- zipcode_fr_pop %>%
  group_by(X.Code_commune_INSEE, POSTCODE) %>%
  filter(n() > 1)

# Ver quantos há em cada caso
n_distinct(dublons_commune$X.Code_commune_INSEE)   # Número de communes duplicadas
n_distinct(dublons_zipcode$POSTCODE)                # Número de zipcodes duplicados
nrow(dublons_commune_zipcode)                      # Número total de duplicatas da combinação


# Número de duplicatas por variável ou combinação
sum(duplicated(zipcode_fr_pop$X.Code_commune_INSEE))
sum(duplicated(zipcode_fr_pop$POSTCODE))
sum(duplicated(zipcode_fr_pop %>% select(X.Code_commune_INSEE, POSTCODE)))



library(dplyr)
library(data.table)

dt <- as.data.table(zipcode_fr_pop)

# marca o "master zip" em cada commune
dt[, is_master := POSTCODE == min(POSTCODE, na.rm = TRUE),
   by = X.Code_commune_INSEE]

# agrega direto só com as linhas master
zip_dt <- dt[is_master == TRUE,
             .(
               population  = sum(PTOT, na.rm = TRUE),
               DGURBA    = first(DGURBA),
               departement = first(REG)
             ),
             by = POSTCODE]

# espiar resultado
head(zip_dt)




library(data.table)

# Supondo que zip_dt já seja data.table:
# Proporção por urbanity
urbanity_prop <- zip_dt[
  , .(pop_total = sum(population, na.rm = TRUE)),        # soma de pop por urbanity
  by = DGURBA
][
  , proporcao := pop_total / sum(pop_total)              # divide pelo total geral
]

# Proporção por região
region_prop <- zip_dt[
  , .(pop_total = sum(population, na.rm = TRUE)),        
  by = region
][
  , proporcao := pop_total / sum(pop_total)
]

# Exibir resultados
urbanity_prop
region_prop


