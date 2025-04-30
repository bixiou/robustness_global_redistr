############################################################################################
#This code computes the urbanity distribution of French population per degree of urbanity  # 
#Data provided in 2015, rarely updated, population described per zipcode                   #
############################################################################################


library(dplyr) # /!\ BUG if plyr (or memisc?) loaded before => detach("package:plyr", unload = TRUE)
library("openxlsx")

#source https://public.opendatasoft.com/explore/dataset/correspondance-code-insee-code-postal/table/
#data from 2015 but they are the best with this scale of precision according to INSEE
data <-read.csv("../robustness_global_redistr/data_ext/source_zipcode/correspondance-code-insee-code-postal.csv", sep=';')
data <- as.data.frame(data)

data <- data %>% 
  select(Code.INSEE, Code.Postal, Population) %>%  
  rename(CODGEO =  Code.INSEE, Code_Postal= Code.Postal, Population_commune = Population)

data <- data %>%
  mutate(Code_Postal = as.integer(substr(Code_Postal, 1, 5)))

#urbanity per zipcode data comes from a code that was created in 2020 accessible athe followong address
#https://github.com/bixiou/oecd_climate/blob/main/code_oecd/FR_commune.R
#source : https://github.com/bixiou/robustness_global_redistr/blob/main/data_ext/zipcode_FR.csv
data2 <-read.csv("../robustness_global_redistr/data_ext/source_zipcode/zipcode_FR.csv")

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
