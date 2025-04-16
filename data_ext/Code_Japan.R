library(dplyr)
library(stringr)

data1 <- read.csv("C:/Users/Erwan Akrour/Downloads/KEN_ALL_ROME/KEN_ALL_ROME.CSV", sep = ",", fileEncoding = "Shift-JIS", header = F) # Zipcode and Municipality 
code <- read.csv("C:/Users/Erwan Akrour/Downloads/JP_zipcode.csv", sep = ",")
data.pop2 <- read.csv2("/Users/Erwan Akrour/Downloads/FEI_CITY_250327223303.csv", sep=",", fileEncoding = "cp932", header = F, skip = 9) #Population by Municipality in 2020

### PART 1  

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

# Assign urbal, if small zip contains at least one zip w/ more than 100k
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


# Copy of merge_1
merge_final <- merge_1 %>% 
mutate(Municipality.type2 = word(Municipality.short, -1))



# EXPORT OLD VERSION OF JAPAN ZIPCODE
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


# Keeping only 4 columns
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


write.csv(merge_final2,"C:/Users/Erwan Akrour/Downloads/Japan_zipcode3.csv", row.names=F)



### POPULATION RURAL/URBAN AND REGION

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


### END



