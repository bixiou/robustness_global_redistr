world <- read.csv2("../data_ext/WID/WID_data_WO.csv") # There is also WO-MER

world_thousandile <- as.numeric(c(quadratic_interpolations(as.numeric(sapply(1:99, function(i) world$value[world$percentile == paste0("p", i-1, "p", i) & world$variable == "aptincj992" & world$country == "WO" & world$year == 2023])), 
                                                                   as.numeric(sapply(1:99, function(i) world$value[world$percentile == paste0("p", i-1, "p", i) & world$variable == "tptincj992" & world$country == "WO" & world$year == 2023])), 
                                                                   0:99, seq(0.1, 99, 0.1)),
                                  world$value[world$percentile == paste0("p99p99.1") & world$variable == "aptincj992" & world$country == "WO" & world$year == 2023], 
                                  sapply(2:9, function(i) world$value[world$percentile == paste0("p99.", (i-1) %% 10, "p99.", i %% 10) & world$variable == "aptincj992" & world$country == "WO" & world$year == 2023]),
                                  world$value[world$percentile == paste0("p99.9p100") & world$variable == "aptincj992" & world$country == "WO" & world$year == 2023]))
write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(world_thousandile)), file = "../data_ext/world_thousandile.csv", row.names = F)

# View(world)
sort(unique(world$variable)) # a: average, t: threshold / pt: pre-tax (but post social contrib), di: post-tax, ca: disposable / inc: income, pfcar: carbon footprint / 992: all above 20, 999: all age / j: Equal-split adults,	i: individual
# gethin <- read.dta13("../data_ext/fisher-gethin-redistribution-2024-06-27.dta") # pdi: disposable (pretax - direct taxes + gov_soc: social assistance transfers), pni = diinc: posttax (pretax - all taxes + all transfers), pni_edp, dina_: venant de DINA existantes, pre: pretax, cons: conso imputée à partir de pretax
# gethin <- gethin[gethin$year == 2019,]
# write.csv(gethin, "../data_ext/fisher-gethin-redistribution-2024-06-27.csv", row.names = F) # Fisher-Post & Gethin (2023) https://www.dropbox.com/scl/fi/yseottljqpzom1lrqga5c?e=1
gethin <- read.csv("../data_ext/fisher-gethin-redistribution-2024-06-27.csv")
View(gethin) # edp: education distributed proportionally (to posttax inc, like in WID, instead of school attendance)
# 174 countries, not missing any important one (~2M people missing in total); data from 2019
# View(gethin[gethin$iso %in% c("FR", "DE", "IT", "ES") & gethin$p %in% c(9000, 74000, 89000),])

inflation <- read.xlsx("../data_ext/inflation_imf.xlsx")
# Inflation from data/inflation_worldbank.xslx. Accessed 12/21/24. Sources: 2020-2023: Ha et al. (2023), https://www.worldbank.org/en/research/brief/inflation-database; 2024: IMF WEO (Oct 2024) https://www.imf.org/external/datamapper/PCPIPCH@WEO/WEOWORLD/VEN
gethin <- merge(gethin, inflation, all.x = T)
for (y in 2018:2024) gethin[[paste0("inflation_", y)]][is.na(gethin[[paste0("inflation_", y)]]) | gethin[[paste0("inflation_", y)]] == "no data"] <- 1
for (y in 2018:2024) gethin[[paste0("inflation_", y)]] <- as.numeric(gethin[[paste0("inflation_", y)]])
gethin$inflation_2020_2022 <- (1+gethin$inflation_2020/100)*(1+gethin$inflation_2021/100)*(1+gethin$inflation_2022/100)
gethin$inflation_2023_2024 <- (1+gethin$inflation_2023/100)*(1+gethin$inflation_2024/100)
gethin$inflation_2018_2024 <- gethin$inflation_2023_2024*gethin$inflation_2020_2022*(1+gethin$inflation_2019/100)*(1+gethin$inflation_2018/100)

mer <- read.csv2("../data_ext/LCU_USD_2023_wid.csv") # Market exchange rate, LCU per USD, 2023, xlcusx_999_i_2023, Fetched from https://wid.world/data/ on 12/21/2024.
gethin <- merge(gethin, mer, all.x = T)
gethin$lcu_usd <- as.numeric(gethin$lcu_usd)
gethin$lcu19_mer23 <- gethin$lcu_usd * gethin$defl / (1+gethin$inflation_2023/100)
gethin$lcu19_mer23[is.na(gethin$lcu19_mer23)] <- 1
gethin$lcu19_mer23[gethin$isoname %in% c("Venezuela", "Zimbabwe", "Sierra Leone")] <- (gethin$xppp_us * gethin$defl)[gethin$isoname %in% c("Venezuela", "Zimbabwe", "Sierra Leone")]

growth <- read.xlsx("../data_ext/growth_imf.xlsx") # Real GDP growth, IMF WEO (Oct 24), Accessed on 12/21/2024, https://www.imf.org/external/datamapper/NGDP_RPCH@WEO/OEMDC/ADVEC/WEOWORLD
gethin <- merge(gethin, growth, all.x = T)
gethin$growth_2020_2024 <- (1+gethin$growth_2020/100)*(1+gethin$growth_2021/100)*(1+gethin$growth_2022/100)*(1+gethin$growth_2023/100)*(1+gethin$growth_2024/100)
gethin$growth_2020_2024[is.na(gethin$growth_2020_2024)] <- 1
sapply(countries, function(c) mean(gethin$growth_2020_2024[gethin$iso == c], na.rm = T))

# Variables are in PPP $ 2024
gethin$lcu19_growth_ppp24 <- gethin$growth_2020_2024 * mean(gethin$inflation_2023_2024[gethin$iso == "US"], na.rm = T) / (gethin$xppp_us * gethin$defl) 
gethin$disposable_inc <- gethin$a_pdi * gethin$lcu19_growth_ppp24
gethin$post_transfer_inc <- (gethin$a_pre + gethin$gov_soc) * gethin$lcu19_growth_ppp24
gethin$pretax_inc <- gethin$a_pre * gethin$lcu19_growth_ppp24

# # Variables are in MER $ 2023
# gethin$disposable_inc <- gethin$a_pdi * gethin$growth_2020_2024 / gethin$lcu19_mer23
# gethin$post_transfer_inc <- (gethin$a_pre + gethin$gov_soc) * gethin$growth_2020_2024 / gethin$lcu19_mer23
# gethin$pretax_inc <- gethin$a_pre * gethin$growth_2020_2024 / gethin$lcu19_mer23

gethin$disposable_inc_ppp22 <- gethin$a_pdi / (gethin$xppp_us * gethin$defl) # convert into constant $ (xppp_us is the LCU/ PPP$ conversion and defl the local inflation until 2022)
gethin$post_transfer_inc_ppp22 <- (gethin$a_pre + gethin$gov_soc) / (gethin$xppp_us * gethin$defl)
gethin$pretax_inc_ppp22 <- gethin$a_pre / (gethin$xppp_us * gethin$defl)

decrit((gethin$growth_2020_2024 / gethin$lcu19_mer23) / (gethin$xppp_us * gethin$defl))
decrit(gethin$isoname[(gethin$growth_2020_2024 / gethin$lcu19_mer23) / (gethin$xppp_us * gethin$defl) < 1e-4])

# Compute world distribution
compute_world_distrib_from_gethin <- function(var, year = 2019) {
  data <- gethin %>% arrange(year, !!as.symbol(var)) %>% group_by(year) %>% mutate(x = cumsum(weight), tot = sum(weight), x = x / tot)
  breakpoints <- c(seq(0, 0.99, by = 0.01), 0.999, 0.9999, 1)
  data <- data %>% mutate(p = cut(x, breaks = breakpoints, labels = breakpoints[-length(breakpoints)], include.lowest = TRUE),
                          p = as.numeric(as.character(p)), p = ifelse(is.na(p), 0.9999, p)) %>% # Replace NA with 0.9999
                   group_by(year, p) %>% dplyr::summarize(!!as.symbol(paste0(var, "_thre")) := min(!!as.symbol(var)), 
                                                   !!as.symbol(paste0(var, "_mean")) := weighted.mean(!!as.symbol(var), weight)) %>% ungroup() # %>%
    # Computes income shares
                  # mutate(p = round(p * 1e5, 1)) %>% 
                  # group_by(year) %>% mutate(pop = ifelse(lead(p, default = 1) > p, (lead(p, default = 1) - p) / 1e5, 1 - p / 1e5)) %>% # compute population fraction
                  # mutate(prod = pop * disposable_inc_mean, world_mean_disposable_inc = sum(prod, na.rm = TRUE), 
                  #        disposable_inc_share = (disposable_inc_mean / world_mean_disposable_inc) * pop)  %>%
                  # arrange(year, desc(p)) %>%  group_by(year) %>% mutate(disposable_inc_top_share = cumsum(disposable_inc_share), disposable_inc_bottom_share = 1 - disposable_inc_top_share) %>% arrange(year, p)

  if (!is.null(year)) data <- data[data$year == year, names(data) != "year"]
  return(data)
}
world_post_transfer_inc <- compute_world_distrib_from_gethin("post_transfer_inc")
View(world_post_transfer_inc)
thousandile_world_post_transfer_inc <- c(quadratic_interpolations(world_post_transfer_inc$post_transfer_inc_mean, world_post_transfer_inc$post_transfer_inc_thre, 
                                                                   c((0:99)/100, .999, 1), seq(0.000, .998, 0.001)), world_post_transfer_inc$post_transfer_inc_mean[101:102] %*% c(.9, .1))
# /!\ Pb: the top interpolation is linear, inflating the mean income => do piecewise linear to preserve the mean (but won't preserve the smoothness)
write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(thousandile_world_post_transfer_inc)), file = "../data_ext/world_post_transfer_inc.csv", row.names = F)

world_disposable_inc_ppp22 <- compute_world_distrib_from_gethin("disposable_inc_ppp22")
thousandile_world_disposable_inc_ppp22 <- c(quadratic_interpolations(pmax(0, world_disposable_inc_ppp22$disposable_inc_ppp22_mean), pmax(0, world_disposable_inc_ppp22$disposable_inc_ppp22_thre), 
                                                               c((0:99)/100, .999, 1), seq(0.000, .998, 0.001)), world_disposable_inc_ppp22$disposable_inc_ppp22_mean[101:102] %*% c(.9, .1))
# /!\ Pb: the top interpolation is linear, inflating the mean income => do piecewise linear to preserve the mean (but won't preserve the smoothness)
write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(thousandile_world_disposable_inc_ppp22)), file = "../data_ext/world_disposable_inc_ppp22.csv", row.names = F)

world_disposable_inc <- compute_world_distrib_from_gethin("disposable_inc")
thousandile_world_disposable_inc <- c(quadratic_interpolations(pmax(0, world_disposable_inc$disposable_inc_mean), pmax(0, world_disposable_inc$disposable_inc_thre), 
                                                                  c((0:99)/100, .999, 1), seq(0.000, .998, 0.001)), world_disposable_inc$disposable_inc_mean[101:102] %*% c(.9, .1))
# /!\ Pb: the top interpolation is linear, inflating the mean income => do piecewise linear to preserve the mean (but won't preserve the smoothness)
write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(thousandile_world_disposable_inc)), file = "../data_ext/world_disposable_inc.csv", row.names = F)

tax_revenue <- function(distr = thousandile_world_disposable_inc, weight = NULL, rate = .1, threshold = 48e3) {
  if (is.null(weight)) return(sum(pmax(0, rate*(distr - threshold)))/sum(distr)) 
  else return(sum(pmax(0, rate*(distr - threshold)) * weight, na.rm = T)/sum(distr * weight, na.rm = T)) } 
tax_cost <- function(threshold = 2555, distr = thousandile_world_disposable_inc, weight = NULL) {
  if (is.null(weight)) return(sum(pmax(0, threshold - distr))/sum(distr)) 
  else return(sum(pmax(0, threshold - distr) * weight, na.rm = T)/sum(distr * weight, na.rm = T)) } 
tax_revenue() # 2.33% world GNI
tax_cost(threshold = 365*7.5*mean(gethin$inflation_2018_2024[gethin$iso == "US"], na.rm = T)) # 2.33%
tax_revenue(rate = .12, threshold = 60e3) # 2.37%
tax_revenue(rate = .1, threshold = 70e3) # 2%
tax_revenue(rate = .1, threshold = 84e3) # 1.75%
tax_revenue(rate = .1, threshold = 100e3) # 1.55%
tax_revenue(rate = .1, threshold = 120e3) # 1.37%
tax_revenue(rate = .15, threshold = 70e3) # 3%
tax_revenue(rate = .15, threshold = 80e3) # 3.5%
tax_revenue(rate = .15, threshold = 100e3) # 2.3%
tax_revenue(rate = .15, threshold = 120e3) # 2.06%
# tax_revenue(rate = .1, threshold = 1e6) # .1%
.1*(world_disposable_inc$disposable_inc_mean[102]-1e6)*1e-4/mean(thousandile_world_disposable_inc) # .2%
tax_revenue(rate = .2) # 4.66%
tax_cost(threshold = 3650) # 4.66%
tax_revenue(rate = .1, threshold = 1e5) # 1.38%
.5*(world_disposable_inc_ppp22$disposable_inc_ppp22_mean[102]-1e6)*1e-4/mean(thousandile_world_disposable_inc_ppp22) # .92%
tax_revenue(rate = .15, threshold = 1e5) # 2.07%
.15*(world_disposable_inc_ppp22$disposable_inc_ppp22_mean[102]-1e6)*1e-4/mean(thousandile_world_disposable_inc_ppp22) # .28%
# A tax of 10% above 48k (or 12%>60k or 13%>70k) can finance a $7/day floor and, topped with an extra tax of 10% above 100k and 60% above 1M (or 15%/30%) can finance, a floor of $10/day can be financed
7.5*mean(gethin$inflation_2018_2024[gethin$iso == "US"], na.rm = T) # 9.46

tax_cost(4800)
mean(thousandile_world_disposable_inc < 3000)*8
mean(thousandile_world_disposable_inc < 4800)*8
mean(thousandile_world_disposable_inc < 10*365)*8
mean(thousandile_world_disposable_inc < 9.46*365)*8
mean(thousandile_world_disposable_inc < 10*365)*8
1-mean(thousandile_world_disposable_inc < 48e3) # 6%
1-mean(thousandile_world_disposable_inc < 60e3) # 4%
1-mean(thousandile_world_disposable_inc < 70e3) # 3%
1-mean(thousandile_world_disposable_inc < 80e3) # 3%
1-mean(thousandile_world_disposable_inc < 100e3) # 1.4%
1-mean(thousandile_world_disposable_inc < 120e3) # 1.4%


thousandile_world_disposable_inc_reform_simple <- pmax(thousandile_world_disposable_inc, 2555) - .1*pmax(0, thousandile_world_disposable_inc - 48e3)
write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(thousandile_world_disposable_inc_reform_simple)), file = "../data_ext/world_disposable_inc_reform_simple.csv", row.names = F)
thousandile_world_disposable_inc_reform_double <- pmax(thousandile_world_disposable_inc, 3650) - .1*pmax(0, thousandile_world_disposable_inc - 48e3) - .15*pmax(0, thousandile_world_disposable_inc - 1e5) - .15*pmax(0, thousandile_world_disposable_inc - 1e6)
write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(thousandile_world_disposable_inc_reform_double)), file = "../data_ext/world_disposable_inc_reform_double.csv", row.names = F)

world_pretax_inc <- compute_world_distrib_from_gethin("pretax_inc")
thousandile_world_pretax_inc <- c(quadratic_interpolations(world_pretax_inc$pretax_inc_mean, world_pretax_inc$pretax_inc_thre, 
                                                                  c((0:99)/100, .999, 1), seq(0.000, .998, 0.001)), world_pretax_inc$pretax_inc_mean[101:102] %*% c(.9, .1))
write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(thousandile_world_pretax_inc)), file = "../data_ext/world_pretax_inc.csv", row.names = F)

national_disposable_inc_by_country <- sapply(unique(gethin$iso), function(c) sum(gethin$disposable_inc[gethin$iso == c] * gethin$weight[gethin$iso == c], na.rm = T))
tax_revenue_by_country <- sapply(unique(gethin$iso), function(c) tax_revenue(gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c]))
transfer_by_country <- round(sapply(unique(gethin$isoname), function(c) tax_cost(gethin$disposable_inc[gethin$isoname == c], gethin$weight[gethin$isoname == c])) - tax_revenue_by_country, 3)
sort(transfer_by_country) # Pb: Brazil, Russia, Turkey would lose ~2% GNI, Mexico ~1%, China 0.2%
View(gethin[gethin$iso == "RU", c("p", 'disposable_inc', "weight")])

countries <- c("FR", "DE", "IT", "PL", "ES", "GB", "CH", "JP", "RU", "SA", "US")
pop <- sapply(countries, function(c) mean(gethin$npop[gethin$iso == c], na.rm = T))
# Inflation from data/inflation_worldbank.xslx. Accessed 12/21/24. Sources: 2020-2023: Ha et al. (2023), https://www.worldbank.org/en/research/brief/inflation-database; 2024: IMF WEO (Oct 2024) https://www.imf.org/external/datamapper/PCPIPCH@WEO/WEOWORLD/VEN
# inflation_2020_2024 <- setNames(c(1.150747862, 1.202690072, 1.116885611, 1.391244653, 1.173488108, 1.239791209, 1.060266139, 1.062257972, 1.536277679, 1.179800076, 1.203010226), countries)
(inflation_2023_2024 <- sapply(countries, function(c) mean(gethin$inflation_2023_2024[gethin$iso == c], na.rm = T)))
(inflation_2024 <- 1+sapply(countries, function(c) mean(gethin$inflation_2024[gethin$iso == c], na.rm = T))/100)
(lcu_usd_2023 <- sapply(countries, function(c) mean(gethin$lcu_usd[gethin$iso == c], na.rm = T)))
(xppp_us <- sapply(countries, function(c) mean(gethin$xppp_us[gethin$iso == c], na.rm = T)))
# lcu_usd_2024 <- setNames(c(0.92316, 0.92316, 0.92316, 3.976265, 0.92316, 0.782297, 0.880159, 151.179433, 94, 3.752279, 1), countries) # Average MER over 2024, Accessed 12/21/2024, https://www.ofx.com/en-ie/forex-news/historical-exchange-rates/yearly-average-rates/
lcu_usd_2024 <- setNames(c(0.96, 0.96, 0.96, 4.09, 0.96, 0.80, 0.89, 156, 103, 3.75, 1), countries) # MER at 12/21/2024, Accessed from xe.com
lcu_usd_2023 * inflation_2024 / lcu_usd_2024
round(70e3*usd_lcu)
round(84e3*usd_lcu)
round(100e3*usd_lcu)
round(120e3*usd_lcu)
for (c in countries) print(paste(c, round(100*tax_revenue(gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c]), 1)))
for (c in countries) print(paste(c, round(100*tax_revenue(gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c], .25, 1e5), 1) + 
                                   round(100*tax_revenue(gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c], .25, 1e5), 1)))
(share_affected_new_tax_simple <- sapply(countries, function(c) 100 - 1e-3*min(gethin$p[gethin$iso == c & gethin$disposable_inc_ppp22 >= 70e3], na.rm = T)))
wtd.mean(share_affected_new_tax_simple, pop) # 10%
(share_affected_new_tax_double <- sapply(countries, function(c) 100 - 1e-3*min(gethin$p[gethin$iso == c & gethin$disposable_inc_ppp22 >= 100e3], na.rm = T)))
wtd.mean(share_affected_new_tax_double, pop) # 5%

# Share top income affected by new tax (in %)
(share_affected_new_tax_simple <- round(sapply(countries, function(c) 100 - 1e-3*min(gethin$p[gethin$iso == c & gethin$disposable_inc >= 70e3], na.rm = T))))
wtd.mean(share_affected_new_tax_simple, pop) # 10%
(share_affected_new_tax_simple <- round(sapply(countries, function(c) 100 - 1e-3*min(gethin$p[gethin$iso == c & gethin$disposable_inc >= 80e3], na.rm = T))))
wtd.mean(share_affected_new_tax_simple, pop) # 10%
(share_affected_new_tax_double <- round(sapply(countries, function(c) 100 - 1e-3*min(gethin$p[gethin$iso == c & gethin$disposable_inc >= 120e3], na.rm = T))))
wtd.mean(share_affected_new_tax_double, pop) # 5%

tax_cost(3000) # 2.08%: .15 > 120k
tax_cost(4800) # 5.1%: .2 > 84k + .1 > 120k + .15 > 1M OR .15 > 84k + .15 > 120k + .2 > 1M

usd_lcu <- xppp_us * inflation_2023_2024 / inflation_2023_2024["US"]
84e3*usd_lcu
120e3*usd_lcu
1e6*usd_lcu

(gdp_transferred_new_tax_simple <- sapply(countries, function(c) round(100*(tax_revenue(gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c], .15, 120e3)
                                                                - tax_cost(3000, gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c])), 1) )) 
wtd.mean(gdp_transferred_new_tax_simple, pop) # 2.1%
(gdp_transferred_new_tax_double <- sapply(countries, function(c) round(100*(tax_revenue(gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c], .15, 80e3)
                                                                     + tax_revenue(gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c], .15, 120e3)
                                                                     + tax_revenue(gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c], .15, 1e6)
                                                                     - tax_cost(4800, gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c])), 2) ))
wtd.mean(gdp_transferred_new_tax_double, pop) # 5.5%

# USA (8% of its GDP), Russia (5%), Japan (4%), Brazil (2%), China (1%), Lebanon (1%), Argentina (.8%), Iran (.5%) contribute 
# Indonesia (3%), India (10%), Pakistan (25%), Nigeria (25%), DR Congo (240%), South Sudan (730%) receive
(gdp_transferred_new_tax_double_all <- sort(sapply(unique(gethin$isoname), function(c) round(100*(tax_revenue(gethin$disposable_inc[gethin$isoname == c], gethin$weight[gethin$isoname == c], .15, 80e3)
                                                                       + tax_revenue(gethin$disposable_inc[gethin$isoname == c], gethin$weight[gethin$isoname == c], .15, 120e3)
                                                                       + tax_revenue(gethin$disposable_inc[gethin$isoname == c], gethin$weight[gethin$isoname == c], .15, 1e6)
                                                                       - tax_cost(4800, gethin$disposable_inc[gethin$isoname == c], gethin$weight[gethin$isoname == c])), 1) )))

HICs <- c("AD", "AT", "BE", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU", "IS", "IE", "IT", "LV", "LT", "LU", "MT", "MC", "NL", "NO", 
          "PL", "PT", "SM", "SK", "SI", "ES", "SE", "CH", "GB", "CA", "US", "BS", "BB", "CL", "PA", "TT", "UY", "AU", "BN", "HK", "JP", "MO", "NZ", 
          "SG", "KR", "TW", "BH", "IL", "KW", "OM", "QA", "SA", "AE", "SC", "MV")
sum(national_disposable_inc_by_country[names(national_disposable_inc_by_country) %in% HICs])/sum(national_disposable_inc_by_country) # 43% of income is in HICs
sum((transfer_by_country*national_disposable_inc_by_country)[names(national_disposable_inc_by_country) %in% HICs])/sum(national_disposable_inc_by_country[names(national_disposable_inc_by_country) %in% HICs]) # 2.9% of HIC income redistributed

plot(c((0:99)/100, .999, 1), world_post_transfer_inc$post_transfer_inc_thre, type = 'l', ylim = c(0, 1e5))
lines(c(1:1000)/1000, world_thousandile)
lines(c((0:99)/100, .999, 1), world_post_transfer_inc$post_transfer_inc_mean)
mean(world_thousandile)
world_post_transfer_inc$post_transfer_inc_mean %*% c(rep(.01, 99), .009, .0009, .0001) # )*.99+world_post_transfer_inc$post_transfer_inc_mean[100]*.009+world_post_transfer_inc$post_transfer_inc_mean[101]*.0009+world_post_transfer_inc$post_transfer_inc_mean[102]*.0001
mean(world_post_transfer_inc$post_transfer_inc_thre[1:99])
mean(world_thousandile[1:110])
mean(world_post_transfer_inc$post_transfer_inc_mean[1:11])

# max(world$year[world$variable == "aptincj992" & world$country == "WO"]) # 2023
# world_inc <- world[world$variable == "aptincj992" & world$country == "WO" & world$year == 2023, c("percentile", "value")] # PPP €/year
# world_tinc <- world[world$variable == "tptincj992" & world$country == "WO" & world$year == 2023, c("percentile", "value")] # PPP €/year
# View(world_tinc)
# world_income <- world_top_income <- world_income_threshold <- c()
# for (p in 0:99) world_income <- c(world_income, as.numeric(world$value[world$percentile == paste0("p", p, "p", p+1) & world$variable == "aptincj992" & world$country == "WO" & world$year == 2023]))
# for (p in 0:99) world_income_threshold <- c(world_income_threshold, as.numeric(world$value[world$percentile == paste0("p", p, "p", p+1) & world$variable == "tptincj992" & world$country == "WO" & world$year == 2023]))
# plot(1:99, world_income[1:99])
# world_income[98:100]
# round(as.numeric(world_inc$value[world_inc$percentile %in% c("p99p99.1", "p99.4p99.5", "p99.8p99.9", "p99.95p99.96", "p99.99p99.991", "p99.995p99.996", "p99.999p100", "p99.99p100", "p99.9p100")])/1e3)
# world_tinc$value <- as.numeric(world_tinc$value)
# View(world_inc[world_inc$percentile %in% c("p99p99.1", "p99.4p99.5", "p99.8p99.9", "p99.95p99.96", "p99.99p99.991", "p99.995p99.996", "p99.999p100", "p99.99p100", "p99.9p100"),])
# View(world_tinc[world_tinc$percentile %in% c("p99p99.1", "p99.4p99.5", "p99.8p99.9", "p99.95p99.96", "p99.99p99.991", "p99.995p99.996", "p99.999p100", "p99.99p100", "p99.9p100"),])

# world_thousandile <- 1:1000
# for (i in 1:990) world_thousandile[i] <- as.numeric(world$value[world$percentile == paste0("p", floor((i-1)/10), "p", floor((i-1)/10)+1) & world$variable == "aptincj992" & world$country == "WO" & world$year == 2023])
# world_thousandile[991] <- as.numeric(world$value[world$percentile == paste0("p99p99.1") & world$variable == "aptincj992" & world$country == "WO" & world$year == 2023])
# for (i in 992:999) world_thousandile[i] <- as.numeric(world$value[world$percentile == paste0("p99.", (i-1) %% 10, "p99.", i %% 10) & world$variable == "aptincj992" & world$country == "WO" & world$year == 2023])
# world_thousandile[1000] <- as.numeric(world$value[world$percentile == paste0("p99.9p100") & world$variable == "aptincj992" & world$country == "WO" & world$year == 2023])
# mean(world_thousandile)
# mean(world_income)
# write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(world_thousandile)), file = "../data_ext/world_thousandile.csv", row.names = F)

# world_inc_averages <- as.numeric(c(sapply(1:99, function(i) world$value[world$percentile == paste0("p", i-1, "p", i) & world$variable == "aptincj992" & world$country == "WO" & world$year == 2023]),
#                         world$value[world$percentile == paste0("p99p99.1") & world$variable == "aptincj992" & world$country == "WO" & world$year == 2023], 
#                         sapply(2:9, function(i) world$value[world$percentile == paste0("p99.", (i-1) %% 10, "p99.", i %% 10) & world$variable == "aptincj992" & world$country == "WO" & world$year == 2023]),
#                         world$value[world$percentile == paste0("p99.9p100") & world$variable == "aptincj992" & world$country == "WO" & world$year == 2023]))
# world_inc_thresholds <- as.numeric(c(sapply(1:99, function(i) world$value[world$percentile == paste0("p", i-1, "p", i) & world$variable == "tptincj992" & world$country == "WO" & world$year == 2023]),
#                                      world$value[world$percentile == paste0("p99p99.1") & world$variable == "tptincj992" & world$country == "WO" & world$year == 2023], 
#                                      sapply(2:9, function(i) world$value[world$percentile == paste0("p99.", (i-1) %% 10, "p99.", i %% 10) & world$variable == "tptincj992" & world$country == "WO" & world$year == 2023]),
#                                      world$value[world$percentile == paste0("p99.9p100") & world$variable == "tptincj992" & world$country == "WO" & world$year == 2023]))


# length(c(1:99, seq(99.1, 99.9, .1), 100))
# lines(seq(0, 100, 0.1), world_thousandile)
# mean(world_income)
# for (i in 2:length(world_thousandile)) if (world_thousandile[i] < world_thousandile[i-1]) print(i)
# world_thousandile[890:896]-world_thousandile[889:895]
# world_inc_averages[99:109]
# world_inc_thresholds[99:109]
# world_income[90:91]
# world_income[83:93]-world_income[82:92]
# mean(world_thousandile)
# quadratic_interpolation(90,91, 40211.3, 41892.1, 40421.9)(seq(90, 91, .1))
# world_thousandile[890:900]
# world_inc_thresholds[90:91]
# world_thousandile[370:380]
# world_income_threshold[38:39]
# world_inc_averages[2:3]


# n=7.84/MDE^2
# MDE=2.8/sqrt(n)
# MDE in a regression between 2 dummies. n is the total sample size (assuming 2 subsamples), y is the average outcome between subsamples, cf. also https://www.evanmiller.org/ab-testing/sample-size.html or https://www.statsig.com/calculator
# Note that these calculations are conservative as they assume a two-sided test, while the tests can generally be one-sided as the hypothesis is directional.
mde <- function(n, alpha = .05, beta = .8, y = .5) return((qnorm(1 - alpha/2) + qnorm(beta))*sqrt(4*y*(1-y))/sqrt(n))
mde(12000) # .025
mde(6000) # .036
mde(3000) # .05
mde(2500) # .06
mde(2000) # .06
mde(1500) # .07
mde(1000) # .09
mde(666) # .11
mde(500) # .125 # MDE for SA, RU (n=1k) with 4 branches: .13. That's fine.
mde(333) # .15
mde(250) # .18

# 1-alpha confidence that true value is in y +/- me
me <- function(n, alpha = .05, y = .5) return(qnorm(1 - alpha/2)*sqrt(y*(1-y)/n))
me(12000) # .01
me(3000) # .018
me(2000) # .022
me(1000) # .031
me(666) # .038
me(500) # .044 
me(333) # .054
me(250) # .062 # ME for SA, RU (n=1k) with 4 branches: .06. That's fine.

plot(1:1000, thousandile_world_disposable_inc, type = 'l', ylim = c(0, 8000*12), col = "red")
lines(1:1000, thousandile_world_post_transfer_inc, type = 'l', col = "blue")
# lines(1:1000, thousandile_world_pretax_inc, type = 'l', col = "blue")
lines(1:1000, thousandile_world_disposable_inc_post_simple_reform, type = 'l', col = "green")



#### Tests memisc/labelled #####

package("labelled")
test <- labelled(c(1, NA, -1), c("No" = 0, "Yes" = 1, "PNR" = -1))
na_values(test) <- c(-1)

test <- as.item(c(1, NA, -1), labels = c(No = 0, Yes = 1, PNR = -1), missing.values = c(NA, -1), annotation = "test")

test
as.character(test) # "Yes" NA "PNR"
as.numeric(test) # 1 NA -1 (or NaN or NA for the last one)
test %in% 1 # TRUE FALSE FALSE
test == 1 # TRUE NA FALSE
test < 1 # FALSE NA TRUE (or NA for the last one)
test %in% "Yes" # TRUE FALSE FALSE
test == "Yes" # TRUE NA FALSE
is.na(test) # FALSE TRUE FALSE
is.missing(test) # FALSE TRUE TRUE 
lm(c(T, T, T) ~ test)$rank # 2 (i.e., keeps missing values that are not NA)
df <- data.frame(test = test, true = c(T, T, T))
lm(true ~ test, data = df)$rank
decrit(df$test, miss = F)

# memisc 0.99.22: characters not recognized; numeric only recognized by == not %in%; missing not interpreted as numeric
# memisc 0.99.31: missing not interpreted as numeric/character (one needs to use as.factor(as.character(include.missings(test))) in regressions)
# labelled: characters not recognized (even error when == used); numeric characters when as.character used; is.na(missing) == T

## deprecated:
plp$test <- plp$income
plp$temp <- 2 * (plp$test %in% "between $201 and $250") - is.na(plp$test)
plp$income <- fct_na_value_to_level(factor(plp$temp, labels = c("between $201 and $250"= 2, "NA" = NA), exclude = NULL))
plp$income <- set_value_labels(plp$test,  labels = c("2"= "between $201 and $250", "NA" = NA))
plp$income <- set_value_labels(as.character(plp$temp),  labels = c("between $201 and $250" = "2", "NA" = tagged_na("-1")))
plp$income <- set_na_values(plp$income, .values = "-1")
plp$income <- labelled(plp$temp, c("between $201 and $250" = 2, "NA" = -1)) # quite good, only issue is (like memisc) character value unrecognized
plp$income <- as.item(plp$temp, labels = structure(c(0, 2, NA), names = c("No", "between $201 and $250", "PNR")),
                      missing.values = c(NA, -1), annotation = attr(plp$test, "label"))
plp$income <- labelled(plp$temp, labels = c("between $201 and $250" = 2, "PNR" = -1, "NA" = NA))
plp$income <- set_na_values(plp$income, na_values = c(-1))
levels(plp$income)
plp$income
plp$income %in% 2
plp$income %in% "between"
is.na(plp$income)
as.character(plp$income)


temp <- as.item(c(-1, 1), labels = structure(c(-1, 0, 1), names = c("PNR", "No", "Yes")), missing.values = c(NA, -1))
temp == "Yes"
temp == 1
temp %in% "Yes"
is.missing(temp)
lm(c(T, T) ~ temp)$rank


(bar <- as.factor(as.character(test)))
bar <- relevel(bar, 'PNR')

df <- within(df, { # Shorthand for ds <- within(ds,...)
  df$test_im <- include.missings(df$test)
}) 
lm(c(T, T, T) ~ test, data = df)$rank

merge_all_countries <- function(df = lapply(countries, function(c) d(c)), weight_adult = T, weight_oecd = F, weight_no_pop = T) {
  all <- Reduce(function(df1, df2) { merge(df1, df2, all = T) }, df)
  if ("weight" %in% names(all)) {
    all$weight_country <- all$weight
    all$weight_pop <- all$weight * population[all$country]
    all$weight_adult <- all$weight * adult_pop[all$country]
    all$weight_pop_oecd <- all$weight * oecd[all$country] * population[all$country]
    all$weight_adult_oecd <- all$weight * oecd[all$country] * adult_pop[all$country]
    for (w in c("weight_country", "weight_pop", "weight_adult", "weight_pop_oecd", "weight_adult_oecd")) all[[w]] <- nrow(all) * as.numeric(all[[w]] / sum(all[[w]]))
    
    if (weight_adult) {
      if (weight_oecd) all$weight <- all$weight_adult_oecd
      else all$weight <- all$weight_adult
    } else {
      if (weight_oecd) all$weight <- all$weight_pop_oecd
      else all$weight <- all$weight_pop
    }
    if (weight_no_pop) all$weight <- all$weight_country    
  } else warning("No weight defined.")
  
  # all$index_pooled_main_policies <- index_zscore("main_policies", df = all, weight = T, dummies = FALSE, require_all_variables = TRUE, efa = FALSE)
  names_indices <<- c("affected", "knowledge", "knowledge_not_dum", "knowledge_footprint", "net_zero_feasible", "worried", "positive_economy", "policies_effective",
                      "affected_subjective", "lose_policies_subjective", "lose_policies_poor", "lose_policies_rich", "fairness", "trust_govt", "willing_change", "care_poverty", "problem_inequality",
                      "standard_policy", "tax_transfers_policy", "investments_policy", "main_policies", "main_policies_all", "main_policies_all", "beef_policies",
                      "international_policies", "other_policies", "all_policies", "standard_effective",
                      "tax_transfers_effective", "investments_effective", "tax_transfers_positive_economy", "standard_positive_economy", "investments_positive_economy", "lose_standard_poor", "lose_standard_rich", "lose_standard_subjective",
                      "lose_investments_poor", "lose_investments_rich", "lose_investments_subjective", "lose_tax_transfers_poor", "lose_tax_transfers_rich", "lose_tax_transfers_subjective", "policies_emissions", "investments_emissions", "tax_emissions_plus", "investments_emissions_plus", "standard_emissions_plus", 
                      "policies_pollution", "investments_pollution", "tax_transfers_pollution", "standard_pollution", "tax_emissions", "standard_emissions", #"policies_others_effective", "investments_others_effective", "tax_transfers_others_effective", 
                      "policies_emissions_plus", "fairness_standard", "fairness_tax_transfers", "fairness_investments", "knowledge_fundamentals", "knowledge_gases", "knowledge_impacts", "worried_old", "concerned_about_CC")
  
  # "constrained", "altruism", "pro_redistribution", "earmarking_vs_transfers", "affected_income", "affected_lifestyle", "pricing_vs_norms", "distribution_critical", "attentive", "pro_climate"
  for (i in names_indices) {
    tryCatch({ temp <- index_zscore(i, df = all, weight = T, dummies = FALSE, require_all_variables = TRUE, efa = FALSE)
    all[[paste0("index_c_", i)]] <- all[[paste0("index_", i)]]
    all[[paste0("index_", i)]] <- temp # all[[paste0("index_pooled_", i)]]
    }, error = function(cond) { print(paste("Index", i, "could not be created")) } )  }
  
  # tryCatch({ for (c in countries) for (i in intersect(paste0("index_c_", names_indices), names(all))) eval(str2expression(paste(tolower(c), '[[paste0("index_c_", i)]] <<- all[[paste0("index_c_", i)]][all$country == c]'))) }, error = function(cond) { print("Fail to put pooled indices into country df") } )
  # tryCatch({ for (c in countries) for (i in intersect(paste0("index_c_", names_indices), names(all))) eval(str2expression(paste(tolower(c), '[[paste0("index_", i)]] <<- all[[paste0("index_", i)]][all$country == c]'))) }, error = function(cond) { print("Fail to put pooled indices into country df") } )
  
  try({
    all$knows_anthropogenic <- all$CC_anthropogenic == 2
    variables_knowledge_efa <- variables_knowledge
    negatives_knowledge_efa <- negatives_knowledge
    # variables_knowledge_efa <- c(variables_knowledge, "knows_anthropogenic")
    # negatives_knowledge_efa <- c(negatives_knowledge, F)
    temp <- all[,c("weight", "treatment", variables_knowledge_efa)]
    for (i in seq_along(variables_knowledge_efa)) temp[[variables_knowledge_efa[i]]] <- z_score_computation(group = c(variables_knowledge_efa[i], negatives_knowledge_efa[i], "", "F"), df = temp, weight = T) # impute mean of same treatment group to missings
    # for (i in seq_along(variables_knowledge_efa)) temp[[variables_knowledge_efa[i]]][is.pnr(temp[[variables_knowledge_efa[i]]])] <- wtd.mean(temp[[variables_knowledge_efa[i]]], weights = temp$weight, na.rm=T) # impute sample mean to missings
    loadings <- as.numeric(factanal(temp[,variables_knowledge_efa], 1)$loadings)
    names(loadings) <- variables_knowledge_efa
    loadings_efa[["all"]] <- loadings
    # print(loadings_z)
    all$index_knowledge_efa_global <- 0
    for (v in variables_knowledge_efa) all$index_knowledge_efa_global <- all$index_knowledge_efa_global + loadings[v]*temp[[v]]
    all$index_knowledge_efa_global <- (all$index_knowledge_efa_global - wtd.mean(all$index_knowledge_efa_global, weights = all$weight, na.rm = T))/sqrt(wtd.var(all$index_knowledge_efa, weights = all$weight, na.rm = T))
    label(all$index_knowledge_efa_global) <- "index_knowledge_efa_global: Weighted average of z-scores of variables in variables_knowledge_efa. Weights are loadings from explanatory factor analysis of all countries jointly (EFA with 1 factor). Each z-score is standardized with survey weights and impute mean of treatment group to missing values."
  })
  
  if ("heating_expenses" %in% names(all)) { # Replace heating expenses for countries where the variable is not defined
    all$heating_expenses_original <- all$heating_expenses
    all$heating_expenses <- as.character(all$heating_expenses)
    all[["heating_expenses"]][all$country  %in% c("MX", "BR", "IA", "ID")] <- "Don't know"
    temp <- 125*(all$heating_expenses == "< 250") + 600*(all$heating_expenses == "251-1,000") + 1250*(all$heating_expenses == "1,001-1,500") +
      2000*(all$heating_expenses == "1,501-2,500") + 3000*(all$heating_expenses == "> 2,500") - 0.1*(all$heating_expenses == "Don't know")
    all$heating_expenses <- as.item(temp, labels = structure(c(-0.1, 125, 600, 1250, 2000, 3000), names = c("Don't know","< 250","251-1,000", "1,001-1,500","1,501-2,500", "> 2,500")), missing.values=-0.1, annotation=Label(all$heating_expenses))
  }
  
  return(all)
}

GBa <- prepare(country = "GB", scope = "all", fetch = T, convert = T, pilot = TRUE, weighting = FALSE)
tail(GBa$dropout)
tail(GBp$date_recorded)

PLa <- prepare(country = "PL", scope = "all", fetch = T, convert = T, pilot = TRUE, weighting = FALSE)
tail(PLa$date[PLa$age %in% 21.5])
tail(PLa$excluded[PLa$urbanity %in% 1])
tail(PLa$date_recorded[PLa$urbanity %in% 1])
PLa$date[PLa$education_quota %in% 1 & PLa$final == T]
PLa$date[PLa$education_quota %in% 1 & PLa$excluded %in% "QuotaMet"]
tail(PLa$excluded[PLa$education_quota %in% 1])
decrit(PLa$education)

df$net_gain_maritime_pc
df$emissions_maritime_mean
revenue_maritime <- maritime_carbon_price*sum(df$emissions_maritime_mean, na.rm = T)
sum(df$emissions_maritime_mean[df$gni_pc_nom_2023 < 10000], na.rm = T)*maritime_carbon_price/revenue_maritime # 33%
df$gni_pc_nom_2023[df$code == "CHN"]/wtd.mean(df$gni_pc_nom_2023, df$pop_2023)
df$gni_pc_nom_2023[df$code == "BRA"]/wtd.mean(df$gni_pc_nom_2023, df$pop_2023)
df$emissions_maritime_mean[df$code == "CHN"]/sum(df$emissions_maritime_mean, na.rm = T) # 9% louche
df$emissions_maritime_mean[df$code == "BRA"]/sum(df$emissions_maritime_mean, na.rm = T) # 10% 
sort(setNames(df$emissions_maritime_mean/sum(df$emissions_maritime_mean, na.rm = T), df$country))
sum(df$gni_pc_nom_2023 * df$pop_2023 * df$code %in% LIC, na.rm = T)/sum(df$gni_pc_nom_2023 * df$pop_2023, na.rm = T) # 0.5%
sum(df$pop_2023 * df$code %in% LIC, na.rm = T)/sum(df$pop_2023, na.rm = T) # 9%
sum(df$emissions_maritime_mean * df$code %in% LIC, na.rm = T)/sum(df$emissions_maritime_mean, na.rm = T) # 1%
# p*u-y
# Sp=1, Sy=u
# y*r+(1-r)u*f=p*u => f=(p*u-y*r/((1-r)*u) 
# Pour s'approcher d'une égale recette par habitant, laisser chaque État récupérer une part réservée r des recettes collectées, et sur le 1-r mis en commun,
# lui attribuer une part proportionnelle à la recette égale par habitant diminué de ce que l'État a déjà obtenu.
# Si pour l'État le plus riche, la recette égale n'est pas inférieure à sa part réservée y*r, on arrive exactement à la recette égalitaire.
# Pour que ce soit le cas, il faudrait que r < 0.25. 
# 1. Redistribution égalitaire des recettes
# 2. Redistribution progressive de 30% aux LMICs et le reste aux compagnies et ports de sorte que non-HICs aient la recette égalitaire.


##### ID #####
all_id <- read.csv("../Adrien's/all_id.csv")
all_id <- merge(all[,!names(all) %in% c("interview", "country")], all_id, by = "n")
nrow(all_id)
for (c in c("IT", "US", "GB", "FR", "PL")) write.csv2(all_id[all_id$country == c & grepl("@", all_id$interview), names(all_id) %in% c("n", variables_sociodemos, "country", "vote_original", "voted", "group_defended", "gcs_support", "latent_support_global_redistr", "share_solidarity_diff", "share_solidarity_supported", "interview")], 
                                                      paste0("../Adrien's/", c, "_id.csv"), quote = F, row.names = F)
rm(all_id)





