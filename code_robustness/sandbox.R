world <- read.csv2("../data/WID/WID_data_WO.csv") # There is also WO-MER

world_thousandile <- as.numeric(c(quadratic_interpolations(as.numeric(sapply(1:99, function(i) world$value[world$percentile == paste0("p", i-1, "p", i) & world$variable == "aptincj992" & world$country == "WO" & world$year == 2023])), 
                                                                   as.numeric(sapply(1:99, function(i) world$value[world$percentile == paste0("p", i-1, "p", i) & world$variable == "tptincj992" & world$country == "WO" & world$year == 2023])), 
                                                                   0:99, seq(0.1, 99, 0.1)),
                                  world$value[world$percentile == paste0("p99p99.1") & world$variable == "aptincj992" & world$country == "WO" & world$year == 2023], 
                                  sapply(2:9, function(i) world$value[world$percentile == paste0("p99.", (i-1) %% 10, "p99.", i %% 10) & world$variable == "aptincj992" & world$country == "WO" & world$year == 2023]),
                                  world$value[world$percentile == paste0("p99.9p100") & world$variable == "aptincj992" & world$country == "WO" & world$year == 2023]))
write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(world_thousandile)), file = "../data/world_thousandile.csv", row.names = F)

# View(world)
sort(unique(world$variable)) # a: average, t: threshold / pt: pre-tax, di: post-tax, ca: disposable / inc: income, pfcar: carbon footprint / 992: all above 20, 999: all age / j: Equal-split adults,	i: individual
# gethin <- read.dta13("../data/fisher-gethin-redistribution-2024-06-27.dta") # pdi: disposable (pretax - direct taxes + gov_soc: social assistance transfers), pni = diinc: posttax (pretax - all taxes + all transfers), pni_edp, dina_: venant de DINA existantes, pre: pretax, cons: conso imputée à partir de pretax
# gethin <- gethin[gethin$year == 2019,]
# write.csv(gethin, "../data/fisher-gethin-redistribution-2024-06-27.csv", row.names = F) # Fisher-Post & Gethin (2023) https://www.dropbox.com/scl/fi/yseottljqpzom1lrqga5c?e=1
gethin <- read.csv("../data/fisher-gethin-redistribution-2024-06-27.csv")
View(gethin) # edp: education distributed proportionally (to posttax inc, like in WID, instead of school attendance)
# 174 countries, not missing any important one (~2M people missing in total)

inflation <- read.xlsx("../data/inflation_imf.xlsx")
# Inflation from data/inflation_worldbank.xslx. Accessed 12/21/24. Sources: 2020-2023: Ha et al. (2023), https://www.worldbank.org/en/research/brief/inflation-database; 2024: IMF WEO (Oct 2024) https://www.imf.org/external/datamapper/PCPIPCH@WEO/WEOWORLD/VEN
gethin <- merge(gethin, inflation, all.x = T)
for (y in 2018:2024) gethin[[paste0("inflation_", y)]][is.na(gethin[[paste0("inflation_", y)]]) | gethin[[paste0("inflation_", y)]] == "no data"] <- 1
for (y in 2018:2024) gethin[[paste0("inflation_", y)]] <- as.numeric(gethin[[paste0("inflation_", y)]])
gethin$inflation_2020_2022 <- (1+gethin$inflation_2020/100)*(1+gethin$inflation_2021/100)*(1+gethin$inflation_2022/100)
gethin$inflation_2023_2024 <- (1+gethin$inflation_2023/100)*(1+gethin$inflation_2024/100)
gethin$inflation_2018_2024 <- gethin$inflation_2023_2024*gethin$inflation_2020_2022*(1+gethin$inflation_2019/100)*(1+gethin$inflation_2018/100)

mer <- read.csv2("../data/LCU_USD_2023_wid.csv") # Market exchange rate, LCU per USD, 2023, xlcusx_999_i_2023, Fetched from https://wid.world/data/ on 12/21/2024.
gethin <- merge(gethin, mer, all.x = T)
gethin$lcu_usd <- as.numeric(gethin$lcu_usd)
gethin$lcu19_mer23 <- gethin$lcu_usd * gethin$defl / (1+gethin$inflation_2023/100)
gethin$lcu19_mer23[is.na(gethin$lcu19_mer23)] <- 1
gethin$lcu19_mer23[gethin$isoname %in% c("Venezuela", "Zimbabwe", "Sierra Leone")] <- (gethin$xppp_us * gethin$defl)[gethin$isoname %in% c("Venezuela", "Zimbabwe", "Sierra Leone")]

growth <- read.xlsx("../data/growth_imf.xlsx") # Real GDP growth, IMF WEO (Oct 24), Accessed on 12/21/2024, https://www.imf.org/external/datamapper/NGDP_RPCH@WEO/OEMDC/ADVEC/WEOWORLD
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
write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(thousandile_world_post_transfer_inc)), file = "../data/world_post_transfer_inc.csv", row.names = F)

world_disposable_inc_ppp22 <- compute_world_distrib_from_gethin("disposable_inc_ppp22")
thousandile_world_disposable_inc_ppp22 <- c(quadratic_interpolations(pmax(0, world_disposable_inc_ppp22$disposable_inc_ppp22_mean), pmax(0, world_disposable_inc_ppp22$disposable_inc_ppp22_thre), 
                                                               c((0:99)/100, .999, 1), seq(0.000, .998, 0.001)), world_disposable_inc_ppp22$disposable_inc_ppp22_mean[101:102] %*% c(.9, .1))
# /!\ Pb: the top interpolation is linear, inflating the mean income => do piecewise linear to preserve the mean (but won't preserve the smoothness)
write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(thousandile_world_disposable_inc_ppp22)), file = "../data/world_disposable_inc_ppp22.csv", row.names = F)

world_disposable_inc <- compute_world_distrib_from_gethin("disposable_inc")
thousandile_world_disposable_inc <- c(quadratic_interpolations(pmax(0, world_disposable_inc$disposable_inc_mean), pmax(0, world_disposable_inc$disposable_inc_thre), 
                                                                  c((0:99)/100, .999, 1), seq(0.000, .998, 0.001)), world_disposable_inc$disposable_inc_mean[101:102] %*% c(.9, .1))
# /!\ Pb: the top interpolation is linear, inflating the mean income => do piecewise linear to preserve the mean (but won't preserve the smoothness)
write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(thousandile_world_disposable_inc)), file = "../data/world_disposable_inc.csv", row.names = F)

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
write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(thousandile_world_disposable_inc_reform_simple)), file = "../data/world_disposable_inc_reform_simple.csv", row.names = F)
thousandile_world_disposable_inc_reform_double <- pmax(thousandile_world_disposable_inc, 3650) - .1*pmax(0, thousandile_world_disposable_inc - 48e3) - .15*pmax(0, thousandile_world_disposable_inc - 1e5) - .15*pmax(0, thousandile_world_disposable_inc - 1e6)
write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(thousandile_world_disposable_inc_reform_double)), file = "../data/world_disposable_inc_reform_double.csv", row.names = F)

world_pretax_inc <- compute_world_distrib_from_gethin("pretax_inc")
thousandile_world_pretax_inc <- c(quadratic_interpolations(world_pretax_inc$pretax_inc_mean, world_pretax_inc$pretax_inc_thre, 
                                                                  c((0:99)/100, .999, 1), seq(0.000, .998, 0.001)), world_pretax_inc$pretax_inc_mean[101:102] %*% c(.9, .1))
write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(thousandile_world_pretax_inc)), file = "../data/world_pretax_inc.csv", row.names = F)

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
# write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(world_thousandile)), file = "../data/world_thousandile.csv", row.names = F)

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


# n=7.84*.25/(MDE^2*p*(1-p))
# MDE=1.4/sqrt(n*p*(1-p))
# n=2500, p=.2, .16
# n=3000, p=.1/3, .2222
# n=2000, p=.5, .25
# n=1000, p=.5, .25

plot(1:1000, thousandile_world_disposable_inc, type = 'l', ylim = c(0, 8000*12), col = "red")
lines(1:1000, thousandile_world_post_transfer_inc, type = 'l', col = "blue")
# lines(1:1000, thousandile_world_pretax_inc, type = 'l', col = "blue")
lines(1:1000, thousandile_world_disposable_inc_post_simple_reform, type = 'l', col = "green")




