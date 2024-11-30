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

gethin$disposable_inc_mean <- gethin$a_pdi / (gethin$xppp_us * gethin$defl) # convert into constant $ (xppp_us is the PPP deflator and defl the inflation/LCU deflator)
gethin$post_transfer_inc <- (gethin$a_pre + gethin$gov_soc) / (gethin$xppp_us * gethin$defl) 

# Compute world distribution
compute_world_distrib_from_gethin <- function(var, year = 2019) {
  data <- gethin %>% arrange(year, !!as.symbol(var)) %>% group_by(year) %>% mutate(x = cumsum(weight), tot = sum(weight), x = x / tot)
  breakpoints <- c(seq(0, 0.99, by = 0.01), 0.999, 0.9999, 1)
  data <- data %>% mutate(p = cut(x, breaks = breakpoints, labels = breakpoints[-length(breakpoints)], include.lowest = TRUE),
                          p = as.numeric(as.character(p)), p = ifelse(is.na(p), 0.9999, p)) %>% # Replace NA with 0.9999
                   group_by(year, p) %>% summarize(!!as.symbol(paste0(var, "_thre")) := min(!!as.symbol(var)), 
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
world_thousandile <- c(quadratic_interpolations(world_post_transfer_inc$post_transfer_inc_mean, world_post_transfer_inc$post_transfer_inc_thre, 
                                                                   c((0:99)/100, .999, 1), seq(0.000, .998, 0.001)), world_post_transfer_inc$post_transfer_inc_mean[101:102] %*% c(.9, .1))
# /!\ Pb: the top interpolation is linear, inflating the mean income => do piecewise linear to preserve the mean (but won't preserve the smoothness)
write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(world_thousandile)), file = "../data/world_thousandile.csv", row.names = F)

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

















