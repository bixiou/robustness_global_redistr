with(p, summary(rq(duration ~ variant_long * cut))) # 19 + 1*long - 4*cut
with(p, summary(rq(duration ~ country))) 
with(p, summary(rq(duration ~ variant_long * cut * country)))
with(p, summary(lm(duration ~ variant_long * cut)))
with(p, summary(lm(duration ~ country))) 
with(p, summary(lm(duration ~ variant_long * cut * country)))
median(p$duration)
median(p$duration[p$variant_long & !p$cut])
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$duration[p$variant_long & !p$cut], na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) print(decrit(d(c)$duration[p$variant_long & !p$cut])))
sum(p$variant_long & !p$cut)
median(p$duration[p$variant_long & p$cut])
median(p$duration[!p$variant_long & p$cut])
median(p$duration[!p$variant_long & !p$cut])

# TODO field, conjoint

decrit("revenue_split_few_global", p)
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$revenue_split_few_global, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$revenue_split_few_global, na.rm = T)))
decrit("split_many_global", p)
decrit("split_both_global", p)
decrit("split_nb_global", p)
decrit("split_both_nb_global", p)
with(p, summary(lm(split_many_global ~ split_nb_global))) 
with(p, summary(lm(split_both_global ~ split_both_nb_global * variant_split))) 

sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$ncs_support, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$gcs_support, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$gcs_belief, na.rm = T), 3))

with(p, summary(lm(ics_support %in% "Yes" ~ variant_gcs)))

with(p, summary(lm((likely_solidarity > 0) ~ info_solidarity + variant_info_solidarity)))
with(p, summary(lm(share_solidarity_supported ~ info_solidarity + variant_info_solidarity))) 
with(p, summary(lm(share_solidarity_short_supported ~ info_solidarity + variant_info_solidarity))) 

sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$global_tax_support, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$hic_tax_support, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$intl_tax_support, na.rm = T), 3))
with(p, summary(lm(wealth_tax_support ~ variant_wealth_tax)))

CrossTable(p$ncqg, p$country, prop.t = F, prop.r = F, prop.chisq = F, prop.c = T, total.c = F, total.r = F, cell.layout = F) 
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$ncqg, na.rm = T), 3)) # 2.6-3
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$ncqg_full, na.rm = T), 3)) # 315-420
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$ncqg, na.rm = T), 3)) # 3
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$ncqg_full, na.rm = T), 3)) # 100
summary(lm(ncqg ~ (age == 21.5) + (education == 3) + race_white, data = USp))
summary(lm(ncqg ~ (education == 2), data = PLp))
 
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$sustainable_future, na.rm = T), 3))
with(p, summary(lm(sustainable_future ~ variant_sustainable_future))) 

sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$top_tax_support, na.rm = T), 3))
with(p, summary(lm(top_tax_support ~ variant_top_tax * variant_long)))  
with(p, summary(lm(top_tax_support ~ variant_top_tax_full)))  
with(p, summary(lm(top_tax_support ~ variant_top_tax)))

sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$vote_intl_coalition > 0, na.rm = T), 3))

sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$reparations_support[!d(c)$reparations_support %in% 0] > 0, na.rm = T), 3))

sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$custom_redistr_satisfied, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$custom_redistr_skip, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$custom_redistr_winners[d(c)$custom_redistr_satisfied], na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$custom_redistr_losers[d(c)$custom_redistr_satisfied], na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$custom_redistr_degree[d(c)$custom_redistr_satisfied], na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$custom_redistr_winners[!d(c)$custom_redistr_winners %in% c(401, 601)], na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$custom_redistr_losers[!d(c)$custom_redistr_losers %in% c(101, 201)], na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$custom_redistr_degree[!d(c)$custom_redistr_degree %in% c(2.1, 7.1)], na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$custom_redistr_winners %in% c(401, 601), na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$custom_redistr_losers %in% c(101, 201), na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$custom_redistr_degree %in% c(2.1, 7.1), na.rm = T), 3))
decrit("custom_redistr_winners", data = p, which = p$custom_redistr_satisfied) # 500
decrit("custom_redistr_losers", data = p, which = p$custom_redistr_satisfied) # 150
decrit("custom_redistr_degree", data = p, which = p$custom_redistr_satisfied) # 5
with(p, summary(lm(custom_redistr_winners ~ variant_sliders)))
with(p, summary(lm(custom_redistr_winners ~ variant_sliders, subset = custom_redistr_satisfied == T)))
with(p, summary(lm(custom_redistr_losers ~ variant_sliders, subset = custom_redistr_satisfied == T)))
with(p, summary(lm(custom_redistr_degree ~ variant_sliders, subset = custom_redistr_satisfied == T)))
with(p, summary(lm(custom_redistr_losers ~ income_exact * country, subset = custom_redistr_satisfied == T))) # TODO: compute transfer for each
# 500-150-5 => transfer: 5.8% / demogrant: 299$/month
# 401-101-7.1 => transfer: 4.3% / demogrant: 298$/month
# 601-201-2.1 => transfer: 4.6% / demogrant: 185$/month

with(p, summary(lm(well_being ~ variant_well_being))) 
with(p, summary(lm(well_being ~ variant_well_being_scale * variant_well_being_wording))) 

sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$group_defended > 0, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$my_tax_global_nation > 0, na.rm = T), 3))

sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$survey_biased > 0, na.rm = T), 3))

sapply(paste0(pilot_countries, "p"), function(c) decrit(paste0("vote_", sub("p", "", c)), d(c)))
sapply(paste0(pilot_countries, "p"), function(c) print(decrit("voted", d(c))))
