##### Duration #####
# long & !cut < 20 min => we are not constrained by duration \o/
with(p, summary(rq(duration ~ variant_long * cut))) # 19 + 1*long - 4*cut
with(p, summary(rq(duration ~ variant_long, subset = cut == 0)))
with(p, summary(rq(duration ~ country))) 
with(p, summary(rq(duration ~ variant_long * cut * country)))
with(p, summary(lm(duration ~ variant_long * cut)))
with(p, summary(lm(duration ~ country))) 
with(p, summary(lm(duration ~ variant_long * cut * country)))
median(p$duration)
median(p$duration[p$variant_long & !p$cut])
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$duration[p$variant_long & !p$cut], na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) print(decrit(d(c)$duration[p$variant_long & !p$cut])))
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$duration, na.rm = T), 3))
sum(p$variant_long & !p$cut)
median(p$duration[p$variant_long & p$cut])
median(p$duration[!p$variant_long & p$cut])
median(p$duration[!p$variant_long & !p$cut])


##### Fields #####
# Global poverty/inequality not a concern nor wish but appears prominently in injustice.
decrit(p$variant_field)
with(p, summary(rq(nchar(field) ~ variant_field)))
with(p, summary(lm(is.na(field) ~ variant_field)))
with(p, summary(rq(duration_field ~ variant_field)))
# concerns & issue give comparable results. issue has slightly shorter/fewer answers.


# => Either keep as is or change "in the world" => "of all" for injustice; or have two versions of injustice and take out concerns.
# -> test "of all" for last 188 US respondents; concerns changed to open question on global tax revenue use. TODO analyze
p$issue_field[!is.na(p$issue_field) & !p$country %in% "PL"] # Short political answers. Main topics: cost of living; immigration; climate; (homelessness; healthcare; animal).
p$concerns_field[!is.na(p$concerns_field) & !p$country %in% "PL"] # Political answers. Main topics: money; cost of living; immigration; (peace; Trump; job/unemployment).
p$injustice_field[!is.na(p$injustice_field) & !p$country %in% "PL"] # Short political answers. Main topics: poverty; hunger; inequality; (wars).
p$wish_field[!is.na(p$wish_field) & !p$country %in% "PL"] # Short answers. Main topics: money (by far); health; happiness; (peace).
# New research questions: What's wrong with immigration? Why do you need money for? Sociodemos determinants.


##### Revenue split #####
# Average of 17 for global item, quite independent of number of global items => people seem to split more or less equally between presented choices.
# -> ask in open-ended field how should global tax revenues be spent / how they should be allocated between countries?
decrit("revenue_split_few_global", p)
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$revenue_split_few_global, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$revenue_split_few_global, na.rm = T)))
decrit("split_many_global", p)
decrit("split_many_global", p, which = p$split_nb_global == 1)
decrit("split_both_global", p)
decrit("split_nb_global", p)
decrit("split_both_nb_global", p)
with(p, summary(lm(split_many_global ~ split_nb_global))) 
with(p, summary(lm((split_many_global/split_nb_global) ~ as.factor(split_nb_global)))) 
with(p, summary(lm(split_both_global ~ split_both_nb_global * variant_split))) 
sort(sapply(variables_split_many, function(c) mean(p[[c]], na.rm = T)), decreasing = T) 
sort(sapply(variables_split_many, function(c) mean(GBp[[c]], na.rm = T)), decreasing = T) 
# global_education_healthcare ranks 8/13; other global 10, 12, 13 justice_police, deficit_reduction are only domestic that rank below it.
sort(sapply(variables_split_few, function(c) mean(p[[c]], na.rm = T)), decreasing = T) # global ranks 5/5
sort(sapply(variables_split_few, function(c) mean(GBp[[c]], na.rm = T)), decreasing = T) # global ranks 3/5


##### Warm glow - substitute #####
# => Wrong that some prefer to pay more to get climate policy at global level; on the contrary it seems a few prefer to not lose and have it domestically
with(p, summary(lm(gcs_support ~ variant_warm_glow))) # No effect of donation; -.03 for NCS
with(p, summary(lm(gcs_support ~ variant_warm_glow * country, subset = !variant_warm_glow %in% "donation"))) # NCS effect driven by PL


##### GCS #####
# -> keep high scenario for all but RU-SA-US
# => higher support for ICS than GCS. Why? loss less salient? more realistic? less alone? more countries than expected?
# => Being without other HIC is worse than without China
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$ncs_support, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$gcs_support, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$ics_support, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$ics_support[d(c)$variant_gcs == "mid"], na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$gcs_support[d(c)$variant_gcs == "mid"], na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$gcs_belief, na.rm = T), 3))


##### ICS #####
# -> add one version with almost all countries?
# -> remove NCS?
with(p, summary(lm(ics_support %in% "Yes" ~ variant_gcs)))
with(p, summary(lm(ics_support %in% "Yes" ~ variant_gcs, subset = country == "US")))
with(p, summary(lm(ics_support %in% "Yes" ~ variant_gcs, subset = country == "GB")))
with(p, summary(lm(ics_support %in% "Yes" ~ variant_gcs, subset = country == "PL")))


##### Solidarity likert #####
# Long info twice the effect of short. Long info => likely solidarity +10pp; share_solidarity_short_supported +2.5pp
# IV works. likely_solidarity positively (not negatively!) correlated with support.
# -> keep only long
with(p, summary(lm((likely_solidarity > 0) ~ variant_info_solidarity)))
with(p, summary(lm(likely_solidarity ~ variant_info_solidarity)))
with(p, summary(lm(share_solidarity_supported ~ variant_info_solidarity))) 
with(p, summary(lm(share_solidarity_short_supported ~ variant_info_solidarity))) 
summary(ivreg(share_solidarity_short_supported ~ (likely_solidarity > 0) | info_solidarity, data = p))
summary(ivreg(share_solidarity_short_supported ~ likely_solidarity | info_solidarity, data = p))
summary(lm(share_solidarity_short_supported ~ (likely_solidarity > 0), data = p))
summary(lm(share_solidarity_short_supported ~ (likely_solidarity > 0), data = p, subset = !info_solidarity))


##### Wealth tax depending on coverage #####
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$global_tax_support, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$hic_tax_support, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$intl_tax_support, na.rm = T), 3))
with(p, summary(lm(wealth_tax_support ~ variant_wealth_tax)))


##### NCQG, Maritime #####
# ?? ncqg: maintain / ncqg_full: $100 bn
CrossTable(p$ncqg, p$country, prop.t = F, prop.r = F, prop.chisq = F, prop.c = T, total.c = F, total.r = F, cell.layout = F) 
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$ncqg, na.rm = T), 3)) # 2.6-3
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$ncqg_full, na.rm = T), 3)) # 315-420
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$ncqg, na.rm = T), 3)) # 3
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$ncqg_full, na.rm = T), 3)) # 100
summary(lm(ncqg ~ (age == 21.5) + (education == 3) + race_white, data = USp))
summary(lm(ncqg ~ (education == 2), data = PLp))

sapply(paste0(pilot_countries_all, "p"), function(c) print(decrit(d(c)$maritime_split_ldc))) # 30
sapply(paste0(pilot_countries_all, "p"), function(c) print(decrit(d(c)$maritime_split_companies))) # 30
sapply(paste0(pilot_countries_all, "p"), function(c) print(decrit(d(c)$maritime_split_decarbonization))) # 40


##### Scenarios #####
# 60-70% for sustainability
# -> use long version
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$sustainable_future, na.rm = T), 3))
with(p, summary(lm(sustainable_future ~ variant_sustainable_future))) 


##### Radical tax #####
# => Strong support
# -> Either take out 1% or keep both
decrit("top1_tax_support", p)
decrit("top3_tax_support", p)
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$top_tax_support > 0, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$top1_tax_support > 0, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$top3_tax_support > 0, na.rm = T), 3))
with(p, summary(lm((top_tax_support > 0) ~ variant_top_tax * variant_long)))  
with(p, summary(lm((top_tax_support > 0) ~ variant_top_tax_full)))  
with(p, summary(lm((top_tax_support > 0) ~ variant_top_tax)))

# more likely if party in coalition: 39% / less likely: 16%
sapply(paste0(pilot_countries_all, "p"), function(c) print(decrit(d(c)$vote_intl_coalition, weight = F)))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$vote_intl_coalition > 0, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$vote_intl_coalition < 0, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$vote_intl_coalition == 0, na.rm = T), 3))

sapply(paste0(pilot_countries_all, "p"), function(c) print(decrit(d(c)$reparations_support, weight = F)))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$reparations_support[!d(c)$reparations_support %in% 0] > 0, na.rm = T), 3))

sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$custom_redistr_satisfied, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$custom_redistr_skip, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$custom_redistr_winners[d(c)$custom_redistr_satisfied], na.rm = T), 3)) # 470-540: 500
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$custom_redistr_losers[d(c)$custom_redistr_satisfied], na.rm = T), 3)) # 150-200: 170
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$custom_redistr_degree[d(c)$custom_redistr_satisfied], na.rm = T), 3)) # 4-5: 5
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$custom_redistr_winners[!d(c)$custom_redistr_winners %in% c(401, 601)], na.rm = T), 3)) # 440-520: 500
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$custom_redistr_losers[!d(c)$custom_redistr_losers %in% c(101, 201)], na.rm = T), 3)) # 150-200: 160
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$custom_redistr_degree[!d(c)$custom_redistr_degree %in% c(2.1, 7.1)], na.rm = T), 3)) # 5: 5
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$custom_redistr_winners[d(c)$custom_redistr_satisfied & !d(c)$custom_redistr_winners %in% c(401, 601)], na.rm = T), 3)) # 450-540: 500
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$custom_redistr_losers[d(c)$custom_redistr_satisfied & !d(c)$custom_redistr_losers %in% c(101, 201)], na.rm = T), 3)) # 150-200: 150
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$custom_redistr_degree[d(c)$custom_redistr_satisfied & !d(c)$custom_redistr_degree %in% c(2.1, 7.1)], na.rm = T), 3)) # 5: 5
sapply(paste0(pilot_countries_all, "p"), function(c) round(median(d(c)$custom_redistr_winners %in% c(401, 601), na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$custom_redistr_losers %in% c(101, 201), na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$custom_redistr_degree %in% c(2.1, 7.1), na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$custom_redistr_winners[d(c)$custom_redistr_satisfied] %in% c(401, 601), na.rm = T), 3))
decrit("custom_redistr_winners", data = p, which = p$custom_redistr_satisfied) # 500
decrit("custom_redistr_losers", data = p, which = p$custom_redistr_satisfied) # 150
decrit("custom_redistr_degree", data = p, which = p$custom_redistr_satisfied) # 5
with(p, summary(lm(custom_redistr_winners ~ variant_sliders)))
with(p, summary(lm(custom_redistr_winners ~ variant_sliders, subset = custom_redistr_satisfied == T)))
with(p, summary(lm(custom_redistr_losers ~ variant_sliders, subset = custom_redistr_satisfied == T)))
with(p, summary(lm(custom_redistr_degree ~ variant_sliders, subset = custom_redistr_satisfied == T)))
with(p, summary(lm(custom_redistr_losers ~ income_exact * country, subset = custom_redistr_satisfied == T))) # TODO: compute transfer for each; tax rates; dummy whether decrease own income; sociodemos determinants
# 500-150-5 => transfer: 5.8% / demogrant: 299$/month
# 401-101-7.1 => transfer: 4.3% / demogrant: 298$/month
# 601-201-2.1 => transfer: 4.6% / demogrant: 185$/month

with(p, summary(lm(well_being ~ variant_well_being))) 
with(p, summary(lm(well_being ~ variant_well_being_scale * variant_well_being_wording))) 

sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$group_defended > 0, na.rm = T), 3))
sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$my_tax_global_nation > 0, na.rm = T), 3)) # -> TODO check

sapply(paste0(pilot_countries_all, "p"), function(c) round(mean(d(c)$survey_biased > 0, na.rm = T), 3))

sapply(paste0(pilot_countries, "p"), function(c) decrit(paste0("vote_", sub("p", "", c)), d(c)))
sapply(paste0(pilot_countries, "p"), function(c) print(decrit("voted", d(c))))


##### Comments #####
p$comment_field[!is.na(p$comment_field) & !p$country %in% "PL"]
# TODO USp$interview & new ncqg after 856 / PLp after 898: GDP convergence


##### Influence order ######
summary(lm((ncqg > 2) ~ (ncqg_order == 7), data = p))
summary(lm((ncqg > 3) ~ (ncqg_order == 7), data = p))
summary(lm(split_many_global ~ mean_order_many_global, data = p))
summary(lm(revenue_split_few_global ~ revenue_split_few_order_global, data = p))
summary(lm(maritime_split_ldc ~ maritime_split_order_ldc, data = p))
summary(lm((vote_intl_coalition == "More likely") ~ (vote_intl_coalition_order_more_likely == 1), data = p))
summary(lm(why_hic_help_lic_duty ~ why_hic_help_lic_order_duty, data = p))
summary(lm(why_hic_help_lic_interest ~ why_hic_help_lic_order_interest, data = p))
summary(lm(transfer_how_cash_unconditional ~ (transfer_how_order_cash_unconditional == 7), data = p))
summary(lm((transfer_how_cash_unconditional > 0) ~ (transfer_how_order_cash_unconditional == 7), data = p))
summary(lm((transfer_how_social_protection > 0) ~ (transfer_how_order_cash_unconditional == 7), data = p))
summary(lm((transfer_how_agencies > 0) ~ (transfer_how_order_cash_unconditional == 7), data = p))
summary(lm((transfer_how_govt_conditional > 0) ~ (transfer_how_order_cash_unconditional == 7), data = p))
summary(lm((transfer_how_govt_unconditional > 0) ~ (transfer_how_order_cash_unconditional == 7), data = p))

# TODO variable that best correlates