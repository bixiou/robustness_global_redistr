# Everyone:
# TODO: no absolute file paths, only relative ones => Erwan JP, ZBOOK DE, US
# TODO: all original .csv in the repository, with a link (URL) to the original source
# TODO: better document the code (explain at the beginning of the file what you do, and briefly explain each step/part)
# Raquel:
# TODO: export without quotes, to ../data_ext/zipcode_[country]_urba_region.csv
# TODO: Unify all files under one .R (with ##### [Country] ##### headers), merging the methodo as comments in .R
# TODO: Re-run everything and check the files are the same as the existing ones.
# TODO: Add two sheets in features: urbanity, region. 
#       In each sheet, one column country (iso2) - several rows will have the same country, 
#         one column urbanity/region (giving the corresponding number), one column urbanity_name or region_name, 
#         (for urbanity) one column definition or (for region) several columns defining the region by listing the States/departments/provinces (one per column)
#       In the sheet Sources, add rows: zipcode, urbanity, region, and more if needed, with the source used in each case
#         the source should be e.g. "OECD, 2021, http://location.of/the.source.file", where 2021 is the year of the data (not the year of their release)
#       Complete the sheet Sources for the quota sources.
# TODO: Add a new sheet in features: elections. With the following columns: country; candidate (this should match the "Albanese" version in Qualtrics); 
#         major: 1 iff the candidate obtained >5% (if you think that another threshold makes more sense for a country, tell me); leaning: 0 if Left, 1 if Center-right or Right, 2 if Far right. 

# TODO: Conjoint analysis: analyze the effect of having an inconsistent program (left+right pols) on conjoint choice and subsequent questions

export_quotas()

##### Duration #####
# long & !cut < 20 min => we are not constrained by duration \o/
with(e, summary(rq(duration ~ variant_long * cut))) # 19 + 1*long - 4*cut
with(e, summary(rq(duration ~ variant_long, subset = cut == 0)))
with(e, summary(rq(duration ~ country))) 
with(e, summary(rq(duration ~ variant_long * cut * country)))
with(e, summary(lm(duration ~ variant_long * cut)))
with(e, summary(lm(duration ~ country))) 
with(e, summary(lm(duration ~ variant_long * cut * country)))
median(e$duration)
median(e$duration[e$variant_long & !e$cut])
sapply(c("all", countries[-9]), function(c) round(median(d(c)$duration[e$variant_long & !e$cut], na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) print(decrit(d(c)$duration[e$variant_long & !e$cut])))
sapply(c("all", countries[-9]), function(c) round(median(d(c)$duration, na.rm = T), 3))
sum(e$variant_long & !e$cut)
median(e$duration[e$variant_long & e$cut])
median(e$duration[!e$variant_long & e$cut])
median(e$duration[!e$variant_long & !e$cut])
for (v in variables_duration) print(decrit(v, e)) # Pbs: ncqg, preferred_transfer_mean
decrit(e$duration_ics - e$duration_gcs)
# Weird: wealth_tax, radical_redistr

View(e[, intersect(variables_duration[-1], names(p))])
rowSums(e[-c(4, 7, 9, 12), intersect(variables_duration[-1], names(p))], na.rm = T)
sum(rowMeans(e[-c(4, 7, 9, 12), intersect(variables_duration[-1], names(p))], na.rm = T))
e$duration[-c(4, 7, 9, 12)]
e$country[c(4, 7, 9, 12)]
e$duration_feedback


##### Variants #####
for (v in variables_variant) print(decrit(v, e)) # Pbs: ncqg, preferred_transfer_mean


##### Weights #####
sapply(countries[-9], function(c) representativity_index(d(c)$weight)) # from .54 (CH) to .94 (JP)
sapply(countries[-c(9:10)], function(c) representativity_index(d(c)$weight_vote)) # from .51 (PL) to .81 (JP)
sapply(c("all", countries[-9]), function(c) round(length(which(d(c)$weight<=0.25 | d(c)$weight>=4))/nrow(d(c)), 3)) # all: . from 0 (JP) to .24 (CH)
sapply(countries[-c(9:10)], function(c) round(length(which(d(c)$weight_vote<=0.25 | d(c)$weight_vote>=4))/nrow(d(c)), 3)) # from 0 (JP) to .34 (CH)
representativity_index(all$weight_country) # .67


##### Fields #####
# Global poverty/inequality not a concern nor wish but appears prominently in injustice.
decrit(e$variant_field)
with(e, summary(rq(nchar(field) ~ variant_field)))
with(e, summary(lm(is.na(field) ~ variant_field)))
with(e, summary(rq(duration_field ~ variant_field)))
# concerns & issue give comparable results. issue has slightly shorter/fewer answers.


# => Either keep as is or change "in the world" => "of all" for injustice; or have two versions of injustice and take out concerns.
# -> test "of all" for last 188 US respondents; concerns changed to open question on global tax revenue use. 
e$issue_field[!is.na(e$issue_field) & !e$country %in% "PL"] # Short political answers. Main topics: cost of living; immigration; climate; (homelessness; healthcare; animal).
e$concerns_field[!is.na(e$concerns_field) & !e$country %in% "PL"] # Political answers. Main topics: money; cost of living; immigration; (peace; Trump; job/unemployment).
e$injustice_field[!is.na(e$injustice_field) & !e$country %in% "PL"] # Short political answers. Main topics: poverty; hunger; inequality; (wars).
e$wish_field[!is.na(e$wish_field) & !e$country %in% "PL"] # Short answers. Main topics: money (by far); health; happiness; (peace).
# New research questions: What's wrong with immigration? Why do you need money for? Sociodemos determinants.

e$comment_field[!is.na(e$comment_field)]


##### Revenue split #####
# Average of 17 for global item, quite independent of number of global items => people seem to split more or less equally between presented choices.
# -> ask in open-ended field how should global tax revenues be spent / how they should be allocated between countries?
decrit("revenue_split_few_global", e)
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$revenue_split_few_global, na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(median(d(c)$revenue_split_few_global, na.rm = T)))
decrit("split_many_global", e)
decrit("split_many_global", e, which = e$split_nb_global == 1)
decrit("split_both_global", e)
decrit("split_nb_global", e)
decrit("split_both_nb_global", e)
with(e, summary(lm(split_many_global ~ split_nb_global))) 
with(e, summary(lm((split_many_global/split_nb_global) ~ as.factor(split_nb_global)))) 
with(e, summary(lm(split_both_global ~ split_both_nb_global * variant_split))) 
sort(sapply(variables_split_many, function(c) mean(e[[c]], na.rm = T)), decreasing = T) 
sort(sapply(variables_split_many, function(c) mean(GB[[c]], na.rm = T)), decreasing = T) 
sort(sapply(variables_split_many, function(c) mean(e[[c]] == 0, na.rm = T)), decreasing = T) 
sort(sapply(variables_split_many, function(c) median(e[[c]], na.rm = T)), decreasing = T) 
# global_education_healthcare ranks 8/13; other global 10, 12, 13 justice_police, deficit_reduction are only domestic that rank below it.
sort(sapply(variables_split_few, function(c) mean(e[[c]], na.rm = T)), decreasing = T) # global ranks 5/5
sort(sapply(variables_split_few, function(c) median(e[[c]], na.rm = T)), decreasing = T)
sort(sapply(variables_split_few, function(c) mean(GB[[c]], na.rm = T)), decreasing = T) # global ranks 3/5
sort(sapply(variables_split_few, function(c) mean(e[[c]] == 0, na.rm = T)), decreasing = T) 
sort(sapply(variables_split_few, function(c) mean(e[[c]][e[[c]] != 0], na.rm = T)), decreasing = T)
sort(sapply(variables_split_few, function(c) mean(e[[c]][e$revenue_split_few_global != 0], na.rm = T)), decreasing = T) 


##### Warm glow - substitute #####
# => Wrong that some prefer to pay more to get climate policy at global level; on the contrary it seems a few prefer to not lose and have it domestically
with(e, summary(lm(gcs_support ~ variant_warm_glow, weights = weight))) # No effect of donation; -.03 for NCS
with(e, summary(lm(gcs_support ~ variant_warm_glow * country, subset = !variant_warm_glow %in% "donation"))) # NCS effect driven by PL


##### GCS #####
# -> keep high scenario for all but RU-SA-US
# => higher support for ICS than GCS. Why? loss less salient? more realistic? less alone? more countries than expected?
# => Being without other HIC is worse than without China
decrit("gcs_understood", all)
decrit("survey_biased", all)
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$gcs_understood, na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$ncs_support, na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$gcs_support, na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$ics_support, na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$ics_support[d(c)$variant_ics == "mid"], na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$gcs_support[d(c)$variant_ics == "mid"], na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$gcs_belief_us, na.rm = T), 0))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$gcs_belief_own, na.rm = T), 0))
summary(lm(gcs_support ~ gcs_understood, data = all, weights = weight)) # -6pp***
summary(lm(ics_support ~ gcs_understood, data = all, weights = weight)) # -5pp***
summary(lm(ics_low_support ~ gcs_understood, data = all, weights = weight)) # -7pp***
summary(lm(ics_high_color_support ~ gcs_understood, data = all, weights = weight)) # -3pp
summary(lm(reg_formula("gcs_support", c(variables_socio_demos, "gcs_understood")), data = all, weights = weight))
sapply(c("all", countries[-9]), function(c) round(wtd.mean(d(c)$gcs_support, d(c)$weight * d(c)$gcs_understood, na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(wtd.mean(d(c)$ics_low_support, d(c)$weight, na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(wtd.mean(d(c)$gcs_support, d(c)$weight * d(c)$gcs_understood, na.rm = T), 3))


##### ICS #####
# -> add one version with almost all countries? no
# -> remove NCS?
with(e, summary(lm(ics_support %in% "Yes" ~ variant_ics)))
with(e, summary(lm(ics_support %in% "Yes" ~ variant_ics, subset = country == "US")))
with(e, summary(lm(ics_support %in% "Yes" ~ variant_ics, subset = country == "GB")))
with(e, summary(lm(ics_support %in% "Yes" ~ variant_ics, subset = country == "PL")))


##### Solidarity likert #####
# Long info twice the effect of short. Long info => likely solidarity +10pp; share_solidarity_short_supported +2.5pp
# IV works. likely_solidarity positively (not negatively!) correlated with support.
# -> keep only long
sort(sapply(variables_solidarity_support, function(c) mean(e[[c]][e[[c]] != 0] > 0, na.rm = T)), decreasing = T) 
sapply(c("all", countries[-9]), function(c) mean(d(c)$solidarity_support_aviation_levy[d(c)$solidarity_support_aviation_levy != 0] > 0))
with(e, summary(lm((likely_solidarity > 0) ~ info_solidarity)))
with(e, summary(lm(likely_solidarity ~ info_solidarity)))
with(e, summary(lm(share_solidarity_supported ~ info_solidarity), weights = weight)) # +1.pp**
with(e, summary(lm(share_solidarity_supported_no_commitment ~ info_solidarity), weights = weight)) # 0
with(e, summary(lm(share_solidarity_supported_no_info ~ info_solidarity), weights = weight))  # 0
with(e, summary(lm(share_solidarity_opposed ~ info_solidarity), weights = weight))  # 0
with(e, summary(lm(share_solidarity_opposed_no_commitment ~ info_solidarity), weights = weight))  # 0
with(e, summary(lm(share_solidarity_opposed_no_info ~ info_solidarity), weights = weight))  # 0
# Effect of info driven by support for existing agreements. Effect on Bridgetown still shows possible effect on items without commitment. But no effect on non-mentioned items.
# *: shipping_levy > 0, ncqg_300bn, loss_damage, bridgetown > 0, foreign_aid > 0, 
# * opposite sign: corporate_tax
for (v in variables_solidarity_support) {
  print(v)
  print(with(e, summary(lm(as.formula(paste(str2expression(v), "> 0 ~ info_solidarity"))), weights = weight)))
  print(with(e, summary(lm(as.formula(paste(str2expression(v), "< 0 ~ info_solidarity"))), weights = weight))) }
with(e, summary(lm(share_solidarity_short_supported ~ variant_info_solidarity))) 
with(e, summary(lm(share_solidarity_short_supported ~ (likely_solidarity > 0)))) 
summary(ivreg(share_solidarity_supported ~ (likely_solidarity > 0) | info_solidarity, data = e, weights = e$weight))
summary(ivreg(share_solidarity_short_supported ~ (likely_solidarity > 0) | info_solidarity, data = e, weights = e$weight))
summary(ivreg(share_solidarity_short_supported ~ likely_solidarity | info_solidarity, data = e))
summary(lm(share_solidarity_short_supported ~ (likely_solidarity > 0), data = e))
summary(lm(share_solidarity_short_supported ~ (likely_solidarity > 0), data = e, subset = !info_solidarity))

sapply(c("all", countries[-9]), function(c) print(decrit(d(c)$solidarity_support_shipping_levy > 0, d(c), which = d(c)$solidarity_support_shipping_levy != 0)))
sapply(c("all", countries[-9]), function(c) print(decrit(d(c)$solidarity_support_shipping_levy > 0, d(c))))
sapply(c("all", countries[-9]), function(c) print(decrit(d(c)$solidarity_support_shipping_levy < 0, d(c))))


##### Wealth tax depending on coverage #####
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$global_tax_support, na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$hic_tax_support, na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$intl_tax_support, na.rm = T), 3))
with(e, summary(lm(wealth_tax_support ~ variant_wealth_tax))) # TODO! bug


##### NCQG, Maritime #####
# ?? ncqg: maintain / ncqg_full: $100 bn
CrossTable(e$ncqg, e$country, prop.t = F, prop.r = F, prop.chisq = F, prop.c = T, total.c = F, total.r = F, cell.layout = F) 
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$ncqg, na.rm = T), 3)) # 2.6-3
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$ncqg_full, na.rm = T), 3)) # 315-420
sapply(c("all", countries[-9]), function(c) round(median(d(c)$ncqg, na.rm = T), 3)) # 3
sapply(c("all", countries[-9]), function(c) round(median(d(c)$ncqg_full, na.rm = T), 3)) # 100
summary(lm(ncqg ~ (age == 21.5) + (education == 3) + race_white, data = USp))
summary(lm(ncqg ~ (education == 2), data = e))

sapply(c("all", countries[-9]), function(c) print(decrit(d(c)$maritime_split_ldc))) # 30
sapply(c("all", countries[-9]), function(c) print(decrit(d(c)$maritime_split_companies))) # 30
sapply(c("all", countries[-9]), function(c) print(decrit(d(c)$maritime_split_decarbonization))) # 40


##### Transfer how #####
sapply(variables_transfer_how, function(v) print(decrit(v, e)))


##### Scenarios #####
# 60-70% for sustainability
# -> use long version
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$sustainable_future, na.rm = T), 3))
with(e, summary(lm(sustainable_future ~ variant_sustainable_future))) 
with(e, summary(lm(sustainable_future ~ variant_sustainable_future * country))) 


##### Radical tax #####
# => Strong support
# -> Either take out 1% or keep both
decrit("top1_tax_support", e)
decrit("top3_tax_support", e)
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$top_tax_support > 0, na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$top1_tax_support > 0, na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(wtd.mean(d(c)$top3_tax_support > 0, d(c)$weight, na.rm = T), 3))
with(e, summary(lm((top_tax_support > 0) ~ variant_top_tax * variant_long)))  
with(e, summary(lm((top_tax_support > 0) ~ variant_long)))  
with(e, summary(lm((top_tax_support > 0) ~ variant_top_tax_full)))  
with(e, summary(lm((top_tax_support > 0) ~ variant_top_tax)))


##### Radical redistribution #####
# more likely if party in coalition: 39% / less likely: 16%
sapply(c("all", countries[-9]), function(c) print(decrit(d(c)$vote_intl_coalition, weight = F)))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$vote_intl_coalition > 0, na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$vote_intl_coalition < 0, na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$vote_intl_coalition == 0, na.rm = T), 3))

sapply(c("all", countries[-9]), function(c) print(decrit(d(c)$reparations_support, weight = F)))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$reparations_support[!d(c)$reparations_support %in% 0] > 0, na.rm = T), 3))


##### Custom redistr #####
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$custom_redistr_satisfied, na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$custom_redistr_skip, na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(median(d(c)$custom_redistr_winners, na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(median(d(c)$custom_redistr_losers, na.rm = T), 3)) 
sapply(c("all", countries[-9]), function(c) round(median(d(c)$custom_redistr_degree, na.rm = T), 3)) 
sapply(c("all", countries[-9]), function(c) round(median(d(c)$custom_redistr_winners[d(c)$custom_redistr_satisfied], na.rm = T), 3)) # 470-540: 49
sapply(c("all", countries[-9]), function(c) round(median(d(c)$custom_redistr_losers[d(c)$custom_redistr_satisfied], na.rm = T), 3)) # 150-200: 18
sapply(c("all", countries[-9]), function(c) round(median(d(c)$custom_redistr_degree[d(c)$custom_redistr_satisfied], na.rm = T), 3)) # 4-5: 5
sapply(c("all", countries[-9]), function(c) round(median(d(c)$custom_redistr_winners[!d(c)$custom_redistr_winners %in% c(401, 601)], na.rm = T), 3)) # 440-520: 500
sapply(c("all", countries[-9]), function(c) round(median(d(c)$custom_redistr_losers[!d(c)$custom_redistr_losers %in% c(101, 201)], na.rm = T), 3)) # 150-200: 160
sapply(c("all", countries[-9]), function(c) round(median(d(c)$custom_redistr_degree[!d(c)$custom_redistr_degree %in% c(2.1, 7.1)], na.rm = T), 3)) # 5: 5
sapply(c("all", countries[-9]), function(c) round(median(d(c)$custom_redistr_winners[d(c)$custom_redistr_satisfied & !d(c)$custom_redistr_winners %in% c(401, 601)], na.rm = T), 3)) # 450-540: 500
sapply(c("all", countries[-9]), function(c) round(median(d(c)$custom_redistr_losers[d(c)$custom_redistr_satisfied & !d(c)$custom_redistr_losers %in% c(101, 201)], na.rm = T), 3)) # 150-200: 150
sapply(c("all", countries[-9]), function(c) round(median(d(c)$custom_redistr_degree[d(c)$custom_redistr_satisfied & !d(c)$custom_redistr_degree %in% c(2.1, 7.1)], na.rm = T), 3)) # 5: 5
sapply(c("all", countries[-9]), function(c) round(median(d(c)$custom_redistr_winners %in% c(401, 601), na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$custom_redistr_losers %in% c(101, 201), na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$custom_redistr_degree %in% c(2.1, 7.1), na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$custom_redistr_winners[d(c)$custom_redistr_satisfied] %in% c(401, 601), na.rm = T), 3))
decrit("custom_redistr_winners", data = e, which = e$custom_redistr_satisfied) # 500
decrit("custom_redistr_losers", data = e, which = e$custom_redistr_satisfied) # 150
decrit("custom_redistr_degree", data = e, which = e$custom_redistr_satisfied) # 5
with(e, summary(lm(custom_redistr_winners ~ variant_sliders)))
with(e, summary(lm(custom_redistr_winners ~ variant_sliders, subset = custom_redistr_satisfied == T)))
with(e, summary(lm(custom_redistr_losers ~ variant_sliders, subset = custom_redistr_satisfied == T)))
with(e, summary(lm(custom_redistr_degree ~ variant_sliders, subset = custom_redistr_satisfied == T)))
with(e, summary(lm(custom_redistr_losers ~ income_exact * country, subset = custom_redistr_satisfied == T))) 
# 500-150-5 => transfer: 5.8% / demogrant: 299$/month
# 401-101-7.1 => transfer: 4.3% / demogrant: 298$/month
# 601-201-2.1 => transfer: 4.6% / demogrant: 185$/month
decrit("custom_redistr_unsatisfied_unskip", e)
decrit("custom_redistr_both_satisfied_skip", e)
decrit("custom_redistr_transfer", e)
decrit("custom_redistr_self_gain", all)
decrit("custom_redistr_self_gain", all[all$custom_redistr_satisfied,])
decrit("custom_redistr_self_lose", all)
decrit("custom_redistr_self_lose", all[all$custom_redistr_satisfied,])
# mean winners = non-losers: 72% / mean transfer: 5.08% / mean demogrant: $243/month
(max_winners <- min(which(mean_custom_redistr[["all"]] < current_inc))) # 725
current_inc[max_winners] # 18k
100*sum(mean_custom_redistr[["all"]][1:max_winners] - current[1:max_winners])/sum(current[1:1000]) # 5% in transfer
mean_custom_redistr[["all"]][1]/12 # 243
summary(lm(custom_redistr_satisfied ~ vote_factor, all, weights = weight))
summary(lm(custom_redistr_satisfied ~ vote_factor, all, weights = weight))
decrit("custom_redistr_satisfied", all) # 56%
decrit("custom_redistr_skip", all) # 43%
decrit("custom_redistr_satisfied", all, which = all$vote_factor == "Far right")
decrit("custom_redistr_satisfied", all, which = all$vote_factor == "Center-right or Right")
decrit("custom_redistr_satisfied", all, which = all$vote_factor == "Left")
decrit("custom_redistr_satisfied", all, which = all$vote_factor == "Non-voter, PNR or Other")

sum(pmax(400*12, world_income_after_tax("top3")))/sum(current_inc) # 1.002
sum(pmax(250*12, world_income_after_tax("top1")))/sum(current_inc) # 1.0004
sum(mean_custom_redistr[["all"]])/sum(current_inc) # 99%
sum(current_inc)
current_inc[970] # 79.3k
current_inc[971] # 80.5k
current_inc[987] # 120k 987<->quantile 98.6, i.e. top 1.4%
current_inc[990] # 131k


###### Well-being #####
with(e, summary(lm(well_being ~ variant_well_being))) 
with(e, summary(lm(well_being ~ variant_well_being_scale * variant_well_being_wording))) 

sapply(c("all", countries[-9]), function(c) print(decrit(d(c)$group_defended)))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$group_defended > 0, na.rm = T), 3))
sapply(c("all", countries[-9]), function(c) round(mean(d(c)$my_tax_global_nation > 0, na.rm = T), 3)) # corresponds well
sapply(variables_why_hic_help_lic, function(c) print(decrit(e[[c]])))

sapply(c("all", countries[-9]), function(c) round(mean(d(c)$survey_biased > 0, na.rm = T), 3))

sapply(paste0(pilot_countries, "p"), function(c) decrit(paste0("vote_", sub("p", "", c)), d(c)))
sapply(paste0(pilot_countries, "p"), function(c) print(decrit("voted", d(c))))

sapply(c("all", countries[-9]), function(c) round(mean(d(c)$convergence_support %in% "Yes"), 2))


##### Comments #####
e$comment_field[!is.na(e$comment_field) & !e$country %in% "PL"]


##### Influence order ######
summary(lm((ncqg > 2) ~ (ncqg_order == 7), data = e))
summary(lm((ncqg > 3) ~ (ncqg_order == 7), data = e))
summary(lm(split_many_global ~ mean_order_many_global, data = e))
summary(lm(revenue_split_few_global ~ revenue_split_few_order_global, data = e))
summary(lm(maritime_split_ldc ~ maritime_split_order_ldc, data = e))
summary(lm((vote_intl_coalition == "More likely") ~ (vote_intl_coalition_order_more_likely == 1), data = e))
summary(lm(why_hic_help_lic_duty ~ why_hic_help_lic_order_duty, data = e))
summary(lm(why_hic_help_lic_interest ~ why_hic_help_lic_order_interest, data = e))
summary(lm(transfer_how_cash_unconditional ~ (transfer_how_order_cash_unconditional == 7), data = e))
summary(lm((transfer_how_cash_unconditional > 0) ~ (transfer_how_order_cash_unconditional == 7), data = e))
summary(lm((transfer_how_social_protection > 0) ~ (transfer_how_order_cash_unconditional == 7), data = e))
summary(lm((transfer_how_agencies > 0) ~ (transfer_how_order_cash_unconditional == 7), data = e))
summary(lm((transfer_how_govt_conditional > 0) ~ (transfer_how_order_cash_unconditional == 7), data = e))
summary(lm((transfer_how_govt_unconditional > 0) ~ (transfer_how_order_cash_unconditional == 7), data = e))


##### Attrition #####
# vote = end sociodemos = 21; 26% dropout at 34 (revenue_split), 7% at 33 (conjoint), 8% at 49 (likely_solidarity), 5% at 59 (scenarios)
100*round(table(a$progress[!a$progress %in% 100])/sum(!a$finished), 2) 
sum(!a$finished)/sum(is.na(a$excluded)) # 20.6% dropout
sum(!a$finished & a$progress > 21)/sum(is.na(a$excluded) & a$progress > 21) # 14% dropout after sociodemos
sum(!a$finished & a$progress == 39)/sum(is.na(a$excluded) & a$progress > 21) # 14% dropout after sociodemos, incl. 5% at revenue_split
summary(lm(!finished ~ vote_US, data = a, subset = is.na(a$excluded)))
summary(lm(!finished ~ vote_PL, data = a, subset = is.na(a$excluded)))
summary(lm(!finished ~ vote_GB, data = a, subset = is.na(a$excluded)))
decrit(a$revenue_split_few_global[a$progress == 34])
decrit(US$finished[is.na(US$excluded)], miss= T)
summary(lm(reg_formula("!finished", variables_sociodemos), data = a, subset = is.na(a$excluded))) 
# Non-voters are ~6pp more likely to drop out (left more likely than right); likely millionaires, man, young, rich, high educ less likely
# Say there is differential attrition but unlikely to affect experiments as sociodemos explain under 12% of variance
summary(lm(reg_formula("share_solidarity_supported", variables_sociodemos), data = all, weights = weight)) 


##### Most correlated variable #####
e$itw <- e$interview == "Yes"
variables_interest <- c(variables_solidarity_support, "wealth_tax_support", "top_tax_support", "reparations_support", "ncs_support", "gcs_support", "ics_support", "convergence_support",
  variables_global_movement, variables_why_hic_help_lic, "revenue_split_few_global", variables_transfer_how, "sustainable_future", "likely_solidarity", "gcs_belief", "humanist", "universalist", 
 "individualist", "nationalist", "ncqg", "vote_intl_coalition", "maritime_split_ldc", "my_tax_global_nation", "group_defended", "ncqg_fusion",  "share_solidarity_supported", "itw")
cors <- cor(e[, variables_interest], use = "pairwise.complete.obs")
corrplot(cors)
sort(rowMeans(abs(cors), na.rm = T)) # share_solidarity_supported .42, solidarity_support_ncqg_300bn 40, my_tax_global_nation .35, vote_intl_coalition .35, ncqg .35, global_movement_no .34, 

cor(my_taxes_global_nation, my_taxes_global_nation_2023, use = "complete.obs") # .72
cor(my_taxes_global_nation[-9], global_nation[5,3:12], use = "complete.obs") # .81
cor(my_taxes_global_nation_2023[-9], global_nation[5,3:12], use = "complete.obs") # .69
wtd.mean(my_taxes_global_nation_2023, adult_pop, na.rm = T) # 55.7%
wtd.mean(my_taxes_global_nation, adult_pop, na.rm = T) # 44.8%
with(all[all$my_tax_global_nation != 0,], wtd.mean(my_tax_global_nation > 0, weight, na.rm = T)) # 59.5%
