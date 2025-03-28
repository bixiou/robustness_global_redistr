##### Labels vars #####
labels_vars <- c(
  # "maritime_split_ldc" = "Maritime levy revenue preferred allocation to:\nSustainable transition in LDCs",
  # "maritime_split_companies" = "Maritime levy revenue preferred allocation to:\nShipping companies to mitigate price increases",
  # "maritime_split_decarbonization" = "Maritime levy revenue preferred allocation to:\nRD&D of zero-emission fuels and ships",
  "maritime_split_ldc" =       "Sustainable transition in LDCs",
  "maritime_split_companies" = "Shipping companies to mitigate price increases",
  "maritime_split_decarbonization" = "RD&D of zero-emission fuels and ships",
  setNames(names(p), names(p))) # TODO!
labels_vars_country <- list()
for (c in pilot_countries) labels_vars_country[[c]] <- labels_vars


##### heatmaps_defs #####
heatmaps_defs <- list()
heatmaps_defs <- list(
  "gcs_support" = list(vars = "gcs_support", conditions = ">= 1"), 
  "duration" = list(vars = variables_duration, conditions = ""),
  "share_solidarity_supported" = list(vars = c("share_solidarity_supported"), conditions = c("")),
  "transfer_how" = list(vars = variables_transfer_how, conditions = ">= 1"), 
  # "solidarity_support" = list(vars = variables_solidarity_support), 
  "global_movement" = list(vars = variables_global_movement, conditions = ">= 1"), 
  "why_hic_help_lic" = list(vars = variables_why_hic_help_lic, conditions = ">= 1")
)
# TODO heatmaps: transfer_how, solidarity_support, global_movement, why_help_lic

##### vars_heatmaps #####
vars_heatmaps <- c("variables_transfer_how", "solidarity_support", "global_movement", "why_hic_help_lic") 
# TODO: special = all; automatize conditions = ">= 1" for binary vars; automatize folder creation; remove dependencies on objects such as countries_names_fr; remove NULL

heatmaps_defs <- fill_heatmaps(vars_heatmaps, heatmaps_defs)

# heatmap_multiple(heatmaps_defs)


##### barres_defs #####
barres_defs <- list( # It cannot contained unnamed strings (e.g. it can contain "var" = "var" but not simply "var")
  
  # "understood_each" = list(vars = variables_understood, width = 850), 
  # "negotiation" = list(width = 940), 
  # "points_mean" = list(vars = variables_points_us_agg, width = 850, sort = FALSE, add_means = T, show_legend_means = T, transform_mean = function(x) return(x/100)), # 1080 points_us
)

vars_barres <- c("ncqg", "ncqg_full", "maritime_split", variables_maritime_split) 

barres_defs <- fill_barres(vars_barres, barres_defs) # , df = us1

vars_barresN <- vars_barres
barresN_defs <- fill_barres(vars_barresN, along = "country_name")


##### Plot #####
# barres_multiple(barresN_defs[c("foreign_aid_raise_support")])
barres_multiple(barres_defs["maritime_split"], export_xls = T)
barres_multiple(barresN_defs[variables_maritime_split], export_xls = T)
barres_multiple(barres_defs)

# heatmap_multiple(heatmaps_defs["var"])
heatmap_multiple(heatmaps_defs)
heatmap_multiple(heatmaps_defs["global_movement"])


##### Maritime #####
mean_maritime_split <- array(NA, dim = c(3, 4), dimnames = list(variables_maritime_split, paste0(pilot_countries_all, "p")))
for (c in paste0(pilot_countries_all, "p")) for (v in variables_maritime_split) mean_maritime_split[v, c] <- wtd.mean(d(c)[[v]], d(c)$weight)
barres(mean_maritime_split/100, save = T, file = "../figures/country_comparison/mean_maritime_split", export_xls = T, 
       miss = F, rev_color = T, sort = F, legend = c("Net-zero fuels & ships", "Shipping companies", "LDCs"), labels = rev(c(countries_names, "All")))
