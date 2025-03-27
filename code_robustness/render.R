##### Labels vars #####
labels_vars <- setNames(names(p), names(p)) # TODO!
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

vars_barres <- c("ncqg", "ncqg_full") 

barres_defs <- fill_barres(vars_barres, barres_defs) # , df = us1

vars_barresN <- c("ncqg", "ncqg_full") 
barresN_defs <- fill_barres(vars_barresN, along = "country_name")


##### Plot #####
# barres_multiple(barresN_defs[c("foreign_aid_raise_support")])
barres_multiple(barres_defs["ncqg"])
barres_multiple(barresN_defs["group_defended"])
barres_multiple(barres_defs)

# heatmap_multiple(heatmaps_defs["var"])
heatmap_multiple(heatmaps_defs)
heatmap_multiple(heatmaps_defs["global_movement"])


##### Maritime #####

