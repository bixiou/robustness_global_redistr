setwd("C:/Users/fabre/Documents/www/robustness_global_redistr/code_robustness")
load(".RData")
v <- all$gcs_support_control
cat("non-NA count:", sum(!is.na(v)), "\n")
cat("unique values:", paste(head(unique(v[!is.na(v)]), 20), collapse=", "), "\n")
cat("class:", class(v), "\n")
# Check gcs_support non-NA values directly
cat("gcs_support unique:", paste(unique(all$gcs_support[!is.na(all$gcs_support)]), collapse=", "), "\n")
# Check comparison
cat("v == 'Yes' first few non-NA:", paste(head(v[!is.na(v)] == "Yes", 10), collapse=", "), "\n")
cat("as.character first few non-NA:", paste(head(as.character(v[!is.na(v)]), 10), collapse=", "), "\n")
# Check variant_warm_glow
cat("variant_warm_glow table:\n")
print(table(all$variant_warm_glow))
cat("gcs_support for control group:\n")
print(table(all$gcs_support[all$variant_warm_glow == "None"]))
