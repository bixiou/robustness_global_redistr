setwd("C:/Users/fabre/Documents/www/robustness_global_redistr/code_robustness")
results <- readRDS("../data_ext/mrp_results.rds")

mrp_summary <- do.call(rbind, lapply(names(results), function(key) {
  r <- results[[key]]
  if (is.null(r)) return(NULL)
  pred_label <- if (grepl("extended", key)) "extended" else "default"
  data.frame(
    country      = r$country,
    outcome      = r$outcome,
    predictors   = pred_label,
    estimate_mrp = round(r$estimate_mrp, 4),
    estimate_raw = round(r$estimate_raw, 4),
    n_survey     = r$n_survey,
    stringsAsFactors = FALSE
  )
}))

mrp_summary <- mrp_summary[order(mrp_summary$country, mrp_summary$outcome, mrp_summary$predictors), ]
print(mrp_summary, row.names = FALSE)
write.csv(mrp_summary, "../data_ext/mrp_summary.csv", row.names = FALSE)
message("mrp_summary.csv updated.")
