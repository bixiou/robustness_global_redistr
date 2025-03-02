survey_list <- all_surveys()
plp <- fetch_survey(survey_list$id[survey_list$name == paste0("PL_pilot", " - robustness_global_redistr")], 
                    include_display_order = T, verbose = T, convert = F, col_types = ) # labels using sjlabelled package
write.csv(plp, paste0("../data_raw/", "plp", ".csv"), quote = F, na = "", row.names = F)
# Slightly different from manual export .csv: no second row with question text; timezone is different (in e.g. startDate); True => TRUE; income bug; some additional "" are removed
View(plp)
  
package("memisc")
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

#### Tests #####

package("labelled")
test <- labelled(c(1, NA, -1), c("No" = 0, "Yes" = 1, "PNR" = -1))
na_values(test) <- c(-1)

test <- include.missings(as.item(c(1, NA, -1), labels = structure(c(0, 1, -1), names = c("No", "Yes", "PNR")), missing.values = c(NA, -1), annotation = "test"))

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

(bar <- as.factor(as.character(test)))
bar <- relevel(bar, 'PNR')

df <- within(df, { # Shorthand for ds <- within(ds,...)
  df$test_im <- include.missings(df$test)
}) 
lm(c(T, T, T) ~ test, data = df)$rank
