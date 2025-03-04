##### Functions #####
remove_id <- function(file, folder = "../data_raw/") {
  filename <- paste(folder, file, ".csv", sep = "")
  
  filename_copy <- paste("./deprecated/", file, sample.int(10^5, 1), ".csv", sep = "") # in case the three last lines don't work
  file.copy(filename, filename_copy)
  data <- read_csv(filename_copy)
  data <- data[,which(!(names(data) %in% c("PSID", "ResponseId", "PID", "tic")))]
  write_csv(data, filename, na = "")
  file.remove(filename_copy)
} 


##### Load data #####
survey_list <- all_surveys()
pilots <- paste0(c("PL", "GB", "US"), "p") 
pilot_names <- setNames(paste0(c("PL", "GB", "US"), "_pilot"), pilots) 
# cut=0 at March 4 2025, 00:05 Paris time (after 46 PL; 118 GB; 46 US)
for (p in pilots) {
  print(p)
  data <- fetch_survey(survey_list$id[survey_list$name == pilot_names[p]], include_display_order = T, verbose = T, convert = F)
  data <- data[,which(!(names(data) %in% c("PSID", "ResponseId", "PID", "tic", "IPAddress", "m")))]
  for (v in names(data)) label(data[[v]]) <- c(v = paste0(v, ": ", label(data[[v]])))
  write.csv(data, paste0("../data_raw/", p, ".csv"), quote = F, na = "", row.names = F)
  eval(str2expression(paste0(p, " <- data")))
}
GBp <- fetch_survey(survey_list$id[survey_list$name == "GB_pilot"], include_display_order = T, verbose = T, convert = F) # labels using sjlabelled package
write.csv(d("GBp"), paste0("../data_raw/GBp.csv"), quote = F, na = "", row.names = F)
# Slightly different from manual export .csv: no second row with question text; timezone is different (in e.g. startDate); True => TRUE; income bug; some additional "" are removed
View(GBp)
  
for (v in names(GBp)[1:80]) { print(decrit(v, GBp)); print("____________");}
for (v in names(GBp)[81:160]) { print(decrit(v, GBp)); print("____________");}
for (v in names(GBp)[161:240]) { print(decrit(v, GBp)); print("____________");}
for (v in names(GBp)) if (grepl("duration", v)) { print(decrit(as.numeric(GBp[[v]]))); print("____________");}

table(GBp$ncqg)
table(GBp$group_defended)
PLp$comment_field
decrit(GBp$Q_TerminateFlag)

stats_exclusion <- function(e) {
  
}

