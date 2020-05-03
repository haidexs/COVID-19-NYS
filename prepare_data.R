library(tidyverse)

## for new data (from 5/2)
old_data = read.csv("positive_tests_zipcode.csv", header = TRUE)
old_data$Date = format(as.Date(old_data$Date, format = "%m/%d"), "%m/%d")

today_data = read.csv("../coronavirus-data/today.csv", header = FALSE)
colnames(today_data) = c("Zipcode", "Positive", "Tested", "Percent")
today_data$Zipcode[1] = "Unknown"
today_data$Date = format(Sys.Date(), "%m/%d")
new_data = rbind(today_data, old_data)


NYC_Total_Pos_Tested_by_Date = as.data.frame(new_data %>% group_by(Date) %>% summarise(TotalPositive = sum(Positive), TotalTested = sum(Tested)))

NYC_dataset = read.csv("NYC_dataset.csv", header = TRUE, sep = ",")
NYC_dataset$Date = format(as.Date(NYC_dataset$Date, format = "%m/%d"), "%m/%d")

today_case_hosp_death = read.csv("../coronavirus-data/case-hosp-death.csv", header = TRUE, sep = ",")
today_case_hosp_death$DATE_OF_INTEREST = format(as.Date(today_case_hosp_death$DATE_OF_INTEREST, format = "%m/%d"), "%m/%d")

NYC_dataset$NYC.New.Positive = today_case_hosp_death$CASE_COUNT
NYC_dataset$NYC.New.Hospitolized = today_case_hosp_death$HOSPITALIZED_COUNT
NYC_dataset$NYC.New.Death = today_case_hosp_death$DEATH_COUNT
# NYC_dataset$Date = format(as.Date(NYC_dataset$Date, format = "%m/%d/%y"), "%m/%d")

NYC_dataset = NYC_dataset %>% add_row(Date = format(Sys.Date(), "%m/%d"))
NYC_dataset$LIC.Total.Positive[as.Date(NYC_dataset$Date, format("%m/%d")) == as.Date(Sys.Date(), "%m/%d")] = 
  today_data$Positive[today_data$Zipcode == 11101]
NYC_dataset$LIC.Total.Tested[as.Date(NYC_dataset$Date, format("%m/%d")) == as.Date(Sys.Date(), "%m/%d")] = 
  today_data$Tested[today_data$Zipcode == 11101]
NYC_dataset$ColumbiaUniv.Total.Positive[as.Date(NYC_dataset$Date, format("%m/%d")) == as.Date(Sys.Date(), "%m/%d")] = 
  today_data$Positive[today_data$Zipcode == 10027]
NYC_dataset$ColumbiaUniv.Total.Tested[as.Date(NYC_dataset$Date, format("%m/%d")) == as.Date(Sys.Date(), "%m/%d")] = 
  today_data$Tested[today_data$Zipcode == 10027]


for (i in 1:length(NYC_Total_Pos_Tested_by_Date$Date)){
  NYC_dataset$NYC.Total.Tested[as.Date(NYC_dataset$Date, format = "%m/%d") == as.Date(NYC_Total_Pos_Tested_by_Date$Date[i], format = "%m/%d")] = 
    NYC_Total_Pos_Tested_by_Date$TotalTested[i]
}

for (i in 2:length(NYC_dataset$Date)){
  NYC_dataset$NYC.New.Tested[i] = as.integer(NYC_dataset$NYC.Total.Tested[i]) - as.integer(NYC_dataset$NYC.Total.Tested[i-1])
}

write.csv(new_data, "positive_tests_zipcode.csv", row.names = FALSE)
write.csv(NYC_dataset, "NYC_dataset.csv", row.names = FALSE)

#######
## gather data from beginning to 5/1 
# zip_code_data_raw = read.csv("../coronavirus-data/tmp3.csv", header = FALSE)
# colnames(zip_code_data_raw) = c("Zipcode", "Positive", "Tested", "Percent")
# 
# start_row_number = which(zip_code_data_raw$Zipcode == 10000)
# end_row_number = c(start_row_number[2:length(start_row_number)] - 1, length(zip_code_data_raw$Zipcode))
# Date_list = seq(from = as.Date("5/1", format = "%m/%d"), to = as.Date("4/1", format = "%m/%d"), by = "-1 day")[c(1:25, 27,28,29,31)]
# zip_code_data_raw$Date = ""
# 
# for (i in (1:length(start_row_number))){
#   zip_code_data_raw$Zipcode[start_row_number[i]] = "Unknown"
#   nn = end_row_number[i] - start_row_number[i] + 1
#   zip_code_data_raw[start_row_number[i]:end_row_number[i], 5] = rep(format(Date_list[i], "%m/%d"), nn)
# }
# 
# zip_code_data_raw = zip_code_data_raw[!(zip_code_data_raw$Zipcode == 11697 & zip_code_data_raw$Positive == 52 & zip_code_data_raw$Date == "04/10"), ]
# zip_code_data_raw = zip_code_data_raw[!(zip_code_data_raw$Zipcode == 99999), ]
# 
# write.csv(zip_code_data_raw, "positive_tests_zipcode.csv", row.names = FALSE)