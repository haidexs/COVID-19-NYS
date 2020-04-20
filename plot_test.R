# plot test
library(shiny)
library(datasets)
library(tidyverse)
library(extrafont)
library(ggplot2)
library(ggthemes)
library(patchwork)

covid_data = read.csv("./dataset.csv", header=TRUE, sep=",")
covid_data$Date = format(as.Date(covid_data$Date), "%m/%d")

region_code = "NYS"

if(identical(region_code, "NYS")) {
  data_to_plot = select(covid_data, Date, NYS.NewPositive, NYS.NewTested, NYS.PositiveRate, NYS.NewHospitolized1, NYS.NewDeath) %>%
    rename(MMDD = Date, NYSNewPositive = NYS.NewPositive, NYSNewTested = NYS.NewTested,
             NYSPosRate = NYS.PositiveRate, NYSNewAdmitted = NYS.NewHospitolized1, NYSNewDeath = NYS.NewDeath)
}
if(identical(region_code, "NYC")) {
  data_to_plot = select(covid_data, Date, NYC.NewPositive, NYC.NewTested, NYC.PositiveRate) %>%
    rename(日期 = Date, NYC新增确诊数 = NYC.NewPositive, NYC新增检测数 = NYC.NewTested,
             NYC单日确诊率 = NYC.PositiveRate)
}
if(identical(region_code, "LIC")) {
  data_to_plot = select(covid_data, Date, LIC.TotalPositive, LIC.TotalTested) %>%
    rename(日期 = Date, LIC累计确诊数 = LIC.TotalPositive, LIC累计检测数 = LIC.TotalTested)
}

col_tested = rgb(0.95, 0.67, 0.33)
col_positive = rgb(0.17, 0.49, 0.69)
col_admitted = rgb(0.86, 0.17, 0.14)
col_death = rgb(0.2, 0.2, 0.2)

coeff = 0.23
g = ggplot(data_to_plot, aes(x=MMDD)) +
  geom_bar(aes(y=NYSNewTested, fill=col_tested), stat="identity", size=0.1, alpha=0.9) + 
  geom_bar(aes(y=NYSNewPositive, fill=col_positive), stat="identity", size=0.1, alpha=1) + 
  geom_line(aes(y=NYSNewDeath/coeff, colour=col_death), size=2, na.rm = TRUE, group = 1) + # Divide by 10 to get the same range than the temperature
  geom_line(aes(y=NYSNewAdmitted/coeff, colour=col_admitted), size=2, na.rm=TRUE, group=1) + 
  
  scale_y_continuous(
    # Features of the first axis
    name = "检测/确诊数",
    breaks = seq(0,max(data_to_plot$NYSNewTested, na.rm = TRUE),10000),
    minor_breaks = seq(0,max(data_to_plot$NYSNewTested, na.rm = TRUE),2500),
    labels = seq(0,max(data_to_plot$NYSNewTested, na.rm = TRUE),2500),
    # name = "Axis 1",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="住院/死亡数",breaks = seq(0,max(data_to_plot$NYSNewAdmitted, na.rm = TRUE)/coeff,500))
    # sec.axis = sec_axis(~.*coeff, name="Axis 2")
  ) + 
  # labs(title = "NYS COVID-19") +
  labs(title = "纽约州新冠疫情数据", x = "日期") +
  # scale_x_date(name = "日期", date_breaks = "3 days") + 
  theme_bw()
  
g + theme(text=element_text(family="SimHei", size=8), legend.position = c(0.1, 0.83)) + 
  theme(plot.title = element_text(face = "bold", size = 10)) + 
  scale_color_manual(name = NULL,
                     labels = c("纽约州新增检测","纽约州新增确诊","纽约州新增死亡","纽约州新增住院"),
                     breaks = c(col_tested, col_positive, col_death, col_admitted),
                     values = c(col_tested, col_positive, col_death, col_admitted), 
                     aesthetics = c("fill","colour")) + 
  theme(axis.text.x = element_text(angle=45, hjust=0.8), aspect.ratio = 0.5)
# aesthetics = c("color","fill"))
          #, legend.title = c("纽约州新增确诊","纽约州新增检测","纽约州新增死亡","纽约州新增住院"))
